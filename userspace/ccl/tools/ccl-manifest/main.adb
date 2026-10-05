with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Interfaces;
with CCL.Manifests;
with CuBit.Failures;
with CCL.Language;

--  Linux-hosted file boundary only. No process execution or network access.
--  stdout receives assembly only after the complete declaration validates.
procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use type Ada.Streams.Stream_IO.Count;
   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.Unsigned_8;
   Input : Ada.Streams.Stream_IO.File_Type;
   Bytes : Ada.Streams.Stream_Element_Array
     (1 .. CCL.Language.MAX_SOURCE_LENGTH);
   Last : Ada.Streams.Stream_Element_Offset;
   Source, Catalog_Source : String (1 .. CCL.Manifests.MAX_DECLARATION_LENGTH);
   Source_Length, Catalog_Length : Natural;
   --  interfaces/executable-manifest.ccl: the CCL declarations typed
   --  manifests are checked against, found beside the catalogs directory.
   Schema_Source : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
   Schema_Length : Natural := 0;
   --  CATALOG MANIFEST, then options as pairs: --ada-output FILE,
   --  --rust-output FILE, --schema FILE.
   function Option (Name : String) return String is
   begin
      for Index in 3 .. Argument_Count - 1 loop
         if Argument (Index) = Name and then (Index - 3) mod 2 = 0 then
            return Argument (Index + 1);
         end if;
      end loop;
      return "";
   end Option;
   function Options_Valid return Boolean is
     (Argument_Count >= 2 and then Argument_Count mod 2 = 0 and then
      (for all Index in 3 .. Argument_Count =>
         (Index - 3) mod 2 = 1 or else
         Argument (Index) in "--ada-output" | "--rust-output" | "--schema"));
   --  The schema's default home: interfaces/ beside the catalogs directory.
   function Schema_Path return String is
     (if Option ("--schema")'Length > 0 then Option ("--schema")
      else Ada.Directories.Containing_Directory
             (Ada.Directories.Containing_Directory (Ada.Directories.Full_Name (Argument (1)))) &
           "/interfaces/executable-manifest.ccl");
   Result : CCL.Manifests.Compilation_Result;
   Ada_Output : Ada.Text_IO.File_Type;
   Hex : constant String := "0123456789abcdef";

   procedure Read_Source (Path : String; Text : out String; Length : out Natural) is
   begin
      Text := [others => ' '];
      Length := 0;
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Path);
      if Ada.Streams.Stream_IO.Size (Input) > Ada.Streams.Stream_IO.Count (Text'Length) then
         Ada.Streams.Stream_IO.Close (Input);
         raise Ada.Streams.Stream_IO.Use_Error;
      end if;
      Ada.Streams.Stream_IO.Read (Input, Bytes, Last);
      Ada.Streams.Stream_IO.Close (Input);
      Length := Natural (Last);
      for Index in 1 .. Length loop
         Text (Index) := Character'Val (Bytes (Ada.Streams.Stream_Element_Offset (Index)));
      end loop;
   end Read_Source;

   procedure Emit (Name : String; Item : CCL.Manifests.Section) is
   begin
      if Item.Length = 0 then return; end if;
      Put_Line (".section " & Name & ","""",@progbits");
      for Index in 1 .. Item.Length loop
         Put_Line (".byte 0x" & Hex (Natural (Item.Data (Index) / 16) + 1)
                   & Hex (Natural (Item.Data (Index) mod 16) + 1));
      end loop;
   end Emit;

   procedure Emit_Ada is
   begin
      Create (Ada_Output, Out_File, Option ("--ada-output"));
      Put_Line (Ada_Output, "-- Generated with the ELF manifest; do not edit.");
      if Result.Binding_Count > 0 then
         Put_Line (Ada_Output, "with Interfaces;");
      end if;
      Put_Line (Ada_Output, "package CCL_Manifest_Bindings with SPARK_Mode => On is");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : String := Binding.Name.Data (1 .. Binding.Name.Length);
         begin
            for C of Name loop
               if C = '-' then C := '_'; end if;
            end loop;
            Put_Line (Ada_Output, "   Slot_" & Name &
              " : constant Interfaces.Unsigned_64 :=" & Natural'Image (Binding.Slot) & ";");
         end;
      end loop;
      Put_Line (Ada_Output, "end CCL_Manifest_Bindings;");
      Close (Ada_Output);
   end Emit_Ada;

   procedure Emit_Rust is
      Output : Ada.Text_IO.File_Type;
   begin
      Create (Output, Out_File, Option ("--rust-output"));
      Put_Line (Output, "// Generated with the ELF manifest; do not edit.");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : String := Binding.Name.Data (1 .. Binding.Name.Length);
         begin
            for C of Name loop
               if C = '-' then C := '_';
               else C := Ada.Characters.Handling.To_Upper (C);
               end if;
            end loop;
            Put_Line (Output, "pub const SLOT_" & Name & " : u64 =" &
                      Natural'Image (Binding.Slot) & ";");
         end;
      end loop;
      Close (Output);
   end Emit_Rust;
begin
   if not Options_Valid then
      Put_Line (Standard_Error,
        "usage: ccl-manifest CATALOG.ccl MANIFEST.ccl [--ada-output bindings.ads | --rust-output bindings.rs] " &
        "[--schema executable-manifest.ccl] > manifest.S");
      Set_Exit_Status (Failure);
      return;
   end if;
   Read_Source (Argument (1), Catalog_Source, Catalog_Length);
   Read_Source (Argument (2), Source, Source_Length);
   if Ada.Directories.Exists (Schema_Path) then
      Read_Source (Schema_Path, Schema_Source, Schema_Length);
   end if;
   --  The interpreter recurses with large frames (about 240 KiB each), more
   --  than a default 8 MiB stack holds for a typed manifest and its schema,
   --  so the compilation runs on a task with room for it.
   declare
      task Compilation with Storage_Size => 256 * 1024 * 1024;
      task body Compilation is
      begin
         CCL.Manifests.Compile
           (Source (1 .. Source_Length), Catalog_Source (1 .. Catalog_Length), Result,
            Schema_Source (1 .. Schema_Length));
      end Compilation;
   begin
      null;
   end;
   if not Result.Success and then CuBit.Failures."/=" (Result.Why.Why, CuBit.Failures.Unspecified) then
      --  A typed manifest says which entry is wrong and how to fix it.
      declare
         Detail : constant String := Result.Why.Detail (1 .. Result.Why.Detail_Length);
      begin
         Put_Line (Standard_Error, Argument (2) & ": " & Detail &
                   (if Detail'Length > 0 and then Detail (Detail'Last) in '.' | '?' then "" else ".") &
                   (if Result.Why.Remedy_Length > 0
                    then " Fix: " & Result.Why.Remedy (1 .. Result.Why.Remedy_Length) & "." else ""));
      end;
      Set_Exit_Status (Failure);
      return;
   elsif not Result.Success then
      Put_Line (Standard_Error,
        Argument (if Result.In_Catalog then 1 else 2) &
        ": character" & Natural'Image (Result.Position) & ": " &
        CCL.Manifests.Diagnostic_Code'Image (Result.Diagnostic) & " / " &
        CCL.Language.Diagnostic_Code'Image (Result.Expression_Diagnostic));
      Set_Exit_Status (Failure);
      return;
   end if;
   if Option ("--ada-output")'Length > 0 or else Option ("--rust-output")'Length > 0 then
      if Option ("--rust-output")'Length > 0 then Emit_Rust;
      else Emit_Ada;
      end if;
   end if;
   Emit (".cubit.id", Result.Identity);
   Emit (".cubit.caps", Result.Capabilities);
   Emit (".cubit.access", Result.Access_Scopes);
   Emit (".cubit.resources", Result.Resources);
   Emit (".cubit.launch", Result.Launch);
   Emit (".cubit.description", Result.Description);
   Put_Line (".section .note.GNU-stack,"""",@progbits");
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot access input/output, or input exceeds 4096-byte bound");
      Set_Exit_Status (Failure);
end Main;
