with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Interfaces;
with CCL.Manifests;
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
     (1 .. CCL.Manifests.MAX_DECLARATION_LENGTH);
   Last : Ada.Streams.Stream_Element_Offset;
   Source, Catalog_Source : String (1 .. CCL.Manifests.MAX_DECLARATION_LENGTH);
   Source_Length, Catalog_Length : Natural;
   Result : CCL.Manifests.Compilation_Result;
   Ada_Output : Ada.Text_IO.File_Type;
   Hex : constant String := "0123456789abcdef";

   procedure Read_Source (Path : String; Text : out String; Length : out Natural) is
   begin
      Text := [others => ' '];
      Length := 0;
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Path);
      if Ada.Streams.Stream_IO.Size (Input) > Bytes'Length then
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
      Create (Ada_Output, Out_File, Argument (4));
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
      Create (Output, Out_File, Argument (4));
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
   if Argument_Count not in 2 | 4 or else
     (Argument_Count = 4 and then
      Argument (3) not in "--ada-output" | "--rust-output")
   then
      Put_Line (Standard_Error,
        "usage: ccl-manifest CATALOG.ccl MANIFEST.ccl [--ada-output bindings.ads | --rust-output bindings.rs] > manifest.S");
      Set_Exit_Status (Failure);
      return;
   end if;
   Read_Source (Argument (1), Catalog_Source, Catalog_Length);
   Read_Source (Argument (2), Source, Source_Length);
   CCL.Manifests.Compile
     (Source (1 .. Source_Length), Catalog_Source (1 .. Catalog_Length), Result);
   if not Result.Success then
      Put_Line (Standard_Error,
        Argument (if Result.In_Catalog then 1 else 2) &
        ": character" & Natural'Image (Result.Position) & ": " &
        CCL.Manifests.Diagnostic_Code'Image (Result.Diagnostic) & " / " &
        CCL.Language.Diagnostic_Code'Image (Result.Expression_Diagnostic));
      Set_Exit_Status (Failure);
      return;
   end if;
   if Argument_Count = 4 then
      if Argument (3) = "--rust-output" then Emit_Rust;
      else Emit_Ada;
      end if;
   end if;
   Emit (".cubit.id", Result.Identity);
   Emit (".cubit.caps", Result.Capabilities);
   Emit (".cubit.access", Result.Access_Scopes);
   Emit (".cubit.streams", Result.Streams);
   Put_Line (".section .note.GNU-stack,"""",@progbits");
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot access input/output, or input exceeds 4096-byte bound");
      Set_Exit_Status (Failure);
end Main;
