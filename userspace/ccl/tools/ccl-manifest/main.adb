with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces;
with CCL.Manifests;
with CuBit.Failures;
with CuBit.Startup_Tables;
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
   --  CATALOG MANIFEST, then options as pairs: --startup-output FILE (the
   --  Ada Startup package), --rust-output FILE (binding names), --schema FILE.
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
         Argument (Index) in "--rust-output" | "--startup-output" | "--schema"));
   --  Where the startup binding goes (none without --startup-output).
   function Startup_Path return String is (Option ("--startup-output"));
   --  The schema's default home: interfaces/ beside the catalogs directory.
   function Schema_Path return String is
     (if Option ("--schema")'Length > 0 then Option ("--schema")
      else Ada.Directories.Containing_Directory
             (Ada.Directories.Containing_Directory (Ada.Directories.Full_Name (Argument (1)))) &
           "/interfaces/executable-manifest.ccl");
   Result : CCL.Manifests.Compilation_Result;
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
            --  The startup entry's name (cubit::startup looks it up).
            Put_Line (Output, "pub const BINDING_" & Name & " : &str = """ &
                      Binding.Name.Data (1 .. Binding.Name.Length) & """;");
         end;
      end loop;
      Close (Output);
   end Emit_Rust;
   ---------------------------------------------------------------------------
   --  The startup binding (CAP-002): package Startup, one function per
   --  startup capability, named after its binding, returning that kind's
   --  private handle (Required) or an Optional_Handle the caller must
   --  examine (Optional); Verify checks the loaded table against the
   --  declaration. No numbers: handles come from the startup table only.
   ---------------------------------------------------------------------------
   package ST renames CuBit.Startup_Tables;

   --  "pointer-events" -> "Pointer_Events".
   function Ada_Name (Binding : String) return String is
      Result : String := Binding;
      Start : Boolean := True;
   begin
      for C of Result loop
         if C = '-' then
            C := '_';
            Start := True;
         elsif Start then
            C := Ada.Characters.Handling.To_Upper (C);
            Start := False;
         end if;
      end loop;
      return Result;
   end Ada_Name;

   --  Names the generated package cannot use as function names: Ada's
   --  reserved words, Standard (the generated code's anchor) and Verify.
   Unusable : constant String :=
     " abort abs abstract accept access aliased all and array at begin body case constant" &
     " declare delay delta digits do else elsif end entry exception exit for function generic" &
     " goto if in interface is limited loop mod new not null of or others out overriding" &
     " package pragma private procedure protected raise range record rem renames requeue" &
     " return reverse select separate some subtype synchronized tagged task terminate then" &
     " type until use when while with xor standard verify ";

   --  The CuBit.Handles type each kind's handle is (DMA and Resource
   --  entries are not capabilities and get no accessor).
   function Handle_Type (Kind : ST.Capability_Kind) return String is
     (case Kind is
         when ST.Endpoint | ST.Render | ST.Network => "Endpoint",
         when ST.Device_Memory | ST.Framebuffer    => "Device_Memory",
         when ST.IO_Ports     => "IO_Ports",
         when ST.Interrupt    => "Interrupt",
         when ST.Notification => "Notification",
         when ST.Scheduling   => "Scheduling",
         when ST.Process      => "Process",
         when ST.DMA | ST.Resource => "");

   --  An enumeration literal's image in Ada's mixed case (IO and DMA stay
   --  upper case, as declared).
   function Mixed (Image : String) return String is
      Result : String := Ada.Characters.Handling.To_Lower (Image);
      Start : Boolean := True;
   begin
      for I in Result'Range loop
         if Start then
            Result (I) := Ada.Characters.Handling.To_Upper (Result (I));
         end if;
         Start := Result (I) = '_';
      end loop;
      if Result'Length >= 2 and then Result (Result'First .. Result'First + 1) = "Io" then
         Result (Result'First + 1) := 'O';
      elsif Result = "Dma" then
         Result := "DMA";
      end if;
      return Result;
   end Mixed;

   function Kind_Image (Kind : ST.Capability_Kind) return String is
     (Mixed (ST.Capability_Kind'Image (Kind)));

   --  False (with a message) if a binding cannot be a function name.
   function Startup_Names_Usable return Boolean is
   begin
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
         begin
            if Ada.Strings.Fixed.Index (Unusable, " " & Name & " ") > 0 then
               Put_Line (Standard_Error, Argument (2) & ": binding """ & Name &
                         """ cannot name a startup function (an Ada reserved word, Standard or Verify). " &
                         "Fix: rename the binding.");
               return False;
            end if;
         end;
      end loop;
      return True;
   end Startup_Names_Usable;

   procedure Emit_Startup is
      Output : Ada.Text_IO.File_Type;
      --  .cubit.id: magic (4), version (2), count (2), then the "id" pair:
      --  key length (1), value length (2), the key "id", the value.
      Value_Length_At : constant := 10;
      Value_At : constant := 14;
      Identity_Length : constant Natural :=
        Natural (Result.Identity.Data (Value_Length_At)) +
        256 * Natural (Result.Identity.Data (Value_Length_At + 1));
      Identity : String (1 .. Identity_Length);
   begin
      for I in Identity'Range loop
         Identity (I) := Character'Val (Result.Identity.Data (Value_At + I - 1));
      end loop;
      Create (Output, Out_File, Startup_Path);
      Put_Line (Output, "--  Generated with the ELF manifest; do not edit.");
      Put_Line (Output, "--  Startup capabilities of " & Identity & ", by name (CAP-002).");
      Put_Line (Output, "--  Handle limit: " &
                (case Result.Handles.Kind is
                    when ST.Standard_Limit => "standard (" &
                      Ada.Strings.Fixed.Trim (Natural'Image (ST.Standard_Handle_Limit), Ada.Strings.Left) & ")",
                    when ST.At_Most => "at most" & ST.Handle_Count'Image (Result.Handles.Count),
                    when ST.Kernel_Limit => "the kernel limit") & ".");
      Put_Line (Output, "pragma Ada_2022;");
      Put_Line (Output, "with CuBit.Handles.Startup;");
      Put_Line (Output, "with CuBit.Startup_State;");
      Put_Line (Output, "with CuBit.Startup_Tables;");
      Put_Line (Output, "package Startup with SPARK_Mode is");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
            use type ST.Need;
            Optional : constant Boolean := Binding.Requirement = ST.Optional;
            Kind_Name : constant String := "Standard.CuBit.Startup_Tables." & Kind_Image (Binding.Kind);
            T : constant String := Handle_Type (Binding.Kind);
         begin
            Put_Line (Output, "");
            Put_Line (Output, "   --  " & Kind_Image (Binding.Kind) & " """ & Name & """, " &
                      (if Optional then "optional" else "required") & ".");
            if T = "" then
               Put_Line (Output, "   --  (not a capability: no handle)");
            elsif Optional then
               Put_Line (Output, "   function " & Ada_Name (Name) &
                         " return Standard.CuBit.Handles.Startup.Optional_" & T & " is");
               Put_Line (Output, "     (Standard.CuBit.Handles.Startup.Optional_" & T &
                         "_Named (""" & Name & """, " & Kind_Name & "));");
            else
               Put_Line (Output, "   function " & Ada_Name (Name) &
                         " return Standard.CuBit.Handles." & T & " is");
               Put_Line (Output, "     (Standard.CuBit.Handles.Startup." & T &
                         "_Named (""" & Name & """, " & Kind_Name & "));");
            end if;
         end;
      end loop;
      Put_Line (Output, "");
      Put_Line (Output, "   --  Whether the loaded startup table holds every capability above,");
      Put_Line (Output, "   --  with its kind; call once at startup and stop if it is not Ready.");
      Put_Line (Output, "   function Verify return Standard.CuBit.Startup_State.Verification is");
      if Result.Binding_Count = 0 then
         Put_Line (Output, "     (Standard.CuBit.Startup_State.Verify ([]));");
      else
         Put_Line (Output, "     (Standard.CuBit.Startup_State.Verify");
         for Index in 1 .. Result.Binding_Count loop
            declare
               Binding : CCL.Manifests.Named_Binding renames Result.Bindings (Index);
               Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
            begin
               Put_Line (Output, (if Index = 1 then "        ([" else "          ") &
                         "(Standard.CuBit.Startup_Tables.To_Name (""" & Name & """), " &
                         "Standard.CuBit.Startup_Tables." & Kind_Image (Binding.Kind) & ", " &
                         "Standard.CuBit.Startup_Tables." & Mixed (ST.Need'Image (Binding.Requirement)) & ")" &
                         (if Index = Result.Binding_Count then "]));" else ","));
            end;
         end loop;
      end if;
      Put_Line (Output, "end Startup;");
      Close (Output);
   end Emit_Startup;
begin
   if not Options_Valid then
      Put_Line (Standard_Error,
        "usage: ccl-manifest CATALOG.ccl MANIFEST.ccl [--startup-output startup.ads] " &
        "[--rust-output bindings.rs] [--schema executable-manifest.ccl] > manifest.S");
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
   if Startup_Path'Length > 0 then
      if not Startup_Names_Usable then
         Set_Exit_Status (Failure);
         return;
      end if;
      Emit_Startup;
   end if;
   if Option ("--rust-output")'Length > 0 then
      Emit_Rust;
   end if;
   Emit (".cubit.id", Result.Identity);
   Emit (".cubit.caps", Result.Capabilities);
   Emit (".cubit.access", Result.Access_Scopes);
   Emit (".cubit.resources", Result.Resources);
   Emit (".cubit.launch", Result.Launch);
   Emit (".cubit.description", Result.Description);
   Emit (".cubit.startup", Result.Startup);
   Put_Line (".section .note.GNU-stack,"""",@progbits");
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot access input/output, or input exceeds 4096-byte bound");
      Set_Exit_Status (Failure);
end Main;
