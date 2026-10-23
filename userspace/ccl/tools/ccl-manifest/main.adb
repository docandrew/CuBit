with CuBit.Channel_Limits;
with CuBit.Channel_Declarations;
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
   --  Ada Startup package), --rust-output FILE (the Rust accessors),
   --  --c-output FILE (the C header), --schema FILE.
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
         Argument (Index) in "--rust-output" | "--c-output" | "--startup-output" | "--schema"));
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


   ---------------------------------------------------------------------------
   --  The startup binding (CAP-002): package Startup, one function per
   --  startup capability, named after its binding, returning that kind's
   --  private handle (Required) or an Optional_Handle the caller must
   --  examine (Optional), typed by the catalog's (client X) where it names
   --  one (CuBit.Handles.Services.X, CAP-003). Its body checks the loaded
   --  table against the declaration while the package elaborates, before
   --  the program's main (CuBit.Startup_Check). No numbers: handles come
   --  from the startup table only.
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
   --  reserved words, Standard (the generated code's anchor) and Expected
   --  (its declared table).
   Unusable : constant String :=
     " abort abs abstract accept access aliased all and array at begin body case constant" &
     " declare delay delta digits do else elsif end entry exception exit for function generic" &
     " goto if in interface is limited loop mod new not null of or others out overriding" &
     " package pragma private procedure protected raise range record rem renames requeue" &
     " return reverse select separate some subtype synchronized tagged task terminate then" &
     " type until use when while with xor standard expected ";

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
                         """ cannot name a startup function (an Ada reserved word, Standard or Expected). " &
                         "Fix: rename the binding.");
               return False;
            end if;
         end;
      end loop;
      return True;
   end Startup_Names_Usable;

   --  The manifest's identity, from its .cubit.id section: magic (4),
   --  version (2), count (2), then the "id" pair: key length (1), value
   --  length (2), the key "id", the value.
   function Identity_Text return String is
      Value_Length_At : constant := 10;
      Value_At : constant := 14;
      Length : constant Natural :=
        Natural (Result.Identity.Data (Value_Length_At)) +
        256 * Natural (Result.Identity.Data (Value_Length_At + 1));
      Text : String (1 .. Length);
   begin
      for I in Text'Range loop
         Text (I) := Character'Val (Result.Identity.Data (Value_At + I - 1));
      end loop;
      return Text;
   end Identity_Text;

   --  "pointer-events" -> "pointer_events": a binding as a Rust or C name.
   function Snake_Name (Binding : String) return String is
      Name : String := Binding;
   begin
      for C of Name loop
         if C = '-' then C := '_'; end if;
      end loop;
      return Name;
   end Snake_Name;

   --  Rust's keywords (strict, reserved and weak), which cannot name an
   --  accessor in the generated startup module.
   Rust_Unusable : constant String :=
     " as async await break const continue crate dyn else enum extern false fn for gen if impl in" &
     " let loop match mod move mut pub ref return self static struct super trait true type unsafe" &
     " use where while abstract become box do final macro override priv try typeof unsized virtual" &
     " yield union ";

   function Rust_Names_Usable return Boolean is
   begin
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : constant String := Snake_Name (Binding.Name.Data (1 .. Binding.Name.Length));
         begin
            if Ada.Strings.Fixed.Index (Rust_Unusable, " " & Name & " ") > 0 then
               Put_Line (Standard_Error, Argument (2) & ": binding """ &
                         Binding.Name.Data (1 .. Binding.Name.Length) &
                         """ cannot name a Rust startup accessor (a Rust keyword). Fix: rename the binding.");
               return False;
            end if;
         end;
      end loop;
      return True;
   end Rust_Names_Usable;

   --  The Rust startup accessors (CAP-003): module startup, one function
   --  per endpoint binding, taking the program's startup table
   --  (cubit::startup::Table) and returning its endpoint when granted.
   --  Names, never numbers or strings in the program's own code.
   procedure Emit_Rust is
      Output : Ada.Text_IO.File_Type;
   begin
      Create (Output, Out_File, Option ("--rust-output"));
      Put_Line (Output, "// Generated with the ELF manifest; do not edit.");
      Put_Line (Output, "// Startup capabilities of " & Identity_Text & ", by name (CAP-002, CAP-003).");
      Put_Line (Output, "pub mod startup {");
      Put_Line (Output, "    #![allow(dead_code)]");
      Put_Line (Output, "    use cubit::startup::Table;");
      Put_Line (Output, "    use cubit::Endpoint;");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
            use type ST.Need;
            Need : constant String :=
              (if Binding.Requirement = ST.Optional then "optional" else "required");
         begin
            Put_Line (Output, "");
            Put_Line (Output, "    /// " & Kind_Image (Binding.Kind) & " """ & Name & """, " & Need & ".");
            if Binding.Kind in ST.Endpoint | ST.Render | ST.Network then
               Put_Line (Output, "    pub fn " & Snake_Name (Name) &
                         "(table: &Table) -> Option<Endpoint> {");
               Put_Line (Output, "        table.endpoint(""" & Name & """)");
               Put_Line (Output, "    }");
            else
               Put_Line (Output, "    // (no Rust accessor for this kind yet)");
            end if;
         end;
      end loop;
      Put_Line (Output, "}");
      Close (Output);
   end Emit_Rust;

   --  The C startup header (CAP-003): one inline function per endpoint
   --  binding, cubit_startup_<binding>, over the libc's lookup
   --  (CuBit.Libc_Startup), which only this header declares.
   procedure Emit_C is
      Output : Ada.Text_IO.File_Type;
   begin
      Create (Output, Out_File, Option ("--c-output"));
      Put_Line (Output, "/* Generated with the ELF manifest; do not edit.");
      Put_Line (Output, " * Startup capabilities of " & Identity_Text & ", by name (CAP-002, CAP-003). */");
      Put_Line (Output, "#ifndef CUBIT_STARTUP_H");
      Put_Line (Output, "#define CUBIT_STARTUP_H");
      Put_Line (Output, "#include <cubit_handles.h>");
      Put_Line (Output, "#ifdef __cplusplus");
      Put_Line (Output, "extern ""C"" {");
      Put_Line (Output, "#endif");
      Put_Line (Output, "/* The endpoint the startup table grants under name, or CUBIT_NO_ENDPOINT");
      Put_Line (Output, " * (CuBit.Libc_Startup); programs call the functions below instead. */");
      Put_Line (Output, "cubit_endpoint cubit_startup_endpoint(const char *name);");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
            use type ST.Need;
            Need : constant String :=
              (if Binding.Requirement = ST.Optional then "optional" else "required");
         begin
            Put_Line (Output, "");
            Put_Line (Output, "/* " & Kind_Image (Binding.Kind) & " """ & Name & """, " & Need & ". */");
            if ST."=" (Binding.Kind, ST.Endpoint) then
               Put_Line (Output, "static inline cubit_endpoint cubit_startup_" & Snake_Name (Name) & "(void)");
               Put_Line (Output, "{");
               Put_Line (Output, "    return cubit_startup_endpoint(""" & Name & """);");
               Put_Line (Output, "}");
            else
               Put_Line (Output, "/* (no C accessor for this kind yet) */");
            end if;
         end;
      end loop;
      Put_Line (Output, "#ifdef __cplusplus");
      Put_Line (Output, "}");
      Put_Line (Output, "#endif");
      Put_Line (Output, "#endif");
      Close (Output);
   end Emit_C;

   function Has_Client return Boolean is
     (for some Binding of Result.Bindings (1 .. Result.Binding_Count) =>
        CCL.Manifests."/=" (Binding.Client, CCL.Manifests.Plain));

   --  startup.adb beside startup.ads: the startup check (CAP-003), run while
   --  the package elaborates, so before the program's main; it stops the
   --  program, saying which entry and why, when the loaded table lacks a
   --  required one. Nothing to check when nothing is declared.
   procedure Emit_Startup_Body (Identity : String) is
      Output : Ada.Text_IO.File_Type;
      Spec : constant String := Startup_Path;
      Log : Natural := 0;
   begin
      for Index in 1 .. Result.Binding_Count loop
         if Log = 0 and then CCL.Manifests."=" (Result.Bindings (Index).Client, CCL.Manifests.Logstore) then
            Log := Index;
         end if;
      end loop;
      Create (Output, Out_File, Spec (Spec'First .. Spec'Last - 1) & "b");
      Put_Line (Output, "--  Generated with the ELF manifest; do not edit.");
      Put_Line (Output, "pragma Ada_2022;");
      if Result.Binding_Count = 0 then
         Put_Line (Output, "package body Startup with SPARK_Mode => Off is");
         Put_Line (Output, "end Startup;");
         Close (Output);
         return;
      end if;
      if Log = 0 then
         Put_Line (Output, "with CuBit.Startup_Check;");
      else
         Put_Line (Output, "with CuBit.Startup_Check.Logged;");
         Put_Line (Output, "with CuBit.Handles.Services;");
      end if;
      Put_Line (Output, "package body Startup with SPARK_Mode => Off is");
      Put_Line (Output, "begin");
      Put_Line (Output, "   --  Stops here, saying which entry and why, if the loaded startup");
      Put_Line (Output, "   --  table lacks a required capability declared above (CAP-003).");
      if Log = 0 then
         Put_Line (Output, "   Standard.CuBit.Startup_Check.Require (Expected, """ & Identity & """);");
      else
         Put_Line (Output, "   Standard.CuBit.Startup_Check.Logged.Require");
         Put_Line (Output, "     (Expected, """ & Identity & """,");
         Put_Line (Output, "      Standard.CuBit.Handles.Services.Logstore.Optional_Named (""" &
                   Result.Bindings (Log).Name.Data (1 .. Result.Bindings (Log).Name.Length) & """));");
      end if;
      Put_Line (Output, "end Startup;");
      Close (Output);
   end Emit_Startup_Body;

   procedure Emit_Startup is
      Output : Ada.Text_IO.File_Type;
      Identity : constant String := Identity_Text;
   begin
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
      if (for some Binding of Result.Bindings (1 .. Result.Binding_Count) =>
            CCL.Manifests."=" (Binding.Client, CCL.Manifests.Plain) and then Handle_Type (Binding.Kind) /= "")
      then
         Put_Line (Output, "with CuBit.Handles.Startup;");
      end if;
      if Result.Binding_Count > 0 then
         Put_Line (Output, "with CuBit.Startup_State;");
         Put_Line (Output, "with CuBit.Startup_Tables;");
      end if;
      if Has_Client then
         Put_Line (Output, "with CuBit.Handles.Services;");
      end if;
      if Result.Channel_Offers.Count > 0 then
         Put_Line (Output, "with CuBit.Channel_Limits;");
      end if;
      Put_Line (Output, "package Startup with SPARK_Mode, Elaborate_Body is");
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
            elsif CCL.Manifests."/=" (Binding.Client, CCL.Manifests.Plain) then
               declare
                  Typed : constant String := "Standard.CuBit.Handles.Services." &
                    CCL.Manifests.Client_Name (Binding.Client);
               begin
                  if Optional then
                     Put_Line (Output, "   function " & Ada_Name (Name) & " return " & Typed &
                               ".Optional_Endpoint is");
                     Put_Line (Output, "     (" & Typed & ".Optional_Named (""" & Name & """));");
                  else
                     Put_Line (Output, "   function " & Ada_Name (Name) & " return " & Typed & ".Endpoint is");
                     Put_Line (Output, "     (" & Typed & ".Named (""" & Name & """));");
                  end if;
               end;
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
      if Result.Channel_Offers.Count > 0 then
         --  The channel offers this program serves (CAP-002 step 3), for
         --  Channel_Servers.Configure: constants per contract, named after
         --  it (dots and hyphens become underscores).
         Put_Line (Output, "");
         Put_Line (Output, "   package Channels is");
         for Index in 1 .. Result.Channel_Offers.Count loop
            declare
               package CL renames CuBit.Channel_Limits;
               O : CuBit.Channel_Declarations.Offer renames Result.Channel_Offers.Offers (Index);
               Contract : constant String := String (O.Contract.Text (1 .. O.Contract.Length));
               Ident : String := Contract;
               function Limit_Image (L : CL.Channel_Limit) return String is
                 (case L.Kind is
                     when CL.At_Most => "(Kind => Standard.CuBit.Channel_Limits.At_Most, Count =>" &
                                        Natural'Image (L.Count) & ")",
                     when CL.Server_Limit => "(Kind => Standard.CuBit.Channel_Limits.Server_Limit)");
            begin
               for C of Ident loop
                  if C in '.' | '_' then
                     C := '-';
                  end if;
               end loop;
               Put_Line (Output, "      --  """ & Contract & """");
               Put_Line (Output, "      " & Ada_Name (Ident) & "_Per_Client : constant " &
                         "Standard.CuBit.Channel_Limits.Channel_Limit :=");
               Put_Line (Output, "        " & Limit_Image (O.Per_Client) & ";");
               Put_Line (Output, "      " & Ada_Name (Ident) & "_Total : constant " &
                         "Standard.CuBit.Channel_Limits.Channel_Limit :=");
               Put_Line (Output, "        " & Limit_Image (O.Total) & ";");
               Put_Line (Output, "      " & Ada_Name (Ident) & "_Class : constant " &
                         "Standard.CuBit.Channel_Limits.Channel_Class :=");
               Put_Line (Output, "        Standard.CuBit.Channel_Limits." &
                         Mixed (CL.Channel_Class'Image (O.Class)) & ";");
            end;
         end loop;
         Put_Line (Output, "   end Channels;");
      end if;
      --  The declared table, which the body checks the loaded one against.
      if Result.Binding_Count > 0 then
         Put_Line (Output, "");
         Put_Line (Output, "   --  The table declared above; the package's elaboration checks the");
         Put_Line (Output, "   --  loaded one against it and stops the program if it falls short.");
         Put_Line (Output, "   Expected : constant Standard.CuBit.Startup_State.Expectations :=");
         for Index in 1 .. Result.Binding_Count loop
            declare
               Binding : CCL.Manifests.Named_Binding renames Result.Bindings (Index);
               Name : constant String := Binding.Name.Data (1 .. Binding.Name.Length);
            begin
               Put_Line (Output, (if Index = 1 then "     [" else "      ") &
                         "(Standard.CuBit.Startup_Tables.To_Name (""" & Name & """), " &
                         "Standard.CuBit.Startup_Tables." & Kind_Image (Binding.Kind) & ", " &
                         "Standard.CuBit.Startup_Tables." & Mixed (ST.Need'Image (Binding.Requirement)) & ")" &
                         (if Index = Result.Binding_Count then "];" else ","));
            end;
         end loop;
      end if;
      Put_Line (Output, "end Startup;");
      Close (Output);
      Emit_Startup_Body (Identity);
   end Emit_Startup;
begin
   if not Options_Valid then
      Put_Line (Standard_Error,
        "usage: ccl-manifest CATALOG.ccl MANIFEST.ccl [--startup-output startup.ads] " &
        "[--rust-output bindings.rs] [--c-output startup.h] [--schema executable-manifest.ccl] > manifest.S");
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
      if Startup_Path'Length < 5 or else Startup_Path (Startup_Path'Last - 3 .. Startup_Path'Last) /= ".ads" then
         Put_Line (Standard_Error, "--startup-output names the package spec, a .ads file " &
                   "(its body goes beside it, as .adb).");
         Set_Exit_Status (Failure);
         return;
      end if;
      if not Startup_Names_Usable then
         Set_Exit_Status (Failure);
         return;
      end if;
      Emit_Startup;
   end if;
   if Option ("--rust-output")'Length > 0 then
      if not Rust_Names_Usable then
         Set_Exit_Status (Failure);
         return;
      end if;
      Emit_Rust;
   end if;
   if Option ("--c-output")'Length > 0 then
      Emit_C;
   end if;
   Emit (".cubit.id", Result.Identity);
   Emit (".cubit.caps", Result.Capabilities);
   Emit (".cubit.access", Result.Access_Scopes);
   Emit (".cubit.resources", Result.Resources);
   Emit (".cubit.launch", Result.Launch);
   Emit (".cubit.description", Result.Description);
   Emit (".cubit.startup", Result.Startup);
   Emit (".cubit.channels", Result.Channels);
   Put_Line (".section .note.GNU-stack,"""",@progbits");
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot access input/output, or input exceeds 4096-byte bound");
      Set_Exit_Status (Failure);
end Main;
