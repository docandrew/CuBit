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
with Setting_Catalogs;
with Setting_Codecs;

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
         Argument (Index) in "--rust-output" | "--c-output" | "--startup-output" | "--schema" | "--settings"));
   --  Where the startup binding goes (none without --startup-output).
   function Startup_Path return String is (Option ("--startup-output"));
   --  The schema's default home: interfaces/ beside the catalogs directory.
   function Schema_Path return String is
     (if Option ("--schema")'Length > 0 then Option ("--schema")
      else Ada.Directories.Containing_Directory
             (Ada.Directories.Containing_Directory (Ada.Directories.Full_Name (Argument (1)))) &
           "/interfaces/executable-manifest.ccl");
   Result : CCL.Manifests.Compilation_Result;
   --  The setting catalog (interfaces/settings/*.ccl), when the manifest
   --  has Config scopes: every scope must name settings in it (CAP-003).
   Settings : Setting_Catalogs.Catalog;
   Settings_Read : Boolean := False;
   Settings_Why : CuBit.Failures.Failure;
   Hex : constant String := "0123456789abcdef";

   --  The setting catalog's directory and schema: settings/ and
   --  setting-catalog.ccl beside the manifest schema.
   function Settings_Directory return String is
     (if Option ("--settings")'Length > 0 then Option ("--settings")
      else Ada.Directories.Containing_Directory (Ada.Directories.Full_Name (Schema_Path)) & "/settings");
   function Settings_Schema_Path return String is
     (Ada.Directories.Containing_Directory (Ada.Directories.Full_Name (Settings_Directory)) &
      "/setting-catalog.ccl");

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

   --  .cubit.access: a 16-byte header (magic, version, count), then 80-byte
   --  entries: rights, prefix length, domain, 5 reserved, the 64-byte
   --  prefix (CCL.Manifests.Encoding).
   Access_Header : constant := 16;
   Access_Entry : constant := 80;
   Access_Count_At : constant := 7;
   Access_Length_At : constant := 2;
   Access_Domain_At : constant := 3;
   Access_Path_At : constant := 9;
   --  CCL.Manifests.Model.Access_Domain'Enum_Rep (Config_Domain).
   Config_Domain : constant := 1;

   function Scope_Count return Natural is
     (if Result.Access_Scopes.Length < Access_Header then 0
      else Natural (Result.Access_Scopes.Data (Access_Count_At)) +
           256 * Natural (Result.Access_Scopes.Data (Access_Count_At + 1)));
   function Scope_Base (Index : Positive) return Natural is (Access_Header + (Index - 1) * Access_Entry);
   function Scope_Is_Config (Index : Positive) return Boolean is
     (Natural (Result.Access_Scopes.Data (Scope_Base (Index) + Access_Domain_At)) = Config_Domain);
   function Scope_Path (Index : Positive) return String is
      Length : constant Natural := Natural (Result.Access_Scopes.Data (Scope_Base (Index) + Access_Length_At));
      Path : String (1 .. Length);
   begin
      for I in Path'Range loop
         Path (I) := Character'Val (Result.Access_Scopes.Data (Scope_Base (Index) + Access_Path_At + I - 1));
      end loop;
      return Path;
   end Scope_Path;

   --  Its rights mask (CCL.Manifests.Model.Access_Right codes: Write is 2).
   Access_Rights_At : constant := 1;
   Write_Right : constant := 2;
   function Scope_Writes (Index : Positive) return Boolean is
     ((Natural (Result.Access_Scopes.Data (Scope_Base (Index) + Access_Rights_At)) / Write_Right) mod 2 = 1);

   function Has_Config_Scopes return Boolean is
     (for some I in 1 .. Scope_Count => Scope_Is_Config (I));

   --  Every catalog file, in name order (a stable catalog whatever the
   --  directory's order).
   procedure Read_Settings is
      use Ada.Directories;
      Search : Search_Type;
      Found : Directory_Entry_Type;
      Names : array (1 .. Setting_Catalogs.Maximum_Settings) of Setting_Catalogs.Bounded_Text;
      Count : Natural := 0;
      Schema_Text, Catalog_Text : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
      Schema_Last, Catalog_Last : Natural;
      Success : Boolean;
   begin
      Settings_Read := False;
      if not Exists (Settings_Directory) or else not Exists (Settings_Schema_Path) then
         Settings_Why := CuBit.Failures.Failed
           (CuBit.Failures.Not_Found, "no setting catalog at " & Settings_Directory,
            "keep interfaces/settings/ and interfaces/setting-catalog.ccl beside the manifest schema");
         return;
      end if;
      Read_Source (Settings_Schema_Path, Schema_Text, Schema_Last);
      Start_Search (Search, Settings_Directory, "*.ccl", [Ordinary_File => True, others => False]);
      while More_Entries (Search) and then Count < Names'Last loop
         Get_Next_Entry (Search, Found);
         Count := Count + 1;
         Names (Count) := (Length => Simple_Name (Found)'Length,
                           Data => Simple_Name (Found) &
                                   [1 .. Setting_Catalogs.Maximum_Text - Simple_Name (Found)'Length => ' ']);
      end loop;
      End_Search (Search);
      for I in 2 .. Count loop
         for J in reverse 2 .. I loop
            exit when Setting_Catalogs.Image (Names (J - 1)) <= Setting_Catalogs.Image (Names (J));
            declare
               Swap : constant Setting_Catalogs.Bounded_Text := Names (J);
            begin
               Names (J) := Names (J - 1);
               Names (J - 1) := Swap;
            end;
         end loop;
      end loop;
      for I in 1 .. Count loop
         Read_Source (Settings_Directory & "/" & Setting_Catalogs.Image (Names (I)), Catalog_Text, Catalog_Last);
         Setting_Catalogs.Add
           (Settings, "settings/" & Setting_Catalogs.Image (Names (I)), Catalog_Text (1 .. Catalog_Last),
            Schema_Text (1 .. Schema_Last), Success, Settings_Why);
         if not Success then
            return;
         end if;
      end loop;
      Settings_Read := True;
   end Read_Settings;

   --  CCL.Setting_Kinds (userspace/ccl/src/ccl-setting_kinds.ads): the
   --  setting kinds CCL.Typed_Settings checks values against, generated
   --  from the catalog's CCL-typed settings (ccl-manifest --setting-kinds
   --  OUTPUT [--settings DIR]).
   procedure Emit_Setting_Kinds (Path : String) is
      Output : Ada.Text_IO.File_Type;
      use Setting_Catalogs;
      function Package_Of (Source : String) return String is
        (Source (Source'First .. Ada.Strings.Fixed.Index (Source, ".", Ada.Strings.Backward) - 1));
      function Kind_Name (S : Setting) return String is (Image (S.Root) & "_Setting");
      Listed : array (1 .. Maximum_Settings) of Bounded_Text;
      Listed_Count : Natural := 0;
      First : Boolean := True;
   begin
      Create (Output, Out_File, Path);
      Put_Line (Output, "--  Generated from userspace/ccl/interfaces/settings/ by");
      Put_Line (Output, "--  ccl-manifest --setting-kinds (CAP-003); do not edit. Regenerate with");
      Put_Line (Output, "--  make -C kernel setting-kinds after changing the catalog.");
      Put_Line (Output, "pragma Ada_2022;");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            declare
               Name : constant String := Package_Of (Image (Settings.Settings (I).Source));
            begin
               if not (for some J in 1 .. Listed_Count => Image (Listed (J)) = Name) then
                  Listed_Count := Listed_Count + 1;
                  Listed (Listed_Count) := (Length => Name'Length,
                                            Data => Name & [1 .. Maximum_Text - Name'Length => ' ']);
                  Put_Line (Output, "with " & Name & ";");
               end if;
            end;
         end if;
      end loop;
      Put_Line (Output, "");
      Put_Line (Output, "--  The settings whose values are CCL values of a declared type, by key");
      Put_Line (Output, "--  (a family's members included), with each type's declarations and root.");
      Put_Line (Output, "package CCL.Setting_Kinds with SPARK_Mode, Pure is");
      Put (Output, "   type Setting_Kind is" & ASCII.LF & "     (Untyped");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            Put (Output, "," & ASCII.LF & "      " & Kind_Name (Settings.Settings (I)));
         end if;
      end loop;
      Put_Line (Output, ");");
      Put_Line (Output, "");
      Put_Line (Output, "   function Starts (Key, Prefix : String) return Boolean is");
      Put_Line (Output, "     (Key'Length > Prefix'Length");
      Put_Line (Output, "      and then Key (Key'First .. Key'First + Prefix'Length - 1) = Prefix);");
      Put_Line (Output, "");
      Put_Line (Output, "   function Kind_Of (Key : String) return Setting_Kind is");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            Put_Line (Output, (if First then "     (if " else "      elsif ") &
                      (if Settings.Settings (I).Form = Family
                       then "Starts (Key, """ & Image (Settings.Settings (I).Key) & """)"
                       else "Key = """ & Image (Settings.Settings (I).Key) & """") &
                      " then " & Kind_Name (Settings.Settings (I)));
            First := False;
         end if;
      end loop;
      Put_Line (Output, (if First then "     (Untyped);" else "      else Untyped);"));
      Put_Line (Output, "");
      Put_Line (Output, "   --  The type's declarations, and its root type's name.");
      Put_Line (Output, "   function Schema (Kind : Setting_Kind) return String is");
      Put_Line (Output, "     (case Kind is");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            Put_Line (Output, "         when " & Kind_Name (Settings.Settings (I)) & " => " &
                      Image (Settings.Settings (I).Source) & ",");
         end if;
      end loop;
      Put_Line (Output, "         when Untyped => """");");
      Put_Line (Output, "   function Root_Name (Kind : Setting_Kind) return String is");
      Put_Line (Output, "     (case Kind is");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            Put_Line (Output, "         when " & Kind_Name (Settings.Settings (I)) & " => """ &
                      Image (Settings.Settings (I).Root) & """,");
         end if;
      end loop;
      Put_Line (Output, "         when Untyped => """");");
      Put_Line (Output, "end CCL.Setting_Kinds;");
      Close (Output);
   end Emit_Setting_Kinds;

   --  Every Config scope names settings in the catalog (CAP-003): false,
   --  with a message, for one that names none.
   function Scopes_Name_Settings return Boolean is
   begin
      for I in 1 .. Scope_Count loop
         if Scope_Is_Config (I)
           and then Scope_Path (I)'Length > 0
           and then not (for some S in 1 .. Settings.Count => Setting_Catalogs.Covers (Settings, Scope_Path (I), S))
         then
            Put_Line (Standard_Error, Argument (2) & ": config scope """ & Scope_Path (I) &
                      """ names no setting in the setting catalog (userspace/ccl/interfaces/settings/). " &
                      "Fix: name a declared key or family, or declare the setting there first.");
            return False;
         end if;
      end loop;
      return True;
   end Scopes_Name_Settings;

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

   --  A startup capability kind as cubit::startup::Kind names it.
   function Rust_Kind (Kind : ST.Capability_Kind) return String is
     (case Kind is
         when ST.Endpoint      => "Endpoint",
         when ST.Notification  => "Notification",
         when ST.Framebuffer   => "Framebuffer",
         when ST.Render        => "Render",
         when ST.Network       => "Network",
         when ST.Device_Memory => "DeviceMemory",
         when ST.IO_Ports      => "IoPorts",
         when ST.Interrupt     => "Interrupt",
         when ST.DMA           => "Dma",
         when ST.Scheduling    => "Scheduling",
         when ST.Process       => "Process",
         when ST.Resource      => "Resource");

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
      --  The declared table and the startup check the shared start code
      --  calls before the language's start (cubit::startup::require).
      Put_Line (Output, "");
      Put_Line (Output, "    /// The startup table this manifest declares.");
      Put_Line (Output, "    pub const EXPECTED: &[cubit::startup::Expectation] = &[");
      for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
         declare
            use type ST.Need;
         begin
            Put_Line (Output, "        cubit::startup::Expectation { name: """ &
                      Binding.Name.Data (1 .. Binding.Name.Length) & """, kind: cubit::startup::Kind::" &
                      Rust_Kind (Binding.Kind) & ", need: cubit::startup::Need::" &
                      (if Binding.Requirement = ST.Optional then "Optional" else "Required") & " },");
         end;
      end loop;
      Put_Line (Output, "    ];");
      Put_Line (Output, "");
      Put_Line (Output, "    /// The startup check (CAP-003): the shared start code");
      Put_Line (Output, "    /// (userspace/runtime/start/cubit_start.S) calls it before the");
      Put_Line (Output, "    /// language's start; it stops the program, saying which entry and");
      Put_Line (Output, "    /// why, when the startup table falls short of EXPECTED.");
      Put_Line (Output, "    #[unsafe(no_mangle)]");
      Put_Line (Output, "    pub extern ""C"" fn __cubit_startup_check() {");
      Put_Line (Output, "        cubit::startup::require(EXPECTED, """ & Identity_Text & """);");
      Put_Line (Output, "    }");
      Put_Line (Output, "}");
      Close (Output);
   end Emit_Rust;

   function Image (Value : Natural) return String is
     (Ada.Strings.Fixed.Trim (Natural'Image (Value), Ada.Strings.Left));

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
      --  The declared table and the startup check the shared start code
      --  calls before the libc's start (CuBit.Libc_Startup.Require). Weak:
      --  every file that includes this header defines the same one.
      Put_Line (Output, "");
      Put_Line (Output, "/* One declared startup capability (CuBit.Startup_Tables codes). */");
      Put_Line (Output, "struct cubit_startup_expectation {");
      Put_Line (Output, "    const char *name;");
      Put_Line (Output, "    unsigned char kind;");
      Put_Line (Output, "    unsigned char need;");
      Put_Line (Output, "};");
      Put_Line (Output, "void cubit_startup_require(const struct cubit_startup_expectation *expected,");
      Put_Line (Output, "                           unsigned long count, const char *program);");
      if Result.Binding_Count > 0 then
         Put_Line (Output, "static const struct cubit_startup_expectation cubit_startup_expected[] = {");
         for Binding of Result.Bindings (1 .. Result.Binding_Count) loop
            Put_Line (Output, "    {""" & Binding.Name.Data (1 .. Binding.Name.Length) & """, " &
                      Image (ST.Capability_Kind'Enum_Rep (Binding.Kind)) & ", " &
                      Image (ST.Need'Enum_Rep (Binding.Requirement)) & "},  /* " &
                      Kind_Image (Binding.Kind) & ", " &
                      Mixed (ST.Need'Image (Binding.Requirement)) & " */");
         end loop;
         Put_Line (Output, "};");
      end if;
      Put_Line (Output, "/* The startup check (CAP-003): the shared start code");
      Put_Line (Output, " * (userspace/runtime/start/cubit_start.S) calls it before the libc's");
      Put_Line (Output, " * start; it stops the program, saying which entry and why, when the");
      Put_Line (Output, " * startup table falls short of the declaration. */");
      Put_Line (Output, "__attribute__((weak)) void __cubit_startup_check(void)");
      Put_Line (Output, "{");
      Put_Line (Output, "    cubit_startup_require(" &
                (if Result.Binding_Count > 0 then "cubit_startup_expected" else "0") & ", " &
                Image (Result.Binding_Count) & ", """ & Identity_Text & """);");
      Put_Line (Output, "}");
      Put_Line (Output, "#ifdef __cplusplus");
      Put_Line (Output, "}");
      Put_Line (Output, "#endif");
      Put_Line (Output, "#endif");
      Close (Output);
   end Emit_C;

   --  The settings this program's Config scopes cover (CAP-003), each with
   --  whether some scope covering it grants write, and whether a scope is
   --  config-all (the Inspector).
   type Covered_Array is array (1 .. Setting_Catalogs.Maximum_Settings) of Boolean;
   Covered, Covered_Writes : Covered_Array := [others => False];
   Inspects : Boolean := False;
   procedure Cover_Settings is
   begin
      for I in 1 .. Scope_Count loop
         if Scope_Is_Config (I) then
            if Scope_Path (I)'Length = 0 then
               Inspects := True;
            else
               for S in 1 .. Settings.Count loop
                  if Setting_Catalogs.Covers (Settings, Scope_Path (I), S)
                    and then Setting_Catalogs."/=" (Settings.Settings (S).Value, Setting_Catalogs.Collection)
                  then
                     Covered (S) := True;
                     Covered_Writes (S) := Covered_Writes (S) or else Scope_Writes (I);
                  end if;
               end loop;
            end if;
         end if;
      end loop;
   end Cover_Settings;
   function Any_Covered return Boolean is (for some S in 1 .. Settings.Count => Covered (S));

   --  The config binding (a Config client) Settings reach Config through.
   function Config_Binding return Natural is
   begin
      for I in 1 .. Result.Binding_Count loop
         if CCL.Manifests."=" (Result.Bindings (I).Client, CCL.Manifests.Config) then
            return I;
         end if;
      end loop;
      return 0;
   end Config_Binding;

   --  "desktop.display.mode." -> "Desktop_Display_Mode".
   function Setting_Name (Key : String) return String is
      Name : String := (if Key (Key'Last) = '.' then Key (Key'First .. Key'Last - 1) else Key);
      Start : Boolean := True;
   begin
      for C of Name loop
         if C in '.' | '-' | '_' then
            C := '_';
            Start := True;
         elsif Start then
            C := Ada.Characters.Handling.To_Upper (C);
            Start := False;
         end if;
      end loop;
      return Name;
   end Setting_Name;

   function Codec_Of (S : Setting_Catalogs.Setting) return String is
     ("Standard." &
      (case S.Value is
          when Setting_Catalogs.Typed => Setting_Catalogs.Image (S.Codec),
          when Setting_Catalogs.Text => "CuBit.Settings.Codecs.Text_Codec",
          when Setting_Catalogs.Address => "CuBit.Settings.Codecs.Address_Codec",
          when Setting_Catalogs.Count | Setting_Catalogs.Collection => "CuBit.Settings.Codecs.Count_Codec"));

   --  The context clauses the Settings package needs.
   procedure Emit_Settings_Withs (Output : Ada.Text_IO.File_Type) is
      use Setting_Catalogs;
      Listed : array (1 .. Maximum_Settings) of Bounded_Text;
      Count : Natural := 0;
      procedure Need (Unit : String) is
      begin
         if not (for some J in 1 .. Count => Image (Listed (J)) = Unit) then
            Count := Count + 1;
            Listed (Count) := (Length => Unit'Length, Data => Unit & [1 .. Maximum_Text - Unit'Length => ' ']);
            Put_Line (Output, "with " & Unit & ";");
         end if;
      end Need;
   begin
      if Inspects then
         Need ("CuBit.Settings.Inspection");
      end if;
      for S in 1 .. Settings.Count loop
         if Covered (S) then
            declare
               Item : Setting renames Settings.Settings (S);
            begin
               Need ("CuBit.Settings." &
                     (if Item.Form = Family
                      then (if Covered_Writes (S) then "Writable_Family" else "Family")
                      else (if Covered_Writes (S) then "Writable" else "Readable")));
               Need (if Item.Value = Typed then Image (Item.Codec) else "CuBit.Settings.Codecs");
            end;
         end if;
      end loop;
   end Emit_Settings_Withs;

   --  package Settings: one accessor per covered setting, and the
   --  Inspector for config-all.
   procedure Emit_Settings (Output : Ada.Text_IO.File_Type) is
      use Setting_Catalogs;
      Service : constant String :=
        Ada_Name (Result.Bindings (Config_Binding).Name.Data
                    (1 .. Result.Bindings (Config_Binding).Name.Length));
   begin
      Put_Line (Output, "");
      Put_Line (Output, "   --  The settings this manifest's Config scopes cover (CAP-003), one");
      Put_Line (Output, "   --  accessor each, typed by the setting catalog");
      Put_Line (Output, "   --  (userspace/ccl/interfaces/settings/).");
      Put_Line (Output, "   package Settings with SPARK_Mode => Off is");
      for S in 1 .. Settings.Count loop
         if Covered (S) then
            declare
               Item : Setting renames Settings.Settings (S);
               Codec : constant String := Codec_Of (Item);
               Writes : constant Boolean := Covered_Writes (S);
               Generic_Name : constant String :=
                 (if Item.Form = Family then (if Writes then "Writable_Family" else "Family")
                  else (if Writes then "Writable" else "Readable"));
            begin
               Put_Line (Output, "      --  """ & Image (Item.Key) & """ (" & Image (Item.File) & "), " &
                         (if Writes then "read and write" else "read only") & ".");
               Put_Line (Output, "      package " & Setting_Name (Image (Item.Key)) &
                         " is new Standard.CuBit.Settings." & Generic_Name);
               Put_Line (Output, "        (" & Codec & ".Value, """ & Image (Item.Key) & """, " &
                         Service & ", " & Codec & ".Decode" &
                         (if Writes then ", " & Codec & ".Encode" else "") & ");");
            end;
         end if;
      end loop;
      if Inspects then
         Put_Line (Output, "      --  config-all: any key, as text (Config still checks each).");
         Put_Line (Output, "      function Inspector return Standard.CuBit.Settings.Inspection.Inspector is");
         Put_Line (Output, "        (Standard.CuBit.Settings.Inspection.Using (" & Service & "));");
      end if;
      Put_Line (Output, "   end Settings;");
   end Emit_Settings;

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
      Emit_Settings_Withs (Output);
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
      if Any_Covered or else Inspects then
         Emit_Settings (Output);
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
   --  ccl-manifest --setting-kinds OUTPUT [--settings DIR] [--schema FILE]
   if Argument_Count >= 2 and then Argument (1) = "--setting-kinds" then
      declare
         task Generation with Storage_Size => 256 * 1024 * 1024;
         task body Generation is
         begin
            Read_Settings;
         end Generation;
      begin
         null;
      end;
      if not Settings_Read then
         Put_Line (Standard_Error, Settings_Why.Detail (1 .. Settings_Why.Detail_Length));
         Set_Exit_Status (Failure);
         return;
      end if;
      Emit_Setting_Kinds (Argument (2));
      return;
   end if;
   --  ccl-manifest --setting-codecs DIRECTORY [--settings DIR] [--schema FILE]
   if Argument_Count >= 2 and then Argument (1) = "--setting-codecs" then
      declare
         Generated : Boolean := False;
         Why : CuBit.Failures.Failure;
      begin
         declare
            task Generation with Storage_Size => 256 * 1024 * 1024;
            task body Generation is
            begin
               Read_Settings;
               if Settings_Read then
                  Setting_Codecs.Emit (Settings, Argument (2), Generated, Why);
               end if;
            end Generation;
         begin
            null;
         end;
         if not Settings_Read then
            Put_Line (Standard_Error, Settings_Why.Detail (1 .. Settings_Why.Detail_Length));
            Set_Exit_Status (Failure);
         elsif not Generated then
            Put_Line (Standard_Error, Why.Detail (1 .. Why.Detail_Length) &
                      (if Why.Remedy_Length > 0 then ": " & Why.Remedy (1 .. Why.Remedy_Length) else ""));
            Set_Exit_Status (Failure);
         end if;
      end;
      return;
   end if;
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
         if Result.Success and then Has_Config_Scopes then
            Read_Settings;
         end if;
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
   if Has_Config_Scopes then
      if not Settings_Read then
         Put_Line (Standard_Error, Argument (2) & ": " &
                   Settings_Why.Detail (1 .. Settings_Why.Detail_Length) &
                   (if Settings_Why.Remedy_Length > 0
                    then ". Fix: " & Settings_Why.Remedy (1 .. Settings_Why.Remedy_Length) & "." else ""));
         Set_Exit_Status (Failure);
         return;
      end if;
      if not Scopes_Name_Settings then
         Set_Exit_Status (Failure);
         return;
      end if;
      Cover_Settings;
      if (Any_Covered or else Inspects) and then Config_Binding = 0 then
         Put_Line (Standard_Error, Argument (2) & ": it has Config scopes but requests no config service " &
                   "(its settings reach Config through it). Fix: request config.");
         Set_Exit_Status (Failure);
         return;
      end if;
      if (Any_Covered or else Inspects) and then
        CuBit.Startup_Tables."=" (Result.Bindings (Config_Binding).Requirement, CuBit.Startup_Tables.Optional)
      then
         Put_Line (Standard_Error, Argument (2) & ": its settings need a required config service. " &
                   "Fix: drop optional from the config request.");
         Set_Exit_Status (Failure);
         return;
      end if;
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
   Emit (".cubit.devices", Result.Devices);
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
