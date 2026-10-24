with Ada.Exceptions;
with Interfaces;
with CCL.Catalog;
with CCL.Evaluation;
with CCL.Language;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Types;
with Storehouse.Paths;

package body Build_Declarations is
   package Views renames CCL.Objects.Views;
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Interpretation_Status;

   --  The value never leaves this process: an in-process schema key.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#4255_494C_442D_4343#, 16#4C2D_4C4F_4341_4C01#, 0, 0];
   Conventional_Main : constant String := "main.adb";
   --  A declaration is a constant expression; this bounds its evaluation.
   DECLARATION_FUEL : constant := 100_000;

   procedure Evaluate
     (Program_Text : String; Schema_Length : Natural; Root_Type : String;
      Object : in out Views.Snapshot)
   is
      Catalog : CCL.Catalog.Interface_Catalog;
      Checked : CCL.Language.Analysis_Result;
      Contract : CCL.Objects.Binding;
      Published, Captured : Boolean;
      Value : CCL.Language.Object_Interpretation_Result;

      --  Positions in the declaration as line:column, not counting the
      --  schema before it.
      function Where (Position : Natural) return String is
         Line, Column : Natural := 1;
      begin
         if Position <= Schema_Length then
            --  The checker placed it in interfaces/build.ccl (a value it
            --  could not match against the schema's types).
            return "against interfaces/build.ccl: ";
         end if;
         for I in Program_Text'First + Schema_Length .. Program_Text'First + Position - 2 loop
            if Program_Text (I) = ASCII.LF then
               Line := Line + 1;
               Column := 1;
            else
               Column := Column + 1;
            end if;
         end loop;
         return "line" & Natural'Image (Line) & ", column" & Natural'Image (Column) & ": ";
      end Where;

      --  What the checker says a diagnostic is about, when it says.
      function About (Subject, Expected, Found : CCL.Types.Name) return String is
        ((if CCL.Types.Image (Subject)'Length > 0 then " (" & CCL.Types.Image (Subject) & ")" else "") &
         (if CCL.Types.Image (Expected)'Length > 0
          then ": expected " & CCL.Types.Image (Expected) & ", found " & CCL.Types.Image (Found) else ""));
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Language.Analyze (Program_Text, Catalog, Checked);
      if CCL.Language.Analysis_Status_Of (Checked) /= CCL.Language.Analysis_Succeeded then
         raise Declaration_Error with
           Where (CCL.Language.Analysis_Diagnostic_Position (Checked)) &
           CCL.Language.Analysis_Diagnostic (Checked)'Image &
           About (CCL.Language.Analysis_Diagnostic_Subject (Checked),
                  CCL.Language.Analysis_Diagnostic_Expected (Checked),
                  CCL.Language.Analysis_Diagnostic_Found (Checked)) &
           " (a " & Root_Type & " is expected)";
      end if;
      CCL.Objects.Bind
        (CCL.Language.Analysis_Types (Checked),
         CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (Root_Type)),
         LOCAL_KEY, Contract, Published);
      if not Published then
         raise Declaration_Error with "the schema declares no persistable " & Root_Type;
      end if;
      CCL.Evaluation.Evaluate_Object (Program_Text, DECLARATION_FUEL, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         raise Declaration_Error with
           Where (Value.Diagnostic_Position) & Value.Diagnostic'Image &
           About (Value.Diagnostic_Subject, Value.Diagnostic_Expected, Value.Diagnostic_Found) &
           " (a " & Root_Type & " is expected)";
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then
         raise Declaration_Error with "the declaration's value could not be read";
      end if;
   end Evaluate;

   --  Reading a captured value by field and alternative name.
   generic
      type Result_Type is private;
      with procedure Read_Value (Object : Views.Snapshot; Result : out Result_Type);
   procedure Read_Document (Source, Schema, Root_Type : String; Result : out Result_Type);
   procedure Read_Document (Source, Schema, Root_Type : String; Result : out Result_Type) is
      Failure : Unbounded_String;
      Done : Boolean := False;
      Value : Result_Type;
   begin
      if Schema'Length + 1 + Source'Length > CCL.Language.MAX_SOURCE_LENGTH then
         raise Declaration_Error with "schema and declaration exceed" &
           Natural'Image (CCL.Language.MAX_SOURCE_LENGTH) & " bytes together";
      end if;
      declare
         task Evaluation is
            pragma Storage_Size (EVALUATION_STACK_BYTES);
         end Evaluation;
         task body Evaluation is
            Object : Views.Snapshot;
         begin
            Evaluate (Schema & ASCII.LF & Source, Schema'Length + 1, Root_Type, Object);
            Read_Value (Object, Value);
            Done := True;
         exception
            when E : Declaration_Error | Constraint_Error =>
               Failure := To_Unbounded_String (Ada.Exceptions.Exception_Message (E));
         end Evaluation;
      begin
         null;
      end;
      if not Done then
         raise Declaration_Error with To_String (Failure);
      end if;
      Result := Value;
   end Read_Document;

   function Alternative (Object : Views.Snapshot; At_Cursor : Views.Cursor) return String is
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
      Choice : constant Natural := Views.Alternative (Object, At_Cursor);
   begin
      return (if Choice in 1 .. Shape.Count then CCL.Types.Image (Shape.Parts (Choice).Identifier) else "");
   end Alternative;

   --  A record's field by name: the schema decides the order.
   function Named (Object : Views.Snapshot; At_Cursor : Views.Cursor; Name : String) return Views.Cursor is
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
   begin
      for Part in 1 .. Shape.Count loop
         if CCL.Types.Image (Shape.Parts (Part).Identifier) = Name then
            return Views.Field (Object, At_Cursor, Part);
         end if;
      end loop;
      raise Declaration_Error with "the schema has no field " & Name;
   end Named;

   function Field (Object : Views.Snapshot; At_Cursor : Views.Cursor; Name : String) return Unbounded_String is
     (To_Unbounded_String (Views.Text (Object, Named (Object, At_Cursor, Name))));

   procedure Texts (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out String_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         Into.Append (Views.Text (Object, Views.Element (Object, At_Cursor, I)));
      end loop;
   end Texts;

   procedure Links (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out Link_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         declare
            Argument : constant Views.Cursor := Views.Element (Object, At_Cursor, I);
            Kind : constant Link_Kind := Link_Kind'Value (Alternative (Object, Argument));
         begin
            Into.Append
              (Link_Argument'(Kind => Kind,
                              Value => (if Kind in Text | File
                                        then To_Unbounded_String (Views.Text (Object, Views.Payload (Object, Argument)))
                                        else Null_Unbounded_String)));
         end;
      end loop;
   end Links;

   function Flag (Object : Views.Snapshot; At_Cursor : Views.Cursor) return Boolean is
     (Interfaces."=" (Views.Scalar (Object, At_Cursor).First, 1));

   function Toolchain_Of (Object : Views.Snapshot; At_Cursor : Views.Cursor) return Toolchain is
     ((Name => Field (Object, At_Cursor, "name"), Identity => Field (Object, At_Cursor, "identity"),
       Prefix => Field (Object, At_Cursor, "prefix")));

   --  Ada_Code's fields, as they appear in Ada_Code and in Ada_Source.
   function Code_Of (Object : Views.Snapshot; At_Cursor : Views.Cursor) return Ada_Code is
     ((Language => Ada_Language'Value (Alternative (Object, Named (Object, At_Cursor, "language"))),
       Level => Optimization'Value (Alternative (Object, Named (Object, At_Cursor, "optimization"))),
       Run_Time_Checks => Checks'Value (Alternative (Object, Named (Object, At_Cursor, "checks"))),
       Frames => Frame_Pointer'Value (Alternative (Object, Named (Object, At_Cursor, "frame_pointer"))),
       Value_Range_Propagation => Flag (Object, Named (Object, At_Cursor, "value_range_propagation"))));

   procedure Unit_Codes (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out Unit_Code_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         declare
            Group : constant Views.Cursor := Views.Element (Object, At_Cursor, I);
            Item : Unit_Code;
         begin
            Texts (Object, Named (Object, Group, "files"), Item.Files);
            Item.Profile := Field (Object, Group, "profile");
            Into.Append (Item);
         end;
      end loop;
   end Unit_Codes;

   procedure Parameters_Value (Object : Views.Snapshot; Result : out Parameters) is
      Root : constant Views.Cursor := Views.Root (Object);
   begin
      Result.Compiler_Tools := Toolchain_Of (Object, Named (Object, Root, "toolchain"));
      Result.Binutils := Toolchain_Of (Object, Named (Object, Root, "binutils"));
      Result.Runtime := Field (Object, Root, "runtime");
      Result.Manifest_Tool := Field (Object, Root, "manifest_tool");
      Result.Manifest_Schema := Field (Object, Root, "manifest_schema");
      Result.Catalogs (Process_Manager) := Field (Object, Root, "process_manager_catalog");
      Result.Catalogs (Device_Manager) := Field (Object, Root, "device_manager_catalog");
      Texts (Object, Named (Object, Root, "target"), Result.Target);
      Texts (Object, Named (Object, Root, "binder"), Result.Binder);
      Links (Object, Named (Object, Root, "linker"), Result.Linker);
   end Parameters_Value;

   procedure Catalog_Value (Object : Views.Snapshot; Result : out Build_Catalog) is
      Root : constant Views.Cursor := Views.Root (Object);
      Profiles : constant Views.Cursor := Named (Object, Root, "profiles");
      Libraries : constant Views.Cursor := Named (Object, Root, "libraries");
   begin
      Result := (others => <>);
      for I in 1 .. Views.Length (Object, Profiles) loop
         declare
            Item : constant Views.Cursor := Views.Element (Object, Profiles, I);
         begin
            Result.Profiles.Append (Code_Profile'(Name => Field (Object, Item, "name"),
                                     Code => Code_Of (Object, Named (Object, Item, "code"))));
         end;
      end loop;
      for I in 1 .. Views.Length (Object, Libraries) loop
         declare
            Item : constant Views.Cursor := Views.Element (Object, Libraries, I);
            L : Library;
         begin
            L.Name := Field (Object, Item, "name");
            Texts (Object, Named (Object, Item, "sources"), L.Sources);
            Unit_Codes (Object, Named (Object, Item, "units"), L.Units);
            Texts (Object, Named (Object, Item, "archives"), L.Archives);
            Result.Libraries.Append (L);
         end;
      end loop;
   end Catalog_Value;

   procedure Read_Parameters_Document is new Read_Document (Parameters, Parameters_Value);
   procedure Read_Catalog_Document is new Read_Document (Build_Catalog, Catalog_Value);

   procedure Read_Parameters (Source, Schema : String; Result : out Parameters) is
   begin
      Read_Parameters_Document (Source, Schema, "Parameters", Result);
   end Read_Parameters;

   procedure Read_Catalog (Source, Schema : String; Result : out Build_Catalog) is
   begin
      Read_Catalog_Document (Source, Schema, "Build_Catalog", Result);
   end Read_Catalog;

   --  A component as declared, before it is resolved against the
   --  parameters and the catalog.
   type Kind is (Application, Service, Driver, Tool);
   type Launcher is (Process_Manager, Device_Manager, Kernel);
   type Declared_Component is record
      Name, Main : Unbounded_String;
      Of_Kind : Kind := Application;
      Launched_By : Launcher := Process_Manager;
      Sources, Libraries : String_Vectors.Vector;
      Code : Ada_Code;
      Debug_Info : Boolean := True;
      Units : Unit_Code_Vectors.Vector;
      Link_Extra : Link_Vectors.Vector;
      Blobs : Blob_Vectors.Vector;
   end record;

   --  The executable's suffix by kind; a tool, like the toolchain's
   --  programs, has none.
   function Executable_Suffix (Of_Kind : Kind) return String is
     (case Of_Kind is
        when Application => ".app", when Service => ".svc",
        when Driver => ".drv", when Tool => "");

   --  SHA256_Digest: eight 32-bit words, big-endian, the hex digest's order.
   Digest_Words : constant := 8;
   Word_Bytes : constant := Storehouse.Digest_Bytes / Digest_Words;
   Byte_Bits : constant := 8;

   function Digest_Of (Object : Views.Snapshot; At_Cursor : Views.Cursor) return Storehouse.Digest is
      use type Interfaces.Unsigned_64;
      Result : Storehouse.Digest;
   begin
      for W in 1 .. Digest_Words loop
         declare
            Word : constant Interfaces.Unsigned_64 :=
              Views.Scalar (Object, Named (Object, At_Cursor, "w" & Character'Val (Character'Pos ('0') + W))).First;
         begin
            for B in 1 .. Word_Bytes loop
               Result ((W - 1) * Word_Bytes + B) := Storehouse.Byte
                 (Interfaces.Shift_Right (Word, Byte_Bits * (Word_Bytes - B)) and 16#FF#);
            end loop;
         end;
      end loop;
      return Result;
   end Digest_Of;

   procedure Blobs_Of (Object : Views.Snapshot; At_Cursor : Views.Cursor; Into : out Blob_Vectors.Vector) is
   begin
      Into.Clear;
      for I in 1 .. Views.Length (Object, At_Cursor) loop
         declare
            Item : constant Views.Cursor := Views.Element (Object, At_Cursor, I);
            Source : constant Views.Cursor := Named (Object, Item, "source");
            Pin : constant Views.Cursor := Views.Payload (Object, Source);
         begin
            Into.Append
              (Blob'(Name => Field (Object, Item, "name"),
                     Origin => Blob_Origin'Value (Alternative (Object, Source)),
                     Location => Field (Object, Pin, "location"),
                     SHA256 => Digest_Of (Object, Named (Object, Pin, "sha256")),
                     Licence => Field (Object, Item, "licence")));
         end;
      end loop;
   end Blobs_Of;

   procedure Component_Value (Object : Views.Snapshot; Result : out Declared_Component) is
      Root : constant Views.Cursor := Views.Root (Object);
      Kind_Value : constant Views.Cursor := Named (Object, Root, "kind");
      Source : constant Views.Cursor := Named (Object, Root, "source");
   begin
      Result := (others => <>);
      Result.Name := Field (Object, Root, "name");
      Result.Of_Kind := Kind'Value (Alternative (Object, Kind_Value));
      if Result.Of_Kind = Service then
         Result.Launched_By := Launcher'Value (Alternative (Object, Views.Payload (Object, Kind_Value)));
      elsif Result.Of_Kind = Driver then
         Result.Launched_By := Device_Manager;
      end if;
      if Alternative (Object, Source) /= "Ada" then
         raise Declaration_Error with "no rule family for source " & Alternative (Object, Source);
      end if;
      declare
         Ada_Source : constant Views.Cursor := Views.Payload (Object, Source);
         Main : constant Views.Cursor := Named (Object, Ada_Source, "main");
      begin
         Result.Main := (if Alternative (Object, Main) = "Conventional" then To_Unbounded_String (Conventional_Main)
                         else To_Unbounded_String (Views.Text (Object, Views.Payload (Object, Main))));
         Texts (Object, Named (Object, Ada_Source, "sources"), Result.Sources);
         Texts (Object, Named (Object, Ada_Source, "libraries"), Result.Libraries);
         Result.Code := Code_Of (Object, Ada_Source);
         Result.Debug_Info := Flag (Object, Named (Object, Ada_Source, "debug_info"));
         Unit_Codes (Object, Named (Object, Ada_Source, "units"), Result.Units);
         Links (Object, Named (Object, Ada_Source, "link_extra"), Result.Link_Extra);
      end;
      Blobs_Of (Object, Named (Object, Root, "embedded"), Result.Blobs);
   end Component_Value;

   procedure Read_Component_Document is new Read_Document (Declared_Component, Component_Value);

   --  GNAT's switches for code described by intent. The order is fixed:
   --  the same intent gives the same command line, so the same step.
   Debug_Switch : constant String := "-g";
   All_Warnings : constant String := "-gnatwa";
   No_Propagation_Warnings : constant String := "-gnatw.X";
   function Level_Switch (Level : Optimization) return String is
     (case Level is when Normal => "-O2", when Maximum => "-O3");
   Ada_2022_Switch : constant String := "-gnat2022";
   Suppress_Checks : constant String := "-gnatp";
   Stack_Usage : constant String := "-fstack-usage";
   Omit_Frame_Pointer : constant String := "-fomit-frame-pointer";
   No_Value_Range_Propagation : constant String := "-fno-tree-vrp";
   GNAT_Switch_Prefix : constant String := "-gnat";

   function Switches_For (Debug : Boolean; Code : Ada_Code; Target : String_Vectors.Vector)
     return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;
   begin
      if Debug then Result.Append (Debug_Switch); end if;
      Result.Append (All_Warnings);
      Result.Append (No_Propagation_Warnings);
      Result.Append (Level_Switch (Code.Level));
      if Code.Language = Ada_2022 then Result.Append (Ada_2022_Switch); end if;
      if Code.Run_Time_Checks = Suppressed then Result.Append (Suppress_Checks); end if;
      Result.Append (Stack_Usage);
      if Code.Frames = Omitted then Result.Append (Omit_Frame_Pointer); end if;
      Result.Append (Target);
      if not Code.Value_Range_Propagation then Result.Append (No_Value_Range_Propagation); end if;
      return Result;
   end Switches_For;

   --  A lowercase kebab name: letters, digits and single hyphens, starting
   --  with a letter.
   function Is_Component_Name (Name : String) return Boolean is
     (Name'Length > 0 and then Name (Name'First) in 'a' .. 'z'
      and then Name (Name'Last) /= '-'
      and then (for all I in Name'Range => Name (I) in 'a' .. 'z' | '0' .. '9' | '-')
      and then (for all I in Name'First .. Name'Last - 1 =>
                  not (Name (I) = '-' and then Name (I + 1) = '-')));

   --  The URL schemes a Fetch may use; the pinned digest, not the
   --  transport, makes the bytes trustworthy.
   HTTP_Scheme : constant String := "http://";
   HTTPS_Scheme : constant String := "https://";

   function Starts_With (Text, Prefix : String) return Boolean is
     (Text'Length > Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);

   procedure Check_Blob (B : Blob) is
      Name : constant String := To_String (B.Name);
      Location : constant String := To_String (B.Location);
   begin
      if not Is_Component_Name (Name) then
         raise Declaration_Error with "blob name """ & Name &
           """ is not a lowercase kebab name (a-z, 0-9 and single hyphens, starting with a letter)";
      end if;
      case B.Origin is
         when Vendored =>
            if not Storehouse.Paths.Is_Relative (Location) or else not Starts_With (Location, Blob_Directory) then
               raise Declaration_Error with "blob """ & Name & """: " & Location &
                 " is not a file under " & Blob_Directory & " (vendored blobs live there only)";
            end if;
         when Fetch =>
            if not (Starts_With (Location, HTTPS_Scheme) or else Starts_With (Location, HTTP_Scheme))
              or else (for some C of Location => C not in '!' .. '~')
            then
               raise Declaration_Error with "blob """ & Name & """: " & Location &
                 " is not an http:// or https:// URL";
            end if;
      end case;
      if not Storehouse.Paths.Is_Relative (To_String (B.Licence)) then
         raise Declaration_Error with "blob """ & Name & """: licence " & To_String (B.Licence) &
           " is not a relative path in the tree";
      end if;
   end Check_Blob;

   procedure Read_Component
     (Source, Schema, Directory : String; Profile : Parameters; Known : Build_Catalog;
      Program : out Ada_Program)
   is
      Declared : Declared_Component;

      function Known_Names (Libraries : Boolean) return String is
         Result : Unbounded_String;
      begin
         if Libraries then
            for L of Known.Libraries loop
               Append (Result, (if Length (Result) = 0 then "" else ", ") & L.Name);
            end loop;
         else
            for P of Known.Profiles loop
               Append (Result, (if Length (Result) = 0 then "" else ", ") & P.Name);
            end loop;
         end if;
         return To_String (Result);
      end Known_Names;

      function Code_Named (Name : String) return Ada_Code is
      begin
         for P of Known.Profiles loop
            if To_String (P.Name) = Name then
               return P.Code;
            end if;
         end loop;
         raise Declaration_Error with "build-catalog.ccl has no code profile """ & Name &
           """ (it has: " & Known_Names (Libraries => False) & ")";
      end Code_Named;

      procedure Add_Units (Units : Unit_Code_Vectors.Vector) is
      begin
         for U of Units loop
            Program.Units.Append
              (Unit_Switches'(Files => U.Files,
                Switches => Switches_For (Declared.Debug_Info, Code_Named (To_String (U.Profile)), Profile.Target)));
         end loop;
      end Add_Units;

      procedure Add_Sources (Directories : String_Vectors.Vector) is
      begin
         for D of Directories loop
            if D = Directory then
               raise Declaration_Error with "sources names the component's own directory " & D;
            end if;
            if not Program.Sources.Contains (D) then
               Program.Sources.Append (D);
            end if;
         end loop;
      end Add_Sources;
   begin
      Read_Component_Document (Source, Schema, "Component", Declared);
      Program := (others => <>);
      if not Is_Component_Name (To_String (Declared.Name)) then
         raise Declaration_Error with "name """ & To_String (Declared.Name) &
           """ is not a lowercase kebab name (a-z, 0-9 and single hyphens, starting with a letter)";
      end if;
      Program.Name := Declared.Name;
      Program.Executable := Declared.Name & Executable_Suffix (Declared.Of_Kind);
      Program.Directory := To_Unbounded_String (Directory);
      Program.Main := Declared.Main;
      Program.Manifest_Catalog :=
        (case Declared.Launched_By is
           when Process_Manager => Process_Manager,
           when Device_Manager => Device_Manager,
           when Kernel => No_Manifest);
      Program.Compiler := Switches_For (Declared.Debug_Info, Declared.Code, Profile.Target);
      --  The binder unit: the program's switches without GNAT's own (as
      --  gprbuild compiles it from the main unit's recorded switches).
      for S of Program.Compiler loop
         if S'Length < GNAT_Switch_Prefix'Length
           or else S (S'First .. S'First + GNAT_Switch_Prefix'Length - 1) /= GNAT_Switch_Prefix
         then
            Program.Binder_Unit.Append (S);
         end if;
      end loop;
      Program.Binder := Profile.Binder;
      Add_Sources (Declared.Sources);
      Add_Units (Declared.Units);
      for Name of Declared.Libraries loop
         declare
            Found : Boolean := False;
         begin
            for L of Known.Libraries loop
               if To_String (L.Name) = Name then
                  Found := True;
                  Add_Sources (L.Sources);
                  Add_Units (L.Units);
                  Program.Archives.Append (L.Archives);
               end if;
            end loop;
            if not Found then
               raise Declaration_Error with "build-catalog.ccl has no library """ & Name &
                 """ (it has: " & Known_Names (Libraries => True) & ")";
            end if;
         end;
      end loop;
      for A of Profile.Linker loop
         if A.Kind /= Manifest_Object or else Program.Manifest_Catalog /= No_Manifest then
            Program.Linker.Append (A);
         end if;
      end loop;
      for A of Declared.Link_Extra loop
         if A.Kind = Manifest_Object then
            raise Declaration_Error with "link_extra names the manifest object, which build-params.ccl already links";
         end if;
         Program.Linker.Append (A);
      end loop;
      for B of Declared.Blobs loop
         Check_Blob (B);
         for Earlier of Program.Blobs loop
            if Earlier.Name = B.Name then
               raise Declaration_Error with "embedded names blob """ & To_String (B.Name) & """ twice";
            end if;
         end loop;
         Program.Blobs.Append (B);
      end loop;
   end Read_Component;

end Build_Declarations;
