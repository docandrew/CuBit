
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;
with Build_Ada_Programs;
with Build_Collection;
with Build_Declarations;
with Build_Encoding;
with Build_Generations;
with Build_Platform;
with Build_Steps;
with Build_Timing;
with Build_Store;
with Storehouse;
with Storehouse.Hex;
with Storehouse.Profiles;

--  ccl-build, hosted on Linux (docs/ccl-build.md):
--
--    ccl-build build --store DIR --source-root DIR [--import NAME=PATH]...
--                    [--out DIR] [--profile NAME] [--timings] DIR/component.ccl...
--                    (with build-params.ccl and build-catalog.ccl; NAME is a
--                    toolchain's, or a fetched library's to seed its tree
--                    without network; --profile makes the components'
--                    outputs the profile's next generation and switches to
--                    it; --timings prints where each step's time went,
--                    Build_Timing)
--    ccl-build check --source-root DIR DIR/component.ccl...   (type-check only)
--    ccl-build verify --store DIR
--    ccl-build import --store DIR TREE   (copy a tree in, print its identity)
--    ccl-build hash-tree DIR
--    ccl-build generations --store DIR --profile NAME
--    ccl-build switch --store DIR --profile NAME GENERATION
--    ccl-build rollback --store DIR --profile NAME
--    ccl-build diff --store DIR --profile NAME GENERATION GENERATION
--    ccl-build gc --store DIR [--keep-generations N] [--dry-run]
procedure Main is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use type Storehouse.Digest;
   use type Storehouse.Entry_Kind;
   use type Build_Store.Byte_Count;
   use Storehouse.Profiles;

   --  The build schema's and the build parameters' places in the tree.
   Schema_Path : constant String := "userspace/ccl/interfaces/build.ccl";
   Parameters_Path : constant String := "build-params.ccl";
   Catalog_Path : constant String := "build-catalog.ccl";
   Writable_File : constant := 8#644#;
   Writable_Executable : constant := 8#755#;
   Actions_Directory : constant String := "actions";
   --  Identities are shown by their first hex digits where people read them.
   Short_Identity : constant := 16;
   Epoch_Year : constant := 1970;

   Usage_Error : exception;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);
   function Trimmed (S : String) return String is (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));
   function Short (D : Storehouse.Digest) return String is (Storehouse.Hex.Image (D) (1 .. Short_Identity));

   --  The command line: the command, its options and its operands.
   type Command is
     (Build_Command, Check_Command, Verify_Command, Import_Command, Hash_Tree_Command, Generations_Command,
      Switch_Command, Rollback_Command, Diff_Command, Collect_Command);
   function Command_Name (C : Command) return String is
     (case C is
        when Build_Command => "build", when Check_Command => "check", when Verify_Command => "verify",
        when Import_Command => "import", when Hash_Tree_Command => "hash-tree",
        when Generations_Command => "generations", when Switch_Command => "switch",
        when Rollback_Command => "rollback", when Diff_Command => "diff", when Collect_Command => "gc");

   type Option is (Store_Option, Source_Root_Option, Import_Option, Out_Option, Profile_Option,
                   Keep_Generations_Option, Dry_Run_Option, Timings_Option);
   function Option_Name (O : Option) return String is
     (case O is
        when Store_Option => "--store", when Source_Root_Option => "--source-root",
        when Import_Option => "--import", when Out_Option => "--out", when Profile_Option => "--profile",
        when Keep_Generations_Option => "--keep-generations", when Dry_Run_Option => "--dry-run",
        when Timings_Option => "--timings");
   --  Options that take no value.
   function Is_Flag (O : Option) return Boolean is (O in Dry_Run_Option | Timings_Option);
   --  Options that may be given more than once.
   function Is_Repeatable (O : Option) return Boolean is (O = Import_Option);

   type Option_Set is array (Option) of Boolean;
   function Allowed (C : Command) return Option_Set is
     (case C is
        when Build_Command =>
          [Store_Option | Source_Root_Option | Import_Option | Out_Option | Profile_Option | Timings_Option => True,
           others => False],
        when Check_Command => [Source_Root_Option => True, others => False],
        when Verify_Command | Import_Command => [Store_Option => True, others => False],
        when Hash_Tree_Command => [others => False],
        when Generations_Command | Switch_Command | Rollback_Command | Diff_Command =>
          [Store_Option | Profile_Option => True, others => False],
        when Collect_Command => [Store_Option | Keep_Generations_Option | Dry_Run_Option => True, others => False]);
   --  How many operands a command takes: exactly Least when Most = Least.
   type Operand_Range is record
      Least, Most : Natural;
   end record;
   Unbounded : constant := Natural'Last;
   function Operands_Of (C : Command) return Operand_Range is
     (case C is
        when Build_Command | Check_Command => (1, Unbounded),
        when Verify_Command | Generations_Command | Rollback_Command | Collect_Command => (0, 0),
        when Import_Command | Hash_Tree_Command | Switch_Command => (1, 1),
        when Diff_Command => (2, 2));

   package Text_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   type Value_Lists is array (Option) of Text_Vectors.Vector;

   The_Command : Command;
   Given : Option_Set := [others => False];
   Values : Value_Lists;
   Operands : Text_Vectors.Vector;

   procedure Parse is
      Found : Boolean := False;
      I : Positive := 2;
   begin
      if Argument_Count < 1 then
         raise Usage_Error with "no command";
      end if;
      for C in Command loop
         if Argument (1) = Command_Name (C) then
            The_Command := C;
            Found := True;
         end if;
      end loop;
      if not Found then
         raise Usage_Error with "unknown command " & Argument (1);
      end if;
      while I <= Argument_Count loop
         declare
            Text : constant String := Argument (I);
            Known : Boolean := False;
         begin
            if Text'Length > 2 and then Text (Text'First .. Text'First + 1) = "--" then
               for O in Option loop
                  if Text = Option_Name (O) then
                     Known := True;
                     if not Allowed (The_Command) (O) then
                        raise Usage_Error with Command_Name (The_Command) & " takes no " & Text;
                     elsif Given (O) and then not Is_Repeatable (O) then
                        raise Usage_Error with Text & " is given twice";
                     end if;
                     Given (O) := True;
                     if not Is_Flag (O) then
                        if I = Argument_Count then
                           raise Usage_Error with Text & " needs a value";
                        end if;
                        I := I + 1;
                        Values (O).Append (Argument (I));
                     end if;
                  end if;
               end loop;
               if not Known then
                  raise Usage_Error with "unknown option " & Text;
               end if;
            else
               Operands.Append (Text);
            end if;
         end;
         I := I + 1;
      end loop;
      declare
         Expected : constant Operand_Range := Operands_Of (The_Command);
         Count : constant Natural := Natural (Operands.Length);
      begin
         if Count < Expected.Least or else Count > Expected.Most then
            raise Usage_Error with Command_Name (The_Command) & " takes" &
              (if Expected.Least = Expected.Most then Expected.Least'Image
               elsif Expected.Most = Unbounded then " at least" & Expected.Least'Image
               else Expected.Least'Image & " to" & Expected.Most'Image) & " operands, not" & Count'Image;
         end if;
      end;
   end Parse;

   function Value_Of (O : Option) return String is (Values (O).First_Element)
   with Pre => not Is_Flag (O) and then Given (O);

   function Required (O : Option) return String is
   begin
      if not Given (O) then
         raise Usage_Error with Command_Name (The_Command) & " needs " & Option_Name (O);
      end if;
      return Value_Of (O);
   end Required;

   function Profile_Name return String is
      Name : constant String := Required (Profile_Option);
   begin
      if not Is_Profile_Name (Name) then
         raise Usage_Error with "profile """ & Name & """ is not a lowercase kebab name of at most" &
           Maximum_Name_Length'Image & " characters";
      end if;
      return Name;
   end Profile_Name;

   function Number_Operand (Text : String) return Generation_Number is
   begin
      if not Is_Generation_Name (Text) then
         raise Usage_Error with """" & Text & """ is not a generation number (1 to" & Last_Generation'Image & ")";
      end if;
      return Value (Text);
   end Number_Operand;

   function Read_File (Name : String) return String is
      File : File_Type;
      Result : Unbounded_String;
   begin
      Open (File, In_File, Name);
      while not End_Of_File (File) loop
         Append (Result, Get_Line (File) & ASCII.LF);
      end loop;
      Close (File);
      return To_String (Result);
   end Read_File;

   --  The host directory an --import NAME=PATH option gives for Name, or "".
   function Import_Path (Name : String) return String is
   begin
      for Import of Values (Import_Option) loop
         declare
            Equals : constant Natural := Ada.Strings.Fixed.Index (Import, "=");
         begin
            if Equals > 0 and then Import (Import'First .. Equals - 1) = Name then
               return Import (Equals + 1 .. Import'Last);
            end if;
         end;
      end loop;
      return "";
   end Import_Path;

   procedure Import (Store : Build_Store.Store; Name : String; Identity : Storehouse.Digest; Toolchain : Boolean);

   --  The toolchain tree the parameters pin for this host: in the store
   --  already, or imported from the directory --import NAME=PATH names
   --  (recorded in place or copied, as Import says).
   function Toolchain (Store : Build_Store.Store; Pin : Build_Declarations.Toolchain) return Storehouse.Digest is
      Name : constant String := To_String (Pin.Name);
      Pinned : constant String := To_String (Pin.Identity);
   begin
      if not Storehouse.Hex.Is_Name (Pinned) then
         raise Usage_Error with "toolchain " & Name & ": identity is not 64 lowercase hex digits";
      end if;
      if not Store.Contains (Storehouse.Tree, Storehouse.Hex.Parse (Pinned)) and then Import_Path (Name) = "" then
         raise Usage_Error with "toolchain " & Name & " is not in the store; give --import " & Name & "=PATH";
      end if;
      Import (Store, Name, Storehouse.Hex.Parse (Pinned), Toolchain => True);
      return Storehouse.Hex.Parse (Pinned);
   end Toolchain;

   --  A pinned tree (a toolchain, a fetched library's tree) not yet in the
   --  store, imported from the host directory --import NAME=PATH names when
   --  one is given: a toolchain tree in a place nothing rewrites is
   --  recorded in place (Build_Platform.Records_In_Place), another copied.
   --  A tree whose identity is not the pin is refused.
   procedure Import (Store : Build_Store.Store; Name : String; Identity : Storehouse.Digest; Toolchain : Boolean) is
      Path : constant String := Import_Path (Name);
      In_Place : constant Boolean := Build_Platform.Records_In_Place (Path, Toolchain);
      Imported : Storehouse.Digest;
   begin
      if Store.Contains (Storehouse.Tree, Identity) or else Path = "" then
         return;
      end if;
      Put_Line ("importing " & Name & " from " & Path);
      --  Hashed first, so a tree that is not the pinned one is never stored
      --  (Put_Tree and Record_Nix_Tree hash it again as they store it).
      Imported := Build_Store.Hash_Tree (Path, Absolute_Links => In_Place);
      if Imported /= Identity then
         --  Identities first: GNAT bounds exception messages.
         raise Usage_Error with Name & " pins " & Storehouse.Hex.Image (Identity) & "; it has identity " &
           Storehouse.Hex.Image (Imported) & " at " & Path;
      end if;
      if In_Place then
         Store.Record_In_Place (Path, Imported);
      else
         Store.Put_Tree (Path, Imported);
      end if;
      if Imported /= Identity then
         raise Usage_Error with Path & " changed while it was imported";
      end if;
   end Import;

   --  A path relative to the source tree's root, which must contain it.
   function Relative_Path (Source_Root, File : String) return String is
      Root : constant String := Ada.Directories.Full_Name (Source_Root) & "/";
      Full : constant String := Ada.Directories.Full_Name (File);
   begin
      if Full'Length <= Root'Length or else Full (Full'First .. Full'First + Root'Length - 1) /= Root then
         raise Build_Declarations.Declaration_Error with "the component is not in the source tree " & Source_Root;
      end if;
      return Full (Full'First + Root'Length .. Full'Last);
   end Relative_Path;

   --  A component's directory relative to the source tree's root: the
   --  directory of its component.ccl, which must lie in the tree.
   function Component_Directory (Source_Root, File : String) return String is
   begin
      if Ada.Directories.Simple_Name (File) /= Build_Declarations.Component_File then
         raise Build_Declarations.Declaration_Error with
           "a component is declared in a file named " & Build_Declarations.Component_File;
      end if;
      return Ada.Directories.Containing_Directory (Relative_Path (Source_Root, File));
   end Component_Directory;

   procedure Export (Store : Build_Store.Store; Directory : String; Products : Build_Ada_Programs.Report) is
   begin
      for P of Products.Products loop
         declare
            Target : constant String := Directory / To_String (P.Path);
         begin
            Ada.Directories.Create_Path (Ada.Directories.Containing_Directory (Target));
            if Ada.Directories.Exists (Target) then
               Ada.Directories.Delete_File (Target);
            end if;
            Ada.Directories.Copy_File (Store.Path_Of (P.Kind, P.Identity), Target);
            Build_Platform.Change_Mode
              (Target, (if P.Kind = Storehouse.Executable_File then Writable_Executable else Writable_File));
         end;
      end loop;
   end Export;

   function Items (Products : Build_Ada_Programs.Product_Vectors.Vector) return Build_Generations.Item_Vectors.Vector is
      Result : Build_Generations.Item_Vectors.Vector;
   begin
      for P of Products loop
         Result.Append (Build_Generations.Item'(P.Path, P.Kind, P.Identity));
      end loop;
      return Result;
   end Items;

   --  The location of the component being read, for diagnostics.
   Current_Declaration : Unbounded_String;

   --  The store, opened under its shared lock (Build_Timing's Store_Open).
   function Open_For_Build return Build_Store.Store is
      Timed : Build_Timing.Scope (Build_Timing.Store_Open);
      pragma Unreferenced (Timed);
   begin
      return Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
   end Open_For_Build;

   procedure Build is
      Store : constant Build_Store.Store := Open_For_Build;
      Source_Root : constant String := Required (Source_Root_Option);
      Checkout : constant String := Ada.Directories.Full_Name (Source_Root);
      Schema : constant String := Read_File (Source_Root / Schema_Path);
      Profile : Build_Declarations.Parameters;
      Known : Build_Declarations.Build_Catalog;
      Compiler, Binutils : Storehouse.Digest;
      --  What a generation records besides the outputs: the declarations
      --  read, by content, and the pinned trees.
      Generation : Build_Generations.Content;
      Failed : Boolean := False;
      procedure Parameter (Name : String; Kind : Storehouse.Entry_Kind; Identity : Storehouse.Digest) is
      begin
         if not (for some P of Generation.Parameters => To_String (P.Name) = Name) then
            Generation.Parameters.Append (Build_Generations.Item'(To_Unbounded_String (Name), Kind, Identity));
         end if;
      end Parameter;
      procedure Declaration (Path : String) is
         Identity : Storehouse.Digest;
      begin
         Store.Put_File (Source_Root / Path, Storehouse.Regular_File, Identity);
         Parameter (Path, Storehouse.Regular_File, Identity);
      end Declaration;
      Components : Unbounded_String;
   begin
      declare
         Timed : Build_Timing.Scope (Build_Timing.Declarations);
         pragma Unreferenced (Timed);
      begin
         Current_Declaration := To_Unbounded_String (Source_Root / Parameters_Path);
         Build_Declarations.Read_Parameters (Read_File (Source_Root / Parameters_Path), Schema, Profile);
         Current_Declaration := To_Unbounded_String (Source_Root / Catalog_Path);
         Build_Declarations.Read_Catalog (Read_File (Source_Root / Catalog_Path), Schema, Known);
      end;
      declare
         Timed : Build_Timing.Scope (Build_Timing.Import);
         pragma Unreferenced (Timed);
      begin
         Compiler := Toolchain (Store, Profile.Compiler_Tools);
         Binutils := Toolchain (Store, Profile.Binutils);
      end;
      if Given (Profile_Option) then
         Declaration (Parameters_Path);
         Declaration (Catalog_Path);
         Declaration (Schema_Path);
         Parameter (To_String (Profile.Compiler_Tools.Name), Storehouse.Tree, Compiler);
         Parameter (To_String (Profile.Binutils.Name), Storehouse.Tree, Binutils);
      end if;
      for File of Operands loop
         Current_Declaration := To_Unbounded_String (File);
         declare
            Program : Build_Declarations.Ada_Program;
            Result : Build_Ada_Programs.Report;
            Pins : Build_Generations.Item_Vectors.Vector;
         begin
            Append (Components, (if Length (Components) = 0 then "" else " ") & File);
            declare
               Timed : Build_Timing.Scope (Build_Timing.Declarations);
               pragma Unreferenced (Timed);
            begin
               Build_Declarations.Read_Component
                 (Read_File (File), Schema, Component_Directory (Source_Root, File), Profile, Known, Program);
            end;
            Pins.Append (Build_Generations.Item'(Profile.Compiler_Tools.Name, Storehouse.Tree, Compiler));
            Pins.Append (Build_Generations.Item'(Profile.Binutils.Name, Storehouse.Tree, Binutils));
            --  A fetched tree may be seeded offline, against the same pin.
            for T of Program.Trees loop
               Import (Store, To_String (T.Name), T.Tree, Toolchain => False);
               Pins.Append (Build_Generations.Item'(T.Name, Storehouse.Tree, T.Tree));
            end loop;
            Build_Ada_Programs.Build (Store, Source_Root, Profile, Program, Compiler, Binutils, Result);
            Store.Put_Root (Checkout, To_String (Program.Executable),
                            Build_Generations.Encode_Root (Items (Result.Products), Pins));
            if Given (Out_Option) then
               declare
                  Timed : Build_Timing.Scope (Build_Timing.Export);
                  pragma Unreferenced (Timed);
               begin
                  Export (Store, Value_Of (Out_Option), Result);
               end;
            end if;
            Put_Line ("ccl-build:" & Result.Ran'Image & " ran," & Result.Answered'Image & " answered by the cache" &
                      (if Result.Not_Reproducible > 0 then "," & Result.Not_Reproducible'Image & " not reproducible"
                       else ""));
            Failed := Failed or else Result.Not_Reproducible > 0;
            if Given (Profile_Option) then
               Declaration (Relative_Path (Source_Root, File));
               for T of Program.Trees loop
                  Parameter (To_String (T.Name), Storehouse.Tree, T.Tree);
               end loop;
               Generation.Components.Append
                 (Build_Generations.Component'(Program.Name, Items (Result.Deliverables)));
            end if;
         end;
      end loop;
      Build_Timing.Report_Build (To_String (Components));
      if Failed then
         Set_Exit_Status (Failure);
      elsif Given (Profile_Option) then
         declare
            Number : Generation_Number;
            Added : Boolean;
         begin
            Build_Generations.Add (Store, Profile_Name, Generation, Number, Added);
            Put_Line ("ccl-build: profile " & Profile_Name & (if Added then " is now at generation "
                      else " unchanged at generation ") & Image (Number));
         end;
      end if;
   end Build;

   --  Type-check build-params.ccl and the given declarations, building nothing.
   procedure Check is
      Source_Root : constant String := Required (Source_Root_Option);
      Schema : constant String := Read_File (Source_Root / Schema_Path);
      Profile : Build_Declarations.Parameters;
      Known : Build_Declarations.Build_Catalog;
      Program : Build_Declarations.Ada_Program;
   begin
      Build_Declarations.Read_Parameters (Read_File (Source_Root / Parameters_Path), Schema, Profile);
      Build_Declarations.Read_Catalog (Read_File (Source_Root / Catalog_Path), Schema, Known);
      for File of Operands loop
         begin
            Build_Declarations.Read_Component
              (Read_File (File), Schema, Component_Directory (Source_Root, File), Profile, Known, Program);
            Put_Line ("checked " & File & ": " & To_String (Program.Executable));
         exception
            when E : Build_Declarations.Declaration_Error =>
               Put_Line (Standard_Error, File & ": " & Ada.Exceptions.Exception_Message (E));
               Set_Exit_Status (Failure);
         end;
      end loop;
   end Check;

   procedure Verify is
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
      Damaged : Natural := 0;
      Checked : Natural;
      procedure Report (Kind : Storehouse.Entry_Kind; Name : String) is
      begin
         Damaged := Damaged + 1;
         Put_Line ("damaged " & Kind'Image & " " & Name);
      end Report;
      procedure Verify_All is new Build_Store.Verify (Report);
      Records, Incomplete : Natural := 0;
   begin
      Verify_All (Store, Checked);
      --  Action records: each must decode and name its own step. An
      --  output that garbage collection removed is not damage: the step
      --  runs again.
      for Name of Store.Names (Actions_Directory) loop
         Records := Records + 1;
         declare
            Step : Storehouse.Digest;
            Outputs : Build_Steps.Recorded_Vectors.Vector;
         begin
            Build_Steps.Decode_Action (Store.Read_Record (Actions_Directory / Name), Step, Outputs);
            if not Storehouse.Hex.Is_Name (Name) or else Storehouse.Hex.Image (Step) /= Name then
               raise Build_Encoding.Decode_Error with "names another step";
            end if;
            if (for some O of Outputs => not Ada.Directories.Exists (Store.Path_Of (O.Kind, O.Identity))) then
               Incomplete := Incomplete + 1;
            end if;
         exception
            when E : Build_Encoding.Decode_Error =>
               Damaged := Damaged + 1;
               Put_Line ("damaged action " & Name & ": " & Ada.Exceptions.Exception_Message (E));
         end;
      end loop;
      Put_Line ("ccl-build:" & Checked'Image & " entries checked," & Records'Image & " action records (" &
                Trimmed (Incomplete'Image) & " naming collected outputs)," & Damaged'Image & " damaged");
      if Damaged > 0 then
         Set_Exit_Status (Failure);
      end if;
   end Verify;

   procedure List_Generations is
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
      Profile : constant String := Profile_Name;
      Now_Current : constant Generation_Count := Build_Generations.Current (Store, Profile);
   begin
      for N of Build_Generations.Numbers (Store, Profile) loop
         declare
            G : constant Build_Generations.Generation := Build_Generations.Read (Store, Profile, N);
            Created : constant String := Ada.Calendar.Formatting.Image
              (Ada.Calendar."+" (Ada.Calendar.Formatting.Time_Of (Epoch_Year, 1, 1), Duration (G.Created)));
         begin
            Put_Line ((if N = Now_Current then "* " else "  ") & Ada.Strings.Fixed.Tail (Image (N), Maximum_Digits) &
                      "  " & Created & " UTC  " & Short (Build_Generations.Identity (G.Value)) & " " &
                      Trimmed (G.Value.Components.Length'Image) & " components");
         end;
      end loop;
      if Now_Current = 0 then
         Put_Line ("profile " & Profile & " has no generations");
      end if;
   end List_Generations;

   procedure Switch is
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
      Number : constant Generation_Number := Number_Operand (Operands.First_Element);
   begin
      Build_Generations.Switch (Store, Profile_Name, Number);
      Put_Line ("ccl-build: profile " & Profile_Name & " is now at generation " & Image (Number));
   end Switch;

   procedure Rollback is
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
      Number : Generation_Number;
   begin
      Build_Generations.Rollback (Store, Profile_Name, Number);
      Put_Line ("ccl-build: profile " & Profile_Name & " rolled back to generation " & Image (Number));
   end Rollback;

   --  What changed from one generation to another, by identity.
   procedure Diff is
      use Build_Generations;
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
      Old_Number : constant Generation_Number := Number_Operand (Operands (1));
      New_Number : constant Generation_Number := Number_Operand (Operands (2));
      Before : constant Content := Read (Store, Profile_Name, Old_Number).Value;
      After : constant Content := Read (Store, Profile_Name, New_Number).Value;
      Same : Natural := 0;
      function Find (List : Item_Vectors.Vector; Name : Unbounded_String; Found : out Item) return Boolean is
      begin
         for I of List loop
            if I.Name = Name then
               Found := I;
               return True;
            end if;
         end loop;
         return False;
      end Find;
      --  Items in Old and New by name: removed, added, changed.
      procedure Compare
        (Old_Items, New_Items : Item_Vectors.Vector; Indent : String; Print : Boolean; Changed : out Boolean)
      is
         Other : Item;
         procedure Put_Line (Text : String) is
         begin
            if Print then
               Ada.Text_IO.Put_Line (Text);
            end if;
         end Put_Line;
      begin
         Changed := False;
         for I of Old_Items loop
            if not Find (New_Items, I.Name, Other) then
               Put_Line (Indent & "- " & To_String (I.Name) & "  " & Short (I.Identity));
               Changed := True;
            elsif Other.Identity /= I.Identity or else Other.Kind /= I.Kind then
               Put_Line (Indent & "~ " & To_String (I.Name) & "  " & Short (I.Identity) & " -> " &
                         Short (Other.Identity));
               Changed := True;
            end if;
         end loop;
         for I of New_Items loop
            if not Find (Old_Items, I.Name, Other) then
               Put_Line (Indent & "+ " & To_String (I.Name) & "  " & Short (I.Identity));
               Changed := True;
            end if;
         end loop;
      end Compare;
      Changed : Boolean;
   begin
      Put_Line ("generation " & Image (Old_Number) & " (" & Short (Identity (Before)) & ") -> " &
                Image (New_Number) & " (" & Short (Identity (After)) & ")");
      Put_Line ("parameters:");
      Compare (Before.Parameters, After.Parameters, "  ", Print => True, Changed => Changed);
      if not Changed then
         Put_Line ("  unchanged");
      end if;
      Put_Line ("components:");
      for C of Before.Components loop
         if not (for some D of After.Components => D.Name = C.Name) then
            Put_Line ("  - " & To_String (C.Name));
         end if;
      end loop;
      for D of After.Components loop
         declare
            Found : Boolean := False;
         begin
            for C of Before.Components loop
               if C.Name = D.Name then
                  Found := True;
                  Compare (C.Outputs, D.Outputs, "", Print => False, Changed => Changed);
                  if Changed then
                     Put_Line ("  ~ " & To_String (D.Name) & ":");
                     Compare (C.Outputs, D.Outputs, "      ", Print => True, Changed => Changed);
                  else
                     Same := Same + 1;
                  end if;
               end if;
            end loop;
            if not Found then
               Put_Line ("  + " & To_String (D.Name));
            end if;
         end;
      end loop;
      Put_Line ("  " & Trimmed (Same'Image) & " unchanged");
   end Diff;

   procedure Collect_Garbage is
      Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Collection);
      Dry_Run : constant Boolean := Given (Dry_Run_Option);
      Keep : constant Generation_Count :=
        (if Given (Keep_Generations_Option) then Number_Operand (Value_Of (Keep_Generations_Option)) else 0);
      Summary : Build_Collection.Summary;
      procedure Report (Kind : Storehouse.Entry_Kind; Identity : Storehouse.Digest; Bytes : Build_Store.Byte_Count) is
      begin
         Put_Line ((if Dry_Run then "would delete " else "delete ") & Kind'Image & " " &
                   Storehouse.Hex.Image (Identity) & Bytes'Image);
      end Report;
      procedure Collect is new Build_Collection.Collect (Report);
   begin
      Collect (Store, Dry_Run, Keep, Summary);
      Put_Line ("ccl-build: " & (if Dry_Run then "would delete" else "deleted") & Summary.Deleted'Image &
                " entries (" & Trimmed (Interfaces.Unsigned_64'Image (Summary.Freed)) & " bytes) and" &
                Summary.Generations_Deleted'Image & " generations; kept" & Summary.Kept'Image & " entries;" &
                Summary.Dead_Roots'Image & " roots of removed checkouts" &
                (if Dry_Run then "" else ";" & Summary.Damaged_Removed'Image & " damaged entries removed"));
   end Collect_Garbage;

begin
   Parse;
   case The_Command is
      when Build_Command =>
         if Given (Timings_Option) then
            Build_Timing.Enable;
         end if;
         Build;
      when Check_Command => Check;
      when Verify_Command => Verify;
      when Import_Command =>
         --  Copy a host tree into the store (a toolchain that is not in the
         --  Nix store), printing the identity to pin.
         declare
            Store : constant Build_Store.Store := Build_Store.Open (Required (Store_Option), Build_Store.Shared_Use);
            Identity : Storehouse.Digest;
         begin
            Store.Put_Tree (Operands.First_Element, Identity);
            Put_Line (Storehouse.Hex.Image (Identity));
         end;
      when Hash_Tree_Command =>
         --  Trees recorded in place (Nix store trees) may link to other
         --  places by absolute names.
         Put_Line (Storehouse.Hex.Image (Build_Store.Hash_Tree
           (Operands.First_Element, Absolute_Links => Build_Platform.Records_In_Place (Operands.First_Element, Toolchain => True))));
      when Generations_Command => List_Generations;
      when Switch_Command => Switch;
      when Rollback_Command => Rollback;
      when Diff_Command => Diff;
      when Collect_Command => Collect_Garbage;
   end case;
exception
   when E : Usage_Error =>
      Put_Line (Standard_Error, "ccl-build: " & Ada.Exceptions.Exception_Message (E));
      Put_Line (Standard_Error,
        "usage: ccl-build build --store DIR --source-root DIR [--import NAME=PATH]... [--out DIR] [--profile NAME]" &
        " DIR/component.ccl..." &
        ASCII.LF & "       ccl-build check --source-root DIR DIR/component.ccl..." &
        ASCII.LF & "       ccl-build verify --store DIR" & ASCII.LF & "       ccl-build import --store DIR TREE" &
        ASCII.LF & "       ccl-build hash-tree DIR" &
        ASCII.LF & "       ccl-build generations --store DIR --profile NAME" &
        ASCII.LF & "       ccl-build switch --store DIR --profile NAME GENERATION" &
        ASCII.LF & "       ccl-build rollback --store DIR --profile NAME" &
        ASCII.LF & "       ccl-build diff --store DIR --profile NAME GENERATION GENERATION" &
        ASCII.LF & "       ccl-build gc --store DIR [--keep-generations N] [--dry-run]");
      Set_Exit_Status (Failure);
   when E : Build_Declarations.Declaration_Error =>
      Put_Line (Standard_Error, To_String (Current_Declaration) & ": " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);
   when E : Build_Steps.Step_Error | Build_Store.Store_Error | Build_Platform.Platform_Error
          | Build_Generations.Generation_Error | Build_Collection.Collection_Error =>
      Put_Line (Standard_Error, "ccl-build: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);
end Main;
