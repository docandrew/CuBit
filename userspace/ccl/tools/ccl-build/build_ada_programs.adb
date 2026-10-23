with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Build_Steps;
with Storehouse.Hex;
with Storehouse.Paths;

package body Build_Ada_Programs is
   use Build_Steps;
   use Build_Declarations;

   --  GNAT's file names: units' sources, objects, library information.
   Spec_Suffix : constant String := ".ads";
   Body_Suffix : constant String := ".adb";
   Object_Suffix : constant String := ".o";
   ALI_Suffix : constant String := ".ali";
   Binder_Prefix : constant String := "b__";
   Generated_Directory : constant String := "generated";
   Startup_Spec : constant String := "startup.ads";
   --  Its body: the startup check, run while the package elaborates (CAP-003).
   Startup_Body : constant String := "startup.adb";
   Manifest_Assembly : constant String := "manifest.S";
   Manifest_Object_Name : constant String := "manifest.o";
   Manifest_File : constant String := "manifest.ccl";
   --  The runtime's search path files (GNAT's --RTS layout).
   Source_Path_File : constant String := "ada_source_path";
   Object_Path_File : constant String := "ada_object_path";
   Object_Directory : constant String := "build";
   Program_Directory : constant String := "bin";
   Compiler_Name : constant String := "gcc";
   Binder_Name : constant String := "gnatbind";
   --  gnatbind's list of the objects to link, in b__main.adb.
   Object_List_First : constant String := "--  BEGIN Object file/option list";
   Object_List_Last : constant String := "--  END Object file/option list";
   Object_List_Item : constant String := "--   ";
   --  The "W" (with) lines of a .ali file: W unit%s source library.
   With_Line : constant String := "W ";
   Short_Identity : constant := 12;
   Build_Id_Switch : constant String := "-Wl,--build-id=sha1";

   package Name_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   package Identity_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Digest);
   package Identity_Names is new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   function Ends_With (Text, Suffix : String) return Boolean is
     (Text'Length >= Suffix'Length and then Text (Text'Last - Suffix'Length + 1 .. Text'Last) = Suffix);

   function Inside (Path, Directory : String) return Boolean is
     (Path'Length > Directory'Length + 1
      and then Path (Path'First .. Path'First + Directory'Length - 1) = Directory
      and then Path (Path'First + Directory'Length) = '/');

   function Base (File : String) return String is
     (File (File'First .. Ada.Strings.Fixed.Index (File, ".", Ada.Strings.Backward) - 1));

   procedure Need_Relative (Path, What : String) is
   begin
      if not Storehouse.Paths.Is_Relative (Path) then
         raise Step_Error with What & " """ & Path & """ is not a relative path without . or ..";
      end if;
   end Need_Relative;

   --  Path, written relative to Directory with ".." allowed (a runtime's
   --  search path files), as a plain relative path from the tree's root.
   function Resolve (Directory, Path : String) return String is
      Result : Unbounded_String := To_Unbounded_String (Directory);
      First : Positive := Path'First;
   begin
      while First <= Path'Last loop
         declare
            Last : Natural := Ada.Strings.Fixed.Index (Path (First .. Path'Last), "/");
         begin
            if Last = 0 then Last := Path'Last + 1; end if;
            declare
               Part : constant String := Path (First .. Last - 1);
               Slash : Natural;
            begin
               if Part = ".." then
                  Slash := Ada.Strings.Fixed.Index (To_String (Result), "/", Ada.Strings.Backward);
                  if Length (Result) = 0 then
                     raise Step_Error with Path & " leaves the source tree";
                  end if;
                  Result := (if Slash = 0 then Null_Unbounded_String
                             else Unbounded_Slice (Result, 1, Slash - 1));
               elsif Part /= "." and then Part /= "" then
                  Result := (if Length (Result) = 0 then To_Unbounded_String (Part) else Result & "/" & Part);
               end if;
            end;
            First := Last + 1;
         end;
      end loop;
      return To_String (Result);
   end Resolve;

   procedure Build
     (Store : Build_Store.Store; Source_Root : String;
      Profile : Parameters; Program : Ada_Program;
      Compiler, Binutils : Digest; Result : out Report)
   is
      Directory : constant String := To_String (Program.Directory);
      Objects : constant String := Directory / Object_Directory;
      Runtime : constant String := To_String (Profile.Runtime);
      Prefix : constant String := To_String (Profile.Compiler_Tools.Prefix);
      Generated : constant String := Objects / Generated_Directory;

      --  The libraries' archives, in the program's library order.
      Archives : String_Vectors.Vector;

      --  A unit's switches: the first group naming it among the program's
      --  own groups, then its libraries' groups; otherwise the program's.
      function Switches_For (File : String) return String_Vectors.Vector is
      begin
         for G of Program.Units loop
            if G.Files.Contains (File) then return G.Switches; end if;
         end loop;
         for L of Program.Libraries loop
            for G of L.Units loop
               if G.Files.Contains (File) then
                  return Result : String_Vectors.Vector := Program.Builder do
                     Result.Append (G.Switches);
                  end return;
               end if;
            end loop;
         end loop;
         return Program.Compiler;
      end Switches_For;

      --  Snapshots of the source tree, each stored once per build.
      Snapshots : Identity_Maps.Map;
      function Snapshot (Path : String; Kind : Entry_Kind) return Digest is
         Key : constant String := Entry_Kind'Image (Kind) & ":" & Path;
         Identity : Digest;
      begin
         Need_Relative (Path, "source");
         if not Snapshots.Contains (Key) then
            if Kind = Tree then
               Store.Put_Tree (Source_Root / Path, Identity);
            else
               Store.Put_File (Source_Root / Path, Kind, Identity);
            end if;
            Snapshots.Insert (Key, Identity);
         end if;
         return Snapshots (Key);
      end Snapshot;

      --  The runtime's files and directories, as GNAT finds them.
      Runtime_Inputs : Input_Vectors.Vector;
      Library_Directory : Unbounded_String;
      procedure Add_Search_Path (File : String; Library : Boolean) is
         Input_File : Ada.Text_IO.File_Type;
      begin
         Runtime_Inputs.Append
           (Input'(To_Unbounded_String (Runtime / File), Regular_File, Snapshot (Runtime / File, Regular_File)));
         Ada.Text_IO.Open (Input_File, Ada.Text_IO.In_File, Source_Root / Runtime / File);
         while not Ada.Text_IO.End_Of_File (Input_File) loop
            declare
               Line : constant String := Ada.Text_IO.Get_Line (Input_File);
            begin
               --  GNAT skips entries that name no directory; so do steps,
               --  which then see the same absence.
               if Line'Length > 0 and then Ada.Directories.Exists (Source_Root / Resolve (Runtime, Line)) then
                  declare
                     Path : constant String := Resolve (Runtime, Line);
                  begin
                     Runtime_Inputs.Append (Input'(To_Unbounded_String (Path), Tree, Snapshot (Path, Tree)));
                     if Library and then Length (Library_Directory) = 0 then
                        Library_Directory := To_Unbounded_String (Path);
                     end if;
                  end;
               end if;
            end;
         end loop;
         Ada.Text_IO.Close (Input_File);
      end Add_Search_Path;

      --  The Ada sources directly in the program's directories: file name
      --  to directory, in search order (gprbuild refuses duplicate units).
      Sources : Identity_Names.Map;
      Search_Directories : String_Vectors.Vector;
      procedure Find_Sources (Source_Directory : String) is
         use Ada.Directories;
         Search : Search_Type;
         Item : Directory_Entry_Type;
      begin
         Need_Relative (Source_Directory, "source directory");
         Start_Search (Search, Source_Root / Source_Directory, "", [Ordinary_File => True, others => False]);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);
            if (Ends_With (Simple_Name (Item), Spec_Suffix) or else Ends_With (Simple_Name (Item), Body_Suffix))
              and then not Sources.Contains (Simple_Name (Item))
            then
               Sources.Insert (Simple_Name (Item), Source_Directory);
            end if;
         end loop;
         End_Search (Search);
      end Find_Sources;

      procedure Run (S : Step; Outcome : out Build_Steps.Outcome) is
      begin
         Build_Steps.Run (Store, S, Outcome);
         if Outcome.Ran then Result.Ran := Result.Ran + 1; else Result.Answered := Result.Answered + 1; end if;
         if Outcome.Not_Reproducible then
            Result.Not_Reproducible := Result.Not_Reproducible + 1;
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
              "ccl-build: warning: " & To_String (S.Label) & " gave different outputs than its recorded run");
         end if;
         Ada.Text_IO.Put_Line ((if Outcome.Ran then "ran  " else "hit  ") &
           Storehouse.Hex.Image (Outcome.Step_Identity) (1 .. Short_Identity) & "  " & To_String (S.Label));
         for N in S.Outputs.First_Index .. S.Outputs.Last_Index loop
            Result.Products.Append
              (Product'(S.Outputs (N).Path,
                (if S.Outputs (N).Executable then Executable_File else Regular_File),
                Outcome.Outputs (N)));
         end loop;
      end Run;

      function New_Step (Label : String; Working : String) return Step is
        ((Label => To_Unbounded_String (Label), Directory => To_Unbounded_String (Working), others => <>));

      function Tool (Path : String) return Build_Steps.Program is
        ((Kind => Toolchain_Program, Toolchain => Compiler, Path => To_Unbounded_String (Path), others => <>));

      --  -B: the linker and other binutils programs the driver runs.
      procedure Tools (S : in out Step) is
      begin
         S.Arguments.Append (Piece'(Kind => Toolchain_Path, Tree => Binutils, Prefix => To_Unbounded_String ("-B"),
                                    Path => To_Unbounded_String (Program_Directory), others => <>));
      end Tools;

      procedure Literal (S : in out Step; Text : String) is
      begin
         S.Arguments.Append (Build_Steps.Text (Text));
      end Literal;

      procedure Literals (S : in out Step; Texts : String_Vectors.Vector) is
      begin
         for T of Texts loop
            Literal (S, T);
         end loop;
      end Literals;

      procedure Piece_Of (S : in out Step; Kind : Piece_Kind; Index : Natural; Prefix : String := "") is
      begin
         S.Arguments.Append (Piece'(Kind => Kind, Index => Index, Prefix => To_Unbounded_String (Prefix), others => <>));
      end Piece_Of;

      procedure Path_Of (S : in out Step; Path : String; Prefix : String) is
      begin
         S.Arguments.Append (Piece'(Kind => Step_Path, Path => To_Unbounded_String (Path),
                              Prefix => To_Unbounded_String (Prefix), others => <>));
      end Path_Of;

      --  Reproducible objects: no command line and no step directory in
      --  the debug information (the make side maps the checkout the same
      --  way: kernel/Makefile REPRODUCIBLE_ADA).
      procedure Reproducible (S : in out Step) is
      begin
         Literal (S, "-gno-record-gcc-switches");
         S.Arguments.Append (Piece'(Kind => Step_Root, Prefix => To_Unbounded_String ("-ffile-prefix-map="),
                              Suffix => To_Unbounded_String ("=."), others => <>));
      end Reproducible;

      --  A directory the runtime's trees already place in every step.
      function In_Runtime (Path : String) return Boolean is
      begin
         for I of Runtime_Inputs loop
            if I.Kind = Tree and then
              (Path = To_String (I.Path) or else Inside (Path, To_String (I.Path)))
            then
               return True;
            end if;
         end loop;
         return False;
      end In_Runtime;

      procedure Add_Runtime (S : in out Step) is
      begin
         for I of Runtime_Inputs loop
            S.Inputs.Append (I);
         end loop;
      end Add_Runtime;

      --  Outputs of earlier steps, by path.
      Produced : Identity_Maps.Map;
      procedure Remember (S : Step; Outcome : Build_Steps.Outcome) is
      begin
         for N in S.Outputs.First_Index .. S.Outputs.Last_Index loop
            Produced.Include (To_String (S.Outputs (N).Path), Outcome.Outputs (N));
         end loop;
      end Remember;
      function Use_Produced (S : in out Step; Path : String) return Input_Number is
        (Add_Input (S, Path, Regular_File, Produced (Path)));

      Toolchain_Tool : constant Build_Steps.Program :=
        Tool (Program_Directory / (Prefix & Compiler_Name));
      Units : Name_Sets.Set;           --  source files compiled
      Objects_In_Order : String_Vectors.Vector;
      ALIs : String_Vectors.Vector;
      Pending : String_Vectors.Vector;
      Main_Base : constant String := Base (To_String (Program.Main));
      Binder_Base : constant String := Binder_Prefix & Main_Base;
   begin
      Result := (others => <>);
      Need_Relative (Directory, "directory");
      Need_Relative (Runtime, "runtime");
      Add_Search_Path (Source_Path_File, Library => False);
      Add_Search_Path (Object_Path_File, Library => True);
      if Length (Library_Directory) = 0 then
         raise Step_Error with Runtime / Object_Path_File & " names no library directory";
      end if;
      for L of Program.Libraries loop
         for A of L.Archives loop
            Need_Relative (A, "archive");
            Archives.Append (A);
         end loop;
      end loop;
      Find_Sources (Directory);
      for D of Program.Sources loop
         Find_Sources (D);
         Search_Directories.Append (D);
      end loop;
      Search_Directories.Append (Generated);

      --  1 and 2. The manifest's ELF section assembly and startup bindings,
      --  and its object; none for a program without a manifest.
      if Program.Manifest_Catalog /= No_Manifest then
         --  1. The manifest: ELF section assembly and the startup bindings.
         declare
            Manifest_Source : constant String := Directory / Manifest_File;
            S : Step := New_Step ("manifest " & Manifest_Source, Objects);
            Tool_Input : constant Input_Number := Add_Input
              (S, To_String (Profile.Manifest_Tool), Executable_File,
               Snapshot (To_String (Profile.Manifest_Tool), Executable_File));
            Catalog_File : constant String := To_String (Profile.Catalogs (Program.Manifest_Catalog));
            Catalog : constant Input_Number := Add_Input
              (S, Catalog_File, Regular_File, Snapshot (Catalog_File, Regular_File));
            Schema : constant Input_Number := Add_Input
              (S, To_String (Profile.Manifest_Schema), Regular_File,
               Snapshot (To_String (Profile.Manifest_Schema), Regular_File));
            Source : constant Input_Number := Add_Input
              (S, Manifest_Source, Regular_File, Snapshot (Manifest_Source, Regular_File));
            Assembly : constant Output_Number := Add_Output (S, Objects / Manifest_Assembly);
            Startup : constant Output_Number := Add_Output (S, Generated / Startup_Spec);
            Outcome : Build_Steps.Outcome;
         begin
            S.Tool := (Kind => Input_Program, Index => Tool_Input, others => <>);
            Piece_Of (S, Input_Path, Catalog);
            Piece_Of (S, Input_Path, Source);
            Literal (S, "--startup-output");
            Piece_Of (S, Output_Path, Startup);
            Literal (S, "--schema");
            Piece_Of (S, Input_Path, Schema);
            S.Capture := Assembly;
            --  ccl-manifest writes the body beside the spec.
            Declare_Output (S, Generated / Startup_Body);
            Run (S, Outcome);
            Remember (S, Outcome);
         end;

         --  2. The manifest object.
         declare
            S : Step := New_Step ("assemble " & Objects / Manifest_Assembly, Objects);
            Assembly : constant Input_Number := Use_Produced (S, Objects / Manifest_Assembly);
            Object : constant Output_Number := Add_Output (S, Objects / Manifest_Object_Name);
            Outcome : Build_Steps.Outcome;
         begin
            S.Tool := Toolchain_Tool;
            Tools (S);
            Literal (S, "-c");
            Piece_Of (S, Local_Input, Assembly);
            Literal (S, "-o");
            Piece_Of (S, Local_Output, Object);
            Run (S, Outcome);
            Remember (S, Outcome);
         end;


      end if;

      --  3. One compile per unit, from main through the .ali "W" lines.
      Pending.Append (To_String (Program.Main));
      Units.Insert (To_String (Program.Main));
      while not Pending.Is_Empty loop
         declare
            File : constant String := Pending.First_Element;
            In_Generated : constant Boolean := not Sources.Contains (File);
            Unit_Directory : constant String := (if In_Generated then Generated else Sources (File));
            S : Step := New_Step ("compile " & File, Objects);
            Unit_Source : Input_Number := 1;
            Outcome : Build_Steps.Outcome;
         begin
            Pending.Delete_First;
            for Position in Sources.Iterate loop
               declare
                  Name : constant String := Identity_Names.Key (Position);
                  Path : constant String := Identity_Names.Element (Position) / Name;
               begin
                  if not In_Runtime (Identity_Names.Element (Position)) then
                     declare
                        Index : constant Input_Number := Add_Input (S, Path, Regular_File, Snapshot (Path, Regular_File));
                     begin
                        if Name = File then Unit_Source := Index; end if;
                     end;
                  end if;
               end;
            end loop;
            if Program.Manifest_Catalog /= No_Manifest then
               declare
                  Startup : constant Input_Number := Use_Produced (S, Generated / Startup_Spec);
                  Startup_Check : constant Input_Number := Use_Produced (S, Generated / Startup_Body);
               begin
                  if In_Generated then
                     Unit_Source := (if File = Startup_Body then Startup_Check else Startup);
                  end if;
               end;
            end if;
            Add_Runtime (S);
            Declare_Output (S, Objects / (Base (File) & Object_Suffix));
            Declare_Output (S, Objects / (Base (File) & ALI_Suffix));
            S.Tool := Toolchain_Tool;
            Literal (S, "-c");
            Literal (S, "-x");
            Literal (S, "ada");
            Literal (S, "-gnatA");
            Tools (S);
            Path_Of (S, Runtime, "--RTS=");
            Reproducible (S);
            Literals (S, Switches_For (File));
            --  The unit's own directory is searched first, then these.
            for D of Search_Directories loop
               if D /= Unit_Directory then
                  Path_Of (S, D, "-I");
               end if;
            end loop;
            if Unit_Directory /= Directory then
               Path_Of (S, Directory, "-I");
            end if;
            if not In_Generated and then In_Runtime (Unit_Directory) then
               --  A program unit whose directory the runtime also searches
               --  (shared/boot): already placed by the runtime's tree.
               Path_Of (S, Unit_Directory / File, "");
            else
               Piece_Of (S, Input_Path, Unit_Source);
            end if;
            Run (S, Outcome);
            Remember (S, Outcome);
            Objects_In_Order.Append (Objects / (Base (File) & Object_Suffix));
            ALIs.Append (Objects / (Base (File) & ALI_Suffix));
            --  Units of this program the new unit withs: compile each once.
            declare
               ALI_File : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Open (ALI_File, Ada.Text_IO.In_File,
                 Store.Path_Of (Regular_File, Produced (Objects / (Base (File) & ALI_Suffix))));
               while not Ada.Text_IO.End_Of_File (ALI_File) loop
                  declare
                     Line : constant String := Ada.Text_IO.Get_Line (ALI_File);
                     Fields : String_Vectors.Vector;
                     First : Natural := 0;
                  begin
                     if Line'Length > With_Line'Length
                       and then Line (Line'First .. Line'First + With_Line'Length - 1) = With_Line
                     then
                        for I in Line'Range loop
                           if Line (I) in ' ' | ASCII.HT then
                              if First /= 0 then Fields.Append (Line (First .. I - 1)); First := 0; end if;
                           elsif First = 0 then
                              First := I;
                           end if;
                        end loop;
                        if First /= 0 then Fields.Append (Line (First .. Line'Last)); end if;
                        if Natural (Fields.Length) >= 3 then
                           declare
                              Withed : constant String := Base (Fields (3));
                              Unit_File : constant String :=
                                (if Sources.Contains (Withed & Body_Suffix) then Withed & Body_Suffix
                                 elsif Withed & Spec_Suffix = Startup_Spec
                                   and then Program.Manifest_Catalog /= No_Manifest
                                 then Startup_Body
                                 else Withed & Spec_Suffix);
                           begin
                              if (Sources.Contains (Unit_File) or else Unit_File = Startup_Body)
                                and then not Units.Contains (Unit_File)
                              then
                                 Units.Insert (Unit_File);
                                 Pending.Append (Unit_File);
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end loop;
               Ada.Text_IO.Close (ALI_File);
            end;
         end;
      end loop;

      --  4. gnatbind -x on the main unit's library information.
      declare
         S : Step := New_Step ("bind " & To_String (Program.Main), Objects);
         Main_ALI : Input_Number := 1;
         Binder_Body : constant Output_Number := Add_Output (S, Objects / (Binder_Base & Body_Suffix));
         Outcome : Build_Steps.Outcome;
      begin
         Declare_Output (S, Objects / (Binder_Base & Spec_Suffix));
         for A of ALIs loop
            declare
               Index : constant Input_Number := Use_Produced (S, A);
            begin
               if A = Objects / (Main_Base & ALI_Suffix) then Main_ALI := Index; end if;
            end;
         end loop;
         --  gnatbind lists only the objects it finds beside their .ali.
         for O of Objects_In_Order loop
            Declare_Input (S, O, Regular_File, Produced (O));
         end loop;
         Add_Runtime (S);
         S.Tool := Tool (Program_Directory / (Prefix & Binder_Name));
         Literal (S, "-o");
         Piece_Of (S, Local_Output, Binder_Body);
         Piece_Of (S, Input_Path, Main_ALI);
         Path_Of (S, Runtime, "--RTS=");
         Literals (S, Program.Binder);
         Literal (S, "-x");
         --  gprbuild also names the object directory in gnatbind's object
         --  search path; the binder unit lists it again (a comment line,
         --  but it moves the line numbers in b__main.o's debug lines).
         Path_Of (S, Objects, "-aO");
         --  Each library archive's directory, in the order gprbuild names
         --  the projects that provide them (they are listed in b__main.adb).
         for A of Archives loop
            Declare_Input (S, A, Regular_File, Snapshot (A, Regular_File));
            Path_Of (S, Ada.Directories.Containing_Directory (A), "-aO");
         end loop;
         Run (S, Outcome);
         Remember (S, Outcome);
      end;

      --  5. The binder unit.
      declare
         S : Step := New_Step ("compile " & Binder_Base & Body_Suffix, Objects);
         Binder_Body : constant Input_Number := Use_Produced (S, Objects / (Binder_Base & Body_Suffix));
         Object : constant Output_Number := Add_Output (S, Objects / (Binder_Base & Object_Suffix));
         Outcome : Build_Steps.Outcome;
      begin
         Declare_Input (S, Objects / (Binder_Base & Spec_Suffix), Regular_File,
                        Produced (Objects / (Binder_Base & Spec_Suffix)));
         Add_Runtime (S);
         S.Tool := Toolchain_Tool;
         Literal (S, "-c");
         Literal (S, "-x");
         Literal (S, "ada");
         Literal (S, "-gnatA");
         Tools (S);
         Path_Of (S, Runtime, "--RTS=");
         Reproducible (S);
         Literal (S, "-c");
         Literal (S, "-gnatA");
         Literal (S, "-gnatWb");
         Literal (S, "-gnatiw");
         Literal (S, "-gnatws");
         Literals (S, Program.Binder_Unit);
         Piece_Of (S, Local_Input, Binder_Body);
         Literal (S, "-o");
         Piece_Of (S, Local_Output, Object);
         Run (S, Outcome);
         Remember (S, Outcome);
      end;

      --  6. The link: main's object, the binder's, the other units', then
      --  the declared arguments.
      declare
         S : Step := New_Step ("link " & Objects / To_String (Program.Executable), Objects);
         Executable : constant Output_Number :=
           Add_Output (S, Objects / To_String (Program.Executable), Executable => True);
         Outcome : Build_Steps.Outcome;
         Ordered : String_Vectors.Vector;
      begin
         --  main's object and the binder's first, then the others in the
         --  order gnatbind lists them (as gprbuild links).
         Ordered.Append (Objects_In_Order.First_Element);
         Ordered.Append (Objects / (Binder_Base & Object_Suffix));
         declare
            Listing : Ada.Text_IO.File_Type;
            Inside : Boolean := False;
         begin
            Ada.Text_IO.Open (Listing, Ada.Text_IO.In_File,
              Store.Path_Of (Regular_File, Produced (Objects / (Binder_Base & Body_Suffix))));
            while not Ada.Text_IO.End_Of_File (Listing) loop
               declare
                  Line : constant String := Ada.Strings.Fixed.Trim (Ada.Text_IO.Get_Line (Listing), Ada.Strings.Both);
               begin
                  if Line = Object_List_First then
                     Inside := True;
                  elsif Line = Object_List_Last then
                     Inside := False;
                  elsif Inside and then Ends_With (Line, Object_Suffix)
                    and then Line'Length > Object_List_Item'Length
                    and then Line (Line'First .. Line'First + Object_List_Item'Length - 1) = Object_List_Item
                  then
                     declare
                        Name : constant String := Ada.Directories.Simple_Name (Line (Line'First + Object_List_Item'Length .. Line'Last));
                     begin
                        if Objects / Name /= Objects_In_Order.First_Element
                          and then Objects_In_Order.Contains (Objects / Name)
                        then
                           Ordered.Append (Objects / Name);
                        end if;
                     end;
                  end if;
               end;
            end loop;
            Ada.Text_IO.Close (Listing);
         end;
         if Natural (Ordered.Length) /= Natural (Objects_In_Order.Length) + 1 then
            raise Step_Error with "gnatbind's object list does not name every compiled unit";
         end if;
         for O of Ordered loop
            Piece_Of (S, Local_Input, Use_Produced (S, O));
         end loop;
         --  Library archives follow the objects, as gprbuild links them.
         for A of Archives loop
            Piece_Of (S, Input_Path, Add_Input (S, A, Regular_File, Snapshot (A, Regular_File)));
         end loop;
         Add_Runtime (S);
         for A of Program.Linker loop
            case A.Kind is
               when Text => Literal (S, To_String (A.Value));
               when File =>
                  Piece_Of (S, Input_Path, Add_Input
                    (S, To_String (A.Value), Regular_File, Snapshot (To_String (A.Value), Regular_File)));
               when Manifest_Object =>
                  if Program.Manifest_Catalog = No_Manifest then
                     raise Step_Error with "the linker names the manifest object of a program without a manifest";
                  end if;
                  Piece_Of (S, Local_Input, Use_Produced (S, Objects / Manifest_Object_Name));
            end case;
         end loop;
         --  What gnat-15.cgpr's linker always adds.
         for A of Archives loop
            Path_Of (S, Ada.Directories.Containing_Directory (A), "-L");
         end loop;
         Path_Of (S, To_String (Library_Directory), "-L");
         Tools (S);
         Literal (S, Build_Id_Switch);
         S.Tool := Toolchain_Tool;
         Literal (S, "-o");
         Piece_Of (S, Local_Output, Executable);
         Run (S, Outcome);
         Remember (S, Outcome);
      end;
   end Build;

end Build_Ada_Programs;
