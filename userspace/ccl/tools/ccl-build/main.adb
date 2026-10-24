with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Build_Ada_Programs;
with Build_Declarations;
with Build_Encoding;
with Build_POSIX;
with Build_Steps;
with Build_Store;
with Storehouse;
with Storehouse.Hex;

--  ccl-build, hosted on Linux (docs/ccl-build.md):
--
--    ccl-build build --store DIR --source-root DIR [--import NAME=PATH]...
--                    [--out DIR] DIR/component.ccl
--                    (with build-params.ccl and build-catalog.ccl)
--    ccl-build check --source-root DIR DIR/component.ccl...   (type-check only)
--    ccl-build verify --store DIR
--    ccl-build import --store DIR TREE   (copy a tree in, print its identity)
--    ccl-build hash-tree DIR
procedure Main is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use type Storehouse.Digest;
   use type Storehouse.Entry_Kind;

   --  The build schema's and the build parameters' places in the tree.
   Schema_Path : constant String := "userspace/ccl/interfaces/build.ccl";
   Parameters_Path : constant String := "build-params.ccl";
   Catalog_Path : constant String := "build-catalog.ccl";
   Root_Tag : constant String := "cubit.root";
   Root_Version : constant := 1;
   Root_Fields : constant := 3;
   Product_Fields : constant := 3;
   Writable_File : constant := 8#644#;
   Writable_Executable : constant := 8#755#;

   Usage_Error : exception;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   --  The value of option Name, or "" when absent.
   function Option (Name : String) return String is
   begin
      for I in 2 .. Argument_Count - 1 loop
         if Argument (I) = Name then
            return Argument (I + 1);
         end if;
      end loop;
      return "";
   end Option;

   function Required (Name : String) return String is
   begin
      if Option (Name) = "" then
         raise Usage_Error with "missing " & Name;
      end if;
      return Option (Name);
   end Required;

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
      for I in 2 .. Argument_Count - 1 loop
         if Argument (I) = "--import" then
            declare
               Import : constant String := Argument (I + 1);
               Equals : constant Natural := Ada.Strings.Fixed.Index (Import, "=");
            begin
               if Equals > 0 and then Import (Import'First .. Equals - 1) = Name then
                  return Import (Equals + 1 .. Import'Last);
               end if;
            end;
         end if;
      end loop;
      return "";
   end Import_Path;

   --  The toolchain tree the parameters pin: in the store already, or
   --  imported from the host directory --import NAME=PATH names (a Nix
   --  store tree is recorded in place, another directory copied).
   function Toolchain (Store : Build_Store.Store; Pin : Build_Declarations.Toolchain) return Storehouse.Digest is
      Name : constant String := To_String (Pin.Name);
      Pinned : constant String := To_String (Pin.Identity);
      Path : constant String := Import_Path (Name);
      Identity, Imported : Storehouse.Digest;
   begin
      if not Storehouse.Hex.Is_Name (Pinned) then
         raise Usage_Error with "toolchain " & Name & ": identity is not 64 lowercase hex digits";
      end if;
      Identity := Storehouse.Hex.Parse (Pinned);
      if Store.Contains (Storehouse.Tree, Identity) then
         return Identity;
      end if;
      if Path = "" then
         raise Usage_Error with "toolchain " & Name & " is not in the store; give --import " & Name & "=PATH";
      end if;
      Put_Line ("importing " & Name & " from " & Path);
      if Path'Length > Build_Store.Nix_Store'Length
        and then Path (Path'First .. Path'First + Build_Store.Nix_Store'Length - 1) = Build_Store.Nix_Store
      then
         Store.Record_Nix_Tree (Path, Imported);
      else
         Store.Put_Tree (Path, Imported);
      end if;
      if Imported /= Identity then
         raise Usage_Error with Path & " has identity " & Storehouse.Hex.Image (Imported) & "; " &
           Name & " pins " & Pinned;
      end if;
      return Identity;
   end Toolchain;

   --  A component's directory relative to the source tree's root: the
   --  directory of its component.ccl, which must lie in the tree.
   function Component_Directory (Source_Root, File : String) return String is
      Root : constant String := Ada.Directories.Full_Name (Source_Root) & "/";
      Full : constant String := Ada.Directories.Full_Name (File);
   begin
      if Ada.Directories.Simple_Name (Full) /= Build_Declarations.Component_File then
         raise Build_Declarations.Declaration_Error with
           "a component is declared in a file named " & Build_Declarations.Component_File;
      end if;
      if Full'Length <= Root'Length or else Full (Full'First .. Full'First + Root'Length - 1) /= Root then
         raise Build_Declarations.Declaration_Error with "the component is not in the source tree " & Source_Root;
      end if;
      return Ada.Directories.Containing_Directory (Full (Full'First + Root'Length .. Full'Last));
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
            Build_POSIX.Change_Mode
              (Target, (if P.Kind = Storehouse.Executable_File then Writable_Executable else Writable_File));
         end;
      end loop;
   end Export;

   procedure Record_Root (Store : Build_Store.Store; Name : String; Products : Build_Ada_Programs.Report) is
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (Root_Fields);
      W.Add_Text (Root_Tag);
      W.Add_Unsigned (Root_Version);
      W.Add_Array (Natural (Products.Products.Length));
      for P of Products.Products loop
         W.Add_Array (Product_Fields);
         W.Add_Text (To_String (P.Path));
         W.Add_Unsigned (Storehouse.Entry_Kind'Pos (P.Kind));
         W.Add_Digest (P.Identity);
      end loop;
      Store.Put_Root (Name, W.Bytes);
   end Record_Root;

   procedure Build is
      Store : constant Build_Store.Store := Build_Store.Open (Required ("--store"));
      Source_Root : constant String := Required ("--source-root");
      Schema : constant String := Read_File (Source_Root / Schema_Path);
      Profile : Build_Declarations.Parameters;
      Known : Build_Declarations.Build_Catalog;
      Program : Build_Declarations.Ada_Program;
      Result : Build_Ada_Programs.Report;
   begin
      Build_Declarations.Read_Parameters (Read_File (Source_Root / Parameters_Path), Schema, Profile);
      Build_Declarations.Read_Catalog (Read_File (Source_Root / Catalog_Path), Schema, Known);
      Build_Declarations.Read_Component
        (Read_File (Argument (Argument_Count)), Schema,
         Component_Directory (Source_Root, Argument (Argument_Count)), Profile, Known, Program);
      Build_Ada_Programs.Build
        (Store, Source_Root, Profile, Program,
         Toolchain (Store, Profile.Compiler_Tools), Toolchain (Store, Profile.Binutils), Result);
      Record_Root (Store, To_String (Program.Executable), Result);
      if Option ("--out") /= "" then
         Export (Store, Option ("--out"), Result);
      end if;
      Put_Line ("ccl-build:" & Result.Ran'Image & " ran," & Result.Answered'Image & " answered by the cache" &
                (if Result.Not_Reproducible > 0 then "," & Result.Not_Reproducible'Image & " not reproducible"
                 else ""));
      if Result.Not_Reproducible > 0 then
         Set_Exit_Status (Failure);
      end if;
   end Build;

   --  Type-check build-params.ccl and the given declarations, building nothing.
   procedure Check is
      Source_Root : constant String := Required ("--source-root");
      Schema : constant String := Read_File (Source_Root / Schema_Path);
      Profile : Build_Declarations.Parameters;
      Known : Build_Declarations.Build_Catalog;
      Program : Build_Declarations.Ada_Program;
      Last : Positive := Argument_Count;
   begin
      Build_Declarations.Read_Parameters (Read_File (Source_Root / Parameters_Path), Schema, Profile);
      Build_Declarations.Read_Catalog (Read_File (Source_Root / Catalog_Path), Schema, Known);
      while Last > 1 and then Argument (Last - 1) /= "--source-root" loop
         Last := Last - 1;
      end loop;
      for I in Last + 1 .. Argument_Count loop
         begin
            Build_Declarations.Read_Component
              (Read_File (Argument (I)), Schema, Component_Directory (Source_Root, Argument (I)),
               Profile, Known, Program);
            Put_Line ("checked " & Argument (I) & ": " & To_String (Program.Executable));
         exception
            when E : Build_Declarations.Declaration_Error =>
               Put_Line (Standard_Error, Argument (I) & ": " & Ada.Exceptions.Exception_Message (E));
               Set_Exit_Status (Failure);
         end;
      end loop;
   end Check;

   procedure Verify is
      Store : constant Build_Store.Store := Build_Store.Open (Required ("--store"));
      Damaged : Natural := 0;
      Checked : Natural;
      procedure Report (Kind : Storehouse.Entry_Kind; Name : String) is
      begin
         Damaged := Damaged + 1;
         Put_Line ("damaged " & Kind'Image & " " & Name);
      end Report;
      procedure Verify_All is new Build_Store.Verify (Report);
   begin
      Verify_All (Store, Checked);
      Put_Line ("ccl-build:" & Checked'Image & " entries checked," & Damaged'Image & " damaged");
      if Damaged > 0 then
         Set_Exit_Status (Failure);
      end if;
   end Verify;

begin
   if Argument_Count < 2 then
      raise Usage_Error with "no command";
   elsif Argument (1) = "build" then
      Build;
   elsif Argument (1) = "check" then
      Check;
   elsif Argument (1) = "verify" then
      Verify;
   elsif Argument (1) = "import" then
      --  Copy a host tree into the store (a toolchain that is not in the
      --  Nix store), printing the identity to pin.
      declare
         Store : constant Build_Store.Store := Build_Store.Open (Required ("--store"));
         Identity : Storehouse.Digest;
      begin
         Store.Put_Tree (Argument (Argument_Count), Identity);
         Put_Line (Storehouse.Hex.Image (Identity));
      end;
   elsif Argument (1) = "hash-tree" then
      --  Nix store trees link to other store paths by absolute names.
      Put_Line (Storehouse.Hex.Image (Build_Store.Hash_Tree
        (Argument (2), Absolute_Links => Ada.Strings.Fixed.Index (Argument (2), Build_Store.Nix_Store) = 1)));
   else
      raise Usage_Error with "unknown command " & Argument (1);
   end if;
exception
   when E : Usage_Error =>
      Put_Line (Standard_Error, "ccl-build: " & Ada.Exceptions.Exception_Message (E));
      Put_Line (Standard_Error,
        "usage: ccl-build build --store DIR --source-root DIR [--import NAME=PATH] [--out DIR] DIR/component.ccl" &
        ASCII.LF & "       ccl-build check --source-root DIR DIR/component.ccl..." &
        ASCII.LF & "       ccl-build verify --store DIR" & ASCII.LF & "       ccl-build import --store DIR TREE" &
        ASCII.LF & "       ccl-build hash-tree DIR");
      Set_Exit_Status (Failure);
   when E : Build_Declarations.Declaration_Error =>
      Put_Line (Standard_Error, Argument (Argument_Count) & ": " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);
   when E : Build_Steps.Step_Error | Build_Store.Store_Error | Build_POSIX.POSIX_Error =>
      Put_Line (Standard_Error, "ccl-build: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);
end Main;
