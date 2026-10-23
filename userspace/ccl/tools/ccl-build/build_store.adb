with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Interfaces.C;
with Build_Encoding;
with Build_POSIX;
with Storehouse.Hashing;
with Storehouse.Hex;

package body Build_Store is
   use Ada.Strings.Unbounded;

   Tree_Tag : constant String := "cubit.tree";
   Tree_Version : constant := 1;
   --  Kinds in a tree listing.
   type Member_Kind is (Member_File, Member_Executable, Member_Link, Member_Directory);

   Read_Piece_Bytes : constant := 1_048_576;
   Work_Counter : Natural := 0;

   package Name_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   package Name_Sorting is new Name_Vectors.Generic_Sorting;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   Step_Prefix : constant String := "step-";
   Owner_Suffix : constant String := ".owner";

   --  The process named in a claim; zero if unreadable.
   function Owner_Of (Claim : String) return Integer is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Claim);
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Close (File);
         return Integer'Value (Line);
      end;
   exception
      when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error | Constraint_Error => return 0;
   end Owner_Of;

   function Kind_Directory (Kind : Entry_Kind) return String is
     (case Kind is
        when Regular_File => "files",
        when Executable_File => "executables",
        when Tree => "trees");

   --  The names in a directory, sorted by bytes, without "." and "..".
   function Members (Directory : String) return Name_Vectors.Vector is
      use GNAT.Directory_Operations;
      Handle : Dir_Type;
      Buffer : String (1 .. 4096);
      Last : Natural;
      Result : Name_Vectors.Vector;
   begin
      Open (Handle, Directory);
      loop
         Read (Handle, Buffer, Last);
         exit when Last = 0;
         if Buffer (1 .. Last) /= "." and then Buffer (1 .. Last) /= ".." then
            Result.Append (Buffer (1 .. Last));
         end if;
      end loop;
      Close (Handle);
      Name_Sorting.Sort (Result);
      return Result;
   end Members;

   function Is_Directory (Name : String) return Boolean is
     (not Build_POSIX.Is_Symbolic_Link (Name) and then GNAT.OS_Lib.Is_Directory (Name));

   function Is_Regular (Name : String) return Boolean is
     (not Build_POSIX.Is_Symbolic_Link (Name) and then GNAT.OS_Lib.Is_Regular_File (Name));

   --  Reads Name in pieces, giving each to Process.
   generic
      with procedure Process (Data : Byte_Array);
   procedure Read_Pieces (Name : String);
   procedure Read_Pieces (Name : String) is
      use Ada.Streams;
      File : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. Read_Piece_Bytes);
      Data : Byte_Array (1 .. Read_Piece_Bytes) with Import, Address => Buffer'Address;
      Last : Stream_Element_Offset;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Name);
      loop
         Stream_IO.Read (File, Buffer, Last);
         exit when Last < Buffer'First;
         Process (Data (1 .. Natural (Last)));
      end loop;
      Stream_IO.Close (File);
   end Read_Pieces;

   function Hash_File (Name : String) return Digest is
      Hash : Storehouse.Hashing.Context;
      Result : Digest;
      procedure Add (Data : Byte_Array) is
      begin
         Storehouse.Hashing.Add (Hash, Data);
      end Add;
      procedure Read_All is new Read_Pieces (Add);
   begin
      if not Is_Regular (Name) then
         raise Store_Error with Name & " is not a regular file";
      end if;
      Storehouse.Hashing.Start (Hash);
      Read_All (Name);
      Storehouse.Hashing.Finish (Hash, Result);
      return Result;
   end Hash_File;

   function Hash_Tree (Name : String; Absolute_Links : Boolean := False) return Digest is
      Names : constant Name_Vectors.Vector := Members (Name);
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (3);
      W.Add_Text (Tree_Tag);
      W.Add_Unsigned (Tree_Version);
      W.Add_Array (Natural (Names.Length));
      for Member of Names loop
         declare
            Path : constant String := Name / Member;
         begin
            W.Add_Array (3);
            W.Add_Text (Member);
            if Build_POSIX.Is_Symbolic_Link (Path) then
               declare
                  Target : constant String := Build_POSIX.Link_Target (Path);
               begin
                  if Target'Length = 0 or else (Target (Target'First) = '/' and then not Absolute_Links) then
                     raise Store_Error with Path & ": a tree's links must be relative";
                  end if;
                  W.Add_Unsigned (Member_Kind'Pos (Member_Link));
                  W.Add_Text (Target);
               end;
            elsif Is_Directory (Path) then
               W.Add_Unsigned (Member_Kind'Pos (Member_Directory));
               W.Add_Digest (Hash_Tree (Path, Absolute_Links));
            elsif Is_Regular (Path) then
               W.Add_Unsigned (Member_Kind'Pos
                 (if Build_POSIX.Is_Executable (Path) then Member_Executable else Member_File));
               W.Add_Digest (Hash_File (Path));
            else
               raise Store_Error with Path & " is not a file, directory or link";
            end if;
         end;
      end loop;
      return W.Identity;
   end Hash_Tree;

   function Open (Root : String) return Store is
      Result : constant Store := (Root => To_Unbounded_String (Root));
   begin
      for Part in Entry_Kind loop
         Ada.Directories.Create_Path (Root / Kind_Directory (Part));
      end loop;
      Ada.Directories.Create_Path (Root / "actions");
      Ada.Directories.Create_Path (Root / "imports");
      Ada.Directories.Create_Path (Root / "roots");
      Ada.Directories.Create_Path (Root / "work");
      --  Work left by processes that are gone (a crash) is garbage: step
      --  directories whose claim names a dead process or is missing (a
      --  claim is placed before its directory and removed after it), and
      --  staging directories named after a dead process.
      for Member of Members (Root / "work") loop
         if Member'Length > Step_Prefix'Length
           and then Member (Member'First .. Member'First + Step_Prefix'Length - 1) = Step_Prefix
         then
            declare
               Claimed : constant Boolean := Member'Length > Owner_Suffix'Length
                 and then Member (Member'Last - Owner_Suffix'Length + 1 .. Member'Last) = Owner_Suffix;
               Claim : constant String := Root / "work" / (if Claimed then Member else Member & Owner_Suffix);
               Owner : constant Integer := Owner_Of (Claim);
            begin
               if Owner = 0 or else not Build_POSIX.Process_Alive (Interfaces.C.int (Owner)) then
                  Remove (Root / "work" / Member);
               end if;
            end;
         else
         declare
            Dash : Natural := 0;
            Owner : Integer;
         begin
            for I in Member'Range loop
               if Member (I) = '-' then Dash := I; exit; end if;
            end loop;
            Owner := Integer'Value (Member (Member'First .. Dash - 1));
            if not Build_POSIX.Process_Alive (Interfaces.C.int (Owner)) then
               Remove (Root / "work" / Member);
            end if;
         exception
            when Constraint_Error =>
               Remove (Root / "work" / Member);
         end;
         end if;
      end loop;
      return Result;
   end Open;

   function Root (S : Store) return String is (To_String (S.Root));

   function Step_Path (S : Store; Shape : Digest) return String is
     (Root (S) / "work" / (Step_Prefix & Storehouse.Hex.Image (Shape)));


   function Claim_Step (S : Store; Shape : Digest) return String is
      Directory : constant String := Step_Path (S, Shape);
      Claim : constant String := Directory & Owner_Suffix;
      Own : constant Integer := Integer (Build_POSIX.Process_Id);
      Work : constant String := New_Work (S);
      Note : constant String := Work / "owner";
      File : Ada.Text_IO.File_Type;
      Placed : Boolean;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Note);
      Ada.Text_IO.Put_Line (File, Ada.Strings.Fixed.Trim (Own'Image, Ada.Strings.Left));
      Ada.Text_IO.Close (File);
      Build_POSIX.Link_New (Note, Claim, Placed);
      if not Placed then
         declare
            Owner : constant Integer := Owner_Of (Claim);
         begin
            if Owner /= 0 and then Owner /= Own and then Build_POSIX.Process_Alive (Interfaces.C.int (Owner)) then
               Remove (Work);
               raise Store_Error with "step " & Storehouse.Hex.Image (Shape) & " is running in process" & Owner'Image;
            end if;
            Ada.Directories.Delete_File (Claim);
            Build_POSIX.Link_New (Note, Claim, Placed);
            if not Placed then
               Remove (Work);
               raise Store_Error with "step " & Storehouse.Hex.Image (Shape) & " was claimed meanwhile";
            end if;
         end;
      end if;
      Remove (Work);
      Remove (Directory);   --  left by a crashed run
      Ada.Directories.Create_Directory (Directory);
      return Directory;
   end Claim_Step;

   procedure Release_Step (S : Store; Shape : Digest; Keep : Boolean := False) is
   begin
      if not Keep then
         Remove (Step_Path (S, Shape));
      end if;
      Ada.Directories.Delete_File (Step_Path (S, Shape) & Owner_Suffix);
   end Release_Step;

   function Entry_Path (S : Store; Kind : Entry_Kind; Identity : Digest) return String is
     (Root (S) / Kind_Directory (Kind) / Storehouse.Hex.Image (Identity));
   Root_Suffix : constant String := ".root";
   Nix_Store_Program : constant String := "nix-store";

   function Import_Record (S : Store; Identity : Digest) return String is
     (Root (S) / "imports" / Storehouse.Hex.Image (Identity));

   --  The Nix store path an import record names, or "".
   function Imported (S : Store; Identity : Digest) return String is
      File : Ada.Text_IO.File_Type;
   begin
      if not Is_Regular (Import_Record (S, Identity)) then
         return "";
      end if;
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Import_Record (S, Identity));
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         Ada.Text_IO.Close (File);
         return (if Is_Directory (Line) then Line else "");
      end;
   end Imported;

   function Path_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return String is
     (if Kind = Tree and then not Is_Directory (Entry_Path (S, Kind, Identity))
        and then Imported (S, Identity) /= ""
      then Imported (S, Identity)
      else Entry_Path (S, Kind, Identity));

   function Contains (S : Store; Kind : Entry_Kind; Identity : Digest) return Boolean is
     (case Kind is
        when Tree => Is_Directory (Path_Of (S, Kind, Identity)),
        when others => Is_Regular (Path_Of (S, Kind, Identity)));

   function New_Work (S : Store) return String is
   begin
      Work_Counter := Work_Counter + 1;
      declare
         use Ada.Strings;
         Name : constant String := Root (S) / "work" /
           (Fixed.Trim (Integer'Image (Integer (Build_POSIX.Process_Id)), Left) & "-" &
            Fixed.Trim (Natural'Image (Work_Counter), Left));
      begin
         Ada.Directories.Create_Path (Name);
         return Name;
      end;
   end New_Work;

   procedure Remove (Name : String) is
   begin
      if Build_POSIX.Is_Symbolic_Link (Name) or else not GNAT.OS_Lib.Is_Directory (Name) then
         if not GNAT.OS_Lib.Is_Regular_File (Name) and then not Build_POSIX.Is_Symbolic_Link (Name) then
            return;
         end if;
         declare
            Deleted : Boolean;
         begin
            GNAT.OS_Lib.Delete_File (Name, Deleted);
            if not Deleted then
               raise Store_Error with "cannot delete " & Name;
            end if;
         end;
         return;
      end if;
      Build_POSIX.Change_Mode (Name, Build_POSIX.Writable_Directory);
      for Member of Members (Name) loop
         Remove (Name / Member);
      end loop;
      Ada.Directories.Delete_Directory (Name);
   end Remove;

   --  Copies a host file, returning the copy's identity.
   function Copy_File (Source, Target : String) return Digest is
      use Ada.Streams;
      Output : Stream_IO.File_Type;
      Hash : Storehouse.Hashing.Context;
      Result : Digest;
      procedure Write (Data : Byte_Array) is
         Elements : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length))
           with Import, Address => Data'Address;
      begin
         Storehouse.Hashing.Add (Hash, Data);
         Stream_IO.Write (Output, Elements);
      end Write;
      procedure Copy_All is new Read_Pieces (Write);
   begin
      Storehouse.Hashing.Start (Hash);
      Stream_IO.Create (Output, Stream_IO.Out_File, Target);
      Copy_All (Source);
      Stream_IO.Close (Output);
      Storehouse.Hashing.Finish (Hash, Result);
      Build_POSIX.Sync (Target);
      return Result;
   end Copy_File;

   --  A finished file entry: read-only, the entry time, synced.
   procedure Seal_File (Name : String; Executable : Boolean) is
   begin
      Build_POSIX.Change_Mode
        (Name, (if Executable then Build_POSIX.Read_Only_Executable else Build_POSIX.Read_Only_File));
      Build_POSIX.Set_Times (Name, Entry_Time_Seconds);
   end Seal_File;

   --  Moves a finished entry into place, or drops it if a concurrent build
   --  placed the same content first.
   procedure Publish (S : Store; Staged : String; Kind : Entry_Kind; Identity : Digest) is
      Placed : Boolean;
   begin
      Build_POSIX.Rename_New (Staged, Entry_Path (S, Kind, Identity), Placed);
      if not Placed then
         Remove (Staged);
      end if;
      Build_POSIX.Sync (Root (S) / Kind_Directory (Kind));
   end Publish;

   procedure Put_File (S : Store; Source : String; Kind : Entry_Kind; Identity : out Digest) is
   begin
      Identity := Hash_File (Source);
      if Contains (S, Kind, Identity) then
         return;
      end if;
      declare
         Staged : constant String := New_Work (S) / "entry";
      begin
         if Copy_File (Source, Staged) /= Identity then
            raise Store_Error with Source & " changed while it was stored";
         end if;
         Seal_File (Staged, Kind = Executable_File);
         Publish (S, Staged, Kind, Identity);
         Remove (Ada.Directories.Containing_Directory (Staged));
      end;
   end Put_File;

   --  Copies a tree, sealing it bottom up. The top directory stays
   --  writable until it is in place: Linux renames a directory only when
   --  its ".." entry can be rewritten.
   procedure Copy_Tree (Source, Target : String; Top : Boolean := False) is
   begin
      Ada.Directories.Create_Directory (Target);
      for Member of Members (Source) loop
         declare
            From : constant String := Source / Member;
            To : constant String := Target / Member;
         begin
            if Build_POSIX.Is_Symbolic_Link (From) then
               Build_POSIX.Symbolic_Link (Build_POSIX.Link_Target (From), To);
            elsif Is_Directory (From) then
               Copy_Tree (From, To);
            else
               declare
                  Ignored : constant Digest := Copy_File (From, To);
               begin
                  Seal_File (To, Build_POSIX.Is_Executable (From));
               end;
            end if;
         end;
      end loop;
      if not Top then
         Build_POSIX.Change_Mode (Target, Build_POSIX.Read_Only_Directory);
      end if;
   end Copy_Tree;

   procedure Put_Tree (S : Store; Source : String; Identity : out Digest) is
   begin
      if not Is_Directory (Source) then
         raise Store_Error with Source & " is not a directory";
      end if;
      Identity := Hash_Tree (Source);
      if Contains (S, Tree, Identity) then
         return;
      end if;
      declare
         Work : constant String := New_Work (S);
         Staged : constant String := Work / "entry";
      begin
         Copy_Tree (Source, Staged, Top => True);
         if Hash_Tree (Staged) /= Identity then
            Remove (Work);
            raise Store_Error with Source & " changed while it was stored";
         end if;
         Build_POSIX.Sync (Staged);
         Publish (S, Staged, Tree, Identity);
         Build_POSIX.Change_Mode (Entry_Path (S, Tree, Identity), Build_POSIX.Read_Only_Directory);
         Remove (Work);
      end;
   end Put_Tree;

   procedure Write_Bytes (Name : String; Data : Byte_Array);

   procedure Record_Nix_Tree (S : Store; Source : String; Identity : out Digest) is
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "import";
      Text : constant String := Source & ASCII.LF;
      Data : Byte_Array (1 .. Text'Length);
   begin
      if Source'Length <= Nix_Store'Length
        or else Source (Source'First .. Source'First + Nix_Store'Length - 1) /= Nix_Store
        or else not Is_Directory (Source)
      then
         Remove (Work);
         raise Store_Error with Source & " is not a directory in " & Nix_Store;
      end if;
      Identity := Hash_Tree (Source, Absolute_Links => True);
      for I in Text'Range loop
         Data (I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
      Write_Bytes (Staged, Data);
      --  A Nix garbage-collector root first, so Nix keeps the tree while
      --  the record names it (2026-10-19: a collected toolchain had to be
      --  rebuilt from source).
      declare
         use GNAT.OS_Lib;
         Arguments : Argument_List_Access := new Argument_List'
           (new String'("--add-root"), new String'(Import_Record (S, Identity) & Root_Suffix),
            new String'("--realise"), new String'(Source));
         Program : GNAT.OS_Lib.String_Access := Locate_Exec_On_Path (Nix_Store_Program);
         Succeeded : Boolean := False;
         Output : constant String := Work / "nix-store.out";
         Code : Integer;
      begin
         if Program /= null then
            Spawn (Program.all, Arguments.all, Output, Succeeded, Code);
            Succeeded := Succeeded and then Code = 0;
            GNAT.OS_Lib.Free (Program);
         end if;
         GNAT.OS_Lib.Free (Arguments);
         if not Succeeded then
            Remove (Work);
            raise Store_Error with "cannot add a Nix garbage-collector root for " & Source &
              " (" & Nix_Store_Program & " --add-root)";
         end if;
      end;
      Build_POSIX.Rename_Replace (Staged, Import_Record (S, Identity));
      Build_POSIX.Sync (Root (S) / "imports");
      Remove (Work);
   end Record_Nix_Tree;

   procedure Write_Bytes (Name : String; Data : Byte_Array) is
      use Ada.Streams;
      Output : Stream_IO.File_Type;
      Elements : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length))
        with Import, Address => Data'Address;
   begin
      Stream_IO.Create (Output, Stream_IO.Out_File, Name);
      Stream_IO.Write (Output, Elements);
      Stream_IO.Close (Output);
      Build_POSIX.Sync (Name);
   end Write_Bytes;

   function Read_Bytes (Name : String) return Byte_Array is
      Size : constant Natural := Natural (Ada.Directories.Size (Name));
      Result : Byte_Array (1 .. Size);
      Filled : Natural := 0;
      procedure Take (Data : Byte_Array) is
      begin
         if Filled + Data'Length > Size then
            raise Store_Error with Name & " grew while it was read";
         end if;
         Result (Filled + 1 .. Filled + Data'Length) := Data;
         Filled := Filled + Data'Length;
      end Take;
      procedure Read_All is new Read_Pieces (Take);
   begin
      Read_All (Name);
      if Filled /= Size then
         raise Store_Error with Name & " shrank while it was read";
      end if;
      return Result;
   end Read_Bytes;

   function Action_Path (S : Store; Step : Digest) return String is
     (Root (S) / "actions" / Storehouse.Hex.Image (Step));

   procedure Put_Action (S : Store; Step : Digest; Data : Byte_Array; Conflicting : out Boolean) is
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "action";
      Placed : Boolean;
   begin
      Write_Bytes (Staged, Data);
      Build_POSIX.Change_Mode (Staged, Build_POSIX.Read_Only_File);
      Build_POSIX.Rename_New (Staged, Action_Path (S, Step), Placed);
      Conflicting := not Placed and then Read_Bytes (Action_Path (S, Step)) /= Data;
      Build_POSIX.Sync (Root (S) / "actions");
      Remove (Work);
   end Put_Action;

   function Has_Action (S : Store; Step : Digest) return Boolean is
     (Is_Regular (Action_Path (S, Step)));

   function Get_Action (S : Store; Step : Digest) return Byte_Array is
     (Read_Bytes (Action_Path (S, Step)));

   procedure Put_Root (S : Store; Name : String; Data : Byte_Array) is
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "root";
   begin
      Write_Bytes (Staged, Data);
      Build_POSIX.Rename_Replace (Staged, Root (S) / "roots" / Name);
      Build_POSIX.Sync (Root (S) / "roots");
      Remove (Work);
   end Put_Root;

   procedure Verify (S : Store; Checked : out Natural) is
   begin
      Checked := 0;
      for Kind in Entry_Kind loop
         for Name of Members (Root (S) / Kind_Directory (Kind)) loop
            declare
               Path : constant String := Root (S) / Kind_Directory (Kind) / Name;
               Good : Boolean := Storehouse.Hex.Is_Name (Name);
            begin
               if Good then
                  Good := (if Kind = Tree then Is_Directory (Path) and then Hash_Tree (Path) = Storehouse.Hex.Parse (Name)
                           else Is_Regular (Path) and then Hash_File (Path) = Storehouse.Hex.Parse (Name));
               end if;
               Checked := Checked + 1;
               if not Good then
                  Report (Kind, Name);
               end if;
            end;
         end loop;
      end loop;
      for Name of Members (Root (S) / "imports") loop
         --  Skip the Nix garbage-collector root links beside the records.
         if not (Name'Length > Root_Suffix'Length
                 and then Name (Name'Last - Root_Suffix'Length + 1 .. Name'Last) = Root_Suffix)
         then
            Checked := Checked + 1;
            if not Storehouse.Hex.Is_Name (Name)
              or else Imported (S, Storehouse.Hex.Parse (Name)) = ""
              or else Hash_Tree (Imported (S, Storehouse.Hex.Parse (Name)), Absolute_Links => True)
                      /= Storehouse.Hex.Parse (Name)
            then
               Report (Tree, Name);
            end if;
         end if;
      end loop;
   end Verify;

end Build_Store;
