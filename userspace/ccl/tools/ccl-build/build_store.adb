with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Interfaces.C;
with Build_Encoding;
with Storehouse.Hashing;
with Storehouse.Hex;
with Storehouse.Paths;

package body Build_Store is
   use Ada.Strings.Unbounded;
   use type Interfaces.Unsigned_64;

   Tree_Tag : constant String := "cubit.tree";
   Tree_Version : constant := 1;
   --  Kinds in a tree listing.
   type Member_Kind is (Member_File, Member_Executable, Member_Link, Member_Directory);

   Read_Piece_Bytes : constant := 1_048_576;
   Work_Counter : Natural := 0;

   package Name_Sorting is new Name_Vectors.Generic_Sorting;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   --  The store's directories.
   Actions_Directory : constant String := "actions";
   Imports_Directory : constant String := "imports";
   Roots_Directory : constant String := "roots";
   Checkouts_Directory : constant String := "checkouts";      --  in roots/
   Profiles_Directory : constant String := "profiles";
   Damaged_Directory : constant String := "damaged";
   Locks_Directory : constant String := "locks";
   Work_Directory : constant String := "work";

   --  roots/checkouts/<key>/checkout: the checkout's absolute path.
   Checkout_Record : constant String := "checkout";
   --  locks/collection: shared by every user of the store, exclusive for
   --  garbage collection.
   Collection_Lock : constant String := "collection";

   Step_Prefix : constant String := "step-";
   Owner_Suffix : constant String := ".owner";
   --  The second line of a claim written by a ccl-build that holds the
   --  step's lock. Claims (work/step-<shape>.owner, the owner's process on
   --  the first line) are kept for older ccl-builds sharing the store,
   --  which have no locks: they keep a step directory whose claim names a
   --  live process. A claim with this line is stale whenever its lock is
   --  free, whatever process now has its number.
   Locked_Claim : constant String := "locked";

   --  This process's uses of stores: the collection lock is taken once
   --  per store and held until the process exits.
   type Session is record
      Root : Unbounded_String;
      Role : Store_Role;
      Collection : Build_POSIX.Lock;
   end record;
   package Session_Vectors is new Ada.Containers.Vectors (Positive, Session);
   Sessions : Session_Vectors.Vector;

   --  Entries this process found intact ("<kind directory>/<identity>").
   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets (String, Ada.Strings.Hash, "=");
   Verified : Name_Sets.Set;

   --  The step locks this process holds, by shape.
   package Claim_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Build_POSIX.Lock, Ada.Strings.Hash, "=", Build_POSIX."=");
   Claims : Claim_Maps.Map;

   function Image (N : Integer) return String is
     (Ada.Strings.Fixed.Trim (Integer'Image (N), Ada.Strings.Left));

   function Starts_With (S, Prefix : String) return Boolean is
     (S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);
   function Ends_With (S, Suffix : String) return Boolean is
     (S'Length >= Suffix'Length and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);

   --  A claim's owner, and whether a ccl-build with locks wrote it; owner
   --  zero if unreadable.
   procedure Read_Claim (Claim : String; Owner : out Integer; Locked : out Boolean) is
      File : Ada.Text_IO.File_Type;
   begin
      Owner := 0;
      Locked := False;
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Claim);
      Owner := Integer'Value (Ada.Text_IO.Get_Line (File));
      if not Ada.Text_IO.End_Of_File (File) then
         Locked := Ada.Text_IO.Get_Line (File) = Locked_Claim;
      end if;
      Ada.Text_IO.Close (File);
   exception
      when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error | Constraint_Error =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Read_Claim;

   --  Whether a claim was placed by a live older ccl-build (no locks).
   function Claimed_By_Older_Build (Claim : String) return Boolean is
      Owner : Integer;
      Locked : Boolean;
   begin
      Read_Claim (Claim, Owner, Locked);
      return not Locked and then Owner > 0 and then Owner /= Integer (Build_POSIX.Process_Id)
        and then Build_POSIX.Process_Alive (Interfaces.C.int (Owner));
   end Claimed_By_Older_Build;

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

   function Names (S : Store; Directory : String) return Name_Vectors.Vector is
     (if Is_Directory (Root (S) / Directory) then Members (Root (S) / Directory) else Name_Vectors.Empty_Vector);

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

   function Hash_Text (Text : String) return Digest is
      Hash : Storehouse.Hashing.Context;
      Data : Byte_Array (1 .. Text'Length);
      Result : Digest;
   begin
      for I in Text'Range loop
         Data (I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
      Storehouse.Hashing.Start (Hash);
      Storehouse.Hashing.Add (Hash, Data);
      Storehouse.Hashing.Finish (Hash, Result);
      return Result;
   end Hash_Text;

   function Hash_Tree (Name : String; Absolute_Links : Boolean := False) return Digest is
      Listed : constant Name_Vectors.Vector := Members (Name);
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (3);
      W.Add_Text (Tree_Tag);
      W.Add_Unsigned (Tree_Version);
      W.Add_Array (Natural (Listed.Length));
      for Member of Listed loop
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

   function Root (S : Store) return String is (To_String (S.Root));

   procedure Remove (Name : String) is
   begin
      if Build_POSIX.Is_Symbolic_Link (Name) or else not GNAT.OS_Lib.Is_Directory (Name) then
         Build_POSIX.Delete_File_If_Present (Name);
         return;
      end if;
      Build_POSIX.Change_Mode (Name, Build_POSIX.Writable_Directory);
      for Member of Members (Name) loop
         Remove (Name / Member);
      end loop;
      Build_POSIX.Delete_Directory_If_Present (Name);
   end Remove;

   function New_Work (S : Store) return String is
      Created : Boolean;
   begin
      --  Made exclusively: a directory left by a dead process whose number
      --  this process now has is never reused.
      loop
         Work_Counter := Work_Counter + 1;
         declare
            Name : constant String := Root (S) / Work_Directory /
              (Image (Integer (Build_POSIX.Process_Id)) & "-" & Image (Work_Counter));
         begin
            Build_POSIX.Make_Directory_New (Name, Created);
            if Created then
               return Name;
            end if;
         end;
      end loop;
   end New_Work;

   --  Takes Name out of view with one rename into this process's work, then
   --  deletes it; absent (another process took it first) is no error.
   procedure Reap (S : Store; Name : String) is
      Work : constant String := New_Work (S);
      Moved : Boolean;
   begin
      if Is_Directory (Name) then
         --  A directory moved to another parent must be writable (its "..").
         begin
            Build_POSIX.Change_Mode (Name, Build_POSIX.Writable_Directory);
         exception
            when Build_POSIX.POSIX_Error => null;   --  gone meanwhile
         end;
      end if;
      Build_POSIX.Rename_If_Present (Name, Work / "reaped", Moved);
      Remove (Work);
   end Reap;

   function Lock_Path (S : Store; Name : String) return String is (Root (S) / Locks_Directory / Name);

   --  Work left by processes that are gone (a crash) is garbage: step
   --  directories whose lock is free (or whose claim names a dead older
   --  ccl-build), and staging directories named after a dead process.
   procedure Clean_Work (S : Store) is
      Own : constant Integer := Integer (Build_POSIX.Process_Id);
   begin
      for Member of Members (Root (S) / Work_Directory) loop
         if Starts_With (Member, Step_Prefix) then
            declare
               Rest : constant String := Member (Member'First + Step_Prefix'Length .. Member'Last);
               Shape : constant String :=
                 (if Ends_With (Rest, Owner_Suffix) then Rest (Rest'First .. Rest'Last - Owner_Suffix'Length)
                  else Rest);
               Claim : constant String := Root (S) / Work_Directory / (Step_Prefix & Shape & Owner_Suffix);
            begin
               if not Storehouse.Hex.Is_Name (Shape) then
                  Reap (S, Root (S) / Work_Directory / Member);
               else
                  declare
                     Held : Build_POSIX.Lock :=
                       Build_POSIX.Acquire (Lock_Path (S, Step_Prefix & Shape), Build_POSIX.Exclusive, Wait => False);
                  begin
                     if Build_POSIX.Held (Held) then
                        if not Claimed_By_Older_Build (Claim) then
                           Reap (S, Root (S) / Work_Directory / (Step_Prefix & Shape));
                           Build_POSIX.Delete_File_If_Present (Claim);
                        end if;
                        Build_POSIX.Release (Held, Delete => True);
                     end if;
                  end;
               end if;
            end;
         else
            declare
               Dash : constant Natural := Ada.Strings.Fixed.Index (Member, "-");
               Owner : Integer := 0;
            begin
               if Dash > Member'First then
                  Owner := Integer'Value (Member (Member'First .. Dash - 1));
               end if;
               if Owner /= Own and then (Owner <= 0 or else not Build_POSIX.Process_Alive (Interfaces.C.int (Owner)))
               then
                  Reap (S, Root (S) / Work_Directory / Member);
               end if;
            exception
               when Constraint_Error =>
                  Reap (S, Root (S) / Work_Directory / Member);
            end;
         end if;
      end loop;
      --  Step locks whose holder is gone: a holder that died after removing
      --  its step directory but before deleting its lock file.
      for Member of Members (Root (S) / Locks_Directory) loop
         if Starts_With (Member, Step_Prefix) then
            declare
               Held : Build_POSIX.Lock :=
                 Build_POSIX.Acquire (Lock_Path (S, Member), Build_POSIX.Exclusive, Wait => False);
            begin
               if Build_POSIX.Held (Held) then
                  if not Ada.Directories.Exists (Root (S) / Work_Directory / Member) then
                     Build_POSIX.Release (Held, Delete => True);
                  else
                     Build_POSIX.Release (Held, Delete => False);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Clean_Work;

   function Open (Given_Root : String; Role : Store_Role) return Store is
      --  Steps run in their own directories, so a relative store would
      --  name the wrong files there. An absolute name is kept as given: the
      --  step directories' names reach tools' records (.ali files).
      Root : constant String :=
        (if Given_Root'Length > 0 and then Given_Root (Given_Root'First) = '/' then Given_Root
         else Ada.Directories.Full_Name (Given_Root));
      Result : constant Store := (Root => To_Unbounded_String (Root));
   begin
      for Existing of Sessions loop
         if To_String (Existing.Root) = Root then
            if Existing.Role /= Role then
               raise Store_Error with Root & " is already open for " & Existing.Role'Image;
            end if;
            Clean_Work (Result);
            return Result;
         end if;
      end loop;
      for Part in Entry_Kind loop
         Ada.Directories.Create_Path (Root / Kind_Directory (Part));
      end loop;
      Ada.Directories.Create_Path (Root / Actions_Directory);
      Ada.Directories.Create_Path (Root / Imports_Directory);
      Ada.Directories.Create_Path (Root / Roots_Directory / Checkouts_Directory);
      Ada.Directories.Create_Path (Root / Profiles_Directory);
      Ada.Directories.Create_Path (Root / Damaged_Directory);
      Ada.Directories.Create_Path (Root / Locks_Directory);
      Ada.Directories.Create_Path (Root / Work_Directory);
      declare
         Mode : constant Build_POSIX.Lock_Mode :=
           (case Role is when Shared_Use => Build_POSIX.Shared, when Collection => Build_POSIX.Exclusive);
         Granted : Build_POSIX.Lock := Build_POSIX.Acquire (Lock_Path (Result, Collection_Lock), Mode, Wait => False);
      begin
         if not Build_POSIX.Held (Granted) then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
              (case Role is
                 when Shared_Use => "ccl-build: waiting for garbage collection of " & Root & " to finish",
                 when Collection => "ccl-build: waiting for the builds using " & Root & " to finish"));
            Granted := Build_POSIX.Acquire (Lock_Path (Result, Collection_Lock), Mode, Wait => True);
         end if;
         Sessions.Append (Session'(To_Unbounded_String (Root), Role, Granted));
      end;
      Clean_Work (Result);
      return Result;
   end Open;

   function Step_Path (S : Store; Shape : String) return String is
     (Root (S) / Work_Directory / (Step_Prefix & Shape));

   function Entry_Path (S : Store; Kind : Entry_Kind; Identity : Digest) return String is
     (Root (S) / Kind_Directory (Kind) / Storehouse.Hex.Image (Identity));
   Root_Suffix : constant String := ".root";
   Nix_Store_Program : constant String := "nix-store";

   function Import_Record (S : Store; Identity : Digest) return String is
     (Root (S) / Imports_Directory / Storehouse.Hex.Image (Identity));

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

   function Is_Imported (S : Store; Identity : Digest) return Boolean is
     (not Is_Directory (Entry_Path (S, Tree, Identity)) and then Imported (S, Identity) /= "");

   function Path_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return String is
     (if Kind = Tree and then Is_Imported (S, Identity) then Imported (S, Identity)
      else Entry_Path (S, Kind, Identity));

   function Verified_Key (Kind : Entry_Kind; Identity : Digest) return String is
     (Kind_Directory (Kind) / Storehouse.Hex.Image (Identity));

   --  Moves a damaged entry or record to damaged/, out of every lookup.
   procedure Quarantine (S : Store; Path, What, Reason : String) is
      Moved : Boolean;
      Target : constant String := Root (S) / Damaged_Directory /
        (What & "-" & Image (Integer (Build_POSIX.Process_Id)) & "-" & Image (Work_Counter + 1));
   begin
      Work_Counter := Work_Counter + 1;
      if Is_Directory (Path) then
         begin
            Build_POSIX.Change_Mode (Path, Build_POSIX.Writable_Directory);
         exception
            when Build_POSIX.POSIX_Error => null;
         end;
      end if;
      Build_POSIX.Rename_If_Present (Path, Target, Moved);
      Build_POSIX.Sync (Ada.Directories.Containing_Directory (Path));
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
        "ccl-build: damaged: " & Path & " " & Reason & "; moved to " & Damaged_Directory & "/ and made again");
   end Quarantine;

   function Contains (S : Store; Kind : Entry_Kind; Identity : Digest) return Boolean is
      Path : constant String := Entry_Path (S, Kind, Identity);
      Key : constant String := Verified_Key (Kind, Identity);
   begin
      if Kind = Tree and then not Is_Directory (Path) then
         --  A Nix store tree recorded in place, checked when recorded.
         return Imported (S, Identity) /= "";
      elsif Kind /= Tree and then not Is_Regular (Path) then
         return False;
      elsif Verified.Contains (Key) then
         return True;
      elsif (if Kind = Tree then Hash_Tree (Path) else Hash_File (Path)) = Identity then
         Verified.Include (Key);
         return True;
      end if;
      Quarantine (S, Path, Kind_Directory (Kind) & "-" & Storehouse.Hex.Image (Identity),
                  "does not match its identity");
      return False;
   end Contains;

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
      return Result;
   end Copy_File;

   --  A finished file entry: read-only, the entry time, then synced, so
   --  its bytes and mode are durable before any rename names it.
   procedure Seal_File (Name : String; Executable : Boolean) is
   begin
      Build_POSIX.Change_Mode
        (Name, (if Executable then Build_POSIX.Read_Only_Executable else Build_POSIX.Read_Only_File));
      Build_POSIX.Set_Times (Name, Entry_Time_Seconds);
      Build_POSIX.Sync (Name);
   end Seal_File;

   --  Moves a finished entry into place, or drops it if a concurrent build
   --  placed the same content first; Placed says which.
   procedure Publish (S : Store; Staged : String; Kind : Entry_Kind; Identity : Digest; Placed : out Boolean) is
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
         Work : constant String := New_Work (S);
         Staged : constant String := Work / "entry";
         Placed : Boolean;
      begin
         if Copy_File (Source, Staged) /= Identity then
            Remove (Work);
            raise Store_Error with Source & " changed while it was stored";
         end if;
         Seal_File (Staged, Kind = Executable_File);
         Publish (S, Staged, Kind, Identity, Placed);
         if Placed then
            Verified.Include (Verified_Key (Kind, Identity));
         end if;
         Remove (Work);
      end;
   end Put_File;

   --  Copies a tree, sealing it bottom up; each directory is synced once
   --  its members are, so the whole tree is durable before its rename. The
   --  top directory stays writable until it is in place: Linux renames a
   --  directory only when its ".." entry can be rewritten.
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
      Build_POSIX.Sync (Target);
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
         Placed : Boolean;
      begin
         Copy_Tree (Source, Staged, Top => True);
         if Hash_Tree (Staged) /= Identity then
            Remove (Work);
            raise Store_Error with Source & " changed while it was stored";
         end if;
         Publish (S, Staged, Tree, Identity, Placed);
         if Placed then
            Build_POSIX.Change_Mode (Entry_Path (S, Tree, Identity), Build_POSIX.Read_Only_Directory);
            Build_POSIX.Sync (Entry_Path (S, Tree, Identity));
            Verified.Include (Verified_Key (Tree, Identity));
         end if;
         Remove (Work);
      end;
   end Put_Tree;

   --  Writes Data to a new file, read-only and synced.
   procedure Write_Sealed (Name : String; Data : Byte_Array) is
      use Ada.Streams;
      Output : Stream_IO.File_Type;
      Elements : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length))
        with Import, Address => Data'Address;
   begin
      Stream_IO.Create (Output, Stream_IO.Out_File, Name);
      Stream_IO.Write (Output, Elements);
      Stream_IO.Close (Output);
      Build_POSIX.Change_Mode (Name, Build_POSIX.Read_Only_File);
      Build_POSIX.Sync (Name);
   end Write_Sealed;

   function Text_Bytes (Text : String) return Byte_Array is
      Data : Byte_Array (1 .. Text'Length);
   begin
      for I in Text'Range loop
         Data (I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
      return Data;
   end Text_Bytes;

   procedure Record_Nix_Tree (S : Store; Source : String; Identity : out Digest) is
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "import";
   begin
      if Source'Length <= Nix_Store'Length
        or else Source (Source'First .. Source'First + Nix_Store'Length - 1) /= Nix_Store
        or else not Is_Directory (Source)
      then
         Remove (Work);
         raise Store_Error with Source & " is not a directory in " & Nix_Store;
      end if;
      Identity := Hash_Tree (Source, Absolute_Links => True);
      Write_Sealed (Staged, Text_Bytes (Source & ASCII.LF));
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
      Build_POSIX.Sync (Root (S) / Imports_Directory);
      Remove (Work);
   end Record_Nix_Tree;

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
     (Root (S) / Actions_Directory / Storehouse.Hex.Image (Step));

   procedure Put_Action (S : Store; Step : Digest; Data : Byte_Array; Conflicting : out Boolean) is
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "action";
      Placed : Boolean;
   begin
      Write_Sealed (Staged, Data);
      Build_POSIX.Rename_New (Staged, Action_Path (S, Step), Placed);
      Conflicting := not Placed and then Read_Bytes (Action_Path (S, Step)) /= Data;
      Build_POSIX.Sync (Root (S) / Actions_Directory);
      Remove (Work);
   end Put_Action;

   function Has_Action (S : Store; Step : Digest) return Boolean is
     (Is_Regular (Action_Path (S, Step)));

   function Get_Action (S : Store; Step : Digest) return Byte_Array is
     (Read_Bytes (Action_Path (S, Step)));

   procedure Quarantine_Action (S : Store; Step : Digest) is
   begin
      Quarantine (S, Action_Path (S, Step), Actions_Directory & "-" & Storehouse.Hex.Image (Step),
                  "cannot be decoded");
   end Quarantine_Action;

   function Record_Path (S : Store; Name : String) return String is
   begin
      if not Storehouse.Paths.Is_Relative (Name) then
         raise Store_Error with """" & Name & """ is not a relative name in the store";
      end if;
      return Root (S) / Name;
   end Record_Path;

   procedure Publish_Record (S : Store; Name : String; Data : Byte_Array; Replace : Boolean; Placed : out Boolean) is
      Target : constant String := Record_Path (S, Name);
      Directory : constant String := Ada.Directories.Containing_Directory (Target);
      Work : constant String := New_Work (S);
      Staged : constant String := Work / "record";
   begin
      Ada.Directories.Create_Path (Directory);
      Write_Sealed (Staged, Data);
      if Replace then
         Build_POSIX.Rename_Replace (Staged, Target);
         Placed := True;
      else
         Build_POSIX.Rename_New (Staged, Target, Placed);
      end if;
      Build_POSIX.Sync (Directory);
      Remove (Work);
   end Publish_Record;

   procedure Publish_New (S : Store; Name : String; Data : Byte_Array; Placed : out Boolean) is
   begin
      Publish_Record (S, Name, Data, Replace => False, Placed => Placed);
   end Publish_New;

   procedure Publish_Replace (S : Store; Name : String; Data : Byte_Array) is
      Placed : Boolean;
   begin
      Publish_Record (S, Name, Data, Replace => True, Placed => Placed);
   end Publish_Replace;

   function Read_Record (S : Store; Name : String) return Byte_Array is (Read_Bytes (Record_Path (S, Name)));

   function Record_Exists (S : Store; Name : String) return Boolean is (Is_Regular (Record_Path (S, Name)));

   procedure Delete_Record (S : Store; Name : String) is
      Path : constant String := Record_Path (S, Name);
   begin
      Build_POSIX.Delete_File_If_Present (Path);
      Build_POSIX.Sync (Ada.Directories.Containing_Directory (Path));
   end Delete_Record;

   function Checkout_Key (Checkout : String) return String is (Storehouse.Hex.Image (Hash_Text (Checkout)));

   procedure Put_Root (S : Store; Checkout, Name : String; Data : Byte_Array) is
      Directory : constant String := Roots_Directory / Checkouts_Directory / Checkout_Key (Checkout);
   begin
      if not Record_Exists (S, Directory / Checkout_Record) then
         Publish_Replace (S, Directory / Checkout_Record, Text_Bytes (Checkout & ASCII.LF));
      end if;
      Publish_Replace (S, Directory / Name, Data);
   end Put_Root;

   procedure Lock (S : Store; Name : String; L : in out Named_Lock; Waiting_Message : String) is
   begin
      if not Storehouse.Paths.Is_Relative (Name) or else Ada.Strings.Fixed.Index (Name, "/") /= 0 then
         raise Store_Error with """" & Name & """ is not a lock name";
      end if;
      L.Held := Build_POSIX.Acquire (Lock_Path (S, Name), Build_POSIX.Exclusive, Wait => False);
      if not Build_POSIX.Held (L.Held) then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Waiting_Message);
         L.Held := Build_POSIX.Acquire (Lock_Path (S, Name), Build_POSIX.Exclusive, Wait => True);
      end if;
   end Lock;

   procedure Unlock (L : in out Named_Lock) is
   begin
      if Build_POSIX.Held (L.Held) then
         Build_POSIX.Release (L.Held, Delete => True);
      end if;
   end Unlock;

   function Claim_Step (S : Store; Shape : Digest; Label : String) return String is
      Name : constant String := Storehouse.Hex.Image (Shape);
      Directory : constant String := Step_Path (S, Name);
      Claim : constant String := Directory & Owner_Suffix;
      Held : Build_POSIX.Lock :=
        Build_POSIX.Acquire (Lock_Path (S, Step_Prefix & Name), Build_POSIX.Exclusive, Wait => False);
   begin
      if not Build_POSIX.Held (Held) then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
           "ccl-build: waiting for " & Label & ": another build is running it");
         Held := Build_POSIX.Acquire (Lock_Path (S, Step_Prefix & Name), Build_POSIX.Exclusive, Wait => True);
      end if;
      --  The claim, for older ccl-builds sharing the store (Locked_Claim).
      declare
         Work : constant String := New_Work (S);
         Note : constant String := Work / "owner";
         File : Ada.Text_IO.File_Type;
         Placed : Boolean;
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Note);
         Ada.Text_IO.Put_Line (File, Image (Integer (Build_POSIX.Process_Id)));
         Ada.Text_IO.Put_Line (File, Locked_Claim);
         Ada.Text_IO.Close (File);
         Build_POSIX.Link_New (Note, Claim, Placed);
         if not Placed then
            if Claimed_By_Older_Build (Claim) then
               Remove (Work);
               Build_POSIX.Release (Held, Delete => True);
               raise Store_Error with "step " & Name & " is running in an older ccl-build";
            end if;
            Build_POSIX.Delete_File_If_Present (Claim);
            Build_POSIX.Link_New (Note, Claim, Placed);
            if not Placed then
               Remove (Work);
               Build_POSIX.Release (Held, Delete => True);
               raise Store_Error with "step " & Name & " was claimed meanwhile by an older ccl-build";
            end if;
         end if;
         Remove (Work);
      end;
      if Ada.Directories.Exists (Directory) then
         Reap (S, Directory);   --  left by a crashed run
      end if;
      Ada.Directories.Create_Directory (Directory);
      Claims.Include (Name, Held);
      return Directory;
   end Claim_Step;

   procedure Release_Step (S : Store; Shape : Digest; Keep : Boolean := False) is
      Name : constant String := Storehouse.Hex.Image (Shape);
      Held : Build_POSIX.Lock := Claims (Name);
   begin
      if not Keep then
         Remove (Step_Path (S, Name));
      end if;
      Build_POSIX.Delete_File_If_Present (Step_Path (S, Name) & Owner_Suffix);
      Claims.Delete (Name);
      Build_POSIX.Release (Held, Delete => True);
   end Release_Step;

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
      for Name of Members (Root (S) / Imports_Directory) loop
         --  Skip the Nix garbage-collector root links beside the records.
         if not Ends_With (Name, Root_Suffix) then
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

   procedure For_Each_Entry (S : Store) is
   begin
      for Kind in Entry_Kind loop
         for Name of Members (Root (S) / Kind_Directory (Kind)) loop
            if Storehouse.Hex.Is_Name (Name) then
               Visit (Kind, Storehouse.Hex.Parse (Name));
            end if;
         end loop;
      end loop;
   end For_Each_Entry;

   function Tree_Size (Name : String) return Byte_Count is
      Total : Byte_Count := 0;
   begin
      for Member of Members (Name) loop
         declare
            Path : constant String := Name / Member;
         begin
            if Is_Directory (Path) then
               Total := Total + Tree_Size (Path);
            elsif Is_Regular (Path) then
               Total := Total + Byte_Count (Ada.Directories.Size (Path));
            end if;
         end;
      end loop;
      return Total;
   end Tree_Size;

   function Size_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return Byte_Count is
      Path : constant String := Entry_Path (S, Kind, Identity);
   begin
      return (case Kind is
                when Tree => (if Is_Directory (Path) then Tree_Size (Path) else 0),
                when others => (if Is_Regular (Path) then Byte_Count (Ada.Directories.Size (Path)) else 0));
   end Size_Of;

   procedure Discard (S : Store; Kind : Entry_Kind; Identity : Digest) is
   begin
      Reap (S, Entry_Path (S, Kind, Identity));
      Build_POSIX.Sync (Root (S) / Kind_Directory (Kind));
      Verified.Exclude (Verified_Key (Kind, Identity));
   end Discard;

   function Checkout_Of (S : Store; Key : String) return String is
      Record_Name : constant String := Roots_Directory / Checkouts_Directory / Key / Checkout_Record;
   begin
      if not Record_Exists (S, Record_Name) then
         return "";
      end if;
      declare
         Data : constant Byte_Array := Read_Record (S, Record_Name);
         Text : String (1 .. Data'Length);
      begin
         for I in Data'Range loop
            Text (I - Data'First + 1) := Character'Val (Data (I));
         end loop;
         return (if Text'Length > 0 and then Text (Text'Last) = ASCII.LF then Text (Text'First .. Text'Last - 1)
                 else Text);
      end;
   end Checkout_Of;

   procedure For_Each_Root (S : Store) is
   begin
      for Name of Names (S, Roots_Directory) loop
         if Is_Regular (Root (S) / Roots_Directory / Name) then
            Visit (Name, Read_Record (S, Roots_Directory / Name), Live => True);
         end if;
      end loop;
      for Key of Names (S, Roots_Directory / Checkouts_Directory) loop
         declare
            Checkout : constant String := Checkout_Of (S, Key);
            Live : constant Boolean := Checkout /= "" and then Is_Directory (Checkout);
         begin
            for Name of Names (S, Roots_Directory / Checkouts_Directory / Key) loop
               if Name /= Checkout_Record then
                  Visit (Checkouts_Directory / Key / Name,
                         Read_Record (S, Roots_Directory / Checkouts_Directory / Key / Name), Live);
               end if;
            end loop;
         end;
      end loop;
   end For_Each_Root;

   function Other_Users (S : Store) return String is
      Own : constant Integer := Integer (Build_POSIX.Process_Id);
      Result : Unbounded_String;
   begin
      for Member of Members (Root (S) / Work_Directory) loop
         declare
            Claim : constant String := Root (S) / Work_Directory / Member;
            Owner : Integer := 0;
            Locked : Boolean := False;
         begin
            if Ends_With (Member, Owner_Suffix) then
               Read_Claim (Claim, Owner, Locked);
            elsif not Starts_With (Member, Step_Prefix) and then Ada.Strings.Fixed.Index (Member, "-") > Member'First
            then
               Owner := Integer'Value (Member (Member'First .. Ada.Strings.Fixed.Index (Member, "-") - 1));
            end if;
            if Owner > 0 and then Owner /= Own and then Build_POSIX.Process_Alive (Interfaces.C.int (Owner)) then
               Append (Result, " " & Image (Owner));
            end if;
         exception
            when Constraint_Error => null;
         end;
      end loop;
      return To_String (Result);
   end Other_Users;

   procedure Remove_Dead_Roots (S : Store) is
   begin
      for Key of Names (S, Roots_Directory / Checkouts_Directory) loop
         declare
            Checkout : constant String := Checkout_Of (S, Key);
         begin
            if Checkout = "" or else not Is_Directory (Checkout) then
               Reap (S, Root (S) / Roots_Directory / Checkouts_Directory / Key);
            end if;
         end;
      end loop;
      Build_POSIX.Sync (Root (S) / Roots_Directory / Checkouts_Directory);
   end Remove_Dead_Roots;

   procedure Empty_Damaged (S : Store; Removed : out Natural) is
   begin
      Removed := 0;
      for Name of Names (S, Damaged_Directory) loop
         Reap (S, Root (S) / Damaged_Directory / Name);
         Removed := Removed + 1;
      end loop;
   end Empty_Damaged;

end Build_Store;
