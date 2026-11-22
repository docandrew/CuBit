------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with Storehouse;
with Storehouse.Hashing;
with System_Generation_Records;

package body Install_Sources is

   use type Install_Tree.Add_Result, Install_Tree.Entry_Kind, Disk_Layout.Install_Volume;
   use type Storehouse.Digest;
   package Records renames System_Generation_Records;

   --  The loan: path names, directory pages and file chunks pass through
   --  it. Its size bounds one Read request.
   Loan_Pages : constant := 16;
   Loan_Bytes : constant := Loan_Pages * CuBit.Memory_Grants.PAGE_SIZE;

   Filesystem_Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address := System.Null_Address;
   Ready_State : Boolean := False;

   Maximum_Source_Root : constant := 512;
   Source_Root : String (1 .. Maximum_Source_Root);
   Source_Length : Natural range 0 .. Maximum_Source_Root := 0;

   Open_Volume : Disk_Layout.Install_Volume := Disk_Layout.Root_Volume;
   Open_Entry : Natural := 0;
   Open_Handle : File_Handle := 0;
   Files_Opened : Unsigned_64 := 0;
   Bytes_Given : Unsigned_64 := 0;
   Generation_Entry : Install_Tree.Entry_Index := Install_Tree.Entry_Index'Last;
   Component_Count : Natural := 0;

   --  The last failure, and the path it was about.
   Failed_Why : Failure := None;
   Maximum_Failed_Path : constant := 256;
   Failed_Path : String (1 .. Maximum_Failed_Path);
   Failed_Length : Natural range 0 .. Maximum_Failed_Path := 0;

   function Files_Read return Unsigned_64 is (Files_Opened);
   function Bytes_Read return Unsigned_64 is (Bytes_Given);
   function Generation_First return Install_Tree.Entry_Index is (Generation_Entry);
   function Components return Natural is (Component_Count);
   function Last_Failure return Failure is (Failed_Why);
   function Last_Failed_Path return String is (Failed_Path (1 .. Failed_Length));

   procedure Fail (Why : Failure; Path : String) is
      Kept : constant Natural := Natural'Min (Path'Length, Maximum_Failed_Path);
   begin
      Failed_Why := Why;
      Failed_Length := Kept;
      Failed_Path (1 .. Kept) := Path (Path'First .. Path'First + Kept - 1);
   end Fail;

   --  One filesystem request with its deadline. Timed_Out: none came.
   function Call (Request : in out Message; Path : String) return Boolean is
      Reply : MessageTag;
   begin
      Reply := capCall (Filesystem_Endpoint, Request, Deadline_After (Request_Deadline_Ms));
      if Reply.label = REPLY_TIMEOUT then
         Fail (Source_Timed_Out, Path);
         return False;
      elsif Request.tag.label /= REPLY_OK then
         Fail (Refused, Path);
         return False;
      end if;
      return True;
   end Call;

   procedure Put_Path (Path : String) is
      Bytes : String (1 .. Path'Length) with Import, Address => Buffer;
   begin
      Bytes := Path;
   end Put_Path;

   procedure Initialize (Filesystem : CuBit.Handles.Endpoint; Ready : out Boolean) is
      Raw : Unsigned_64;
   begin
      if not Ready_State then
         Filesystem_Endpoint := Filesystem;
         Raw := syscall (SYSCALL_SBRK, (Loan_Pages + 1) * CuBit.Memory_Grants.PAGE_SIZE);
         if Raw /= 0 and then Raw /= Unsigned_64'Last then
            Buffer := To_Address (Integer_Address
              ((Raw + CuBit.Memory_Grants.PAGE_SIZE - 1) and not (CuBit.Memory_Grants.PAGE_SIZE - 1)));
            CuBit.Memory_Grants.Create_Via_Capability
              (Filesystem_Endpoint, Buffer, Loan_Pages, True, Loan, Ready_State);
         end if;
      end if;
      Ready := Ready_State;
   end Initialize;

   ---------------------------------------------------------------------------
   --  INDEX and the sources of each entry's bytes.
   ---------------------------------------------------------------------------
   Index_Name : constant String := "/INDEX";
   Field_Separator : constant Character := ASCII.HT;
   --  A named ceiling: the device-build image's INDEX (the image closure
   --  and about 3,500 toolchain, Storehouse and source files, each named
   --  twice, now with their digests) is about 600 KiB.
   Maximum_Index : constant := 2 * 1024 * 1024;
   subtype Index_Position is Natural range 0 .. Maximum_Index;
   Index_Text : String (1 .. Maximum_Index);
   Index_Length : Index_Position := 0;

   Executable_Mark : constant String := "executable";
   Digest_Mark : constant String := "sha256=";
   Volume_Mark : constant Character := '@';

   --  Where an entry's bytes are: a slice of Index_Text below the payload
   --  directory or the medium's root, a whole CuBit name, or text this
   --  install makes (a generation record).
   type Source_Kind is (In_Payload, On_Medium, Named, Generated);
   type File_Source is record
      Kind  : Source_Kind := In_Payload;
      First : Index_Position := 1;
      Last  : Index_Position := 0;
      --  Generated: which text.
      Text  : Natural range 0 .. 2 := 0;
   end record;
   type Source_Table is array (Disk_Layout.Install_Volume, Install_Tree.Entry_Index) of File_Source;
   Sources : Source_Table;

   --  The components' SHA-256 as INDEX gives them, and what Read hashed.
   type Digest_Table is array (Disk_Layout.Install_Volume, Install_Tree.Entry_Index) of Storehouse.Digest;
   type Flag_Table is array (Disk_Layout.Install_Volume, Install_Tree.Entry_Index) of Boolean;
   Expected, Written : Digest_Table;
   Has_Expected, Has_Written : Flag_Table := [others => [others => False]];

   --  The generation records' text.
   Generation_Paths : constant array (1 .. 2) of access constant String :=
     [new String'("1"), new String'("current")];
   Maximum_Generated : constant := 512;
   type Generated_Text is record
      Length : Natural range 0 .. Maximum_Generated := 0;
      Data   : String (1 .. Maximum_Generated) := [others => ' '];
   end record;
   Generated_Texts : array (1 .. 2) of Generated_Text;

   --  The medium's root: the source directory's volume ("@cd:0").
   function Medium return String is
   begin
      for I in 1 .. Source_Length loop
         if Source_Root (I) = '/' then
            return Source_Root (1 .. I - 1);
         end if;
      end loop;
      return Source_Root (1 .. Source_Length);
   end Medium;

   function Source_Path (Item : File_Source) return String is
     (case Item.Kind is
        when Named => Index_Text (Item.First .. Item.Last),
        when On_Medium => Medium & "/" & Index_Text (Item.First .. Item.Last),
        when In_Payload => Source_Root (1 .. Source_Length) & "/" & Index_Text (Item.First .. Item.Last),
        when Generated => "(generated)");

   --  Opens Path; on success its handle.
   function Open (Path : String; Handle : out File_Handle) return Boolean is
      Request : Message;
   begin
      Handle := 0;
      if Path'Length = 0 or else Path'Length > MAXIMUM_PATH_BYTES or else Path'Length > Loan_Bytes then
         Fail (Refused, Path);
         return False;
      end if;
      Put_Path (Path);
      Request := Open_Request (Loan, Path'Length);
      if not Call (Request, Path) then
         return False;
      end if;
      Handle := File_Handle (Request.words (0));
      return True;
   end Open;

   procedure Close (Handle : File_Handle; Path : String) is
      Request : Message := Close_Request (Handle);
      Ignore : Boolean;
   begin
      Ignore := Call (Request, Path);
   end Close;

   --  A file's size: its end, by a seek.
   function Size_Of (Path : String; Size : out Unsigned_64) return Boolean is
      Handle : File_Handle;
      Request : Message;
      OK : Boolean;
   begin
      Size := 0;
      if not Open (Path, Handle) then
         return False;
      end if;
      Request := Seek_Request (Handle, 0, From_End);
      OK := Call (Request, Path);
      if OK then
         Size := Request.words (0);
      end if;
      Close (Handle, Path);
      return OK;
   end Size_Of;

   function Read_Index (Path : String) return Walk_Result is
      Handle : File_Handle;
      Request : Message;
   begin
      Index_Length := 0;
      if not Open (Path, Handle) then
         return (if Failed_Why = Source_Timed_Out then Timed_Out else Unreadable);
      end if;
      loop
         declare
            Chunk : String (1 .. Loan_Bytes) with Import, Address => Buffer;
         begin
            Request := Read_At_Request (Handle, Loan, Loan_Bytes, Unsigned_64 (Index_Length));
            if not Call (Request, Path) then
               Close (Handle, Path);
               return (if Failed_Why = Source_Timed_Out then Timed_Out else Unreadable);
            end if;
            declare
               Got : constant Natural := Natural (Unsigned_64'Min (Request.words (0), Loan_Bytes));
            begin
               exit when Got = 0;
               if Index_Length + Got > Maximum_Index then
                  Close (Handle, Path);
                  return Too_Large;
               end if;
               Index_Text (Index_Length + 1 .. Index_Length + Got) := Chunk (1 .. Got);
               Index_Length := Index_Length + Got;
            end;
         end;
      end loop;
      Close (Handle, Path);
      return Walked;
   end Read_Index;

   --  The entry Name under Parent in Volume's tree (0: none).
   function Child (Volume : Disk_Layout.Install_Volume; Parent : Install_Tree.Parent_Index;
                   Name : String) return Natural is
   begin
      for E in 1 .. Trees (Volume).Count loop
         if Trees (Volume).Entries (E).Parent = Parent
           and then Install_Tree.Name_Of (Trees (Volume), E) = Name
         then
            return E;
         end if;
      end loop;
      return 0;
   end Child;

   --  The directory Name under Parent, made when absent.
   procedure Directory
     (Volume : Disk_Layout.Install_Volume; Parent : Install_Tree.Parent_Index; Name : String;
      Made : out Install_Tree.Parent_Index; Result : in out Walk_Result)
   is
      Existing : constant Natural := Child (Volume, Parent, Name);
      Added : Install_Tree.Add_Result;
   begin
      Made := Install_Tree.Volume_Root;
      if Existing /= 0 then
         if Trees (Volume).Entries (Existing).Kind /= Install_Tree.Directory then
            Result := Malformed;
         else
            Made := Existing;
         end if;
         return;
      end if;
      Install_Tree.Add (Trees (Volume), Install_Tree.Directory, Parent, Name, 0, Added);
      if Added /= Install_Tree.Added then
         Result := (if Added = Install_Tree.Tree_Full then Too_Large else Malformed);
      else
         Made := Trees (Volume).Count;
      end if;
   end Directory;

   --  A regular file Name under Parent, Size bytes from Source.
   procedure File
     (Volume : Disk_Layout.Install_Volume; Parent : Install_Tree.Parent_Index; Name : String;
      Size : Unsigned_64; Source : File_Source; Executable : Boolean; Result : in out Walk_Result)
   is
      Added : Install_Tree.Add_Result;
   begin
      if Size > Install_Tree.Maximum_File_Bytes then
         Result := Too_Large;
         return;
      end if;
      Install_Tree.Add (Trees (Volume), Install_Tree.Regular_File, Parent, Name, Size, Added, Executable);
      if Added /= Install_Tree.Added then
         Result := (if Added = Install_Tree.Tree_Full then Too_Large else Malformed);
      else
         Sources (Volume, Trees (Volume).Count) := Source;
         Has_Expected (Volume, Trees (Volume).Count) := False;
      end if;
   end File;

   --  Lowercase hex: 64 characters for a SHA-256.
   procedure From_Hex (Text : String; Item : out Storehouse.Digest; Valid : out Boolean) is
      Converted : Records.Digest;
   begin
      Records.From_Hex (Text, Converted, Valid);
      Item := Storehouse.Digest (Converted);
   end From_Hex;

   function Hex (Item : Storehouse.Digest) return String is (Records.Hex (Records.Digest (Item)));

   --  The live system's generations stay behind (an installed system
   --  starts at generation 1).
   Profile_Prefix : constant String := "root/storehouse/profiles/system/";

   --  Adds the file or directory at payload path Line.
   procedure Add_Path
     (Line : String; Source : File_Source; Executable : Boolean; Digest_Known : Boolean;
      Digest : Storehouse.Digest; Result : in out Walk_Result)
   is
      Volume : Disk_Layout.Install_Volume;
      Rest_First : Positive;
      Parent : Install_Tree.Parent_Index := Install_Tree.Volume_Root;
      Component_First : Positive;
      Size : Unsigned_64;
   begin
      if Line'Length >= Profile_Prefix'Length
        and then Line (Line'First .. Line'First + Profile_Prefix'Length - 1) = Profile_Prefix
      then
         return;
      elsif Line'Length > 4 and then Line (Line'First .. Line'First + 3) = "esp/" then
         Volume := Disk_Layout.ESP_Volume;
         Rest_First := Line'First + 4;
      elsif Line'Length > 5 and then Line (Line'First .. Line'First + 4) = "root/" then
         Volume := Disk_Layout.Root_Volume;
         Rest_First := Line'First + 5;
      else
         Result := Malformed;
         return;
      end if;
      Component_First := Rest_First;
      for I in Rest_First .. Line'Last loop
         if Line (I) = '/' then
            Directory (Volume, Parent, Line (Component_First .. I - 1), Parent, Result);
            if Result /= Walked then
               return;
            end if;
            Component_First := I + 1;
         end if;
      end loop;
      if Component_First > Line'Last then
         --  "root/<path>/", alone on its line: a directory, made above.
         if Source.Kind /= In_Payload or else Executable or else Digest_Known then
            Result := Malformed;
         end if;
         return;
      end if;
      if not Size_Of (Source_Path (Source), Size) then
         Result := (if Failed_Why = Source_Timed_Out then Timed_Out else Unreadable);
         return;
      end if;
      File (Volume, Parent, Line (Component_First .. Line'Last), Size, Source, Executable, Result);
      if Result = Walked and then Digest_Known then
         Expected (Volume, Trees (Volume).Count) := Digest;
         Has_Expected (Volume, Trees (Volume).Count) := True;
      end if;
   end Add_Path;

   --  One INDEX line: "<path>" [TAB "<where>" [TAB field]...].
   procedure Add_Line (Whole : String; Result : in out Walk_Result) is
      Path_Last : Natural := Whole'Last;
      Source : File_Source := (Kind => In_Payload, First => Whole'First, Last => Whole'Last, Text => 0);
      Executable, Digest_Known : Boolean := False;
      Digest : Storehouse.Digest := [others => 0];
      Field_First : Positive;
      Field_Index : Natural := 0;
   begin
      for I in Whole'Range loop
         if Whole (I) = Field_Separator then
            Path_Last := I - 1;
            exit;
         end if;
      end loop;
      Field_First := Path_Last + 2;
      --  The fields after the path: where, then marks.
      while Path_Last < Whole'Last and then Field_First <= Whole'Last + 1 loop
         declare
            Field_Last : Natural := Whole'Last;
         begin
            for I in Field_First .. Whole'Last loop
               if Whole (I) = Field_Separator then
                  Field_Last := I - 1;
                  exit;
               end if;
            end loop;
            Field_Index := Field_Index + 1;
            declare
               Field : String renames Whole (Field_First .. Field_Last);
            begin
               if Field_Index = 1 then
                  if Field'Length > 0 then
                     Source := (Kind => (if Field (Field'First) = Volume_Mark then Named else On_Medium),
                                First => Field'First, Last => Field'Last, Text => 0);
                  end if;
               elsif Field = Executable_Mark then
                  Executable := True;
               elsif Field'Length = Digest_Mark'Length + Records.Hex_Length
                 and then Field (Field'First .. Field'First + Digest_Mark'Length - 1) = Digest_Mark
               then
                  From_Hex (Field (Field'First + Digest_Mark'Length .. Field'Last), Digest, Digest_Known);
                  if not Digest_Known then
                     Result := Malformed;
                     return;
                  end if;
               else
                  Result := Malformed;
                  return;
               end if;
            end;
            exit when Field_Last >= Whole'Last;
            Field_First := Field_Last + 2;
         end;
      end loop;
      if Source.Kind = In_Payload then
         Source := (Kind => In_Payload, First => Whole'First, Last => Path_Last, Text => 0);
      end if;
      if Path_Last < Whole'First then
         Result := Malformed;
         return;
      end if;
      Add_Path (Whole (Whole'First .. Path_Last), Source, Executable, Digest_Known, Digest, Result);
   end Add_Line;

   --  Every component once more by content in the root's storehouse.
   procedure Add_Components (Result : in out Walk_Result) is
      Store, Kind_Directory : Install_Tree.Parent_Index;
      Root_Count : constant Install_Tree.Entry_Count := Trees (Disk_Layout.Root_Volume).Count;
   begin
      Component_Count := 0;
      Directory (Disk_Layout.Root_Volume, Install_Tree.Volume_Root, "storehouse", Store, Result);
      for Volume in Disk_Layout.Install_Volume loop
         exit when Result /= Walked;
         Directory (Disk_Layout.Root_Volume, Store,
                    (if Volume = Disk_Layout.Root_Volume then "executables" else "files"), Kind_Directory, Result);
         for E in 1 .. (if Volume = Disk_Layout.Root_Volume then Root_Count else Trees (Volume).Count) loop
            exit when Result /= Walked;
            if Has_Expected (Volume, E) and then Trees (Volume).Entries (E).Kind = Install_Tree.Regular_File then
               declare
                  Name : constant String := Hex (Expected (Volume, E));
               begin
                  --  The same bytes twice in the image: one entry.
                  if Child (Disk_Layout.Root_Volume, Kind_Directory, Name) = 0 then
                     File (Disk_Layout.Root_Volume, Kind_Directory, Name, Trees (Volume).Entries (E).Size,
                           Sources (Volume, E), Executable => Volume = Disk_Layout.Root_Volume,
                           Result => Result);
                     if Result = Walked then
                        Expected (Disk_Layout.Root_Volume, Trees (Disk_Layout.Root_Volume).Count) :=
                          Expected (Volume, E);
                        Has_Expected (Disk_Layout.Root_Volume, Trees (Disk_Layout.Root_Volume).Count) := True;
                        Component_Count := Component_Count + 1;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Add_Components;

   --  Generation 1 and the current pointer, last of the root tree.
   procedure Add_Generation (Result : in out Walk_Result) is
      Store, Profiles, System_Profile : Install_Tree.Parent_Index;
      First_Generation : constant Records.Generation_Value :=
        (Number => 1, Previous => Records.Image_Generation, Count => 0, Pins => <>);
      Record_Text : constant String := Records.Record_Text (First_Generation) & ASCII.LF;
      Current_Text : constant String := "1" & ASCII.LF;
   begin
      Generated_Texts (1) := (Length => Record_Text'Length, Data => <>);
      Generated_Texts (1).Data (1 .. Record_Text'Length) := Record_Text;
      Generated_Texts (2) := (Length => Current_Text'Length, Data => <>);
      Generated_Texts (2).Data (1 .. Current_Text'Length) := Current_Text;
      Directory (Disk_Layout.Root_Volume, Install_Tree.Volume_Root, "storehouse", Store, Result);
      if Result = Walked then
         Directory (Disk_Layout.Root_Volume, Store, "profiles", Profiles, Result);
      end if;
      if Result = Walked then
         Directory (Disk_Layout.Root_Volume, Profiles, "system", System_Profile, Result);
      end if;
      for G in Generated_Texts'Range loop
         exit when Result /= Walked;
         File (Disk_Layout.Root_Volume, System_Profile, Generation_Paths (G).all,
               Unsigned_64 (Generated_Texts (G).Length), (Kind => Generated, First => 1, Last => 0, Text => G),
               Executable => False, Result => Result);
         if Result = Walked and then G = Generated_Texts'First then
            Generation_Entry := Trees (Disk_Layout.Root_Volume).Count;
         end if;
      end loop;
   end Add_Generation;

   procedure Walk (Source : String; Result : out Walk_Result) is
      Line_First : Positive := 1;
   begin
      Finish;
      Files_Opened := 0;
      Bytes_Given := 0;
      Failed_Why := None;
      Failed_Length := 0;
      Generation_Entry := Install_Tree.Entry_Index'Last;
      Has_Expected := [others => [others => False]];
      Has_Written := [others => [others => False]];
      for Volume in Trees'Range loop
         Trees (Volume).Count := 0;
         Trees (Volume).Used := 0;
      end loop;
      if not Ready_State then
         Result := Unreadable;
         Fail (Not_Prepared, Source);
         return;
      elsif Source'Length > Source_Root'Length then
         Result := Too_Large;
         return;
      end if;
      Source_Length := Source'Length;
      Source_Root (1 .. Source_Length) := Source;
      Result := Read_Index (Source & Index_Name);
      if Result /= Walked then
         return;
      end if;
      for I in 1 .. Index_Length loop
         exit when Result /= Walked;
         if Index_Text (I) = ASCII.LF then
            if I > Line_First then
               Add_Line (Index_Text (Line_First .. I - 1), Result);
            end if;
            Line_First := I + 1;
         end if;
      end loop;
      if Result = Walked and then Line_First <= Index_Length then
         Add_Line (Index_Text (Line_First .. Index_Length), Result);
      end if;
      if Result = Walked then
         Add_Components (Result);
      end if;
      if Result = Walked then
         Add_Generation (Result);
      end if;
   end Walk;

   procedure Finish is
      Request : Message;
      Ignore : Boolean;
   begin
      if Open_Entry /= 0 then
         Request := Close_Request (Open_Handle);
         Ignore := Call (Request, "(close)");
         Open_Entry := 0;
      end if;
   end Finish;

   ---------------------------------------------------------------------------
   --  The bytes, hashed as they pass.
   ---------------------------------------------------------------------------
   Writing_Hash, Checking_Hash : Storehouse.Hashing.Context;
   Empty_Digest : Storehouse.Digest := [others => 0];
   Empty_Known : Boolean := False;

   function Empty return Storehouse.Digest is
      Nothing : constant Storehouse.Byte_Array (1 .. 0) := [others => 0];
   begin
      if not Empty_Known then
         Empty_Digest := Storehouse.Hashing.Of_Bytes (Nothing);
         Empty_Known := True;
      end if;
      return Empty_Digest;
   end Empty;

   procedure Add_To (Hash : in out Storehouse.Hashing.Context; Data : Disk_Layout.Bytes) is
      Same : constant Storehouse.Byte_Array (1 .. Data'Length) with Import, Address => Data'Address;
   begin
      Storehouse.Hashing.Add (Hash, Same);
   end Add_To;

   procedure Read
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index;
      Offset : Unsigned_64; Data : out Disk_Layout.Bytes; OK : out Boolean)
   is
      Request : Message;
      Done : Natural := 0;
      Item : File_Source;
      Size : Unsigned_64;
   begin
      Data := [others => 0];
      OK := False;
      if File > Trees (Volume).Count then
         return;
      end if;
      Item := Sources (Volume, File);
      Size := Trees (Volume).Entries (File).Size;
      if Item.Kind = Generated then
         if Item.Text not in Generated_Texts'Range
           or else Offset + Unsigned_64 (Data'Length) > Unsigned_64 (Generated_Texts (Item.Text).Length)
         then
            return;
         end if;
         for I in Data'Range loop
            Data (I) := Character'Pos (Generated_Texts (Item.Text).Data (Natural (Offset) + I - Data'First + 1));
         end loop;
      else
         if Open_Entry /= File or else Open_Volume /= Volume then
            Finish;
            declare
               Path : constant String := Source_Path (Item);
            begin
               Put_Path (Path);
               Request := Open_Request (Loan, Path'Length);
               if Path'Length > MAXIMUM_PATH_BYTES or else not Call (Request, Path) then
                  return;
               end if;
            end;
            Open_Handle := File_Handle (Request.words (0));
            Open_Entry := File;
            Open_Volume := Volume;
            Files_Opened := Files_Opened + 1;
         end if;
         while Done < Data'Length loop
            declare
               Want : constant Natural := Natural'Min (Data'Length - Done, Loan_Bytes);
               Chunk : Disk_Layout.Bytes (1 .. Want) with Import, Address => Buffer;
            begin
               Request := Read_At_Request (Open_Handle, Loan, Unsigned_64 (Want),
                                           Offset + Unsigned_64 (Done));
               if not Call (Request, Source_Path (Item)) then
                  return;
               elsif Request.words (0) /= Unsigned_64 (Want) then
                  Fail (Short, Source_Path (Item));   --  the file changed under the install
                  return;
               end if;
               Data (Data'First + Done .. Data'First + Done + Want - 1) := Chunk;
               Done := Done + Want;
            end;
         end loop;
      end if;
      --  Hashed in order: the writer reads each file from offset 0 up.
      if Offset = 0 then
         Storehouse.Hashing.Start (Writing_Hash);
      end if;
      Add_To (Writing_Hash, Data);
      if Offset + Unsigned_64 (Data'Length) = Size then
         Storehouse.Hashing.Finish (Writing_Hash, Written (Volume, File));
         Has_Written (Volume, File) := True;
         if Has_Expected (Volume, File) and then Written (Volume, File) /= Expected (Volume, File) then
            Fail (Source_Changed, Source_Path (Item));
            return;
         end if;
      end if;
      Bytes_Given := Bytes_Given + Unsigned_64 (Data'Length);
      OK := True;
   end Read;

   procedure Check
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Disk_Layout.Bytes; Final : Boolean; Matches : out Boolean)
   is
      Read_Back : Storehouse.Digest;
   begin
      if Offset = 0 then
         Storehouse.Hashing.Start (Checking_Hash);
      end if;
      Add_To (Checking_Hash, Data);
      Matches := True;
      if Final then
         Storehouse.Hashing.Finish (Checking_Hash, Read_Back);
         Matches :=
           (if Has_Written (Volume, File) then Read_Back = Written (Volume, File)
            else Trees (Volume).Entries (File).Size = 0 and then Read_Back = Empty);
      end if;
   end Check;

end Install_Sources;
