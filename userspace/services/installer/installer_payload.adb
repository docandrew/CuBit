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
with Startup;

package body Installer_Payload is

   use type Install_Tree.Add_Result, Install_Tree.Entry_Kind, Disk_Layout.Install_Volume;

   --  The loan: path names, directory pages and file chunks pass through
   --  it. Its size bounds one Read.
   Loan_Pages : constant := 16;
   Loan_Bytes : constant := Loan_Pages * CuBit.Memory_Grants.PAGE_SIZE;

   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address := System.Null_Address;
   Ready_State : Boolean := False;

   Source_Root : String (1 .. 512);
   Source_Length : Natural := 0;

   Open_Volume : Disk_Layout.Install_Volume := Disk_Layout.Root_Volume;
   Open_Entry : Natural := 0;
   Open_Handle : File_Handle := 0;

   function Call (Request : in out Message) return Boolean is
   begin
      Request.tag := capCall (Startup.Filesystem, Request, Wait_Forever);
      return Request.tag.label = REPLY_OK;
   end Call;

   procedure Put_Path (Path : String) is
      Bytes : String (1 .. Path'Length) with Import, Address => Buffer;
   begin
      Bytes := Path;
   end Put_Path;

   procedure Initialize (Ready : out Boolean) is
      Raw : Unsigned_64;
   begin
      if not Ready_State then
         Raw := syscall (SYSCALL_SBRK, (Loan_Pages + 1) * CuBit.Memory_Grants.PAGE_SIZE);
         if Raw /= 0 and then Raw /= Unsigned_64'Last then
            Buffer := To_Address (Integer_Address
              ((Raw + CuBit.Memory_Grants.PAGE_SIZE - 1) and not (CuBit.Memory_Grants.PAGE_SIZE - 1)));
            CuBit.Memory_Grants.Create_Via_Capability
              (Startup.Filesystem, Buffer, Loan_Pages, True, Loan, Ready_State);
         end if;
      end if;
      Ready := Ready_State;
   end Initialize;

   --  The payload's index: one line per file, "esp/<path>" or
   --  "root/<path>", directories implied by the paths (the ISO9660 live
   --  medium lists no directories). A line may name where the file is
   --  instead, after a tab: a path below the medium's root (the live
   --  image's own files, which the image closure is, are not stored
   --  twice). Written by the image build.
   Index_Name : constant String := "/INDEX";
   Medium_Separator : constant Character := ASCII.HT;
   Maximum_Index : constant := 256 * 1024;
   subtype Index_Position is Natural range 0 .. Maximum_Index;
   Index_Text : String (1 .. Maximum_Index);
   Index_Length : Index_Position := 0;

   --  Where each file's bytes are: a slice of Index_Text, below the
   --  payload directory or (Medium_Relative) below the medium's root.
   type File_Source is record
      First : Index_Position := 1;
      Last : Index_Position := 0;
      Medium_Relative : Boolean := False;
   end record;
   type Source_Table is array (Disk_Layout.Install_Volume, Install_Tree.Entry_Index) of File_Source;
   Sources : Source_Table;

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
     ((if Item.Medium_Relative then Medium else Source_Root (1 .. Source_Length))
      & "/" & Index_Text (Item.First .. Item.Last));

   --  Opens Path; on success its handle.
   function Open (Path : String; Handle : out File_Handle) return Boolean is
      Request : Message;
   begin
      Handle := 0;
      if Path'Length > MAXIMUM_PATH_BYTES or else Path'Length > Loan_Bytes then
         return False;
      end if;
      Put_Path (Path);
      Request := Open_Request (Loan, Path'Length);
      if not Call (Request) then
         return False;
      end if;
      Handle := File_Handle (Request.words (0));
      return True;
   end Open;

   procedure Close (Handle : File_Handle) is
      Request : Message := Close_Request (Handle);
      Ignore : Boolean;
   begin
      Ignore := Call (Request);
   end Close;

   --  A file's size: its end, by a seek (no metadata query needed).
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
      OK := Call (Request);
      if OK then
         Size := Request.words (0);
      end if;
      Close (Handle);
      return OK;
   end Size_Of;

   function Read_Index (Path : String) return Walk_Result is
      Handle : File_Handle;
      Request : Message;
   begin
      Index_Length := 0;
      if not Open (Path, Handle) then
         return Unreadable;
      end if;
      loop
         declare
            Chunk : String (1 .. Loan_Bytes) with Import, Address => Buffer;
         begin
            Request := Read_At_Request (Handle, Loan, Loan_Bytes, Unsigned_64 (Index_Length));
            exit when not Call (Request);
            declare
               Got : constant Natural := Natural (Unsigned_64'Min (Request.words (0), Loan_Bytes));
            begin
               exit when Got = 0;
               if Index_Length + Got > Maximum_Index then
                  Close (Handle);
                  return Too_Large;
               end if;
               Index_Text (Index_Length + 1 .. Index_Length + Got) := Chunk (1 .. Got);
               Index_Length := Index_Length + Got;
            end;
         end;
      end loop;
      Close (Handle);
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

   --  Adds the file at payload path Line (esp/... or root/...), its bytes
   --  at Source, and the directories its path implies.
   procedure Add_Path (Line : String; Source : File_Source; Result : in out Walk_Result);

   --  Adds one index line's file.
   procedure Add_Line (Whole : String; Result : in out Walk_Result) is
      Line_Last : Natural := Whole'Last;
      Source : File_Source := (First => Whole'First, Last => Whole'Last, Medium_Relative => False);
      Volume : Disk_Layout.Install_Volume;
      Rest_First : Positive;
      Parent : Install_Tree.Parent_Index := Install_Tree.Volume_Root;
      Component_First : Positive;
      Added : Install_Tree.Add_Result;
      Size : Unsigned_64;
   begin
      for I in Whole'Range loop
         if Whole (I) = Medium_Separator then
            Line_Last := I - 1;
            Source := (First => I + 1, Last => Whole'Last, Medium_Relative => True);
            exit;
         end if;
      end loop;
      if Source.Last < Source.First then
         Result := Unreadable;
         return;
      end if;
      Add_Path (Whole (Whole'First .. Line_Last), Source, Result);
   end Add_Line;

   procedure Add_Path (Line : String; Source : File_Source; Result : in out Walk_Result) is
      Volume : Disk_Layout.Install_Volume;
      Rest_First : Positive;
      Parent : Install_Tree.Parent_Index := Install_Tree.Volume_Root;
      Component_First : Positive;
      Added : Install_Tree.Add_Result;
      Size : Unsigned_64;
   begin
      if Line'Length > 4 and then Line (Line'First .. Line'First + 3) = "esp/" then
         Volume := Disk_Layout.ESP_Volume;
         Rest_First := Line'First + 4;
      elsif Line'Length > 5 and then Line (Line'First .. Line'First + 4) = "root/" then
         Volume := Disk_Layout.Root_Volume;
         Rest_First := Line'First + 5;
      else
         Result := Unreadable;
         return;
      end if;
      Component_First := Rest_First;
      for I in Rest_First .. Line'Last loop
         if Line (I) = '/' then
            declare
               Name : constant String := Line (Component_First .. I - 1);
               Existing : constant Natural := Child (Volume, Parent, Name);
            begin
               if Existing /= 0 then
                  if Trees (Volume).Entries (Existing).Kind /= Install_Tree.Directory then
                     Result := Unreadable;
                     return;
                  end if;
                  Parent := Existing;
               else
                  Install_Tree.Add (Trees (Volume), Install_Tree.Directory, Parent, Name, 0, Added);
                  if Added /= Install_Tree.Added then
                     Result := Too_Large;
                     return;
                  end if;
                  Parent := Trees (Volume).Count;
               end if;
            end;
            Component_First := I + 1;
         end if;
      end loop;
      if Component_First > Line'Last then
         Result := Unreadable;
         return;
      end if;
      if not Size_Of (Source_Path (Source), Size) then
         Result := Unreadable;
         return;
      elsif Size > Install_Tree.Maximum_File_Bytes then
         Result := Too_Large;
         return;
      end if;
      Install_Tree.Add (Trees (Volume), Install_Tree.Regular_File, Parent,
                        Line (Component_First .. Line'Last), Size, Added);
      if Added /= Install_Tree.Added then
         Result := (if Added = Install_Tree.Tree_Full then Too_Large else Unreadable);
      else
         Sources (Volume, Trees (Volume).Count) := Source;
      end if;
   end Add_Path;

   procedure Walk (Source : String; Result : out Walk_Result) is
      Line_First : Positive := 1;
   begin
      Finish;
      for Volume in Trees'Range loop
         Trees (Volume).Count := 0;
         Trees (Volume).Used := 0;
      end loop;
      if Source'Length > Source_Root'Length then
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
   end Walk;

   procedure Finish is
      Request : Message;
      Ignore : Boolean;
   begin
      if Open_Entry /= 0 then
         Request := Close_Request (Open_Handle);
         Ignore := Call (Request);
         Open_Entry := 0;
      end if;
   end Finish;

   procedure Read
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index;
      Offset : Unsigned_64; Data : out Disk_Layout.Bytes; OK : out Boolean)
   is
      Request : Message;
      Done : Natural := 0;
   begin
      Data := [others => 0];
      OK := False;
      if File > Trees (Volume).Count then
         return;
      end if;
      if Open_Entry /= File or else Open_Volume /= Volume then
         Finish;
         declare
            Path : constant String := Source_Path (Sources (Volume, File));
         begin
            if Path'Length > MAXIMUM_PATH_BYTES then
               return;
            end if;
            Put_Path (Path);
            Request := Open_Request (Loan, Path'Length);
         end;
         if not Call (Request) then
            return;
         end if;
         Open_Handle := File_Handle (Request.words (0));
         Open_Entry := File;
         Open_Volume := Volume;
      end if;
      while Done < Data'Length loop
         declare
            Want : constant Natural := Natural'Min (Data'Length - Done, Loan_Bytes);
            Chunk : Disk_Layout.Bytes (1 .. Want) with Import, Address => Buffer;
         begin
            Request := Read_At_Request (Open_Handle, Loan, Unsigned_64 (Want),
                                        Offset + Unsigned_64 (Done));
            if not Call (Request) or else Request.words (0) /= Unsigned_64 (Want) then
               return;   --  short: the file changed under the install
            end if;
            Data (Data'First + Done .. Data'First + Done + Want - 1) := Chunk;
            Done := Done + Want;
         end;
      end loop;
      OK := True;
   end Read;

end Installer_Payload;
