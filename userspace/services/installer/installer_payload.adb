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
with CuBit.Directory_Pages;
with Startup;

package body Installer_Payload is

   use type Install_Tree.Add_Result, Install_Tree.Entry_Kind, Disk_Layout.Install_Volume;
   package DP renames CuBit.Directory_Pages;

   --  The loan: path names, directory pages and file chunks pass through
   --  it. Its size bounds one Read.
   Loan_Pages : constant := 16;
   Loan_Bytes : constant := Loan_Pages * CuBit.Memory_Grants.PAGE_SIZE;
   --  A directory listing longer than this is refused, not truncated.
   Maximum_Directory_Pages : constant := 64;
   Maximum_Depth : constant := 32;

   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : System.Address := System.Null_Address;
   Ready_State : Boolean := False;

   Source_Root : String (1 .. 512);
   Source_Length : Natural := 0;

   Open_Volume : Disk_Layout.Install_Volume := Disk_Layout.Root_Volume;
   Open_Entry : Natural := 0;
   Open_Handle : File_Handle := 0;

   function Subtree_Name (Volume : Disk_Layout.Install_Volume) return String is
     (case Volume is when Disk_Layout.ESP_Volume => "/esp", when Disk_Layout.Root_Volume => "/root");

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

   --  The path of an entry of Volume's tree (Install_Tree.Volume_Root: the
   --  subtree itself).
   function Path_Of (Volume : Disk_Layout.Install_Volume; Item : Install_Tree.Parent_Index)
     return String
   is
      Base : constant String := Source_Root (1 .. Source_Length) & Subtree_Name (Volume);
   begin
      if Item = Install_Tree.Volume_Root then
         return Base;
      end if;
      declare
         E : constant Install_Tree.Tree_Entry := Trees (Volume).Entries (Item);
      begin
         return Path_Of (Volume, E.Parent) & "/" & Install_Tree.Name_Of (Trees (Volume), Item);
      end;
   end Path_Of;

   procedure List
     (Volume : Disk_Layout.Install_Volume; Parent : Install_Tree.Parent_Index;
      Depth : Natural; Result : in out Walk_Result)
   is
      Path : constant String := Path_Of (Volume, Parent);
      Request : Message;
      Directory : Directory_Handle;
      Previous_Resume : Unsigned_64 := 0;
      First_Child : constant Natural := Trees (Volume).Count + 1;
      Last_Child : Natural;
   begin
      if Path'Length > MAXIMUM_PATH_BYTES or else Path'Length > Loan_Bytes then
         Result := Too_Large;
         return;
      end if;
      Put_Path (Path);
      Request := Open_Directory_Request (Loan, Path'Length);
      if not Call (Request) then
         Result := Unreadable;
         return;
      end if;
      Directory := Directory_Handle (Request.words (0));
      Pages :
      for Page_Number in 1 .. Maximum_Directory_Pages loop
         Request := Read_Directory_Page_Request (Directory, Loan);
         if not Call (Request) then
            Result := Unreadable;
            exit Pages;
         end if;
         declare
            Shared : constant DP.Page with Import, Address => Buffer;
            Copy : constant DP.Page := Shared;   --  copied, then checked
            Valid, Ended, OK : Boolean;
            Count : DP.Entry_Count;
            Used : DP.Used_Bytes;
            Resume, Stamp : Unsigned_64;
            At_Entry, Next : Natural;
            Item : DP.Facts;
            Bytes : DP.Name_Bytes;
            Length : DP.Name_Length;
            Added : Install_Tree.Add_Result;
         begin
            DP.Check (Copy, Valid, Count, Used, Ended, Resume, Stamp);
            if not Valid then
               Result := Unreadable;
               exit Pages;
            end if;
            At_Entry := DP.Header_Bytes;
            for Index in 1 .. Count loop
               DP.Get (Copy, At_Entry, Used, Item, Bytes, Length, Next, OK);
               if not OK then
                  Result := Unreadable;
                  exit Pages;
               end if;
               At_Entry := Next;
               declare
                  Name : String (1 .. Length);
               begin
                  for C in Name'Range loop
                     Name (C) := Character'Val (Bytes (C));
                  end loop;
                  if Name /= "." and then Name /= ".."
                    and then Item.Kind in DIRECTORY_KIND_FILE | DIRECTORY_KIND_DIRECTORY
                  then
                     Install_Tree.Add
                       (Trees (Volume),
                        (if Item.Kind = DIRECTORY_KIND_DIRECTORY then Install_Tree.Directory
                         else Install_Tree.Regular_File),
                        Parent, Name,
                        (if Item.Kind = DIRECTORY_KIND_DIRECTORY then 0
                         else Unsigned_64'Min (Item.Size, Install_Tree.Maximum_File_Bytes)),
                        Added);
                     if Added /= Install_Tree.Added
                       or else Item.Size > Install_Tree.Maximum_File_Bytes
                     then
                        Result := Too_Large;
                        exit Pages;
                     end if;
                  end if;
               end;
            end loop;
            exit Pages when Ended;
            if Count = 0 or else Resume = Previous_Resume or else Page_Number = Maximum_Directory_Pages then
               Result := Unreadable;   --  no progress, or too long: never a partial list
               exit Pages;
            end if;
            Previous_Resume := Resume;
         end;
      end loop Pages;
      Request := Close_Directory_Request (Directory);
      if not Call (Request) and then Result = Walked then
         Result := Unreadable;
      end if;
      Last_Child := Trees (Volume).Count;
      if Result /= Walked then
         return;
      end if;
      if Depth = Maximum_Depth and then Last_Child >= First_Child then
         Result := Too_Large;
         return;
      end if;
      for Child in First_Child .. Last_Child loop
         exit when Result /= Walked;
         if Trees (Volume).Entries (Child).Kind = Install_Tree.Directory then
            List (Volume, Child, Depth + 1, Result);
         end if;
      end loop;
   end List;

   procedure Walk (Source : String; Result : out Walk_Result) is
   begin
      Finish;
      Result := Walked;
      for Volume in Trees'Range loop
         Trees (Volume).Count := 0;
         Trees (Volume).Used := 0;
      end loop;
      if Source'Length > Source_Root'Length then
         Result := Too_Large;
      else
         Source_Length := Source'Length;
         Source_Root (1 .. Source_Length) := Source;
         List (Disk_Layout.ESP_Volume, Install_Tree.Volume_Root, 0, Result);
         if Result = Walked then
            List (Disk_Layout.Root_Volume, Install_Tree.Volume_Root, 0, Result);
         end if;
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
            Path : constant String := Path_Of (Volume, File);
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
