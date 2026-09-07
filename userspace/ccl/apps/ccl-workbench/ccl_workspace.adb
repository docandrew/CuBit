with CCL_Workspace_Names;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Memory_Grants;

package body CCL_Workspace is
   use CCL_Workspace_Names;
   type Workspace_Kind is (Not_Selected, NVMe_Workspace, Live_Workspace);
   Selected : Workspace_Kind := Not_Selected;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer_Address : Unsigned_64 := 0;
   Ready : Boolean := False;
   Maximum_Directory_Pages : constant := 64;

   function Supported return Boolean is (True);
   function Root (Kind : Workspace_Kind) return String is
     (case Kind is
         when NVMe_Workspace => "@nvme:0/work",
         when Live_Workspace => "@mem:0/work",
         when Not_Selected => "");
   function Location return String is
     (if Selected = Live_Workspace then "@mem:0/work (lost on reboot)"
      elsif Selected = NVMe_Workspace then "@nvme:0/work"
      else "workspace not selected");

   function Outcome (Label : Unsigned_32) return Storage_Result is
     (case Label is
         when REPLY_OK => Succeeded,
         when REPLY_ACCESS_DENIED | REPLY_READ_ONLY => Access_Denied,
         when REPLY_ALREADY_EXISTS => Conflict,
         when REPLY_NOT_FOUND => Not_Found,
         when REPLY_RECOVERY_REQUIRED => Recovery_Required,
         when others => IO_Failed);

   procedure Put (Text : String) is
      Bytes : String (1 .. Text'Length)
        with Import, Address => To_Address (Integer_Address (Buffer_Address));
   begin
      Bytes := Text;
   end Put;

   function Call (Request : in out Message) return Storage_Result is
   begin
      Request.tag := capCall (CAP_SLOT_FS, Request);
      return Outcome (Request.tag.label);
   end Call;

   procedure Shutdown is
      OK : Boolean;
   begin
      if Ready then
         CuBit.Memory_Grants.Revoke (Loan, OK);
         if OK then
            Ready := False;
         end if;
      end if;
   end Shutdown;

   function Initialize return Boolean is
      Raw : Unsigned_64;
   begin
      if Ready then
         return True;
      end if;
      if Buffer_Address = 0 then
         Raw := syscall (SYSCALL_SBRK, 2 * DIRECTORY_PAGE_BYTES);
         if Raw = Unsigned_64'Last then
            return False;
         end if;
         Buffer_Address := (Raw + DIRECTORY_PAGE_BYTES - 1) and
           not Unsigned_64 (DIRECTORY_PAGE_BYTES - 1);
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_FS, To_Address (Integer_Address (Buffer_Address)), 1,
         True, Loan, Ready);
      return Ready;
   end Initialize;

   procedure Scan
     (Highest : out Revision; Result : out Storage_Result;
      Files : out CuBit.File_Selection.File_List; Collect : Boolean := False)
   is
      Request : Message;
      Directory : Directory_Handle := INVALID_DIRECTORY_HANDLE;
      Close_Result : Storage_Result;

      procedure Pages is
         Header : Directory_Page_Header
           with Import, Address => To_Address (Integer_Address (Buffer_Address));
         Entries : Directory_Entries
           with Import, Address => To_Address
             (Integer_Address (Buffer_Address + DIRECTORY_PAGE_HEADER_BYTES));
         Previous_Cursor : Unsigned_64 := 0;
         Number : Revision;
         Pending : Boolean;
         Accepted : Boolean;
      begin
         for Page in 1 .. Maximum_Directory_Pages loop
            Request := Read_Directory_Page_Request (Directory, Loan);
            Result := Call (Request);
            if Result /= Succeeded then return; end if;
            if Header.version /= PROTOCOL_VERSION or else
              Header.headerBytes /= DIRECTORY_PAGE_HEADER_BYTES or else
              Header.entryBytes /= DIRECTORY_ENTRY_BYTES or else
              Header.entryCount > MAXIMUM_DIRECTORY_PAGE_ENTRIES or else
              (Header.flags and not DIRECTORY_PAGE_END) /= 0
            then
               Result := IO_Failed;
               return;
            end if;
            for Index in 1 .. Natural (Header.entryCount) loop
               declare
                  Entry_Info : Directory_Entry renames Entries (Index - 1);
                  Name : String (1 .. MAXIMUM_DIRECTORY_NAME_BYTES);
               begin
                  if Entry_Info.nameLength > MAXIMUM_DIRECTORY_NAME_BYTES then
                     Result := IO_Failed;
                     return;
                  end if;
                  for C in 1 .. Natural (Entry_Info.nameLength) loop
                     Name (C) := Character'Val (Entry_Info.name (C));
                  end loop;
                  if Collect and then Entry_Info.kind = DIRECTORY_KIND_FILE and then
                    Valid_Source_Name (Name (1 .. Natural (Entry_Info.nameLength)))
                  then
                     CuBit.File_Selection.Append
                       (Files, Name (1 .. Natural (Entry_Info.nameLength)), Accepted);
                     if not Accepted then
                        Result := Limit_Reached;
                        return;
                     end if;
                  end if;
                  Decode (Name (1 .. Natural (Entry_Info.nameLength)), Number, Pending);
                  Highest := Revision'Max (Highest, Number);
               end;
            end loop;
            if (Header.flags and DIRECTORY_PAGE_END) /= 0 then return; end if;
            if Header.nextCursor <= Previous_Cursor then
               Result := IO_Failed;
               return;
            end if;
            Previous_Cursor := Header.nextCursor;
         end loop;
         Result := Limit_Reached; -- Never mistake an incomplete scan for latest.
      end Pages;
   begin
      Files := (others => <>);
      Highest := 0;
      Result := Unavailable;
      if not Initialize then return; end if;
      for Kind in NVMe_Workspace .. Live_Workspace loop
         if Selected = Not_Selected or else Selected = Kind then
            Put (Root (Kind));
            Request := Open_Directory_Request (Loan, Root (Kind)'Length);
            Result := Call (Request);
            if Result = Succeeded then
               Selected := Kind;
               Directory := Directory_Handle (Request.words (0));
               exit;
            elsif Selected /= Not_Selected then
               return; -- Never silently move an established workspace.
            end if;
         end if;
      end loop;
      if Directory = INVALID_DIRECTORY_HANDLE then
         Result := Unavailable;
         return;
      end if;
      Pages;
      Request := Close_Directory_Request (Directory);
      Close_Result := Call (Request);
      if Result = Succeeded then Result := Close_Result; end if;
   end Scan;

   function Valid_Text (Text : String) return Boolean is
     (for all C of Text => C in ' ' .. '~' | ASCII.LF | ASCII.CR | ASCII.HT);

   procedure Read_Source
     (Path : String; Text : out Source_Buffer; Length : out Source_Length;
      Result : out Storage_Result)
   is
      Request : Message;
      File : File_Handle;
      Close_Result : Storage_Result;
      Bytes : Source_Buffer
        with Import, Address => To_Address (Integer_Address (Buffer_Address));
   begin
      Text := [others => ' '];
      Length := 0;
      Put (Path);
      Request := Open_Request (Loan, Path'Length);
      Result := Call (Request);
      if Result /= Succeeded then return; end if;
      File := File_Handle (Request.words (0));
      Request := Read_Request (File, Loan, Maximum_Source_Bytes);
      Result := Call (Request);
      if Result = Succeeded then
         if Request.words (0) > Maximum_Source_Bytes then
            Result := IO_Failed;
         else
            Length := Natural (Request.words (0));
            Text (1 .. Length) := Bytes (1 .. Length);
            Request := Read_Request (File, Loan, 1);
            Result := Call (Request);
            if Result = Succeeded and then Request.words (0) /= 0 then
               Result := Limit_Reached;
            elsif Result = Succeeded and then not Valid_Text (Text (1 .. Length)) then
               Result := Invalid_Source;
            end if;
         end if;
      end if;
      Request := Close_Request (File);
      Close_Result := Call (Request);
      if Result = Succeeded then Result := Close_Result; end if;
   end Read_Source;

   procedure Publish (Pending_Name, Final_Name, Text : String; Result : out Storage_Result)
   is
      Request : Message;
      File : File_Handle;
      Close_Result : Storage_Result;
      Verified : Source_Buffer;
      Length : Source_Length;
   begin
      declare
         Pending_Path : constant String := Root (Selected) & "/" & Pending_Name;
         Final_Path : constant String := Root (Selected) & "/" & Final_Name;
      begin
         Put (Pending_Path);
         Request := Open_Request
           (Loan, Pending_Path'Length, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE);
         Result := Call (Request);
         if Result /= Succeeded then return; end if;
         File := File_Handle (Request.words (0));
         if Text'Length > 0 then
            Put (Text);
            Request := Write_Request (File, Loan, Text'Length);
            Result := Call (Request);
            if Result = Succeeded and then Request.words (0) /= Text'Length then
               Result := IO_Failed;
            end if;
         end if;
         Request := Close_Request (File);
         Close_Result := Call (Request);
         if Result = Succeeded then Result := Close_Result; end if;
         if Result /= Succeeded then return; end if;
         Read_Source (Pending_Path, Verified, Length, Result);
         if Result /= Succeeded then return; end if;
         if Verified (1 .. Length) /= Text then
            Result := IO_Failed;
            return;
         end if;
         Put (Pending_Path & Final_Path);
         Request := Rename_Request (Loan, Pending_Path'Length, Final_Path'Length);
         Result := Call (Request);
      end;
   end Publish;

   procedure List_Files
     (Files : out CuBit.File_Selection.File_List; Result : out Storage_Result)
   is
      Highest : Revision;
   begin
      Scan (Highest, Result, Files, Collect => True);
      if Result /= Succeeded then Files := (others => <>); end if;
   end List_Files;

   procedure Suggest_Name
     (Name : out CuBit.File_Selection.File_Name; Result : out Storage_Result)
   is
      Highest : Revision;
      Files : CuBit.File_Selection.File_List;
      Accepted : Boolean;
   begin
      Name := (others => <>);
      Scan (Highest, Result, Files);
      if Result /= Succeeded then return; end if;
      if Highest = Revision'Last then
         Result := Limit_Reached;
      else
         CuBit.File_Selection.Set (Name, Filename (Highest + 1, False), Accepted);
         if not Accepted then Result := Invalid_Name; end if;
      end if;
   end Suggest_Name;

   procedure Load
     (Name : String; Text : out Source_Buffer; Length : out Source_Length;
      Result : out Storage_Result)
   is
      Highest : Revision;
      Files : CuBit.File_Selection.File_List;
   begin
      Text := [others => ' '];
      Length := 0;
      Result := Invalid_Name;
      if not Valid_Source_Name (Name) then return; end if;
      Scan (Highest, Result, Files, Collect => True);
      if Result /= Succeeded then return; end if;
      if not (for some Index in 1 .. Files.Count =>
        CuBit.File_Selection.Value (Files.Names (Index)) = Name)
      then
         Result := Not_Found;
         return;
      end if;
      Read_Source (Root (Selected) & "/" & Name, Text, Length, Result);
      if Result = Succeeded then
         debugPrint ("ccl-workbench: workspace opened " & Name & ASCII.LF);
      end if;
   end Load;

   procedure Save_New (Name, Text : String; Result : out Storage_Result) is
      Highest : Revision;
      Files : CuBit.File_Selection.File_List;
      Number : Revision;
      Pending : Boolean;
   begin
      Result := Invalid_Name;
      if not Valid_Source_Name (Name) then return; end if;
      Result := Invalid_Source;
      if Text'Length > Maximum_Source_Bytes or else not Valid_Text (Text) then return; end if;
      Scan (Highest, Result, Files, Collect => True);
      if Result /= Succeeded then return; end if;
      if (for some Index in 1 .. Files.Count =>
        CuBit.File_Selection.Value (Files.Names (Index)) = Name)
      then
         Result := Conflict;
         return;
      elsif Files.Count = CuBit.File_Selection.Maximum_Files then
         Result := Limit_Reached;
         return;
      end if;
      Decode (Name, Number, Pending);
      --  Keep numbered revisions and abandoned pending names in one namespace.
      if Number /= 0 then
         Publish (Filename (Number, True), Name, Text, Result);
      else
         Publish (Name & ".pending", Name, Text, Result);
      end if;
      if Result = Succeeded then
         debugPrint ("ccl-workbench: workspace saved " & Name & ASCII.LF);
      end if;
   end Save_New;

end CCL_Workspace;
