with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Memory_Grants;

procedure Main is
   Slot : constant CapabilitySlot := 1;
   Page_Size : constant Unsigned_64 := 4096;
   Raw, Address, Ignored : Unsigned_64;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   OK : Boolean;
   Failures : Natural := 0;
   Msg : Message;
   Tag : MessageTag;
   Directory, Child : Directory_Handle;
   File : File_Handle;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         debugPrint ("FILESYSTEM-SCOPE-CHECK: FAIL " & Name & ASCII.LF);
      end if;
   end Check;

   procedure Put (Value : String) is
      View : String (Value'Range)
        with Import, Address => To_Address (Integer_Address (Address));
   begin
      View := Value;
   end Put;

   function Open_File (Name : String; Options : Open_Options := OPEN_READ_ONLY)
     return File_Handle
   is
   begin
      Put (Name);
      Msg := Open_Request (Loan, Name'Length, Options);
      Tag := capCall (Slot, Msg);
      return (if Tag.label = REPLY_OK then File_Handle (Msg.words (0))
              else INVALID_FILE_HANDLE);
   end Open_File;

   procedure Close_File (Handle : File_Handle) is
   begin
      Msg := Close_Request (Handle);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK, "close file");
   end Close_File;

   procedure Reject_Rename (Before, After : String) is
   begin
      Put (Before & After);
      Msg := Rename_Request (Loan, Before'Length, After'Length);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_ACCESS_DENIED, "rename requires both scopes");
      File := Open_File (Before);
      Check (File /= INVALID_FILE_HANDLE, "denied rename preserves source");
      if File /= INVALID_FILE_HANDLE then
         Close_File (File);
      end if;
   end Reject_Rename;
begin
   Raw := syscall (SYSCALL_SBRK, 2 * Page_Size);
   if Raw = Unsigned_64'Last then
      debugPrint ("FILESYSTEM-SCOPE-CHECK: FAIL allocation" & ASCII.LF);
      Ignored := syscall (SYSCALL_EXIT, 1);
      return;
   end if;
   Address := (Raw + Page_Size - 1) and not (Page_Size - 1);
   CuBit.Memory_Grants.Create_Via_Capability
     (Slot, To_Address (Integer_Address (Address)), 1, True, Loan, OK);
   if not OK then
      debugPrint ("FILESYSTEM-SCOPE-CHECK: FAIL grant" & ASCII.LF);
      Ignored := syscall (SYSCALL_EXIT, 1);
      return;
   end if;

   Put ("@nvme:0/");
   Msg := Open_Directory_Request (Loan, 8);
   Tag := capCall (Slot, Msg);
   Check (Tag.label = REPLY_ACCESS_DENIED, "root denied");

   File := Open_File ("@nvme:0/scope-allowed/readme");
   Check (File /= INVALID_FILE_HANDLE, "read authorized despite ext2 mode 000");
   if File /= INVALID_FILE_HANDLE then
      Msg := Read_Request (File, Loan, 16);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK and then Msg.words (0) = 16, "read content");
      Msg := Write_Request (File, Loan, 1);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_ACCESS_DENIED, "read handle cannot write");
      Close_File (File);
   end if;
   File := Open_File ("@nvme:0/scope-allowed/readme", OPEN_READ_WRITE);
   Check (File = INVALID_FILE_HANDLE and then Tag.label = REPLY_ACCESS_DENIED,
          "cannot acquire stronger handle");
   File := Open_File ("@nvme:0/scope-allowed-other/readme");
   Check (File = INVALID_FILE_HANDLE and then Tag.label = REPLY_ACCESS_DENIED,
          "component boundary excludes sibling");
   File := Open_File ("@nvme:0/scope-work/new", OPEN_READ_WRITE or OPEN_CREATE);
   Check (File /= INVALID_FILE_HANDLE, "scoped create");
   if File /= INVALID_FILE_HANDLE then
      Put ("CuBit");
      Msg := Write_Request (File, Loan, 5);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK and then Msg.words (0) = 5, "scoped write");
      Close_File (File);
   end if;
   File := Open_File ("@nvme:0/scope-allowed/new", OPEN_READ_WRITE or OPEN_CREATE);
   Check (File = INVALID_FILE_HANDLE and then Tag.label = REPLY_ACCESS_DENIED,
          "read scope cannot create");

   Reject_Rename
     ("@nvme:0/scope-allowed/readme", "@nvme:0/scope-work/stolen");
   Reject_Rename
     ("@nvme:0/scope-work/new", "@nvme:0/scope-allowed/injected");
   Reject_Rename
     ("@nvme:0/scope-work/new", "@nvme:0/scope-allowed-other/injected");

   Put ("@nvme:0/scope-allowed");
   Msg := Open_Directory_Request (Loan, 21);
   Tag := capCall (Slot, Msg);
   Check (Tag.label = REPLY_OK, "scoped directory");
   if Tag.label = REPLY_OK then
      Directory := Directory_Handle (Msg.words (0));
      Put ("nested");
      Msg := Open_Child_Directory_Request (Directory, Loan, 6);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK, "derive child within scope");
      if Tag.label = REPLY_OK then
         Child := Directory_Handle (Msg.words (0));
         Msg := Close_Directory_Request (Child);
         Tag := capCall (Slot, Msg);
         Check (Tag.label = REPLY_OK, "close child");
      end if;

      --  Knowing administrative labels is not policy-installation authority.
      Msg := NULL_MESSAGE;
      Msg.tag := (label => OP_SET_ACL, length => 4, flags => 0, reserved => 0);
      Msg.words := [0 => syscall (SYSCALL_GETPID), others => 0];
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_ACCESS_DENIED, "self-grant denied");
      Msg.tag := (label => OP_REVOKE_ACL, length => 1, flags => 0, reserved => 0);
      Msg.words := [0 => syscall (SYSCALL_GETPID), others => 0];
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_ACCESS_DENIED, "untrusted policy edit denied");
      Msg := Rewind_Directory_Request (Directory);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK, "denied update preserved existing handle");
      Msg := Close_Directory_Request (Directory);
      Tag := capCall (Slot, Msg);
      Check (Tag.label = REPLY_OK, "close directory");
   end if;
   CuBit.Memory_Grants.Revoke (Loan, OK);
   Check (OK, "revoke loan");
   if Failures = 0 then
      debugPrint ("FILESYSTEM-SCOPE-CHECK: PASS" & ASCII.LF);
   end if;
   Ignored := syscall (SYSCALL_EXIT, Unsigned_64 (Failures));
end Main;
