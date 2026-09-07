------------------------------------------------------------------------------
--  CuBit live-storage diagnostic
--
--  Exercises create, write, close, stale-handle rejection, reopen, and read
--  against the live system's writable NVMe filesystem.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;

procedure main is
   use ASCII;

   PAGE_SIZE : constant Unsigned_64 := 4096;
   PATH : constant String := "@nvme:0/config.dat";
   CREATE_PATH : constant String := "@nvme:0/cubit-new.dat";
   PAYLOAD : constant String := "CuBit storage ready";

   raw      : Unsigned_64;
   aligned  : Unsigned_64;
   grantRef : CuBit.Memory_Grants.Grant_Reference;
   grantOk  : Boolean;

   function exerciseGrantReferences return Boolean is
      use CuBit.Memory_Grants;
      pid         : constant ProcessID := syscall (SYSCALL_GETPID);
      wrongOwner  : constant ProcessID := (if pid = 1 then 2 else 1);
      firstRef    : Grant_Reference;
      secondRef   : Grant_Reference;
      mapped      : System.Address;
      ok          : Boolean;
   begin
      Create_For_Process
        (grantee   => pid,
         localAddr => To_Address (Integer_Address (aligned)),
         numPages  => 1,
         readWrite => False,
         reference => firstRef,
         success   => ok);
      if not ok then
         return False;
      end if;

      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if not ok or else mapped = System.Null_Address then
         return False;
      end if;

      --  Multiple accepted uses share one mapping-owned pin but retain distinct
      --  return obligations.
      Acquire
        (firstRef, pid, 8, 8, Read_Access, mapped, ok);
      if not ok or else mapped = System.Null_Address then
         return False;
      end if;

      Acquire
        (firstRef, pid, 0, 16, Write_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (firstRef, pid, PAGE_SIZE - 4, 8, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (firstRef, wrongOwner, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Revoke (firstRef, ok);
      if not ok then
         return False;
      end if;

      -- Revocation is accepted while acquired, but becomes pending: no new
      -- borrower can enter and the existing mapping remains pinned.
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Return_Acquisition (firstRef, ok);
      if not ok then
         return False;
      end if;

      --  One borrower remains, so revocation is still pending.
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Return_Acquisition (firstRef, ok);
      if not ok then
         return False;
      end if;

      --  The final return completes revocation and retires this generation.
      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Create_For_Process
        (grantee   => pid,
         localAddr => To_Address (Integer_Address (aligned)),
         numPages  => 1,
         readWrite => False,
         reference => secondRef,
         success   => ok);
      if not ok or else secondRef.slot /= firstRef.slot or else
         secondRef.generation = firstRef.generation
      then
         return False;
      end if;

      Acquire
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Acquire
        (secondRef, pid, 0, 16, Read_Access, mapped, ok);
      if not ok then
         return False;
      end if;

      Return_Acquisition (secondRef, ok);
      if not ok then
         return False;
      end if;

      Acquire_Via_Capability
        (CAP_SLOT_SELF, secondRef, 0, 16, Read_Access, mapped, ok);
      if not ok then
         return False;
      end if;

      Return_Acquisition (secondRef, ok);
      if not ok then
         return False;
      end if;

      Revoke (secondRef, ok);
      return ok;
   end exerciseGrantReferences;

   function exerciseStorage return Boolean is
      msg       : Message := NULL_MESSAGE;
      handle    : File_Handle;
      staleTag  : MessageTag;
      staleGeneration : constant Unsigned_64 :=
        (if grantRef.generation > 1 then grantRef.generation - 1
         else grantRef.generation + 1);
      staleRef : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot       => grantRef.slot,
         generation =>
           CuBit.Memory_Grants.Grant_Generation (staleGeneration));

      function directoryContainsExpectedFiles return Boolean is
         directoryPath : constant String := "@nvme:0/";
         directory : Directory_Handle;
         foundConfig : Boolean := False;
         foundCreated : Boolean := False;

         function entryEquals
           (item : Directory_Entry;
            expected : String) return Boolean
         is
         begin
            if Natural (item.nameLength) /= expected'Length then
               return False;
            end if;
            for index in 1 .. expected'Length loop
               if Character'Val (item.name (index)) /=
                 expected (expected'First + index - 1)
               then
                  return False;
               end if;
            end loop;
            return True;
         end entryEquals;
      begin
         declare
            pathBuffer : String (directoryPath'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            pathBuffer := directoryPath;
         end;

         msg := Open_Request (grantRef, directoryPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_WRONG_OBJECT_TYPE then
            debugPrint
              ("STORAGE-CHECK: directory accepted as file handle" & LF);
            return False;
         end if;

         msg := Open_Directory_Request (grantRef, directoryPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            debugPrint ("STORAGE-CHECK: directory open failed" & LF);
            return False;
         end if;
         directory := Directory_Handle (msg.words (0));

         loop
            msg := Read_Directory_Page_Request (directory, grantRef);
            msg.tag := capCall (CAP_SLOT_FS, msg);
            if msg.tag.label /= REPLY_OK then
               debugPrint ("STORAGE-CHECK: directory page failed" & LF);
               return False;
            end if;

            declare
               pageAddress : constant System.Address :=
                 To_Address (Integer_Address (aligned));
               header : Directory_Page_Header
                 with Import, Address => pageAddress;
               entries : Directory_Entries
                 with Import,
                      Address => pageAddress + DIRECTORY_PAGE_HEADER_BYTES;
            begin
               if header.version /= PROTOCOL_VERSION or else
                 header.headerBytes /= DIRECTORY_PAGE_HEADER_BYTES or else
                 header.entryBytes /= DIRECTORY_ENTRY_BYTES or else
                 header.entryCount > MAXIMUM_DIRECTORY_PAGE_ENTRIES
               then
                  debugPrint ("STORAGE-CHECK: invalid directory page" & LF);
                  return False;
               end if;

               if header.entryCount > 0 then
                  for index in 0 .. Natural (header.entryCount) - 1 loop
                     if entries (index).nameLength >
                       MAXIMUM_DIRECTORY_NAME_BYTES
                     then
                        return False;
                     end if;
                     foundConfig := foundConfig or else
                       entryEquals (entries (index), "config.dat");
                     foundCreated := foundCreated or else
                       entryEquals (entries (index), "cubit-new.dat");
                  end loop;
               end if;

               exit when (header.flags and DIRECTORY_PAGE_END) /= 0;
            end;
         end loop;

         msg := Close_Directory_Request (directory);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         return msg.tag.label = REPLY_OK and foundConfig and foundCreated;
      end directoryContainsExpectedFiles;

      function rejectsMalformedDirectory return Boolean is
         corruptPath : constant String := "@nvme:0/corrupt-dir";
         directory : Directory_Handle;
         closeMessage : Message;
      begin
         declare
            pathBuffer : String (corruptPath'Range)
              with Import, Address => To_Address (Integer_Address (aligned));
         begin
            pathBuffer := corruptPath;
         end;

         msg := Open_Directory_Request (grantRef, corruptPath'Length);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         if msg.tag.label /= REPLY_OK then
            --  The adversarial fixture is installed only by the focused
            --  headless test. Its absence is not a storage failure.
            return True;
         end if;
         directory := Directory_Handle (msg.words (0));

         msg := Read_Directory_Page_Request (directory, grantRef);
         msg.tag := capCall (CAP_SLOT_FS, msg);
         closeMessage := Close_Directory_Request (directory);
         closeMessage.tag := capCall (CAP_SLOT_FS, closeMessage);
         if msg.tag.label /= REPLY_MALFORMED_FILESYSTEM or else
           closeMessage.tag.label /= REPLY_OK
         then
            debugPrint
              ("STORAGE-CHECK: malformed directory was not rejected" & LF);
            return False;
         end if;
         debugPrint ("MALFORMED-DIRECTORY-CHECK: PASS" & LF);
         return True;
      end rejectsMalformedDirectory;
   begin
      --  Possessing a normal filesystem endpoint does not convey policy
      --  administration. In particular, an application cannot grant itself
      --  a wildcard profile by calling the management operation directly.
      msg := NULL_MESSAGE;
      msg.tag := (label => OP_SET_ACL, length => 4,
                  flags => 0, badge => 0);
      msg.words :=
        (0 => syscall (SYSCALL_GETPID), 1 => 0, 2 => 0, 3 => 0);
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label /= REPLY_ACCESS_DENIED then
         debugPrint ("STORAGE-CHECK: ordinary endpoint changed FS policy" & LF);
         return False;
      end if;

      declare
         pathBuffer : String (PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := PATH;
      end;

      --  The filesystem must independently reject a stale, nonzero
      --  generation even when the slot and path bytes are otherwise valid.
      msg := Open_Request (staleRef, PATH'Length);
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: stale grant accepted by FS" & LF);
         return False;
      end if;

      --  Unknown flags must not silently alter the authority installed on a
      --  returned handle.
      msg := Open_Request (grantRef, PATH'Length, Open_Options (4));
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: invalid open options accepted" & LF);
         return False;
      end if;

      --  Exercise the distinct read-write mode against a sparse scratch file.
      --  Its first write must allocate outside a full block group in the
      --  headless fixture.
      msg := Open_Request
        (grantRef, PATH'Length, OPEN_READ_WRITE);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: read-write open failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      declare
         payloadBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         payloadBuffer := PAYLOAD;
      end;

      msg := Write_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint
           ("STORAGE-CHECK: write failed label=" &
            Unsigned_32'Image (msg.tag.label) & " count=" &
            Unsigned_64'Image (msg.words (0)) & LF);
         return False;
      end if;

      --  The current writer deliberately supports direct and single-indirect
      --  blocks only.  A range it cannot represent must be reported as such,
      --  never as a successful zero-byte write.
      msg := Seek_Request (handle, 16#0100_0000#, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: large seek failed" & LF);
         return False;
      end if;

      msg := Write_Request (handle, grantRef, 1);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_FILE_RANGE_UNSUPPORTED or else
         msg.words (0) /= 0
      then
         debugPrint
           ("STORAGE-CHECK: unsupported write range was not explicit" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: read-write seek failed" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: read-write handle lost read access" & LF);
         return False;
      end if;

      declare
         readWriteBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readWriteBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: read-write content mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: close failed" & LF);
         return False;
      end if;

      --  The just-closed handle must remain invalid even though the next open
      --  is likely to reuse the same table slot.
      msg := Read_Request (handle, grantRef, 1);
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: stale handle accepted" & LF);
         return False;
      end if;

      declare
         pathBuffer : String (PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := PATH;
      end;

      msg := Open_Request (grantRef, PATH'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: reopen failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      --  One page was granted.  The service must not trust the protocol's
      --  requested count as if the entire 16 MiB aperture slot were mapped.
      msg := Read_Request (handle, grantRef, PAGE_SIZE + 1);
      staleTag := capCall (CAP_SLOT_FS, msg);
      if staleTag.label = REPLY_OK then
         debugPrint ("STORAGE-CHECK: oversized grant range accepted" & LF);
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: read failed" & LF);
         return False;
      end if;

      declare
         readBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: content mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      --  Exercise inode allocation and directory insertion independently of
      --  the pre-created sparse fixture.
      declare
         pathBuffer : String (CREATE_PATH'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         pathBuffer := CREATE_PATH;
      end;

      msg := Open_Request
        (grantRef, CREATE_PATH'Length,
         OPEN_READ_WRITE or OPEN_CREATE);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         debugPrint ("STORAGE-CHECK: create failed" & LF);
         return False;
      end if;
      handle := File_Handle (msg.words (0));

      declare
         payloadBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         payloadBuffer := PAYLOAD;
      end;

      msg := Write_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: created-file write failed" & LF);
         return False;
      end if;

      msg := Seek_Request (handle, 0, From_Start);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      msg := Read_Request (handle, grantRef, PAYLOAD'Length);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK or else msg.words (0) /= PAYLOAD'Length then
         debugPrint ("STORAGE-CHECK: created-file read failed" & LF);
         return False;
      end if;

      declare
         readBuffer : String (PAYLOAD'Range)
           with Import, Address => To_Address (Integer_Address (aligned));
      begin
         if readBuffer /= PAYLOAD then
            debugPrint ("STORAGE-CHECK: created-file mismatch" & LF);
            return False;
         end if;
      end;

      msg := Close_Request (handle);
      msg.tag := capCall (CAP_SLOT_FS, msg);
      if msg.tag.label /= REPLY_OK then
         return False;
      end if;

      return directoryContainsExpectedFiles and then rejectsMalformedDirectory;
   end exerciseStorage;

begin
   raw := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
   if raw = Unsigned_64'Last then
      debugPrint ("STORAGE-CHECK: buffer allocation failed" & LF);
      return;
   end if;

   aligned := (raw + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);

   if exerciseGrantReferences then
      debugPrint ("GRANT-REFERENCE-CHECK: PASS" & LF);
   else
      debugPrint ("GRANT-REFERENCE-CHECK: FAIL" & LF);
      return;
   end if;

   -- Reuse one backing frame and the same grant slots beyond the pin-count
   -- limit. Leaked mapping pins fail this run; stale epochs must not satisfy
   -- later shootdowns. The headless guest runs with four online CPUs.
   for Round in 1 .. 128 loop
      if not exerciseGrantReferences then
         debugPrint ("GRANT-RECLAMATION-CHECK: FAIL" & LF);
         return;
      end if;
   end loop;
   debugPrint ("GRANT-RECLAMATION-CHECK: PASS" & LF);

   CuBit.Memory_Grants.Create_Via_Capability
     (slot      => CAP_SLOT_FS,
      localAddr => To_Address (Integer_Address (aligned)),
      numPages  => 1,
      readWrite => True,
      reference => grantRef,
      success   => grantOk);

   if not grantOk then
      debugPrint ("STORAGE-CHECK: grant failed" & LF);
      return;
   end if;

   if exerciseStorage then
      debugPrint ("STORAGE-CHECK: PASS" & LF);
   else
      debugPrint ("STORAGE-CHECK: FAIL" & LF);
   end if;

   CuBit.Memory_Grants.Revoke (grantRef, grantOk);
end main;
