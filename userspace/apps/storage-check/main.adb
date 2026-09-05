------------------------------------------------------------------------------
--  CuBit live-storage diagnostic
--
--  Exercises create, write, close, stale-handle rejection, reopen, and read
--  against the live system's writable memory filesystem.
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
   PATH : constant String := "@mem:/tmp/storage-check.txt";
   PAYLOAD : constant String := "CuBit writable storage ready";

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

      Resolve
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if not ok or else mapped = System.Null_Address then
         return False;
      end if;

      Resolve
        (firstRef, pid, 0, 16, Write_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Resolve
        (firstRef, pid, PAGE_SIZE - 4, 8, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Resolve
        (firstRef, wrongOwner, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Revoke (firstRef, ok);
      if not ok then
         return False;
      end if;

      Resolve
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

      Resolve
        (firstRef, pid, 0, 16, Read_Access, mapped, ok);
      if ok then
         return False;
      end if;

      Resolve
        (secondRef, pid, 0, 16, Read_Access, mapped, ok);
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

      --  Exercise the distinct read-write mode. The resulting handle must
      --  retain both rights rather than being collapsed to write-only.
      msg := Open_Request
        (grantRef, PATH'Length,
         OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE);
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
         debugPrint ("STORAGE-CHECK: write failed" & LF);
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
      return msg.tag.label = REPLY_OK;
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
