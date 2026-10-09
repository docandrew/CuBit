with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Kernel_Calls;
with CuBit.Kernel_ABI;
with Cpio;
procedure DMA_Retirement_Native is
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Child : constant Unsigned_64 := 42;
   Failed : constant Unsigned_64 := Unsigned_64'Last;
   Resource_Capability : constant Unsigned_64 := 9;
   Resource_Slot : constant Unsigned_64 := 20;
   Base : constant Unsigned_64 := 16#7600_0000#;
   Block_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
   Physical : array (1 .. 40) of Unsigned_64;
   ELF : System.Address;
   ELF_Size, Ignored, Result : Unsigned_64;
   Archive : Cpio.Archive;
   OK : Boolean;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Mapped : System.Address;
   Msg : aliased Message;
   From : Process_ID;
   Exit_Seen : Boolean := False;
   function Spawn return Unsigned_64 is
     (syscall (SYSCALL_SPAWN, Unsigned_64 (To_Integer (ELF)), ELF_Size,
               5, 0, Child, PID));
   procedure Drain_Events is
   begin
      -- This disposable supervisor is the sole reader of its event lane.
      -- All children and grants are owned by this fixture; after explicit
      -- loan return these reports need no cached runtime consumer. Poll alone
      -- would fill Process_Events' 32-entry exit cache and hold later PIDs.
      for Work in 1 .. 64 loop
         exit when CuBit.Kernel_Calls.Call
           (CuBit.Kernel_ABI.Receive_Event_Nonblocking,
            Unsigned_64 (To_Integer (Msg'Address))) /= CuBit.Kernel_ABI.Event_Received;
      end loop;
   end Drain_Events;
   procedure Check (Value : Boolean; Detail : String) is
   begin
      if not Value then
         debugPrint ("TEST: FAIL native DMA retirement: " & Detail & ASCII.LF);
         Ignored := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   if PID = Child then
      receive (From, Msg);
      declare
         Word : Unsigned_64 with Import, Volatile,
           Address => To_Address (Integer_Address (Base));
      begin
         Word := 16#ABCDEF42#;
         CuBit.Memory_Grants.Create_For_Process
           (From, Word'Address, 1, False, Loan, OK);
         Check (OK, "child grant");
      end;
      Msg.words (0) := CuBit.Grant_References.Encode (Loan);
      Msg.tag := (16#F000#, 1, 0, 0);
      Ignored := reply (From, Msg);
      loop Ignored := syscall (SYSCALL_SLEEP, 100); end loop;
   end if;
   debugPrint ("native DMA retirement: disposable real owner cleanup (NO GPU)" & ASCII.LF);
   Cpio.init (Archive, To_Address (16#0000_5000_0000_0000#),
     getInfo (SYSINFO_RAMDISK_SIZE), OK);
   Check (OK, "initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK and then Spawn = Child, "spawn");
   for I in Physical'Range loop
      Physical (I) := syscall (SYSCALL_ALLOC_DMA, Child, 9,
        Base + Unsigned_64 (I - 1) * Block_Bytes, 3, 2 ** 32);
      Check (Physical (I) /= Failed, "allocation" & I'Image);
   end loop;
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, Child,
     0, 3, 15) /= Failed, "child endpoint");
   -- ELF loading has already charged more than one ordinary page. Refusal
   -- must leave the same child suspended and its allocations intact. Repeat
   -- to catch accidental launch-state transitions or quota rollback damage.
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, Child,
     Resource_Capability, 1, 0, 0, Resource_Slot) = 0, "small quota");
   for Attempt in 1 .. 3 loop
      Check (syscall (SYSCALL_RESUME, Child) = Failed,
        "overcharged resume must refuse");
   end loop;
   -- 32 MiB exceeds the ELF's ordinary footprint but not its 80 MiB DMA
   -- backing. This refusal specifically requires DMA quota integration.
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, Child,
     Resource_Capability, 8192, 0, 0, Resource_Slot) = 0, "DMA-small quota");
   Check (syscall (SYSCALL_RESUME, Child) = Failed, "DMA charge adoption refusal");
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, Child,
     Resource_Capability, 22000, 0, 0, Resource_Slot) = 0, "finite quota");
   Check (syscall (SYSCALL_RESUME, Child) = 0, "resume child");
   -- Failed overlap maps must refund bound charges exactly once. Four leaked
   -- 512-page reservations would exhaust the finite headroom below.
   for Attempt in 1 .. 4 loop
      Check (syscall (SYSCALL_ALLOC_DMA, Child, 9, Base, 0, 2 ** 32) = Failed,
        "overlap DMA refusal");
   end loop;
   Check (syscall (SYSCALL_ALLOC_DMA, Child, 11,
     Base + 64 * Block_Bytes, 0, 2 ** 32) = Failed, "running DMA quota refusal");
   Check (syscall (SYSCALL_ALLOC_DMA, Child, 9,
     Base + 64 * Block_Bytes, 0, 2 ** 32) /= Failed, "rollback preserves quota headroom");
   Msg := NULL_MESSAGE;
   Msg.tag := (1, 0, 0, 0);
   Msg.tag := capCall (15, Msg, Wait_Forever);
   Check (Msg.tag.label = 16#F000#, "child response");
   debugPrint ("native DMA quota: adoption runtime denial overlap rollback and child response PASS" & ASCII.LF);
   Loan := CuBit.Grant_References.Decode (Msg.words (0));
   CuBit.Memory_Grants.Acquire_Via_Capability
     (15, Loan, 0, 4096, CuBit.Memory_Grants.Read_Access, Mapped, OK);
   Check (OK, "borrow DMA");
   Check (killProcess (Process_ID (Child)) = 0, "kill");
   for Attempt in 1 .. 2000 loop
      if CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Receive_Event_Nonblocking,
         Unsigned_64 (To_Integer (Msg'Address))) = CuBit.Kernel_ABI.Event_Received
      then
         Exit_Seen := Msg.tag.label = 16#0103# and then Msg.tag.length = 4
           and then Msg.words (0) = Child;
         exit when Exit_Seen;
      end if;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (Exit_Seen, "owner retirement report");
   Check (Spawn = Failed, "acquired DMA grant retains PID after report drain");
   declare
      Word : Unsigned_64 with Import, Volatile, Address => Mapped;
   begin
      Check (Word = 16#ABCDEF42#, "DMA contents survive owner death");
   end;
   CuBit.Memory_Grants.Return_Acquisition (Loan, OK);
   Check (OK, "return retained DMA loan");
   debugPrint ("native DMA retirement: live grant pins dead owner and readable backing PASS" & ASCII.LF);
   Result := Failed;
   -- Reports also retain a PID; drain them rather than bypassing that gate.
   for Attempt in 1 .. 2000 loop
      Drain_Events;
      Result := Spawn;
      exit when Result = Child;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (Result = Child, "bounded cleanup finishes and exact PID reusable");
   for I in 1 .. 8 loop
      Result := syscall (SYSCALL_ALLOC_DMA, Child, 9,
        Base + Unsigned_64 (I - 1) * Block_Bytes, 0, 2 ** 32);
      Check (Result /= Failed, "replacement ordinary allocation");
      for Old of Physical loop
         Check (Result /= Old, "retained orphan backing reused");
      end loop;
   end loop;
   Check (killProcess (Process_ID (Child)) = 0, "replacement kill");
   Result := Failed;
   for Attempt in 1 .. 2000 loop
      Drain_Events;
      Result := Spawn;
      exit when Result = Child;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (Result = Child, "ordinary cleanup completes");
   -- Each fresh incarnation needs its own arena and record page. Leaking
   -- even one of these per exit would exhaust the 512MiB fixture's shared
   -- metadata budget before completing this loop. The original retained
   -- owner's orphan backing must remain untouched throughout.
   for Round in 1 .. 160 loop
      Result := syscall (SYSCALL_ALLOC_DMA, Child, 0, Base, 0, 2 ** 32);
      Check (Result /= Failed, "metadata churn allocation" & Round'Image);
      for Old of Physical loop
         Check (Result < Old or else Result >= Old + Block_Bytes,
           "churn reused retained orphan page");
      end loop;
      Check (killProcess (Process_ID (Child)) = 0, "metadata churn kill");
      Result := Failed;
      for Attempt in 1 .. 2000 loop
         Drain_Events;
         Result := Spawn;
         exit when Result = Child;
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Check (Result = Child, "metadata churn PID reuse" & Round'Image);
   end loop;
   debugPrint ("native DMA metadata: 160 owner lifecycles reclaim slabs and preserve orphan backing PASS" & ASCII.LF);
   debugPrint ("TEST: PASS native DMA retirement 40 retained records orphan backing survives PID reuse ordinary cleanup (NO GPU)" & ASCII.LF);
   loop Ignored := syscall (SYSCALL_SLEEP, 100); end loop;
end DMA_Retirement_Native;
