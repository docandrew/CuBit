with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Kernel_Calls;
with CuBit.Kernel_ABI;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;
with Cpio;
procedure DMA_Retirement_Native is
   package HC renames CuBit.Handles.Control;
   --  The boot image's size: the kernel's launch configuration for the
   --  process it starts in devmgr's place.
   function Boot_Image_Bytes return Unsigned_64 is
      Configuration : Launch_Configuration.Device_Manager_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      return (if Configured then Configuration.Boot_Image else 0);
   end Boot_Image_Bytes;
   --  The supervisor runs in devmgr's place (boot roots); the children it
   --  spawns from the same ELF have none.
   Roots : constant CuBit.Handles.Boot_Roots.Roots := CuBit.Handles.Boot_Roots.Get;
   Failed : constant Unsigned_64 := Unsigned_64'Last;
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
   Child : HC.Spawned_Child;
   --  A fresh suspended child from this ELF, or False if SPAWN refuses
   --  (for instance while a dead owner's cleanup is still pending).
   function Spawn return Boolean is
      Answer : constant HC.Spawn_Result :=
        HC.Spawn (Roots.Process_Root, ELF, ELF_Size, 5, "dma-owner",
                  CuBit.Process_IDs.No_Process,
                  (Kind => CuBit.Startup_Tables.Standard_Limit));
   begin
      if Answer.Spawned then
         Child := Answer.Value;
      end if;
      return Answer.Spawned;
   end Spawn;
   --  Kill the current child and give up this process's handles to it, so
   --  the lifecycle loops below do not accumulate them.
   function Kill return Boolean is
      Killed : constant Boolean := HC.Kill (Child.Child, Child.Control);
      Closed : CuBit.Handles.IPC.Close_Result;
      pragma Unreferenced (Closed);
   begin
      Closed := CuBit.Handles.IPC.Close (Child.Mailbox);
      Closed := CuBit.Handles.IPC.Close (Child.Space);
      Closed := CuBit.Handles.IPC.Close (Child.Control);
      return Killed;
   end Kill;
   function Allocate_DMA (Order, Virtual, Mode, Ceiling : Unsigned_64) return Unsigned_64 is
     (HC.Allocate_DMA (Child.Child, Roots.Process_Root, Order, Virtual, Mode, Ceiling));
   function Resume (Max_Frames : Unsigned_64) return Boolean is
     (HC.Resume (Child.Child, Child.Control, Max_Frames, 0));
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
   if not Roots.Valid then
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
      Msg.tag := (label => 16#F000#, length => 1, protocol => 0, ipc => <>, reserved => 0);
      Ignored := reply (From, Msg);
      loop Ignored := syscall (SYSCALL_SLEEP, 100); end loop;
   end if;
   debugPrint ("native DMA retirement: disposable real owner cleanup (NO GPU)" & ASCII.LF);
   Cpio.init (Archive, To_Address (16#0000_5000_0000_0000#),
     Boot_Image_Bytes, OK);
   Check (OK, "initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK and then Roots.Valid and then Spawn, "spawn");
   for I in Physical'Range loop
      Physical (I) := Allocate_DMA (9, Base + Unsigned_64 (I - 1) * Block_Bytes, 3, 2 ** 32);
      Check (Physical (I) /= Failed, "allocation" & I'Image);
   end loop;
   -- ELF loading has already charged more than one ordinary page. Refusal
   -- must leave the same child suspended and its allocations intact. Repeat
   -- to catch accidental launch-state transitions or quota rollback damage.
   for Attempt in 1 .. 3 loop
      Check (not Resume (1), "overcharged resume must refuse");
   end loop;
   -- 32 MiB exceeds the ELF's ordinary footprint but not its 80 MiB DMA
   -- backing. This refusal specifically requires DMA quota integration.
   Check (not Resume (8192), "DMA charge adoption refusal");
   Check (Resume (22000), "resume child");
   -- Failed overlap maps must refund bound charges exactly once. Four leaked
   -- 512-page reservations would exhaust the finite headroom below.
   for Attempt in 1 .. 4 loop
      Check (Allocate_DMA (9, Base, 0, 2 ** 32) = Failed,
        "overlap DMA refusal");
   end loop;
   Check (Allocate_DMA (11, Base + 64 * Block_Bytes, 0, 2 ** 32) = Failed,
     "running DMA quota refusal");
   Check (Allocate_DMA (9, Base + 64 * Block_Bytes, 0, 2 ** 32) /= Failed,
     "rollback preserves quota headroom");
   Msg := NULL_MESSAGE;
   Msg.tag := (label => 1, length => 0, protocol => 0, ipc => <>, reserved => 0);
   Msg.tag := capCall (Child.Mailbox, Msg, Wait_Forever);
   Check (Msg.tag.label = 16#F000#, "child response");
   debugPrint ("native DMA quota: adoption runtime denial overlap rollback and child response PASS" & ASCII.LF);
   Loan := CuBit.Grant_References.Decode (Msg.words (0));
   CuBit.Memory_Grants.Acquire_Via_Capability
     (Child.Mailbox, Loan, 0, 4096, CuBit.Memory_Grants.Read_Access, Mapped, OK);
   Check (OK, "borrow DMA");
   Check (Kill, "kill");
   for Attempt in 1 .. 2000 loop
      if CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Receive_Event_Nonblocking,
         Unsigned_64 (To_Integer (Msg'Address))) = CuBit.Kernel_ABI.Event_Received
      then
         Exit_Seen := Msg.tag.label = 16#0103# and then Msg.tag.length = 4
           and then Msg.words (0) = CuBit.Process_IDs.To_Word (Child.Child);
         exit when Exit_Seen;
      end if;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (Exit_Seen, "owner retirement report");
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
   OK := False;
   for Attempt in 1 .. 2000 loop
      Drain_Events;
      OK := Spawn;
      exit when OK;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (OK, "bounded cleanup finishes and a replacement spawns");
   for I in 1 .. 8 loop
      Result := Allocate_DMA (9, Base + Unsigned_64 (I - 1) * Block_Bytes, 0, 2 ** 32);
      Check (Result /= Failed, "replacement ordinary allocation");
      for Old of Physical loop
         Check (Result /= Old, "retained orphan backing reused");
      end loop;
   end loop;
   Check (Kill, "replacement kill");
   OK := False;
   for Attempt in 1 .. 2000 loop
      Drain_Events;
      OK := Spawn;
      exit when OK;
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (OK, "ordinary cleanup completes");
   -- Each fresh incarnation needs its own arena and record page. Leaking
   -- even one of these per exit would exhaust the 512MiB fixture's shared
   -- metadata budget before completing this loop. The original retained
   -- owner's orphan backing must remain untouched throughout.
   for Round in 1 .. 160 loop
      Result := Allocate_DMA (0, Base, 0, 2 ** 32);
      Check (Result /= Failed, "metadata churn allocation" & Round'Image);
      for Old of Physical loop
         Check (Result < Old or else Result >= Old + Block_Bytes,
           "churn reused retained orphan page");
      end loop;
      Check (Kill, "metadata churn kill");
      OK := False;
      for Attempt in 1 .. 2000 loop
         Drain_Events;
         OK := Spawn;
         exit when OK;
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Check (OK, "metadata churn respawn" & Round'Image);
   end loop;
   debugPrint ("native DMA metadata: 160 owner lifecycles reclaim slabs and preserve orphan backing PASS" & ASCII.LF);
   debugPrint ("TEST: PASS native DMA retirement 40 retained records orphan backing survives owner replacement ordinary cleanup (NO GPU)" & ASCII.LF);
   loop Ignored := syscall (SYSCALL_SLEEP, 100); end loop;
end DMA_Retirement_Native;
