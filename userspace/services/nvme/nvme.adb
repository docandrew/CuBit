------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  NVMe controller operations.
--
--  Implements controller reset, admin commands (Identify, Create I/O Queue),
--  and I/O read via submission/completion queue pairs.  All register access
--  is through MMIO mapped at BAR_VIRT_BASE via SYSCALL_MAP_DEVICE.  Queue
--  memory lives in the DMA region at DMA_VIRT_BASE, pre-mapped by kernel.
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Busy_Poll;
#if nvme_io_profile = "on" then
with CuBit.Benchmark_Clock;
#end if;

package body NVMe is

   ADMIN_SLEEP_POLLS      : constant Positive := 5_000;
   CONTROLLER_SLEEP_POLLS : constant Positive := 10_000;
   --  An I/O completion is awaited in three stages: spinning (most
   --  commands finish within tens of microseconds), then yielding the CPU
   --  between looks (a busy host), then sleeping a millisecond between
   --  looks (a flush the host is writing back), until the timeout.
   IO_SPIN_MICROSECONDS   : constant := 200;
   IO_YIELD_MICROSECONDS  : constant := 20_000;
   IO_SLEEP_POLLS         : constant Positive := 1_000;
   --  With completion interrupts: the same overall bound, waited for in
   --  slices that re-check the queue.
   IO_WAIT_MICROSECONDS   : constant := 1_020_000;
   IO_WAIT_SLICE_MILLISECONDS : constant := 2;

   ---------------------------------------------------------------------------
   --  MMIO helpers: volatile reads/writes through the mapped BAR0
   ---------------------------------------------------------------------------
   barBase : System.Address := System.Null_Address;
   dmaBase : Unsigned_64 := 0;  --  Physical base of DMA region

   --  Doorbell stride in bytes (4 << DSTRD from CAP register)
   doorbellStride : Unsigned_32 := 4;

   --  Admin queue state
   adminSqTail  : Natural := 0;
   adminCqHead  : Natural := 0;
   adminPhase   : Unsigned_16 := 1;
   adminCmdId   : Unsigned_16 := 0;

   --  I/O queue state
   ioSqTail  : Natural := 0;
   ioCqHead  : Natural := 0;
   ioPhase   : Unsigned_16 := 1;
   ioCmdId   : Unsigned_16 := 0;
   ioFailed  : Boolean := False;
   --  Completion-interrupt notifications taken, and slice deadlines that
   --  expired without one (reported by reportWaits).
   interruptsSeen, sliceTimeouts : Unsigned_64 := 0;

#if nvme_io_profile = "on" then
   --  Test-only attribution, compiled out completely in ordinary builds.
   --  This includes descheduling inside the wait, not just device latency.
   type IO_Kind is (Read_Command, Write_Command, Flush_Command);
   type Wait_Metrics is record
      Commands, Ticks, Slow, Sleeps : Unsigned_64 := 0;
   end record;
   Metrics : array (IO_Kind) of Wait_Metrics;
   Flushes : Natural range 0 .. 64 := 0;
   Profile_Interval : Unsigned_64 := 0;

   procedure Report_Waits is
   begin
      --  Rare diagnostic output still perturbs timing. Do not present these
      --  runs as production benchmark results; rerun with the profile off.
      Profile_Interval := Profile_Interval + 1;
      for Kind in IO_Kind loop
         debugPrint
           ("NVME-WAIT: interval=" & Profile_Interval'Image &
            " kind=" & (case Kind is
              when Read_Command => "read",
              when Write_Command => "write",
              when Flush_Command => "flush") &
            " commands=" & Metrics (Kind).Commands'Image &
            " ticks=" & Metrics (Kind).Ticks'Image &
            " slow=" & Metrics (Kind).Slow'Image &
            " sleeps=" & Metrics (Kind).Sleeps'Image & ASCII.LF);
      end loop;
      Metrics := [others => (others => 0)];
      Flushes := 0;
   end Report_Waits;
#end if;

   ---------------------------------------------------------------------------
   --  Volatile MMIO read/write via overlay
   ---------------------------------------------------------------------------
   procedure writeReg32 (offset : Storage_Offset; val : Unsigned_32) is
      reg : Unsigned_32 with Volatile, Import,
            Address => barBase + offset;
   begin
      reg := val;
   end writeReg32;

   function readReg32 (offset : Storage_Offset) return Unsigned_32 is
      reg : Unsigned_32 with Volatile, Import,
            Address => barBase + offset;
   begin
      return reg;
   end readReg32;

   procedure writeReg64 (offset : Storage_Offset; val : Unsigned_64) is
      regLo : Unsigned_32 with Volatile, Import,
              Address => barBase + offset;
      regHi : Unsigned_32 with Volatile, Import,
              Address => barBase + offset + 4;
   begin
      regLo := Unsigned_32 (val and 16#FFFF_FFFF#);
      regHi := Unsigned_32 (Shift_Right (val, 32));
   end writeReg64;

   function readReg64 (offset : Storage_Offset) return Unsigned_64 is
      regLo : Unsigned_32 with Volatile, Import,
              Address => barBase + offset;
      regHi : Unsigned_32 with Volatile, Import,
              Address => barBase + offset + 4;
   begin
      return Shift_Left (Unsigned_64 (regHi), 32) or Unsigned_64 (regLo);
   end readReg64;

   ---------------------------------------------------------------------------
   --  Doorbell writes
   ---------------------------------------------------------------------------
   procedure ringSqDoorbell (queueId : Natural; tail : Natural) is
      offset : constant Storage_Offset :=
        DOORBELL_BASE +
        Storage_Offset (2 * queueId) * Storage_Offset (doorbellStride);
   begin
      --  Publish payload/SQ writes before notifying this x86 coherent-DMA
      --  device. x86 provides store ordering; prohibit compiler reordering.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      writeReg32 (offset, Unsigned_32 (tail));
   end ringSqDoorbell;

   procedure ringCqDoorbell (queueId : Natural; head : Natural) is
      offset : constant Storage_Offset :=
        DOORBELL_BASE +
        Storage_Offset (2 * queueId + 1) * Storage_Offset (doorbellStride);
   begin
      writeReg32 (offset, Unsigned_32 (head));
   end ringCqDoorbell;

   ---------------------------------------------------------------------------
   --  Queue overlays (overlaid on DMA region)
   ---------------------------------------------------------------------------
   adminSq : SQArray with Import,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + ADMIN_SQ_OFFSET);

   adminCq : CQArray with Import,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + ADMIN_CQ_OFFSET);

   ioSq : SQArray with Import,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + IO_SQ_OFFSET);

   ioCq : CQArray with Import, Volatile,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + IO_CQ_OFFSET);

   ---------------------------------------------------------------------------
   --  submitAdmin - submit a command to the admin SQ and poll for completion
   --  Returns True on success (status code = 0).
   ---------------------------------------------------------------------------
   function submitAdmin (cmd : SubmissionEntry) return Boolean is
      cqe    : CompletionEntry;
      ignore : Unsigned_64;
   begin
      adminSq (adminSqTail) := cmd;
      adminSqTail := (adminSqTail + 1) mod QUEUE_DEPTH;
      ringSqDoorbell (0, adminSqTail);

      --  Poll completion queue
      for attempt in 1 .. ADMIN_SLEEP_POLLS loop
         if (adminCq (adminCqHead).status and 1) = adminPhase then
            System.Machine_Code.Asm
              ("", Clobber => "memory", Volatile => True);
            cqe := adminCq (adminCqHead);
            --  Advance CQ head
            adminCqHead := (adminCqHead + 1) mod QUEUE_DEPTH;
            if adminCqHead = 0 then
               adminPhase := adminPhase xor 1;
            end if;
            ringCqDoorbell (0, adminCqHead);

            --  Check status (bits 15:1 = status code)
            if (Shift_Right (cqe.status, 1) and 16#7FFF#) = 0 then
               return True;
            else
               return False;
            end if;
         end if;

         --  Brief delay
         ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;

      return False;  --  Timeout
   end submitAdmin;

   ---------------------------------------------------------------------------
   --  makeCdw0 - build CDW0 from opcode and command ID
   ---------------------------------------------------------------------------
   function makeCdw0 (opcode : Unsigned_8; cid : Unsigned_16) return Unsigned_32 is
   begin
      return Unsigned_32 (opcode) or
             Shift_Left (Unsigned_32 (cid), 16);
   end makeCdw0;

   ---------------------------------------------------------------------------
   --  initController
   ---------------------------------------------------------------------------
   procedure initController (barPhys : Unsigned_64; dmaPhys : Unsigned_64) is
      ignore : Unsigned_64;
      cap    : Unsigned_64;
      dstrd  : Unsigned_32;
      csts   : Unsigned_32;
   begin
      --  1. Map BAR0 MMIO into our address space
      ignore := syscall (SYSCALL_MAP_DEVICE, barPhys,
                          BAR_VIRT_BASE, BAR_MAP_PAGES);

      barBase := System.Storage_Elements.To_Address (BAR_VIRT_BASE);
      dmaBase := dmaPhys;

      --  2. Read CAP register for doorbell stride
      cap := readReg64 (REG_CAP);
      dstrd := Unsigned_32 (Shift_Right (cap, 32) and 16#F#);
      doorbellStride := Shift_Left (Unsigned_32'(4), Natural (dstrd));

      debugPrint ("NVMe: Disabling controller..." & ASCII.LF);

      --  3. Disable controller (clear CC.EN)
      writeReg32 (REG_CC, 0);

      --  4. Wait for CSTS.RDY = 0
      for attempt in 1 .. CONTROLLER_SLEEP_POLLS loop
         csts := readReg32 (REG_CSTS);
         exit when (csts and CSTS_RDY) = 0;
         ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;

      --  5. Zero out queue + PRP list memory (pages 0-5)
      declare
         dmaMem : String (1 .. 6 * PAGE_SIZE) with Import,
           Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE);
      begin
         for i in dmaMem'Range loop
            dmaMem (i) := Character'Val (0);
         end loop;
      end;

      --  6. Set Admin Queue Attributes: ACQS=QUEUE_DEPTH-1, ASQS=QUEUE_DEPTH-1
      writeReg32 (REG_AQA,
        Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), 16) or
        Unsigned_32 (QUEUE_DEPTH - 1));

      --  7. Set Admin SQ/CQ base addresses (physical)
      writeReg64 (REG_ASQ, dmaPhys + ADMIN_SQ_OFFSET);
      writeReg64 (REG_ACQ, dmaPhys + ADMIN_CQ_OFFSET);

      --  8. Enable controller: CC.EN=1, IOSQES=6, IOCQES=4
      writeReg32 (REG_CC, CC_EN or CC_IOSQES or CC_IOCQES);

      --  9. Wait for CSTS.RDY = 1
      debugPrint ("NVMe: Waiting for controller ready..." & ASCII.LF);
      for attempt in 1 .. CONTROLLER_SLEEP_POLLS loop
         csts := readReg32 (REG_CSTS);
         exit when (csts and CSTS_RDY) /= 0;
         ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;

      if (readReg32 (REG_CSTS) and CSTS_RDY) = 0 then
         debugPrint ("NVMe: Controller failed to become ready!" & ASCII.LF);
      else
         debugPrint ("NVMe: Controller ready." & ASCII.LF);
      end if;

      --  Reset queue state
      adminSqTail := 0;
      adminCqHead := 0;
      adminPhase  := 1;
      adminCmdId  := 0;
      ioSqTail    := 0;
      ioCqHead    := 0;
      ioPhase     := 1;
      ioCmdId     := 0;
      ioFailed    := False;
   end initController;

   ---------------------------------------------------------------------------
   --  identifyController
   ---------------------------------------------------------------------------
   procedure identifyController is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      ok  : Boolean;
   begin
      adminCmdId := adminCmdId + 1;
      cmd.cdw0 := makeCdw0 (ADMIN_IDENTIFY, adminCmdId);
      cmd.nsid := 0;
      cmd.prp1 := dmaBase + IDENTIFY_OFFSET;
      cmd.prp2 := 0;
      cmd.cdw10 := CNS_CONTROLLER;

      ok := submitAdmin (cmd);

      if ok then
         --  Parse MDTS (byte 77 of identify controller data)
         declare
            idBuf : constant System.Address :=
              System.Storage_Elements.To_Address
                (DMA_VIRT_BASE + IDENTIFY_OFFSET);
            mdtsByte : Unsigned_8 with Import,
              Address => idBuf + 77;
            vwcByte : Unsigned_8 with Import,
              Address => idBuf + IDENTIFY_VWC_OFFSET;
            bufLimit : constant Unsigned_64 :=
              Unsigned_64 (DATA_BUF_PAGES) * Unsigned_64 (PAGE_SIZE);
         begin
            if mdtsByte > 0 and mdtsByte <= 20 then
               maxTransferBytes :=
                 Shift_Left (Unsigned_64 (PAGE_SIZE),
                             Natural (mdtsByte));
               if maxTransferBytes > bufLimit then
                  maxTransferBytes := bufLimit;
               end if;
            else
               --  MDTS=0 means unlimited by controller
               maxTransferBytes := bufLimit;
            end if;
            volatileWriteCache := (vwcByte and VWC_PRESENT) /= 0;
         end;
         debugPrint ("NVMe: Identify Controller OK." & ASCII.LF);
      else
         debugPrint ("NVMe: Identify Controller FAILED." & ASCII.LF);
      end if;
   end identifyController;

   ---------------------------------------------------------------------------
   --  identifyNamespace
   ---------------------------------------------------------------------------
   procedure identifyNamespace is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      ok  : Boolean;

      --  Identify NS data at identify buffer (first 16 bytes suffice for NSZE+NCAP)
      idBuf : constant System.Address :=
        System.Storage_Elements.To_Address (DMA_VIRT_BASE + IDENTIFY_OFFSET);
   begin
      --  Clear identify buffer first
      declare
         buf : String (1 .. PAGE_SIZE) with Import, Address => idBuf;
      begin
         for i in buf'Range loop
            buf (i) := Character'Val (0);
         end loop;
      end;

      adminCmdId := adminCmdId + 1;
      cmd.cdw0 := makeCdw0 (ADMIN_IDENTIFY, adminCmdId);
      cmd.nsid := 1;
      cmd.prp1 := dmaBase + IDENTIFY_OFFSET;
      cmd.prp2 := 0;
      cmd.cdw10 := CNS_NAMESPACE;

      ok := submitAdmin (cmd);

      if ok then
         --  Parse NSZE (8 bytes at offset 0) and LBAF0 (4 bytes at offset 128+0)
         declare
            nsze : Unsigned_64 with Import,
              Address => idBuf;
            --  LBA Format 0 at offset 128: RP(1:0), LBADS(7:0 at bits 16..23), MS
            lbaf0 : Unsigned_32 with Import,
              Address => idBuf + 128;
            lbads : Natural;
         begin
            nsBlockCount := nsze;
            lbads := Natural (Shift_Right (lbaf0, 16) and 16#FF#);
            if lbads >= 9 and lbads <= 16 then
               nsSectorSize := Shift_Left (Unsigned_32'(1), lbads);
            else
               nsSectorSize := 512;
            end if;
         end;

         debugPrint ("NVMe: Identify NS1 OK." & ASCII.LF);
      else
         debugPrint ("NVMe: Identify NS1 FAILED." & ASCII.LF);
      end if;
   end identifyNamespace;

   ---------------------------------------------------------------------------
   --  createIOQueues
   ---------------------------------------------------------------------------
   procedure createIOQueues
     (msixTable : Unsigned_64 := NO_MSIX; vector : Unsigned_64 := 0;
      ok : out Boolean)
   is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      MSI_ADDRESS : constant Unsigned_32 := 16#FEE0_0000#; --  APIC 0
      ENTRY_MASKED : constant Unsigned_32 := 1;
      CQ_PHYSICALLY_CONTIGUOUS : constant Unsigned_32 := 1;
      CQ_INTERRUPTS_ENABLED : constant Unsigned_32 := 2;
      useMSIX : constant Boolean := msixTable /= NO_MSIX;
   begin
      interruptsEnabled := False;
      if useMSIX then
         --  Entry zero: masked while written, then unmasked; devmgr clears
         --  the function mask after we reply.
         declare
            tableEntry : array (0 .. 3) of Unsigned_32 with Volatile,
              Import, Address => barBase + Storage_Offset (msixTable);
         begin
            tableEntry (3) := ENTRY_MASKED;
            tableEntry (0) := MSI_ADDRESS;
            tableEntry (1) := 0;
            tableEntry (2) := Unsigned_32 (vector);
            tableEntry (3) := 0;
            ok := tableEntry (2) = Unsigned_32 (vector);
         end;
         if not ok then
            return;
         end if;
      end if;
      --  Create I/O Completion Queue (ID=1)
      adminCmdId := adminCmdId + 1;
      cmd := NULL_SUBMISSION;
      cmd.cdw0  := makeCdw0 (ADMIN_CREATE_IO_CQ, adminCmdId);
      cmd.prp1  := dmaBase + IO_CQ_OFFSET;
      cmd.cdw10 := Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), 16) or 1;  -- size | QID=1
      --  PC=1; IEN=1 with interrupt vector 0 (MSI-X entry zero) if MSI-X.
      cmd.cdw11 := CQ_PHYSICALLY_CONTIGUOUS or
        (if useMSIX then CQ_INTERRUPTS_ENABLED else 0);

      ok := submitAdmin (cmd);
      if not ok then
         debugPrint ("NVMe: Create I/O CQ FAILED." & ASCII.LF);
         return;
      end if;

      --  Create I/O Submission Queue (ID=1, CQ=1)
      adminCmdId := adminCmdId + 1;
      cmd := NULL_SUBMISSION;
      cmd.cdw0  := makeCdw0 (ADMIN_CREATE_IO_SQ, adminCmdId);
      cmd.prp1  := dmaBase + IO_SQ_OFFSET;
      cmd.cdw10 := Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), 16) or 1;  -- size | QID=1
      cmd.cdw11 := Shift_Left (Unsigned_32'(1), 16) or 1;  -- CQID=1 | phys contiguous

      ok := submitAdmin (cmd);
      if not ok then
         debugPrint ("NVMe: Create I/O SQ FAILED." & ASCII.LF);
         return;
      end if;

      interruptsEnabled := useMSIX;
      debugPrint ("NVMe: I/O queues created." & ASCII.LF);
   end createIOQueues;

   ---------------------------------------------------------------------------
   --  I/O commands in flight. A transfer is split into chunks, each with its
   --  own slice of the DMA data area and its own PRP list, and up to
   --  MAX_IN_FLIGHT chunks are outstanding at once: the controller works
   --  on the next while the driver copies the last (and the host backend
   --  may run them in parallel). Completions may arrive in any order and
   --  are matched by command identifier.
   ---------------------------------------------------------------------------
   CHUNK_BYTES : constant := 128 * 1024;
   CHUNK_PAGES : constant := CHUNK_BYTES / PAGE_SIZE;
   MAX_IN_FLIGHT : constant := (DATA_BUF_PAGES * PAGE_SIZE) / CHUNK_BYTES;
   pragma Compile_Time_Error
     (MAX_IN_FLIGHT < 1 or else MAX_IN_FLIGHT >= QUEUE_DEPTH,
      "in-flight commands must fit the data area and the I/O queue");
   --  One PRP list per in-flight slot, packed into the PRP list page.
   PRP_LIST_BYTES : constant := CHUNK_PAGES * 8;
   pragma Compile_Time_Error
     (MAX_IN_FLIGHT * PRP_LIST_BYTES > PAGE_SIZE, "PRP lists overflow their page");

   subtype Flight_Slot is Natural range 0 .. MAX_IN_FLIGHT - 1;
   type Flight is record
      Active  : Boolean := False;
      Cid     : Unsigned_16 := 0;
      Chunk   : Natural := 0;           --  index within the transfer
      Bytes   : Unsigned_64 := 0;
      Offset  : Unsigned_64 := 0;       --  within the caller's buffer
   end record;
   flights : array (Flight_Slot) of Flight;

   function slotDmaOffset (slot : Flight_Slot) return Unsigned_64 is
     (DATA_BUF_OFFSET + Unsigned_64 (slot) * CHUNK_BYTES);

   ---------------------------------------------------------------------------
   --  nextCompletion - wait for the next I/O completion entry: spin (most
   --  commands finish within tens of microseconds), then yield between
   --  looks, then sleep a millisecond between looks, until the timeout.
   --  found is False on timeout. The entry is consumed (CQ doorbell rung).
   ---------------------------------------------------------------------------
   procedure nextCompletion
     (opcode : Unsigned_8; cqe : out CompletionEntry; found : out Boolean)
   is
      ignore : Unsigned_64;
#if nvme_io_profile = "on" then
      Start_Ticks : constant Unsigned_64 := CuBit.Benchmark_Clock.Read_Counter;
      Sleep_Count : Unsigned_64 := 0;
      Slow : Boolean := False;
      Kind : constant IO_Kind :=
        (if opcode = IO_READ then Read_Command
         elsif opcode = IO_WRITE then Write_Command else Flush_Command);
#else
      pragma Unreferenced (opcode);
#end if;
   begin
      found := False;
      cqe := (dw0 => 0, dw1 => 0, sqHead => 0, sqId => 0, cid => 0, status => 0);
      declare
         Started : constant Unsigned_64 := CuBit.Busy_Poll.Now;
      begin
         loop
            if (ioCq (ioCqHead).status and 1) = ioPhase then
               found := True;
               exit;
            end if;
            exit when interruptsEnabled and then
              not CuBit.Busy_Poll.Within (Started, IO_SPIN_MICROSECONDS);
            exit when not CuBit.Busy_Poll.Within (Started, IO_YIELD_MICROSECONDS);
            if CuBit.Busy_Poll.Within (Started, IO_SPIN_MICROSECONDS) then
               CuBit.Busy_Poll.Relax;
            else
               ignore := syscall (SYSCALL_YIELD);
            end if;
         end loop;
      end;
      if not found and then interruptsEnabled then
         --  Sleep until the completion interrupt. The kernel keeps a latched
         --  notification, so one arriving between the check and the wait
         --  is not lost. A request queued meanwhile also ends the wait; then
         --  poll at the millisecond. The same overall timeout applies.
         declare
            Started : constant Unsigned_64 := CuBit.Busy_Poll.Now;
            event : Message;
            sawEvent : Boolean;
            activity : Activity_Result;
         begin
            loop
               sawEvent := False;
               while Poll_Event (event) loop
                  sawEvent := True;
                  interruptsSeen := interruptsSeen + 1;
               end loop;
               if (ioCq (ioCqHead).status and 1) = ioPhase then
                  found := True;
                  exit;
               end if;
               exit when not CuBit.Busy_Poll.Within (Started, IO_WAIT_MICROSECONDS);
               if sawEvent then
                  null;   --  a shared vector's other device: look again
               else
                  activity := Wait_For_Activity_Until
                    (syscall (SYSCALL_GETTIME) + IO_WAIT_SLICE_MILLISECONDS);
                  if activity = Deadline_Reached then
                     sliceTimeouts := sliceTimeouts + 1;
                  end if;
                  if activity = Unavailable then
                     ignore := syscall (SYSCALL_SLEEP, 1);
                  elsif activity = Work_Available and then
                    (ioCq (ioCqHead).status and 1) /= ioPhase
                  then
                     --  Pending IPC (not ours to take here) or an event:
                     --  drain events next time; never spin on requests.
                     if not Poll_Event (event) then
                        ignore := syscall (SYSCALL_SLEEP, 1);
                     end if;
                  end if;
               end if;
            end loop;
         end;
      elsif not found then
#if nvme_io_profile = "on" then
         Slow := True;
#end if;
         for attempt in 1 .. IO_SLEEP_POLLS loop
            if (ioCq (ioCqHead).status and 1) = ioPhase then
               found := True;
               exit;
            end if;
            ignore := syscall (SYSCALL_SLEEP, 1);
#if nvme_io_profile = "on" then
            Sleep_Count := Sleep_Count + 1;
#end if;
         end loop;
      end if;
#if nvme_io_profile = "on" then
      Metrics (Kind).Ticks := Metrics (Kind).Ticks +
        (CuBit.Benchmark_Clock.Read_Counter - Start_Ticks);
      Metrics (Kind).Commands := Metrics (Kind).Commands + 1;
      Metrics (Kind).Slow := Metrics (Kind).Slow + Boolean'Pos (Slow);
      Metrics (Kind).Sleeps := Metrics (Kind).Sleeps + Sleep_Count;
      if Kind = Flush_Command then
         Flushes := Flushes + 1;
         if Flushes = 64 then
            Report_Waits;
         end if;
      end if;
#end if;
      if not found then
         return;
      end if;
      --  Read the controller's phase publication BEFORE copying other
      --  fields; the compiler barrier keeps the snapshot after the check.
      --  The controller cannot reuse this entry until our CQ doorbell.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      cqe := ioCq (ioCqHead);
      ioCqHead := (ioCqHead + 1) mod QUEUE_DEPTH;
      if ioCqHead = 0 then
         ioPhase := ioPhase xor 1;
      end if;
      ringCqDoorbell (1, ioCqHead);
   end nextCompletion;

   procedure reportWaits is
   begin
      debugPrint ("NVMe: interrupts=" & interruptsSeen'Image &
                  " slice-timeouts=" & sliceTimeouts'Image & ASCII.LF);
   end reportWaits;

   function statusOk (cqe : CompletionEntry) return Boolean is
     ((Shift_Right (cqe.status, 1) and 16#7FFF#) = 0);

   procedure submitIO (cmd : SubmissionEntry) is
   begin
      ioSq (ioSqTail) := cmd;
      ioSqTail := (ioSqTail + 1) mod QUEUE_DEPTH;
      ringSqDoorbell (1, ioSqTail);
   end submitIO;

   function nextCid return Unsigned_16 is
   begin
      ioCmdId := ioCmdId + 1;
      return ioCmdId;
   end nextCid;

   function flush return Boolean is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      cqe : CompletionEntry;
      found : Boolean;
      cid : Unsigned_16;
   begin
      if ioFailed or else nsBlockCount = 0 then
         return False;
      end if;
      cid := nextCid;
      cmd.cdw0 := makeCdw0 (IO_FLUSH, cid);
      cmd.nsid := 1;
      submitIO (cmd);
      nextCompletion (IO_FLUSH, cqe, found);
      --  Nothing else is outstanding: only this command may complete.
      if not found or else cqe.sqId /= 1 or else cqe.cid /= cid then
         ioFailed := True;
         return False;
      end if;
      return statusOk (cqe);
   end flush;

   ---------------------------------------------------------------------------
   --  transfer - read or write count sectors at lba through the pipeline.
   --  Returns the bytes of the longest prefix whose chunks all completed
   --  successfully. Every submitted chunk is reaped before returning, so
   --  no command still owns a DMA slice; a timeout retires the queue.
   ---------------------------------------------------------------------------
   function transfer
     (opcode : Unsigned_8; lba : Unsigned_64; count : Unsigned_32;
      buf : System.Address; fua : Boolean) return Unsigned_64
   is
      sector : constant Unsigned_64 := Unsigned_64 (nsSectorSize);
      totalBytes : constant Unsigned_64 := Unsigned_64 (count) * sector;
      chunkLimit : constant Unsigned_64 :=
        Unsigned_64'Min (CHUNK_BYTES, maxTransferBytes) / sector * sector;
      chunks : Natural;
      submitted, completed : Natural := 0;
      failedChunk : Natural := Natural'Last;
      prefix : Natural;
      --  Chunks completed successfully; a transfer is bounded by the grant
      --  (the service rejects larger ones below).
      MAX_CHUNKS : constant := 256;
      doneMask : array (0 .. MAX_CHUNKS - 1) of Boolean := [others => False];

      procedure Launch (slot : Flight_Slot) is
         chunk : constant Natural := submitted;
         offset : constant Unsigned_64 := Unsigned_64 (chunk) * chunkLimit;
         bytes : constant Unsigned_64 := Unsigned_64'Min (chunkLimit, totalBytes - offset);
         pages : constant Unsigned_64 := (bytes + PAGE_SIZE - 1) / PAGE_SIZE;
         dmaPhys : constant Unsigned_64 := dmaBase + slotDmaOffset (slot);
         dmaVirt : constant System.Address := System.Storage_Elements.To_Address
           (Integer_Address (DMA_VIRT_BASE + slotDmaOffset (slot)));
         listOffset : constant Unsigned_64 :=
           PRP_LIST_OFFSET + Unsigned_64 (slot) * PRP_LIST_BYTES;
         cmd : SubmissionEntry := NULL_SUBMISSION;
         cid : constant Unsigned_16 := nextCid;
         sectors : constant Unsigned_32 := Unsigned_32 (bytes / sector);
         chunkLba : constant Unsigned_64 := lba + offset / sector;
      begin
         if opcode = IO_WRITE then
            declare
               srcBuf : String (1 .. Natural (bytes))
                 with Import, Address => buf + Storage_Offset (offset);
               dstBuf : String (1 .. Natural (bytes)) with Import, Address => dmaVirt;
            begin
               dstBuf := srcBuf;
            end;
         end if;
         if pages > 2 then
            declare
               list : array (0 .. CHUNK_PAGES - 1) of Unsigned_64
                 with Import, Address => System.Storage_Elements.To_Address
                   (Integer_Address (DMA_VIRT_BASE + listOffset));
            begin
               for i in 1 .. Natural (pages) - 1 loop
                  list (i - 1) := dmaPhys + Unsigned_64 (i) * PAGE_SIZE;
               end loop;
            end;
         end if;
         cmd.cdw0 := makeCdw0 (opcode, cid);
         cmd.nsid := 1;
         cmd.prp1 := dmaPhys;
         cmd.prp2 := (if pages <= 1 then 0
                      elsif pages = 2 then dmaPhys + PAGE_SIZE
                      else dmaBase + listOffset);
         cmd.cdw10 := Unsigned_32 (chunkLba and 16#FFFF_FFFF#);
         cmd.cdw11 := Unsigned_32 (Shift_Right (chunkLba, 32));
         cmd.cdw12 := (sectors - 1) or
           (if opcode = IO_WRITE and then fua then CDW12_FUA else 0);
         flights (slot) := (Active => True, Cid => cid, Chunk => chunk,
                            Bytes => bytes, Offset => offset);
         submitted := submitted + 1;
         submitIO (cmd);
      end Launch;

      cqe : CompletionEntry;
      found : Boolean;
      slotOf : Integer;
   begin
      if ioFailed or else nsSectorSize = 0 or else chunkLimit = 0 or else count = 0 then
         return 0;
      end if;
      chunks := Natural ((totalBytes + chunkLimit - 1) / chunkLimit);
      if chunks > doneMask'Length then
         return 0;   --  larger than any grant this driver accepts
      end if;
      flights := [others => (others => <>)];
      for slot in Flight_Slot loop
         exit when submitted = chunks;
         Launch (slot);
      end loop;
      while completed < submitted loop
         nextCompletion (opcode, cqe, found);
         slotOf := -1;
         if found and then cqe.sqId = 1 then
            for slot in Flight_Slot loop
               if flights (slot).Active and then flights (slot).Cid = cqe.cid then
                  slotOf := slot;
               end if;
            end loop;
         end if;
         if slotOf < 0 then
            --  A timeout, or a completion for no command of ours: the
            --  outstanding commands may still own their DMA slices.
            ioFailed := True;
            debugPrint ("NVMe: I/O completion lost or foreign." & ASCII.LF);
            exit;
         end if;
         declare
            f : Flight renames flights (slotOf);
         begin
            f.Active := False;
            completed := completed + 1;
            if statusOk (cqe) then
               if opcode = IO_READ then
                  declare
                     srcBuf : String (1 .. Natural (f.Bytes)) with Import,
                       Address => System.Storage_Elements.To_Address
                         (Integer_Address (DMA_VIRT_BASE + slotDmaOffset (slotOf)));
                     dstBuf : String (1 .. Natural (f.Bytes))
                       with Import, Address => buf + Storage_Offset (f.Offset);
                  begin
                     dstBuf := srcBuf;
                  end;
               end if;
               doneMask (f.Chunk) := True;
            else
               failedChunk := Natural'Min (failedChunk, f.Chunk);
            end if;
            --  Reuse the slot for the next chunk unless a chunk failed.
            if failedChunk = Natural'Last and then submitted < chunks then
               Launch (slotOf);
            end if;
         end;
      end loop;
      prefix := 0;
      while prefix < chunks and then doneMask (prefix) loop
         prefix := prefix + 1;
      end loop;
      return Unsigned_64'Min (totalBytes, Unsigned_64 (prefix) * chunkLimit);
   end transfer;

   function readBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64
   is
      bytes : constant Unsigned_64 := transfer (IO_READ, lba, count, buf, False);
   begin
      if bytes /= Unsigned_64 (count) * Unsigned_64 (nsSectorSize) then
         debugPrint ("NVMe: Read failed." & ASCII.LF);
      end if;
      return bytes;
   end readBlocks;

   function writeBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address;
      fua   : Boolean := False) return Unsigned_64
   is
      bytes : constant Unsigned_64 := transfer (IO_WRITE, lba, count, buf, fua);
   begin
      if bytes /= Unsigned_64 (count) * Unsigned_64 (nsSectorSize) then
         debugPrint ("NVMe: Write failed." & ASCII.LF);
      end if;
      return bytes;
   end writeBlocks;

end NVMe;
