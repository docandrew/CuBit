------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  NVMe controller operations.
--
--  Implements controller reset, admin commands (Identify, Create I/O Queue,
--  Abort), and I/O through one submission/completion queue pair. All
--  register access is through MMIO mapped at BAR_VIRT_BASE through its
--  register handle. Queue memory lives in the DMA region at DMA_VIRT_BASE,
--  pre-mapped by the kernel (NVMe_Layout).
--
--  Every command ends (BUG-003). Completions are matched by identifier
--  (NVMe_Command_Ids); every posted entry is consumed, so the queue cursors
--  (NVMe_Queue_Cursors) never fall out of step, and a late or foreign
--  completion is discarded with a counted, logged outcome. A command past
--  its deadline is recovered as Linux does (NVMe_Recovery): Abort, then a
--  controller reset that re-creates the queues, then a bounded re-issue;
--  only then does the request fail, with a typed outcome.
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.Devices;
with CuBit.Busy_Poll;
#if nvme_io_profile = "on" then
with CuBit.Benchmark_Clock;
#end if;

with CuBit.NVMe_Control;
with NVMe_Protocol; use NVMe_Protocol;
with NVMe_Queue_Cursors; use NVMe_Queue_Cursors;
with NVMe_Command_Ids; use NVMe_Command_Ids;
with NVMe_Recovery; use NVMe_Recovery;
with NVMe_Layout; use NVMe_Layout;

package body NVMe is

   --  An I/O completion is awaited in three stages: spinning (most
   --  commands finish within tens of microseconds), then yielding the CPU
   --  between looks (a busy host), then sleeping until the completion
   --  interrupt, or a millisecond between looks when polled, until the
   --  command's deadline (NVMe_Recovery.IO_Command_Deadline).
   IO_SPIN_MICROSECONDS   : constant := 200;
   IO_YIELD_MICROSECONDS  : constant := 20_000;
   IO_WAIT_SLICE_MILLISECONDS : constant := 2;
   --  Between looks at the admin queue and at CSTS.RDY.
   POLL_SLEEP_MILLISECONDS : constant := 1;
   MILLISECOND_MICROSECONDS : constant := 1_000;

   ---------------------------------------------------------------------------
   --  MMIO helpers: volatile reads/writes through the mapped BAR0
   ---------------------------------------------------------------------------
   barBase : System.Address := System.Null_Address;
   dmaBase : Unsigned_64 := 0;  --  Physical base of DMA region

   --  Doorbell stride in bytes (4 << DSTRD from CAP register)
   doorbellStride : Unsigned_32 := 4;
   --  CAP.TO: how long CSTS.RDY may take to follow CC.EN.
   readyTimeout : Ready_Microseconds := Ready_Microseconds'First;

   --  Admin queue state
   adminSqTail  : Queue_Index := 0;
   adminCursor  : Cursor := Initial;
   adminCmdId   : Command_Identifier := 0;

   --  I/O queue state
   ioSqTail  : Queue_Index := 0;
   ioCursor  : Cursor := Initial;
   commands  : NVMe_Command_Ids.Tracker := NVMe_Command_Ids.Empty;
   phase     : Recovery_Phase := Serving;
   --  Whether the I/O completion queue should interrupt (re-created so by
   --  a reset).
   queuesInterrupt : Boolean := False;

   --  Completion-interrupt notifications taken, and slice deadlines that
   --  expired without one (reported by reportWaits).
   interruptsSeen, sliceTimeouts : Unsigned_64 := 0;
   --  Recovery counters (reported by reportWaits and each recovery).
   expiredCommands, abortsSent, abortsPerformed, resets, failedResets,
     lateCompletions, foreignCompletions, failedRequests : Unsigned_64 := 0;

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
   procedure ringSqDoorbell (queueId : Queue_Identifier; tail : Queue_Index) is
      offset : constant Storage_Offset :=
        DOORBELL_BASE +
        Storage_Offset (2 * Natural (queueId)) * Storage_Offset (doorbellStride);
   begin
      --  Publish payload/SQ writes before notifying this x86 coherent-DMA
      --  device. x86 provides store ordering; prohibit compiler reordering.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      writeReg32 (offset, Unsigned_32 (tail));
   end ringSqDoorbell;

   procedure ringCqDoorbell (queueId : Queue_Identifier; head : Queue_Index) is
      offset : constant Storage_Offset :=
        DOORBELL_BASE +
        Storage_Offset (2 * Natural (queueId) + 1) * Storage_Offset (doorbellStride);
   begin
      writeReg32 (offset, Unsigned_32 (head));
   end ringCqDoorbell;

   ---------------------------------------------------------------------------
   --  Queue overlays (overlaid on DMA region)
   ---------------------------------------------------------------------------
   adminSq : SQArray with Import,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + ADMIN_SQ_OFFSET);

   adminCq : CQArray with Import, Volatile,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + ADMIN_CQ_OFFSET);

   ioSq : SQArray with Import,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + IO_SQ_OFFSET);

   ioCq : CQArray with Import, Volatile,
     Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE + IO_CQ_OFFSET);

   function makeCdw0 (opcode : Unsigned_8; cid : Command_Identifier) return Unsigned_32 is
     (Unsigned_32 (opcode) or Shift_Left (Unsigned_32 (cid), 16));

   function Milliseconds (Value : Microseconds) return String is
     (Microseconds'Image (Value / MILLISECOND_MICROSECONDS));

   function Hex16 (Value : Unsigned_16) return String is
      Digits_Text : constant String := "0123456789abcdef";
      Text : String (1 .. 4);
      Rest : Unsigned_16 := Value;
   begin
      for Index in reverse Text'Range loop
         Text (Index) := Digits_Text (Natural (Rest and 16#F#) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Text;
   end Hex16;

   ---------------------------------------------------------------------------
   --  Completion queue consumption: take the entry at the cursor (posted),
   --  advance the cursor and publish the new head. Every posted entry is
   --  taken exactly once, whatever it names.
   ---------------------------------------------------------------------------
   function adminPosted return Boolean is
     (Is_Posted (adminCursor, Status_Field (adminCq (adminCursor.Head).status)));

   function ioPosted return Boolean is
     (Is_Posted (ioCursor, Status_Field (ioCq (ioCursor.Head).status)));

   procedure takeAdmin (cqe : out CompletionEntry) is
   begin
      --  Read the controller's phase publication BEFORE copying other
      --  fields; the compiler barrier keeps the snapshot after the check.
      --  The controller cannot reuse this entry until our CQ doorbell.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      cqe := adminCq (adminCursor.Head);
      adminCursor := Advance (adminCursor);
      ringCqDoorbell (Admin_Queue, adminCursor.Head);
   end takeAdmin;

   procedure takeIO (cqe : out CompletionEntry) is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      cqe := ioCq (ioCursor.Head);
      ioCursor := Advance (ioCursor);
      ringCqDoorbell (IO_Queue, ioCursor.Head);
   end takeIO;

   ---------------------------------------------------------------------------
   --  submitAdmin - submit one admin command and wait for its completion
   --  (Admin_Command_Deadline). Entries naming another identifier are
   --  consumed and discarded. Result: completion DW0.
   ---------------------------------------------------------------------------
   type Admin_Outcome is (Admin_Completed, Admin_Failed, Admin_Timed_Out);

   procedure submitAdmin
     (opcode  : Unsigned_8;
      cmd     : in out SubmissionEntry;
      outcome : out Admin_Outcome;
      result  : out Unsigned_32)
   is
      cqe     : CompletionEntry;
      ignore  : Unsigned_64;
      started : Unsigned_64;
   begin
      adminCmdId := adminCmdId + 1;
      cmd.cdw0 := makeCdw0 (opcode, adminCmdId);
      adminSq (adminSqTail) := cmd;
      adminSqTail := Next_Tail (adminSqTail);
      ringSqDoorbell (Admin_Queue, adminSqTail);

      result := 0;
      started := CuBit.Busy_Poll.Now;
      loop
         while adminPosted loop
            takeAdmin (cqe);
            if Queue_Identifier (cqe.sqId) = Admin_Queue
              and then Command_Identifier (cqe.cid) = adminCmdId
            then
               result := cqe.dw0;
               outcome := (if Succeeded (Status_Field (cqe.status))
                           then Admin_Completed else Admin_Failed);
               return;
            end if;
            foreignCompletions := foreignCompletions + 1;
            debugPrint ("NVMe: discarded admin completion cid=" &
                        Hex16 (cqe.cid) & ASCII.LF);
         end loop;
         exit when not CuBit.Busy_Poll.Within
           (started, Unsigned_64 (Admin_Command_Deadline));
         ignore := syscall (SYSCALL_SLEEP, POLL_SLEEP_MILLISECONDS);
      end loop;
      outcome := Admin_Timed_Out;
      debugPrint ("NVMe: admin command timed out: opcode=" &
                  Unsigned_8'Image (opcode) & " after" &
                  Milliseconds (Admin_Command_Deadline) & " ms" & ASCII.LF);
   end submitAdmin;

   function submitAdmin (opcode : Unsigned_8; cmd : SubmissionEntry) return Boolean is
      entryCopy : SubmissionEntry := cmd;
      outcome : Admin_Outcome;
      ignore : Unsigned_32;
   begin
      submitAdmin (opcode, entryCopy, outcome, ignore);
      return outcome = Admin_Completed;
   end submitAdmin;

   ---------------------------------------------------------------------------
   --  Controller enable: disable (CC.EN = 0), wait for CSTS.RDY = 0, zero
   --  the queues, program the admin queue, enable, wait for CSTS.RDY = 1.
   --  Disabling ends every command the controller held (3.5.2): afterwards
   --  it no longer writes any data slice.
   ---------------------------------------------------------------------------
   function waitReady (ready : Boolean) return Boolean is
      started : constant Unsigned_64 := CuBit.Busy_Poll.Now;
      ignore  : Unsigned_64;
   begin
      loop
         if ((readReg32 (REG_CSTS) and CSTS_RDY) /= 0) = ready then
            return True;
         end if;
         exit when not CuBit.Busy_Poll.Within (started, readyTimeout);
         ignore := syscall (SYSCALL_SLEEP, POLL_SLEEP_MILLISECONDS);
      end loop;
      return ((readReg32 (REG_CSTS) and CSTS_RDY) /= 0) = ready;
   end waitReady;

   procedure enableController (ok : out Boolean) is
   begin
      writeReg32 (REG_CC, 0);
      if not waitReady (False) then
         debugPrint ("NVMe: controller did not disable" & ASCII.LF);
         ok := False;
         return;
      end if;

      --  Queue and PRP list memory (everything before the data slices).
      declare
         dmaMem : String (1 .. Data_Offset) with Import,
           Address => System.Storage_Elements.To_Address (DMA_VIRT_BASE);
      begin
         dmaMem := [others => Character'Val (0)];
      end;

      --  Admin Queue Attributes: ACQS = ASQS = QUEUE_DEPTH - 1.
      writeReg32 (REG_AQA,
        Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), 16) or
        Unsigned_32 (QUEUE_DEPTH - 1));
      writeReg64 (REG_ASQ, dmaBase + ADMIN_SQ_OFFSET);
      writeReg64 (REG_ACQ, dmaBase + ADMIN_CQ_OFFSET);

      adminSqTail := 0;
      adminCursor := Initial;
      ioSqTail    := 0;
      ioCursor    := Initial;

      --  CC.EN=1, IOSQES=6, IOCQES=4
      writeReg32 (REG_CC, CC_EN or CC_IOSQES or CC_IOCQES);
      ok := waitReady (True);
      if not ok then
         debugPrint ("NVMe: Controller failed to become ready!" & ASCII.LF);
      end if;
   end enableController;

   ---------------------------------------------------------------------------
   --  initController
   ---------------------------------------------------------------------------
   procedure initController
     (registers : CuBit.Handles.Device_Memory; barPhys : Unsigned_64; dmaPhys : Unsigned_64) is
      mapped : Boolean;
      cap    : Unsigned_64;
      dstrd  : Unsigned_32;
      ok     : Boolean;
   begin
      --  1. Map BAR0 MMIO into our address space
      mapped := CuBit.Handles.Devices.Map_Device
        (registers, barPhys, BAR_VIRT_BASE, BAR_MAP_PAGES, Read_Only => False);
      if not mapped then
         debugPrint ("NVMe: register window map refused" & ASCII.LF);
      end if;

      barBase := System.Storage_Elements.To_Address (BAR_VIRT_BASE);
      dmaBase := dmaPhys;

      --  2. CAP: doorbell stride and the ready timeout
      cap := readReg64 (REG_CAP);
      dstrd := Unsigned_32 (Shift_Right (cap, 32) and 16#F#);
      doorbellStride := Shift_Left (Unsigned_32'(4), Natural (dstrd));
      readyTimeout := Ready_Timeout (cap);

      debugPrint ("NVMe: Disabling controller..." & ASCII.LF);
      enableController (ok);
      if ok then
         debugPrint ("NVMe: Controller ready." & ASCII.LF);
      end if;

      adminCmdId := 0;
      commands   := NVMe_Command_Ids.Empty;
      phase      := Serving;
   end initController;

   ---------------------------------------------------------------------------
   --  identifyController
   ---------------------------------------------------------------------------
   procedure identifyController is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      ok  : Boolean;
   begin
      cmd.nsid := 0;
      cmd.prp1 := dmaBase + IDENTIFY_OFFSET;
      cmd.prp2 := 0;
      cmd.cdw10 := CNS_CONTROLLER;

      ok := submitAdmin (ADMIN_IDENTIFY, cmd);

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

      cmd.nsid := 1;
      cmd.prp1 := dmaBase + IDENTIFY_OFFSET;
      cmd.prp2 := 0;
      cmd.cdw10 := CNS_NAMESPACE;

      ok := submitAdmin (ADMIN_IDENTIFY, cmd);

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
   procedure createIOQueues (interrupts : Boolean; ok : out Boolean)
   is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      CQ_PHYSICALLY_CONTIGUOUS : constant Unsigned_32 := 1;
      CQ_INTERRUPTS_ENABLED : constant Unsigned_32 := 2;
      INTERRUPT_VECTOR_SHIFT : constant := 16;
      QUEUE_SIZE_SHIFT : constant := 16;
      COMPLETION_QUEUE_SHIFT : constant := 16;
      SQ_PHYSICALLY_CONTIGUOUS : constant Unsigned_32 := 1;
   begin
      interruptsEnabled := False;
      --  Create I/O Completion Queue (ID=1)
      cmd := NULL_SUBMISSION;
      cmd.prp1  := dmaBase + IO_CQ_OFFSET;
      cmd.cdw10 := Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), QUEUE_SIZE_SHIFT) or
        Unsigned_32 (IO_Queue);
      --  PC=1; with interrupts IEN=1 and IV = the MSI-X entry devmgr
      --  programmed for completions.
      cmd.cdw11 := CQ_PHYSICALLY_CONTIGUOUS or
        (if interrupts then
           CQ_INTERRUPTS_ENABLED or
           Shift_Left (CuBit.NVMe_Control.Completion_Message,
                       INTERRUPT_VECTOR_SHIFT)
         else 0);

      ok := submitAdmin (ADMIN_CREATE_IO_CQ, cmd);
      if not ok then
         debugPrint ("NVMe: Create I/O CQ FAILED." & ASCII.LF);
         return;
      end if;

      --  Create I/O Submission Queue (ID=1, CQ=1)
      cmd := NULL_SUBMISSION;
      cmd.prp1  := dmaBase + IO_SQ_OFFSET;
      cmd.cdw10 := Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), QUEUE_SIZE_SHIFT) or
        Unsigned_32 (IO_Queue);
      cmd.cdw11 := Shift_Left (Unsigned_32 (IO_Queue), COMPLETION_QUEUE_SHIFT) or
        SQ_PHYSICALLY_CONTIGUOUS;

      ok := submitAdmin (ADMIN_CREATE_IO_SQ, cmd);
      if not ok then
         debugPrint ("NVMe: Create I/O SQ FAILED." & ASCII.LF);
         return;
      end if;

      queuesInterrupt := interrupts;
      interruptsEnabled := interrupts;
      debugPrint ("NVMe: I/O queues created." & ASCII.LF);
   end createIOQueues;

   ---------------------------------------------------------------------------
   --  resetController - Linux's last resort: disable and re-enable the
   --  controller and re-create the I/O queues as they were. The controller
   --  then holds no command, so every slot is free again.
   ---------------------------------------------------------------------------
   procedure resetController (ok : out Boolean) is
      csts : constant Unsigned_32 := readReg32 (REG_CSTS);
   begin
      resets := resets + 1;
      debugPrint ("NVMe: resetting controller: csts=" &
                  Hex16 (Unsigned_16 (csts and 16#FFFF#)) &
                  ((if (csts and Fatal_Status) /= 0 then " fatal" else "")) &
                  " resets=" & resets'Image & ASCII.LF);
      enableController (ok);
      if ok then
         createIOQueues (queuesInterrupt, ok);
      end if;
      if ok then
         NVMe_Command_Ids.Reset_All (commands);
         debugPrint ("NVMe: controller reset done; queues re-created" & ASCII.LF);
      else
         failedResets := failedResets + 1;
         debugPrint ("NVMe: controller reset FAILED; offline until the next request" &
                     ASCII.LF);
      end if;
   end resetController;

   ---------------------------------------------------------------------------
   --  waitIO - wait until the I/O CQ's head entry is posted (found) or
   --  limit microseconds have passed since since: spin, then yield, then
   --  sleep until the completion interrupt (or poll at the millisecond).
   ---------------------------------------------------------------------------
   procedure waitIO
     (opcode : Unsigned_8;
      since  : Unsigned_64;
      limit  : Microseconds;
      found  : out Boolean)
   is
      ignore : Unsigned_64;
      started : constant Unsigned_64 := CuBit.Busy_Poll.Now;
      function live return Boolean is
        (CuBit.Busy_Poll.Within (since, Unsigned_64 (limit)));
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
      loop
         if ioPosted then
            found := True;
            exit;
         end if;
         exit when not live;
         exit when interruptsEnabled and then
           not CuBit.Busy_Poll.Within (started, IO_SPIN_MICROSECONDS);
         exit when not CuBit.Busy_Poll.Within (started, IO_YIELD_MICROSECONDS);
         if CuBit.Busy_Poll.Within (started, IO_SPIN_MICROSECONDS) then
            CuBit.Busy_Poll.Relax;
         else
            ignore := syscall (SYSCALL_YIELD);
         end if;
      end loop;
      if not found and then interruptsEnabled then
         --  Sleep until the completion interrupt. The kernel keeps a latched
         --  notification, so one arriving between the check and the wait
         --  is not lost. A request queued meanwhile also ends the wait; then
         --  poll at the millisecond.
         declare
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
               if ioPosted then
                  found := True;
                  exit;
               end if;
               exit when not live;
               if sawEvent then
                  null;   --  a shared vector's other device: look again
               else
                  activity := Wait_For_Activity_Until
                    (syscall (SYSCALL_GETTIME) + IO_WAIT_SLICE_MILLISECONDS);
                  if activity = Deadline_Reached then
                     sliceTimeouts := sliceTimeouts + 1;
                  end if;
                  if activity = Unavailable then
                     ignore := syscall (SYSCALL_SLEEP, POLL_SLEEP_MILLISECONDS);
                  elsif activity = Work_Available and then not ioPosted then
                     --  Pending IPC (not ours to take here) or an event:
                     --  drain events next time; never spin on requests.
                     if not Poll_Event (event) then
                        ignore := syscall (SYSCALL_SLEEP, POLL_SLEEP_MILLISECONDS);
                     end if;
                  end if;
               end if;
            end loop;
         end;
      elsif not found then
#if nvme_io_profile = "on" then
         Slow := True;
#end if;
         loop
            if ioPosted then
               found := True;
               exit;
            end if;
            exit when not live;
            ignore := syscall (SYSCALL_SLEEP, POLL_SLEEP_MILLISECONDS);
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
   end waitIO;

   procedure reportWaits is
   begin
      debugPrint ("NVMe: interrupts=" & interruptsSeen'Image &
                  " slice-timeouts=" & sliceTimeouts'Image &
                  " expired=" & expiredCommands'Image &
                  " aborts=" & abortsSent'Image &
                  " resets=" & resets'Image &
                  " late=" & lateCompletions'Image &
                  " foreign=" & foreignCompletions'Image &
                  " failed-requests=" & failedRequests'Image & ASCII.LF);
   end reportWaits;

   procedure submitIO (cmd : SubmissionEntry) is
   begin
      ioSq (ioSqTail) := cmd;
      ioSqTail := Next_Tail (ioSqTail);
      ringSqDoorbell (IO_Queue, ioSqTail);
   end submitIO;

   function opcodeName (opcode : Unsigned_8) return String is
     (if opcode = IO_READ then "read"
      elsif opcode = IO_WRITE then "write"
      else "flush");

   function outcomeName (outcome : Request_Outcome) return String is
     (case outcome is
        when Completed      => "completed",
        when Device_Error   => "device-error",
        when Timed_Out      => "timed-out",
        when Device_Offline => "offline");

   ---------------------------------------------------------------------------
   --  One request's commands. A transfer is split into chunks, each run on
   --  an in-flight slot with that slot's slice of the DMA data area and its
   --  own PRP list; up to Flight_Slots chunks are outstanding at once: the
   --  controller works on the next while the driver copies the last.
   --  Completions may arrive in any order and are matched by identifier.
   --  A flush is one chunk with no data.
   ---------------------------------------------------------------------------
   --  A transfer is bounded by the grant (the service rejects larger ones).
   MAX_CHUNKS : constant := 256;
   subtype Chunk_Index is Natural range 0 .. MAX_CHUNKS - 1;
   type Chunk_State is (Pending, In_Flight, Done, Failed);

   type Request is record
      Opcode     : Unsigned_8 := IO_READ;
      First      : Unsigned_64 := 0;            --  LBA
      Total      : Unsigned_64 := 0;            --  bytes
      Chunk      : Unsigned_64 := 0;            --  bytes per chunk
      Chunks     : Natural := 0;
      Buffer     : System.Address := System.Null_Address;
      FUA        : Boolean := False;
      Failed     : Boolean := False;            --  a chunk failed by status
   end record;

   active   : Request;
   chunkOf   : array (Chunk_Index) of Chunk_State := [others => Pending];
   slotChunk : array (Flight_Slot) of Chunk_Index := [others => 0];
   issuedAt  : array (Flight_Slot) of Unsigned_64 := [others => 0];

   function slotDmaOffset (slot : Flight_Slot) return Unsigned_64 is
     (Unsigned_64 (Slot_Data_Offset (slot)));

   function chunkBytes (chunk : Chunk_Index) return Unsigned_64 is
     (if active.Opcode = IO_FLUSH then 0
      else Unsigned_64'Min (active.Chunk,
                            active.Total - Unsigned_64 (chunk) * active.Chunk));

   procedure launch (slot : Flight_Slot; chunk : Chunk_Index) is
      sector : constant Unsigned_64 := Unsigned_64 (nsSectorSize);
      offset : constant Unsigned_64 := Unsigned_64 (chunk) * active.Chunk;
      bytes : constant Unsigned_64 := chunkBytes (chunk);
      pages : constant Unsigned_64 := (bytes + PAGE_SIZE - 1) / PAGE_SIZE;
      dmaPhys : constant Unsigned_64 := dmaBase + slotDmaOffset (slot);
      dmaVirt : constant System.Address := System.Storage_Elements.To_Address
        (Integer_Address (DMA_VIRT_BASE + slotDmaOffset (slot)));
      listOffset : constant Unsigned_64 := Unsigned_64 (Slot_PRP_List_Offset (slot));
      cmd : SubmissionEntry := NULL_SUBMISSION;
      cid : Command_Identifier;
   begin
      NVMe_Command_Ids.Issue (commands, slot, cid);
      cmd.cdw0 := makeCdw0 (active.Opcode, cid);
      cmd.nsid := 1;
      if active.Opcode /= IO_FLUSH then
         declare
            sectors : constant Unsigned_32 := Unsigned_32 (bytes / sector);
            chunkLba : constant Unsigned_64 := active.First + offset / sector;
         begin
            if active.Opcode = IO_WRITE then
               declare
                  srcBuf : String (1 .. Natural (bytes))
                    with Import, Address => active.Buffer + Storage_Offset (offset);
                  dstBuf : String (1 .. Natural (bytes)) with Import, Address => dmaVirt;
               begin
                  dstBuf := srcBuf;
               end;
            end if;
            if pages > 2 then
               declare
                  list : array (0 .. Chunk_Pages - 1) of Unsigned_64
                    with Import, Address => System.Storage_Elements.To_Address
                      (Integer_Address (DMA_VIRT_BASE + listOffset));
               begin
                  for i in 1 .. Natural (pages) - 1 loop
                     list (i - 1) := dmaPhys + Unsigned_64 (i) * PAGE_SIZE;
                  end loop;
               end;
            end if;
            cmd.prp1 := dmaPhys;
            cmd.prp2 := (if pages <= 1 then 0
                         elsif pages = 2 then dmaPhys + PAGE_SIZE
                         else dmaBase + listOffset);
            cmd.cdw10 := Unsigned_32 (chunkLba and 16#FFFF_FFFF#);
            cmd.cdw11 := Unsigned_32 (Shift_Right (chunkLba, 32));
            cmd.cdw12 := (sectors - 1) or
              (if active.Opcode = IO_WRITE and then active.FUA then CDW12_FUA else 0);
         end;
      end if;
      slotChunk (slot) := chunk;
      chunkOf (chunk) := In_Flight;
      issuedAt (slot) := CuBit.Busy_Poll.Now;
      submitIO (cmd);
   end launch;

   function chunkLbaImage (chunk : Chunk_Index) return String is
     (if active.Opcode = IO_FLUSH then ""
      else " lba=" & Unsigned_64'Image
             (active.First + Unsigned_64 (chunk) * active.Chunk /
                Unsigned_64 (nsSectorSize)) &
           " bytes=" & Unsigned_64'Image (chunkBytes (chunk)));

   --  Consumes one posted I/O completion and settles what it names.
   --  recovering: an aborted status is our Abort (the chunk is re-issued);
   --  otherwise it is a device error like any other.
   procedure settle (recovering : Boolean) is
      cqe : CompletionEntry;
      found : NVMe_Command_Ids.Match;
      status : Status_Field;
   begin
      takeIO (cqe);
      status := Status_Field (cqe.status);
      if Queue_Identifier (cqe.sqId) /= IO_Queue then
         found := No_Match;
      else
         NVMe_Command_Ids.Retire (commands, Command_Identifier (cqe.cid), found);
      end if;
      case found.Kind is
         when Foreign =>
            foreignCompletions := foreignCompletions + 1;
            debugPrint ("NVMe: discarded foreign completion: sq=" & Hex16 (cqe.sqId) &
                        " cid=" & Hex16 (cqe.cid) &
                        " status=" & Hex16 (cqe.status) & ASCII.LF);
         when Current | Late =>
            declare
               slot : constant Flight_Slot := found.Slot;
               chunk : constant Chunk_Index := slotChunk (slot);
            begin
               if found.Kind = Late then
                  lateCompletions := lateCompletions + 1;
                  debugPrint ("NVMe: late completion: " & opcodeName (active.Opcode) &
                              chunkLbaImage (chunk) & " cid=" & Hex16 (cqe.cid) &
                              " status=" & Hex16 (cqe.status) & ASCII.LF);
               end if;
               if Succeeded (status) then
                  if active.Opcode = IO_READ then
                     declare
                        bytes : constant Natural := Natural (chunkBytes (chunk));
                        srcBuf : String (1 .. bytes) with Import,
                          Address => System.Storage_Elements.To_Address
                            (Integer_Address (DMA_VIRT_BASE + slotDmaOffset (slot)));
                        dstBuf : String (1 .. bytes) with Import,
                          Address => active.Buffer +
                            Storage_Offset (Unsigned_64 (chunk) * active.Chunk);
                     begin
                        dstBuf := srcBuf;
                     end;
                  end if;
                  chunkOf (chunk) := Done;
               elsif recovering and then Was_Aborted (status) then
                  chunkOf (chunk) := Pending;
               else
                  chunkOf (chunk) := Failed;
                  active.Failed := True;
                  debugPrint ("NVMe: " & opcodeName (active.Opcode) & " error:" &
                              chunkLbaImage (chunk) &
                              " status=" & Hex16 (cqe.status) & ASCII.LF);
               end if;
            end;
      end case;
   end settle;

   --  After a reset (or with the controller offline) a chunk whose slot is
   --  no longer outstanding goes back to Pending.
   procedure requeueLost is
   begin
      for chunk in 0 .. active.Chunks - 1 loop
         if chunkOf (chunk) = In_Flight then
            declare
               held : Boolean := False;
            begin
               for slot in Flight_Slot loop
                  held := held or else
                    (NVMe_Command_Ids.Outstanding (commands, slot)
                     and then slotChunk (slot) = chunk);
               end loop;
               if not held then
                  chunkOf (chunk) := Pending;
               end if;
            end;
         end if;
      end loop;
   end requeueLost;

   --  Moves phase along an expected event.
   procedure step (event : Recovery_Event) is
   begin
      phase := Next (phase, event);
   end step;

   ---------------------------------------------------------------------------
   --  recover - some commands are Expired. Abort each; give the outstanding
   --  ones Abort_Settle_Deadline to complete; if any is still out, reset.
   ---------------------------------------------------------------------------
   procedure recover is
      abortsOk : Boolean := True;
      started : Unsigned_64;
      posted : Boolean;
      resetOk : Boolean;
   begin
      step (Commands_Expired);
      for slot in Flight_Slot loop
         if commands (slot).State = Expired then
            declare
               cmd : SubmissionEntry := NULL_SUBMISSION;
               outcome : Admin_Outcome;
               result : Unsigned_32;
            begin
               cmd.cdw10 := Abort_Dword_10 (IO_Queue, Identifier (commands, slot));
               abortsSent := abortsSent + 1;
               submitAdmin (Admin_Abort, cmd, outcome, result);
               if outcome /= Admin_Completed then
                  --  The admin queue itself is stuck: only a reset helps.
                  abortsOk := False;
                  exit;
               end if;
               if Abort_Performed (result) then
                  abortsPerformed := abortsPerformed + 1;
               end if;
               debugPrint ("NVMe: abort cid=" &
                           Hex16 (Unsigned_16 (Identifier (commands, slot))) &
                           (if Abort_Performed (result) then " performed"
                            else " not performed") & ASCII.LF);
            end;
         end if;
      end loop;
      if abortsOk then
         started := CuBit.Busy_Poll.Now;
         while not All_Free (commands) loop
            waitIO (active.Opcode, started, Abort_Settle_Deadline, posted);
            exit when not posted;
            settle (recovering => True);
         end loop;
      end if;
      if abortsOk and then All_Free (commands) then
         step (Abort_Settled);
         debugPrint ("NVMe: recovered without a reset" & ASCII.LF);
         return;
      end if;
      step (Abort_Unsettled);
      resetController (resetOk);
      step (if resetOk then Reset_Succeeded else Reset_Failed);
      if not resetOk then
         --  The controller may still own every outstanding slot's slice:
         --  none is reused until a later reset succeeds.
         for slot in Flight_Slot loop
            if commands (slot).State = Busy then
               NVMe_Command_Ids.Expire (commands, slot);
            end if;
         end loop;
      end if;
      requeueLost;
   end recover;

   --  Before a request: an Offline controller gets one reset attempt.
   function ensureServing return Boolean is
      resetOk : Boolean;
   begin
      step (Request_Arrived);
      if phase = Resetting then
         resetController (resetOk);
         step (if resetOk then Reset_Succeeded else Reset_Failed);
      end if;
      return phase = Serving;
   end ensureServing;

   --  One pass: launch every Pending chunk (until one fails) and reap them
   --  all, or recover once a command outlives IO_Command_Deadline.
   procedure runPass (result : out Pass_Result) is
      nextPending : Natural := 0;
      posted : Boolean;
      oldest : Unsigned_64;
      anyBusy : Boolean;
   begin
      loop
         --  Launch into free slots, lowest pending chunk first.
         if not active.Failed then
            for slot in Flight_Slot loop
               if commands (slot).State = Free then
                  while nextPending < active.Chunks
                    and then chunkOf (nextPending) /= Pending
                  loop
                     nextPending := nextPending + 1;
                  end loop;
                  exit when nextPending >= active.Chunks;
                  launch (slot, nextPending);
               end if;
            end loop;
         end if;
         if All_Free (commands) then
            result := Pass_Complete;
            return;
         end if;
         --  The earliest deadline among the commands in flight.
         anyBusy := False;
         oldest := Unsigned_64'Last;
         for slot in Flight_Slot loop
            if commands (slot).State = Busy then
               anyBusy := True;
               oldest := Unsigned_64'Min (oldest, issuedAt (slot));
            end if;
         end loop;
         if not anyBusy then
            --  Only Expired slots remain (cannot happen while Serving).
            recover;
            result := (if phase = Serving then Pass_Recovered else Pass_Offline);
            return;
         end if;
         waitIO (active.Opcode, oldest, IO_Command_Deadline, posted);
         if posted then
            settle (recovering => False);
         else
            for slot in Flight_Slot loop
               if commands (slot).State = Busy
                 and then not CuBit.Busy_Poll.Within
                   (issuedAt (slot), Unsigned_64 (IO_Command_Deadline))
               then
                  NVMe_Command_Ids.Expire (commands, slot);
                  expiredCommands := expiredCommands + 1;
                  debugPrint ("NVMe: command timed out: " &
                              opcodeName (active.Opcode) &
                              chunkLbaImage (slotChunk (slot)) &
                              " cid=" & Hex16 (Unsigned_16 (Identifier (commands, slot))) &
                              " after" & Milliseconds (IO_Command_Deadline) & " ms" &
                              ASCII.LF);
               end if;
            end loop;
            recover;
            result := (if phase = Serving then Pass_Recovered else Pass_Offline);
            return;
         end if;
      end loop;
   end runPass;

   ---------------------------------------------------------------------------
   --  execute - run one request to its typed outcome. moved: the bytes of
   --  the longest prefix whose chunks all completed successfully.
   ---------------------------------------------------------------------------
   procedure execute
     (opcode  : Unsigned_8;
      lba     : Unsigned_64;
      count   : Unsigned_32;
      buf     : System.Address;
      fua     : Boolean;
      moved   : out Unsigned_64;
      outcome : out Request_Outcome)
   is
      sector : constant Unsigned_64 := Unsigned_64 (nsSectorSize);
      chunkLimit : constant Unsigned_64 :=
        (if sector = 0 then 0
         else Unsigned_64'Min (Chunk_Bytes, maxTransferBytes) / sector * sector);
      pass : Attempt := Attempt'First;
      result : Pass_Result;
      decision : Verdict;
      prefix : Natural := 0;
   begin
      moved := 0;
      outcome := Device_Error;
      if nsBlockCount = 0 or else
        (opcode /= IO_FLUSH and then (chunkLimit = 0 or else count = 0))
      then
         return;
      end if;
      active := (Opcode => opcode, First => lba,
                  Total  => Unsigned_64 (count) * sector,
                  Chunk  => chunkLimit, Chunks => 1,
                  Buffer => buf, FUA => fua, Failed => False);
      if opcode /= IO_FLUSH then
         if (active.Total + chunkLimit - 1) / chunkLimit > MAX_CHUNKS then
            return;   --  larger than any grant this driver accepts
         end if;
         active.Chunks := Natural ((active.Total + chunkLimit - 1) / chunkLimit);
      end if;
      chunkOf := [others => Pending];
      if not ensureServing then
         outcome := Device_Offline;
      else
         loop
            runPass (result);
            decision := Decide (pass, result, active.Failed);
            if decision.Final then
               outcome := decision.Outcome;
               exit;
            end if;
            pass := decision.Next_Attempt;
            debugPrint ("NVMe: re-issuing " & opcodeName (opcode) &
                        " lba=" & lba'Image & " attempt=" & pass'Image & ASCII.LF);
         end loop;
      end if;
      while prefix < active.Chunks and then chunkOf (prefix) = Done loop
         prefix := prefix + 1;
      end loop;
      if opcode /= IO_FLUSH then
         moved := Unsigned_64'Min (active.Total, Unsigned_64 (prefix) * chunkLimit);
      end if;
      if outcome /= Completed then
         failedRequests := failedRequests + 1;
         debugPrint ("NVMe: " & opcodeName (opcode) & " failed: lba=" & lba'Image &
                     " blocks=" & count'Image &
                     " outcome=" & outcomeName (outcome) &
                     " attempts=" & pass'Image &
                     " deadline-ms=" & Milliseconds (IO_Command_Deadline) &
                     " moved=" & moved'Image & ASCII.LF);
         reportWaits;
      end if;
   end execute;

   function flush return Boolean is
      moved : Unsigned_64;
      outcome : Request_Outcome;
   begin
      execute (IO_FLUSH, 0, 0, System.Null_Address, False, moved, outcome);
      return outcome = Completed;
   end flush;

   function readBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64
   is
      moved : Unsigned_64;
      outcome : Request_Outcome;
   begin
      execute (IO_READ, lba, count, buf, False, moved, outcome);
      if outcome /= Completed then
         debugPrint ("NVMe: Read failed." & ASCII.LF);
      end if;
      return moved;
   end readBlocks;

   function writeBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address;
      fua   : Boolean := False) return Unsigned_64
   is
      moved : Unsigned_64;
      outcome : Request_Outcome;
   begin
      execute (IO_WRITE, lba, count, buf, fua, moved, outcome);
      if outcome /= Completed then
         debugPrint ("NVMe: Write failed." & ASCII.LF);
      end if;
      return moved;
   end writeBlocks;

end NVMe;
