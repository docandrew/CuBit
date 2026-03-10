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

with CuBit.Messages; use CuBit.Messages;

package body NVMe is

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

   ioCq : CQArray with Import,
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
      for attempt in 1 .. 1_000_000 loop
         cqe := adminCq (adminCqHead);
         if (cqe.status and 1) = adminPhase then
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
      for attempt in 1 .. 100_000 loop
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
      for attempt in 1 .. 100_000 loop
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
   procedure createIOQueues is
      cmd : SubmissionEntry := NULL_SUBMISSION;
      ok  : Boolean;
   begin
      --  Create I/O Completion Queue (ID=1)
      adminCmdId := adminCmdId + 1;
      cmd := NULL_SUBMISSION;
      cmd.cdw0  := makeCdw0 (ADMIN_CREATE_IO_CQ, adminCmdId);
      cmd.prp1  := dmaBase + IO_CQ_OFFSET;
      cmd.cdw10 := Shift_Left (Unsigned_32 (QUEUE_DEPTH - 1), 16) or 1;  -- size | QID=1
      cmd.cdw11 := 1;  -- Physically contiguous, interrupts enabled

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

      debugPrint ("NVMe: I/O queues created." & ASCII.LF);
   end createIOQueues;

   ---------------------------------------------------------------------------
   --  PRP list overlay (page 5 of DMA region)
   ---------------------------------------------------------------------------
   type PRPArray is array (Natural range 0 .. PRP_ENTRIES_PER_PAGE - 1)
     of Unsigned_64 with Convention => C;

   prpList : PRPArray with Import,
     Address => System.Storage_Elements.To_Address
       (DMA_VIRT_BASE + PRP_LIST_OFFSET);

   ---------------------------------------------------------------------------
   --  pollIOCompletion - spin-poll then sleep-poll the I/O CQ
   --  Returns True on success, False on timeout or error.
   ---------------------------------------------------------------------------
   function pollIOCompletion return Boolean is
      cqe   : CompletionEntry;
      found : Boolean := False;
      ignore : Unsigned_64;
   begin
      --  Spin-poll: NVMe typically completes in microseconds
      for spin in 1 .. 65536 loop
         cqe := ioCq (ioCqHead);
         if (cqe.status and 1) = ioPhase then
            found := True;
            exit;
         end if;
      end loop;

      --  Fallback: sleep-poll for slow completions
      if not found then
         for attempt in 1 .. 1_000_000 loop
            cqe := ioCq (ioCqHead);
            if (cqe.status and 1) = ioPhase then
               found := True;
               exit;
            end if;
            ignore := syscall (SYSCALL_SLEEP, 1);
         end loop;
      end if;

      if not found then
         return False;
      end if;

      ioCqHead := (ioCqHead + 1) mod QUEUE_DEPTH;
      if ioCqHead = 0 then
         ioPhase := ioPhase xor 1;
      end if;
      ringCqDoorbell (1, ioCqHead);

      --  Check status (bits 15:1 = status code)
      return (Shift_Right (cqe.status, 1) and 16#7FFF#) = 0;
   end pollIOCompletion;

   ---------------------------------------------------------------------------
   --  readBlocks - multi-page reads via PRP lists
   ---------------------------------------------------------------------------
   function readBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64
   is
      remaining  : Unsigned_32 := count;
      curLba     : Unsigned_64 := lba;
      dstOff     : Storage_Offset := 0;
      bytesRead  : Unsigned_64 := 0;
      thisBytes  : Unsigned_64;
      thisSectors : Unsigned_32;
      numPages   : Unsigned_64;
      cmd        : SubmissionEntry;
      dataBufPhys : constant Unsigned_64 := dmaBase + DATA_BUF_OFFSET;
      dataBufVirt : constant System.Address :=
        System.Storage_Elements.To_Address
          (Integer_Address (DMA_VIRT_BASE + DATA_BUF_OFFSET));
   begin
      if nsSectorSize = 0 or maxTransferBytes = 0 then
         return 0;
      end if;

      while remaining > 0 loop
         --  Calculate how many bytes/sectors this transfer
         thisBytes := Unsigned_64 (remaining) * Unsigned_64 (nsSectorSize);
         if thisBytes > maxTransferBytes then
            thisBytes := maxTransferBytes;
         end if;
         thisSectors := Unsigned_32 (thisBytes / Unsigned_64 (nsSectorSize));
         numPages := (thisBytes + Unsigned_64 (PAGE_SIZE) - 1) /
                     Unsigned_64 (PAGE_SIZE);

         --  Build PRP list for transfers > 2 pages
         if numPages > 2 then
            for i in 1 .. Natural (numPages) - 1 loop
               prpList (i - 1) :=
                 dataBufPhys + Unsigned_64 (i) * Unsigned_64 (PAGE_SIZE);
            end loop;
         end if;

         --  Build NVMe Read command
         cmd := NULL_SUBMISSION;
         ioCmdId := ioCmdId + 1;
         cmd.cdw0  := makeCdw0 (IO_READ, ioCmdId);
         cmd.nsid  := 1;
         cmd.prp1  := dataBufPhys;

         if numPages <= 1 then
            cmd.prp2 := 0;
         elsif numPages = 2 then
            cmd.prp2 := dataBufPhys + Unsigned_64 (PAGE_SIZE);
         else
            cmd.prp2 := dmaBase + PRP_LIST_OFFSET;
         end if;

         cmd.cdw10 := Unsigned_32 (curLba and 16#FFFF_FFFF#);
         cmd.cdw11 := Unsigned_32 (Shift_Right (curLba, 32));
         cmd.cdw12 := thisSectors - 1;  --  0-based count

         --  Submit to I/O SQ
         ioSq (ioSqTail) := cmd;
         ioSqTail := (ioSqTail + 1) mod QUEUE_DEPTH;
         ringSqDoorbell (1, ioSqTail);

         if not pollIOCompletion then
            debugPrint ("NVMe: Read failed." & ASCII.LF);
            return bytesRead;
         end if;

         --  Copy from DMA buffer to caller's buffer
         declare
            copyLen : constant Unsigned_64 :=
              Unsigned_64 (thisSectors) * Unsigned_64 (nsSectorSize);
            srcBuf : String (1 .. Natural (copyLen))
              with Import, Address => dataBufVirt;
            dstBuf : String (1 .. Natural (copyLen))
              with Import, Address => buf + dstOff;
         begin
            dstBuf := srcBuf;
            bytesRead := bytesRead + copyLen;
            dstOff := dstOff + Storage_Offset (copyLen);
         end;

         curLba    := curLba + Unsigned_64 (thisSectors);
         remaining := remaining - thisSectors;
      end loop;

      return bytesRead;
   end readBlocks;

   ---------------------------------------------------------------------------
   --  writeBlocks - multi-page writes via PRP lists
   ---------------------------------------------------------------------------
   function writeBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64
   is
      remaining    : Unsigned_32 := count;
      curLba       : Unsigned_64 := lba;
      srcOff       : Storage_Offset := 0;
      bytesWritten : Unsigned_64 := 0;
      thisBytes    : Unsigned_64;
      thisSectors  : Unsigned_32;
      numPages     : Unsigned_64;
      cmd          : SubmissionEntry;
      dataBufPhys  : constant Unsigned_64 := dmaBase + DATA_BUF_OFFSET;
      dataBufVirt  : constant System.Address :=
        System.Storage_Elements.To_Address
          (Integer_Address (DMA_VIRT_BASE + DATA_BUF_OFFSET));
   begin
      if nsSectorSize = 0 or maxTransferBytes = 0 then
         return 0;
      end if;

      while remaining > 0 loop
         --  Calculate how many bytes/sectors this transfer
         thisBytes := Unsigned_64 (remaining) * Unsigned_64 (nsSectorSize);
         if thisBytes > maxTransferBytes then
            thisBytes := maxTransferBytes;
         end if;
         thisSectors := Unsigned_32 (thisBytes / Unsigned_64 (nsSectorSize));
         numPages := (thisBytes + Unsigned_64 (PAGE_SIZE) - 1) /
                     Unsigned_64 (PAGE_SIZE);

         --  Copy source data into DMA buffer
         declare
            copyLen : constant Unsigned_64 :=
              Unsigned_64 (thisSectors) * Unsigned_64 (nsSectorSize);
            srcBuf : String (1 .. Natural (copyLen))
              with Import, Address => buf + srcOff;
            dstBuf : String (1 .. Natural (copyLen))
              with Import, Address => dataBufVirt;
         begin
            dstBuf := srcBuf;
         end;

         --  Build PRP list for transfers > 2 pages
         if numPages > 2 then
            for i in 1 .. Natural (numPages) - 1 loop
               prpList (i - 1) :=
                 dataBufPhys + Unsigned_64 (i) * Unsigned_64 (PAGE_SIZE);
            end loop;
         end if;

         --  Build NVMe Write command
         cmd := NULL_SUBMISSION;
         ioCmdId := ioCmdId + 1;
         cmd.cdw0  := makeCdw0 (IO_WRITE, ioCmdId);
         cmd.nsid  := 1;
         cmd.prp1  := dataBufPhys;

         if numPages <= 1 then
            cmd.prp2 := 0;
         elsif numPages = 2 then
            cmd.prp2 := dataBufPhys + Unsigned_64 (PAGE_SIZE);
         else
            cmd.prp2 := dmaBase + PRP_LIST_OFFSET;
         end if;

         cmd.cdw10 := Unsigned_32 (curLba and 16#FFFF_FFFF#);
         cmd.cdw11 := Unsigned_32 (Shift_Right (curLba, 32));
         cmd.cdw12 := thisSectors - 1;  --  0-based count

         --  Submit to I/O SQ
         ioSq (ioSqTail) := cmd;
         ioSqTail := (ioSqTail + 1) mod QUEUE_DEPTH;
         ringSqDoorbell (1, ioSqTail);

         if not pollIOCompletion then
            debugPrint ("NVMe: Write failed." & ASCII.LF);
            return bytesWritten;
         end if;

         bytesWritten := bytesWritten + thisBytes;
         srcOff := srcOff + Storage_Offset (thisBytes);
         curLba    := curLba + Unsigned_64 (thisSectors);
         remaining := remaining - thisSectors;
      end loop;

      return bytesWritten;
   end writeBlocks;

end NVMe;
