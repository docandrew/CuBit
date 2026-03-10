------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  NVMe controller register definitions and queue entry types.
--
--  Reference: NVM Express Base Specification, Revision 2.0
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package NVMe is

   ---------------------------------------------------------------------------
   --  Controller Register Offsets (within BAR0 MMIO)
   ---------------------------------------------------------------------------
   REG_CAP   : constant := 16#00#;  --  Controller Capabilities (8 bytes)
   REG_VS    : constant := 16#08#;  --  Version (4 bytes)
   REG_CC    : constant := 16#14#;  --  Controller Configuration (4 bytes)
   REG_CSTS  : constant := 16#1C#;  --  Controller Status (4 bytes)
   REG_AQA   : constant := 16#24#;  --  Admin Queue Attributes (4 bytes)
   REG_ASQ   : constant := 16#28#;  --  Admin SQ Base Address (8 bytes)
   REG_ACQ   : constant := 16#30#;  --  Admin CQ Base Address (8 bytes)

   --  Doorbells start at offset 0x1000; stride from CAP.DSTRD
   DOORBELL_BASE : constant := 16#1000#;

   ---------------------------------------------------------------------------
   --  Controller Configuration (CC) bits
   ---------------------------------------------------------------------------
   CC_EN     : constant Unsigned_32 := 16#0000_0001#;  --  Enable
   --  I/O SQ entry size = 6 (64 bytes = 2^6)
   CC_IOSQES : constant Unsigned_32 := Shift_Left (6, 16);
   --  I/O CQ entry size = 4 (16 bytes = 2^4)
   CC_IOCQES : constant Unsigned_32 := Shift_Left (4, 20);

   ---------------------------------------------------------------------------
   --  Controller Status (CSTS) bits
   ---------------------------------------------------------------------------
   CSTS_RDY  : constant Unsigned_32 := 16#0000_0001#;

   ---------------------------------------------------------------------------
   --  Admin Command Opcodes
   ---------------------------------------------------------------------------
   ADMIN_IDENTIFY      : constant Unsigned_8 := 16#06#;
   ADMIN_CREATE_IO_CQ  : constant Unsigned_8 := 16#05#;
   ADMIN_CREATE_IO_SQ  : constant Unsigned_8 := 16#01#;

   ---------------------------------------------------------------------------
   --  I/O Command Opcodes
   ---------------------------------------------------------------------------
   IO_READ  : constant Unsigned_8 := 16#02#;
   IO_WRITE : constant Unsigned_8 := 16#01#;

   ---------------------------------------------------------------------------
   --  Identify CNS values
   ---------------------------------------------------------------------------
   CNS_NAMESPACE  : constant Unsigned_32 := 0;
   CNS_CONTROLLER : constant Unsigned_32 := 1;

   ---------------------------------------------------------------------------
   --  Queue Depth
   ---------------------------------------------------------------------------
   QUEUE_DEPTH : constant := 16;

   ---------------------------------------------------------------------------
   --  Submission Queue Entry (64 bytes)
   ---------------------------------------------------------------------------
   type SubmissionEntry is record
      cdw0  : Unsigned_32;  --  opcode(7:0), fuse(9:8), psdt(15:14), cid(31:16)
      nsid  : Unsigned_32;  --  Namespace ID
      cdw2  : Unsigned_32;  --  Reserved
      cdw3  : Unsigned_32;  --  Reserved
      mptr  : Unsigned_64;  --  Metadata Pointer
      prp1  : Unsigned_64;  --  PRP Entry 1
      prp2  : Unsigned_64;  --  PRP Entry 2
      cdw10 : Unsigned_32;
      cdw11 : Unsigned_32;
      cdw12 : Unsigned_32;
      cdw13 : Unsigned_32;
      cdw14 : Unsigned_32;
      cdw15 : Unsigned_32;
   end record with Convention => C, Size => 512;

   for SubmissionEntry use record
      cdw0  at  0 range 0 .. 31;
      nsid  at  4 range 0 .. 31;
      cdw2  at  8 range 0 .. 31;
      cdw3  at 12 range 0 .. 31;
      mptr  at 16 range 0 .. 63;
      prp1  at 24 range 0 .. 63;
      prp2  at 32 range 0 .. 63;
      cdw10 at 40 range 0 .. 31;
      cdw11 at 44 range 0 .. 31;
      cdw12 at 48 range 0 .. 31;
      cdw13 at 52 range 0 .. 31;
      cdw14 at 56 range 0 .. 31;
      cdw15 at 60 range 0 .. 31;
   end record;

   NULL_SUBMISSION : constant SubmissionEntry :=
     (cdw0 => 0, nsid => 0, cdw2 => 0, cdw3 => 0,
      mptr => 0, prp1 => 0, prp2 => 0,
      cdw10 => 0, cdw11 => 0, cdw12 => 0,
      cdw13 => 0, cdw14 => 0, cdw15 => 0);

   ---------------------------------------------------------------------------
   --  Completion Queue Entry (16 bytes)
   ---------------------------------------------------------------------------
   type CompletionEntry is record
      dw0    : Unsigned_32;  --  Command-specific
      dw1    : Unsigned_32;  --  Reserved
      sqHead : Unsigned_16;  --  SQ Head Pointer
      sqId   : Unsigned_16;  --  SQ Identifier
      cid    : Unsigned_16;  --  Command Identifier
      status : Unsigned_16;  --  Status Field (phase bit = bit 0)
   end record with Convention => C, Size => 128;

   for CompletionEntry use record
      dw0    at  0 range 0 .. 31;
      dw1    at  4 range 0 .. 31;
      sqHead at  8 range 0 .. 15;
      sqId   at 10 range 0 .. 15;
      cid    at 12 range 0 .. 15;
      status at 14 range 0 .. 15;
   end record;

   ---------------------------------------------------------------------------
   --  Queue Arrays
   ---------------------------------------------------------------------------
   type SQArray is array (Natural range 0 .. QUEUE_DEPTH - 1) of SubmissionEntry
     with Convention => C;

   type CQArray is array (Natural range 0 .. QUEUE_DEPTH - 1) of CompletionEntry
     with Convention => C;

   ---------------------------------------------------------------------------
   --  DMA Layout offsets (within 1 MiB at DMA_VIRT_BASE = 0x7000_0000_0000)
   --  Page 0:      Admin SQ (16 x 64B = 1024B)
   --  Page 1:      Admin CQ (16 x 16B = 256B)
   --  Page 2:      I/O SQ (16 x 64B = 1024B)
   --  Page 3:      I/O CQ (16 x 16B = 256B)
   --  Page 4:      Identify/scratch buffer (4KB)
   --  Page 5:      PRP list (512 x 8B entries = 4KB)
   --  Pages 6-255: Data buffers (250 x 4KB = 1000KB)
   ---------------------------------------------------------------------------
   DMA_VIRT_BASE     : constant := 16#0000_7000_0000_0000#;
   PAGE_SIZE         : constant := 4096;

   ADMIN_SQ_OFFSET   : constant := 0 * PAGE_SIZE;
   ADMIN_CQ_OFFSET   : constant := 1 * PAGE_SIZE;
   IO_SQ_OFFSET      : constant := 2 * PAGE_SIZE;
   IO_CQ_OFFSET      : constant := 3 * PAGE_SIZE;
   IDENTIFY_OFFSET   : constant := 4 * PAGE_SIZE;
   PRP_LIST_OFFSET   : constant := 5 * PAGE_SIZE;
   DATA_BUF_OFFSET   : constant := 6 * PAGE_SIZE;
   DATA_BUF_PAGES    : constant := 250;

   --  Number of PRP entries that fit in one page (512 for 4KB pages)
   PRP_ENTRIES_PER_PAGE : constant := PAGE_SIZE / 8;

   ---------------------------------------------------------------------------
   --  BAR0 mapping virtual address
   ---------------------------------------------------------------------------
   BAR_VIRT_BASE     : constant := 16#0000_6000_0000_0000#;
   BAR_MAP_PAGES     : constant := 4;  --  16KB

   ---------------------------------------------------------------------------
   --  Controller state
   ---------------------------------------------------------------------------

   --  Initialize the NVMe controller.
   --  barPhys = BAR0 physical address (for SYSCALL_MAP_DEVICE)
   --  dmaPhys = DMA region physical address (for queue base addresses)
   procedure initController (barPhys : Unsigned_64; dmaPhys : Unsigned_64);

   --  Identify Controller (admin opcode 0x06, CNS=1)
   procedure identifyController;

   --  Identify Namespace 1 (admin opcode 0x06, CNS=0)
   --  Sets nsBlockCount and nsSectorSize.
   procedure identifyNamespace;

   --  Create I/O submission and completion queues
   procedure createIOQueues;

   --  Read sectors from namespace 1 via I/O queue.
   --  lba    = starting logical block address
   --  count  = number of sectors (each nsSectorSize bytes)
   --  buf    = destination address in driver's address space
   --  Returns number of bytes read, or 0 on error.
   function readBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64;

   --  Write sectors to namespace 1 via I/O queue.
   --  lba    = starting logical block address
   --  count  = number of sectors (each nsSectorSize bytes)
   --  buf    = source address in driver's address space
   --  Returns number of bytes written, or 0 on error.
   function writeBlocks
     (lba   : Unsigned_64;
      count : Unsigned_32;
      buf   : System.Address) return Unsigned_64;

   --  Namespace info (populated after identifyNamespace)
   nsBlockCount : Unsigned_64 := 0;
   nsSectorSize : Unsigned_32 := 512;

   --  Max transfer size in bytes (set from MDTS or clamped to buffer)
   maxTransferBytes : Unsigned_64 := 0;

end NVMe;
