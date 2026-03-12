------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace NVMe block device driver.
--
--  Maps the NVMe controller BAR0 MMIO via SYSCALL_MAP_DEVICE, initializes
--  the controller and I/O queues, then enters an IPC server loop handling
--  OP_READ_BLOCK / OP_IDENTIFY from clients (e.g., filesystem server).
--
--  DMA region is pre-mapped by the kernel at 0x7000_0000_0000 (1 MiB).
--  BAR0 is mapped at 0x6000_0000_0000 (16KB) at startup.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with NVMe;

procedure main is
   use ASCII;

   --  IPC operation labels (must match kernel/src/ipc_labels.ads)
   OP_READ_BLOCK  : constant Unsigned_32 := 16#0210#;
   OP_WRITE_BLOCK : constant Unsigned_32 := 16#0211#;
   OP_IDENTIFY    : constant Unsigned_32 := 16#0212#;
   REPLY_OK       : constant Unsigned_32 := 16#F000#;
   REPLY_ERR      : constant Unsigned_32 := 16#F001#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;  --  1 MiB

   --  Conversion helper
   function toAddr is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   ---------------------------------------------------------------------------
   --  sendReply - send a reply message
   ---------------------------------------------------------------------------
   procedure sendReply
     (dest   : ProcessID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   ---------------------------------------------------------------------------
   --  handleReadBlock
   --  words(0) = LBA (sector number)
   --  words(1) = grant_id (buffer to write data into)
   --  words(2) = sector_count (number of sectors to read)
   ---------------------------------------------------------------------------
   procedure handleReadBlock (sender : ProcessID; msg : Message) is
      lba       : constant Unsigned_64 := msg.words (0);
      grantId   : constant Unsigned_64 := msg.words (1);
      sectorCt  : constant Unsigned_32 := Unsigned_32 (msg.words (2) and 16#FFFF#);
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      bytesRead : Unsigned_64;
   begin
      if sectorCt = 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      bytesRead := NVMe.readBlocks (lba, sectorCt, toAddr (grantAddr));
      sendReply (sender, REPLY_OK, bytesRead);
   end handleReadBlock;

   ---------------------------------------------------------------------------
   --  handleWriteBlock
   --  words(0) = LBA (sector number)
   --  words(1) = grant_id (buffer to read data from)
   --  words(2) = sector_count (number of sectors to write)
   ---------------------------------------------------------------------------
   procedure handleWriteBlock (sender : ProcessID; msg : Message) is
      lba          : constant Unsigned_64 := msg.words (0);
      grantId      : constant Unsigned_64 := msg.words (1);
      sectorCt     : constant Unsigned_32 :=
        Unsigned_32 (msg.words (2) and 16#FFFF#);
      grantAddr    : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      bytesWritten : Unsigned_64;
   begin
      if sectorCt = 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      bytesWritten := NVMe.writeBlocks (lba, sectorCt, toAddr (grantAddr));
      sendReply (sender, REPLY_OK, bytesWritten);
   end handleWriteBlock;

   ---------------------------------------------------------------------------
   --  handleIdentify
   --  Reply with disk size in sectors
   ---------------------------------------------------------------------------
   procedure handleIdentify (sender : ProcessID) is
   begin
      if NVMe.nsBlockCount > 0 then
         sendReply (sender, REPLY_OK, NVMe.nsBlockCount);
      else
         sendReply (sender, REPLY_ERR, 0);
      end if;
   end handleIdentify;

   --  Main message loop variables
   sender   : ProcessID;
   msg      : Message;
   bar0Phys : Unsigned_64;
   dmaPhys  : Unsigned_64;
begin
   debugPrint ("NVMe Driver: Starting..." & LF);

   --  1. Query sysinfo for NVMe BAR0 and DMA physical addresses
   bar0Phys := getInfo (SYSINFO_NVME_BAR0);
   dmaPhys  := getInfo (SYSINFO_NVME_DMA_PHYS);

   if bar0Phys = 0 or bar0Phys = Unsigned_64'Last then
      debugPrint ("NVMe Driver: No BAR0 in sysinfo, exiting." & LF);
      --  Signal devmgr that no hardware is present
      declare
         CAP_SLOT_READY  : constant Unsigned_64 := 15;
         OP_NOT_PRESENT  : constant Unsigned_32 := 16#FF01#;
         rdyIgnore : MessageTag;
         ignore : Unsigned_64;
      begin
         rdyIgnore := capSend (CAP_SLOT_READY,
            (tag      => (label => OP_NOT_PRESENT, length => 0,
                          flags => 0, badge => 0),
             capBadge => 0,
             words    => (others => 0)));
         ignore := syscall (SYSCALL_EXIT);
      end;
      return;
   end if;

   debugPrint ("NVMe Driver: Initializing controller..." & LF);

   --  2. Initialize controller (maps BAR0, resets, sets up admin queues)
   NVMe.initController (bar0Phys, dmaPhys);

   --  3. Identify controller and namespace
   NVMe.identifyController;
   NVMe.identifyNamespace;

   --  4. Create I/O queues
   NVMe.createIOQueues;

   --  5. Register as NVMe driver
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_NVME);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
   end;

   debugPrint ("NVMe Driver: Ready, entering message loop." & LF);

   --  6. IPC receive loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_READ_BLOCK =>
            handleReadBlock (sender, msg);
         when OP_WRITE_BLOCK =>
            handleWriteBlock (sender, msg);
         when OP_IDENTIFY =>
            handleIdentify (sender);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
