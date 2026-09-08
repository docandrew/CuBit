------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace NVMe block device driver.
--
--  Maps the NVMe controller BAR0 MMIO via SYSCALL_MAP_DEVICE, initializes
--  the controller and I/O queues, then offers the shared Block.Device.V1
--  protocol to authorized clients.
--
--  DMA region is pre-mapped by the kernel at 0x7000_0000_0000 (1 MiB).
--  BAR0 is mapped at 0x6000_0000_0000 (16KB) at startup.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Memory_Grants;
with NVMe;

procedure main is
   use ASCII;

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
                       reserved  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   ---------------------------------------------------------------------------
   --  handleReadBlock
   --  words(0) = LBA (sector number)
   --  words(1) = grant slot (buffer to write data into)
   --  words(2) = sector_count (number of sectors to read)
   --  words(3) = grant generation
   ---------------------------------------------------------------------------
   procedure handleReadBlock (sender : ProcessID; msg : Message) is
      lba       : constant Unsigned_64 := msg.words (0);
      sectorCt  : Unsigned_32 := 0;
      grantAddr : System.Address := System.Null_Address;
      resolved  : Boolean := False;
      returned  : Boolean := False;
      bytesRead : Unsigned_64;
      expectedBytes : Unsigned_64;
   begin
      if msg.tag.length /= 4 or else
         msg.words (1) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         msg.words (3) = 0 or else
         msg.words (3) > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         msg.words (2) = 0 or else
         msg.words (2) > Unsigned_64 (Unsigned_32'Last) or else
         NVMe.nsSectorSize = 0 or else
         lba >= NVMe.nsBlockCount or else
         msg.words (2) > NVMe.nsBlockCount - lba
      then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      sectorCt := Unsigned_32 (msg.words (2));
      expectedBytes :=
        Unsigned_64 (sectorCt) * Unsigned_64 (NVMe.nsSectorSize);
      CuBit.Memory_Grants.Acquire
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     =>
           Unsigned_64 (sectorCt) * Unsigned_64 (NVMe.nsSectorSize),
         --  A device read writes the result into the caller's grant.
         requiredAccess => CuBit.Memory_Grants.Write_Access,
         mappedAddress  => grantAddr,
         success        => resolved);
      if not resolved then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      bytesRead := NVMe.readBlocks (lba, sectorCt, grantAddr);
      CuBit.Memory_Grants.Return_Acquisition
        ((slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
          generation =>
            CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         returned);
      if not returned then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;
      if bytesRead = expectedBytes then
         sendReply (sender, REPLY_OK, bytesRead);
      else
         --  A short controller transfer is an error.  The completed prefix
         --  remains in word 0 for diagnostics and future recovery policy.
         sendReply (sender, REPLY_ERROR, bytesRead);
      end if;
   end handleReadBlock;

   ---------------------------------------------------------------------------
   --  handleWriteBlock
   --  words(0) = LBA (sector number)
   --  words(1) = grant slot (buffer to read data from)
   --  words(2) = sector_count (number of sectors to write)
   --  words(3) = grant generation
   ---------------------------------------------------------------------------
   procedure handleWriteBlock (sender : ProcessID; msg : Message) is
      lba          : constant Unsigned_64 := msg.words (0);
      sectorCt     : Unsigned_32 := 0;
      grantAddr    : System.Address := System.Null_Address;
      resolved     : Boolean := False;
      returned     : Boolean := False;
      bytesWritten : Unsigned_64;
      expectedBytes : Unsigned_64;
   begin
      if msg.tag.length /= 4 or else
         msg.words (1) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         msg.words (3) = 0 or else
         msg.words (3) > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         msg.words (2) = 0 or else
         msg.words (2) > Unsigned_64 (Unsigned_32'Last) or else
         NVMe.nsSectorSize = 0 or else
         lba >= NVMe.nsBlockCount or else
         msg.words (2) > NVMe.nsBlockCount - lba
      then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      sectorCt := Unsigned_32 (msg.words (2));
      expectedBytes :=
        Unsigned_64 (sectorCt) * Unsigned_64 (NVMe.nsSectorSize);
      CuBit.Memory_Grants.Acquire
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     =>
           Unsigned_64 (sectorCt) * Unsigned_64 (NVMe.nsSectorSize),
         --  A device write reads source bytes from the caller's grant.
         requiredAccess => CuBit.Memory_Grants.Read_Access,
         mappedAddress  => grantAddr,
         success        => resolved);
      if not resolved then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      bytesWritten := NVMe.writeBlocks (lba, sectorCt, grantAddr);
      CuBit.Memory_Grants.Return_Acquisition
        ((slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
          generation =>
            CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         returned);
      if not returned then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;
      if bytesWritten = expectedBytes then
         sendReply (sender, REPLY_OK, bytesWritten);
      else
         --  Never turn a short controller transfer into protocol success.
         sendReply (sender, REPLY_ERROR, bytesWritten);
      end if;
   end handleWriteBlock;

   ---------------------------------------------------------------------------
   --  handleDescribe
   --  Return Block.Device.V1 geometry and features.
   ---------------------------------------------------------------------------
   procedure handleDescribe (sender : ProcessID) is
      replyMsg : Message;
      ignore   : Unsigned_64;
      maxBlocks64 : Unsigned_64;
   begin
      if NVMe.nsBlockCount > 0 then
         maxBlocks64 := NVMe.maxTransferBytes /
           Unsigned_64 (NVMe.nsSectorSize);
         if maxBlocks64 = 0 then
            sendReply (sender, REPLY_ERROR, 0);
            return;
         end if;
         if maxBlocks64 > Unsigned_64 (Unsigned_32'Last) then
            maxBlocks64 := Unsigned_64 (Unsigned_32'Last);
         end if;

         replyMsg.tag := (label => REPLY_OK, length => 4,
                          flags => 0, reserved => 0);
         replyMsg.authorityTag := 0;
         replyMsg.words :=
           (0 => NVMe.nsBlockCount,
            1 => Pack_Sizes
              (Logical_Block_Size (NVMe.nsSectorSize),
               Logical_Block_Size (NVMe.nsSectorSize)),
            2 => maxBlocks64,
            3 => Pack_Properties (0, Fixed_Media));
         ignore := reply (sender, replyMsg);
      else
         sendReply (sender, REPLY_ERROR, 0);
      end if;
   end handleDescribe;

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
                          flags => 0, reserved => 0),
             authorityTag => 0,
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
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   debugPrint ("NVMe Driver: Ready, entering message loop." & LF);

   --  6. IPC receive loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_READ_BLOCKS =>
            handleReadBlock (sender, msg);
         when OP_WRITE_BLOCKS =>
            handleWriteBlock (sender, msg);
         when OP_DESCRIBE_DEVICE =>
            handleDescribe (sender);
         when others =>
            sendReply (sender, REPLY_ERROR, 0);
      end case;
   end loop;
end main;
