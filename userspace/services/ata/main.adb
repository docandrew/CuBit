------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace ATA/IDE PIO driver.
--
--  Performs PIO reads on the primary ATA channel. Runs IDENTIFY at startup
--  to detect the drive, then offers the shared Block.Device.V1 protocol.
--
--  All hardware access is via capability-checked portOutp8/portInp8/portInp16
--  syscalls,
--  validated by IOPORT capabilities granted at load time.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Memory_Grants;

procedure main is
   use ASCII;

   --  ATA primary channel ports
   IO_BASE   : constant Unsigned_16 := 16#1F0#;
   CTRL_PORT : constant Unsigned_16 := 16#3F6#;


   --  Register offsets from IO_BASE
   REG_DATA       : constant Unsigned_16 := IO_BASE + 0;
   REG_SECTOR_CT  : constant Unsigned_16 := IO_BASE + 2;
   REG_LBA_LOW    : constant Unsigned_16 := IO_BASE + 3;
   REG_LBA_MID    : constant Unsigned_16 := IO_BASE + 4;
   REG_LBA_HI     : constant Unsigned_16 := IO_BASE + 5;
   REG_DRIVE_SEL  : constant Unsigned_16 := IO_BASE + 6;
   REG_STATUS     : constant Unsigned_16 := IO_BASE + 7;
   REG_COMMAND    : constant Unsigned_16 := IO_BASE + 7;

   --  ATA commands
   CMD_IDENTIFY   : constant Unsigned_8 := 16#EC#;
   CMD_READ_PIO   : constant Unsigned_8 := 16#20#;
   CMD_WRITE_PIO  : constant Unsigned_8 := 16#30#;

   --  Drive select values
   SELECT_MASTER  : constant Unsigned_8 := 16#A0#;
   SELECT_LBA     : constant Unsigned_8 := 16#40#;

   --  Status register bits
   STATUS_BSY     : constant Unsigned_8 := 16#80#;
   STATUS_DRQ     : constant Unsigned_8 := 16#08#;
   STATUS_ERR     : constant Unsigned_8 := 16#01#;

   --  Sector size
   SECTOR_SIZE    : constant := 512;

   --  Drive detected flag
   drivePresent : Boolean := False;
   driveBlockCount : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  outb / inb wrappers (call syscall port I/O)
   ---------------------------------------------------------------------------
   procedure outb (port : Unsigned_16; val : Unsigned_8) is
      ignore : Unsigned_64;
   begin
      ignore := portOutp8 (port, val);
   end outb;

   function inb (port : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (portInp8 (port) and 16#FF#);
   end inb;

   --  Floating bus value (no controller present)
   STATUS_FLOAT   : constant Unsigned_8 := 16#FF#;

   ---------------------------------------------------------------------------
   --  waitBSY - poll until BSY clears, return False if no controller
   ---------------------------------------------------------------------------
   function waitBSY return Boolean is
      st : Unsigned_8;
   begin
      for i in 1 .. 100_000 loop
         st := inb (REG_STATUS);
         if st = STATUS_FLOAT then
            return False;
         end if;
         if (st and STATUS_BSY) = 0 then
            return True;
         end if;
      end loop;
      return False;  --  Timeout
   end waitBSY;

   ---------------------------------------------------------------------------
   --  waitDRQ - poll until DRQ sets (or ERR)
   --  Returns True if DRQ is set, False on error.
   ---------------------------------------------------------------------------
   function waitDRQ return Boolean is
      st : Unsigned_8;
   begin
      loop
         st := inb (REG_STATUS);
         if (st and STATUS_ERR) /= 0 then
            return False;
         end if;
         exit when (st and STATUS_DRQ) /= 0;
      end loop;
      return True;
   end waitDRQ;

   ---------------------------------------------------------------------------
   --  ata400nsDelay - read alternate status 4 times for 400ns delay
   ---------------------------------------------------------------------------
   procedure ata400nsDelay is
      ignore : Unsigned_8;
   begin
      for i in 1 .. 4 loop
         ignore := inb (CTRL_PORT);
      end loop;
   end ata400nsDelay;

   ---------------------------------------------------------------------------
   --  doIdentify - run ATA IDENTIFY on primary master
   --  Returns True if a drive is present.
   ---------------------------------------------------------------------------
   function doIdentify return Boolean is
      st   : Unsigned_8;
      mid  : Unsigned_8;
      hi   : Unsigned_8;

      --  256-word (512 byte) identify buffer
      type IdentifyBuf is array (0 .. 255) of Unsigned_16;
      buf : IdentifyBuf := (others => 0);
   begin
      --  Select master drive
      outb (REG_DRIVE_SEL, SELECT_MASTER);
      ata400nsDelay;

      --  Zero out sector count and LBA registers
      outb (REG_SECTOR_CT, 0);
      outb (REG_LBA_LOW, 0);
      outb (REG_LBA_MID, 0);
      outb (REG_LBA_HI, 0);

      --  Send IDENTIFY
      outb (REG_COMMAND, CMD_IDENTIFY);
      ata400nsDelay;

      --  Read status; if 0 or 0xFF, no drive/controller
      st := inb (REG_STATUS);
      if st = 0 or st = STATUS_FLOAT then
         debugPrint ("ATA: No drive on primary master." & LF);
         return False;
      end if;

      --  Wait for BSY to clear
      if not waitBSY then
         debugPrint ("ATA: BSY timeout during IDENTIFY." & LF);
         return False;
      end if;

      --  Check for ATAPI/SATA by reading LBA mid/hi
      mid := inb (REG_LBA_MID);
      hi := inb (REG_LBA_HI);
      if mid /= 0 or hi /= 0 then
         debugPrint ("ATA: Non-ATA device detected (ATAPI/SATA)." & LF);
         return False;
      end if;

      --  Poll until DRQ or ERR
      if not waitDRQ then
         debugPrint ("ATA: IDENTIFY failed (error bit set)." & LF);
         return False;
      end if;

      --  Read 256 words of identify data via individual portInp16 calls.
      --  The former bulk rep-insw syscall was not reliable from userspace.
      for i in buf'Range loop
         buf (i) := Unsigned_16 (portInp16 (REG_DATA) and 16#FFFF#);
      end loop;

      --  This driver currently issues 28-bit PIO commands, so report only the
      --  range it can actually address even if IDENTIFY advertises LBA48.
      driveBlockCount := Unsigned_64 (buf (60)) or
        Shift_Left (Unsigned_64 (buf (61)), 16);
      if driveBlockCount > 16#1000_0000# then
         driveBlockCount := 16#1000_0000#;
      end if;
      if driveBlockCount = 0 then
         return False;
      end if;

      debugPrint ("ATA: Drive detected on primary master." & LF);
      return True;
   end doIdentify;


   ---------------------------------------------------------------------------
   --  readSectors - PIO read of count sectors starting at LBA into addr
   --  Returns number of bytes read, or 0 on error.
   ---------------------------------------------------------------------------
   function readSectors (lba   : Unsigned_32;
                         count : Unsigned_8;
                         addr  : System.Address) return Unsigned_64
   is
      drv    : Unsigned_8;
   begin
      if count = 0 then
         return 0;
      end if;

      --  Select drive with LBA mode + top 4 bits of LBA
      drv := SELECT_MASTER or SELECT_LBA or
             Unsigned_8 (Shift_Right (lba, 24) and 16#0F#);
      outb (REG_DRIVE_SEL, drv);
      ata400nsDelay;

      --  Set sector count
      outb (REG_SECTOR_CT, count);

      --  Set LBA bytes
      outb (REG_LBA_LOW, Unsigned_8 (lba and 16#FF#));
      outb (REG_LBA_MID, Unsigned_8 (Shift_Right (lba, 8) and 16#FF#));
      outb (REG_LBA_HI,  Unsigned_8 (Shift_Right (lba, 16) and 16#FF#));

      --  Send READ PIO command
      outb (REG_COMMAND, CMD_READ_PIO);

      --  Read each sector
      for sec in 0 .. Natural (count) - 1 loop
         ata400nsDelay;
         if not waitBSY then
            debugPrint ("ATA: BSY timeout during read." & LF);
            return Unsigned_64 (sec * SECTOR_SIZE);
         end if;

         if not waitDRQ then
            debugPrint ("ATA: Read error at sector." & LF);
            return Unsigned_64 (sec * SECTOR_SIZE);
         end if;

         --  Read 256 words (512 bytes) via individual portInp16 calls.
         --  The former bulk rep-insw syscall was not reliable from userspace,
         --  because that implementation wrote to a user address from ring 0.
         declare
            type WordBuf is array (0 .. 255) of Unsigned_16
              with Pack;
            sectorWords : WordBuf
              with Import,
                   Address => addr +
                     System.Storage_Elements.Storage_Offset
                       (sec * SECTOR_SIZE);
         begin
            for i in sectorWords'Range loop
               sectorWords (i) :=
                 Unsigned_16 (portInp16 (REG_DATA) and 16#FFFF#);
            end loop;
         end;
      end loop;

      return Unsigned_64 (Natural (count) * SECTOR_SIZE);
   end readSectors;

   ---------------------------------------------------------------------------
   --  writeSectors - PIO write of count sectors starting at LBA from addr
   --  Returns number of bytes written, or 0 on error.
   ---------------------------------------------------------------------------
   function writeSectors (lba   : Unsigned_32;
                          count : Unsigned_8;
                          addr  : System.Address) return Unsigned_64
   is
      drv : Unsigned_8;
   begin
      if count = 0 then
         return 0;
      end if;

      --  Select drive with LBA mode + top 4 bits of LBA
      drv := SELECT_MASTER or SELECT_LBA or
             Unsigned_8 (Shift_Right (lba, 24) and 16#0F#);
      outb (REG_DRIVE_SEL, drv);
      ata400nsDelay;

      --  Set sector count
      outb (REG_SECTOR_CT, count);

      --  Set LBA bytes
      outb (REG_LBA_LOW, Unsigned_8 (lba and 16#FF#));
      outb (REG_LBA_MID, Unsigned_8 (Shift_Right (lba, 8) and 16#FF#));
      outb (REG_LBA_HI,  Unsigned_8 (Shift_Right (lba, 16) and 16#FF#));

      --  Send WRITE PIO command
      outb (REG_COMMAND, CMD_WRITE_PIO);

      --  Write each sector
      for sec in 0 .. Natural (count) - 1 loop
         ata400nsDelay;
         if not waitBSY then
            debugPrint ("ATA: BSY timeout during write." & LF);
            return Unsigned_64 (sec * SECTOR_SIZE);
         end if;

         if not waitDRQ then
            debugPrint ("ATA: Write error at sector." & LF);
            return Unsigned_64 (sec * SECTOR_SIZE);
         end if;

         --  Write 256 words (512 bytes) via individual portOutp16 calls
         declare
            type WordBuf is array (0 .. 255) of Unsigned_16
              with Pack;
            sectorWords : WordBuf
              with Import,
                   Address => addr +
                     System.Storage_Elements.Storage_Offset
                       (sec * SECTOR_SIZE);
            ignore : Unsigned_64;
         begin
            for i in sectorWords'Range loop
               ignore := portOutp16 (REG_DATA, sectorWords (i));
            end loop;
         end;
      end loop;

      --  Flush the write cache
      ata400nsDelay;
      if not waitBSY then
         debugPrint ("ATA: BSY timeout after write." & LF);
      end if;

      return Unsigned_64 (Natural (count) * SECTOR_SIZE);
   end writeSectors;

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
   --  words(1) = grant slot (buffer to write data into)
   --  words(2) = sector_count (number of sectors to read)
   --  words(3) = grant generation
   ---------------------------------------------------------------------------
   procedure handleReadBlock (sender : ProcessID; msg : Message) is
      lba       : Unsigned_32 := 0;
      sectorCt  : Unsigned_8 := 0;
      grantAddr : System.Address := System.Null_Address;
      resolved  : Boolean := False;
      bytesRead : Unsigned_64;
   begin
      if not drivePresent then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      if msg.tag.length /= 4 or else
         msg.words (1) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         msg.words (3) = 0 or else
         msg.words (3) > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         msg.words (2) = 0 or else
         msg.words (0) >= driveBlockCount or else
         msg.words (0) > Unsigned_64 (Unsigned_32'Last) or else
         msg.words (2) > 255 or else
         msg.words (2) > driveBlockCount - msg.words (0)
      then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      lba := Unsigned_32 (msg.words (0));
      sectorCt := Unsigned_8 (msg.words (2));
      CuBit.Memory_Grants.Resolve
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     => Unsigned_64 (sectorCt) * SECTOR_SIZE,
         requiredAccess => CuBit.Memory_Grants.Write_Access,
         mappedAddress  => grantAddr,
         success        => resolved);
      if not resolved then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      bytesRead := readSectors (lba, sectorCt, grantAddr);
      sendReply (sender, REPLY_OK, bytesRead);
   end handleReadBlock;

   ---------------------------------------------------------------------------
   --  handleWriteBlock
   --  words(0) = LBA (sector number)
   --  words(1) = grant slot (buffer to read data from)
   --  words(2) = sector_count (number of sectors to write)
   --  words(3) = grant generation
   ---------------------------------------------------------------------------
   procedure handleWriteBlock (sender : ProcessID; msg : Message) is
      lba          : Unsigned_32 := 0;
      sectorCt     : Unsigned_8 := 0;
      grantAddr    : System.Address := System.Null_Address;
      resolved     : Boolean := False;
      bytesWritten : Unsigned_64;
   begin
      if not drivePresent then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      if msg.tag.length /= 4 or else
         msg.words (1) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         msg.words (3) = 0 or else
         msg.words (3) > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         msg.words (2) = 0 or else
         msg.words (0) >= driveBlockCount or else
         msg.words (0) > Unsigned_64 (Unsigned_32'Last) or else
         msg.words (2) > 255 or else
         msg.words (2) > driveBlockCount - msg.words (0)
      then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      lba := Unsigned_32 (msg.words (0));
      sectorCt := Unsigned_8 (msg.words (2));
      CuBit.Memory_Grants.Resolve
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (msg.words (1)),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (msg.words (3))),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     => Unsigned_64 (sectorCt) * SECTOR_SIZE,
         requiredAccess => CuBit.Memory_Grants.Read_Access,
         mappedAddress  => grantAddr,
         success        => resolved);
      if not resolved then
         sendReply (sender, REPLY_ERROR, 0);
         return;
      end if;

      bytesWritten := writeSectors (lba, sectorCt, grantAddr);
      sendReply (sender, REPLY_OK, bytesWritten);
   end handleWriteBlock;

   ---------------------------------------------------------------------------
   --  handleDescribe
   --  Return Block.Device.V1 geometry and features.
   ---------------------------------------------------------------------------
   procedure handleDescribe (sender : ProcessID) is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      if drivePresent then
         replyMsg.tag := (label => REPLY_OK, length => 4,
                          flags => 0, badge => 0);
         replyMsg.capBadge := 0;
         replyMsg.words :=
           (0 => driveBlockCount,
            1 => Pack_Sizes (512, 512),
            2 => 255,
            3 => Pack_Properties (0, Fixed_Media));
         ignore := reply (sender, replyMsg);
      else
         sendReply (sender, REPLY_ERROR, 0);
      end if;
   end handleDescribe;

   --  Main message loop variables
   sender : ProcessID;
   msg    : Message;
begin
   debugPrint ("ATA Driver: Starting..." & LF);

   --  Run IDENTIFY to detect primary master drive
   drivePresent := doIdentify;

   if not drivePresent then
      debugPrint ("ATA Driver: No drive found, entering message loop anyway." & LF);
   end if;

   --  Register ATA driver
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (2);  --  DRIVER_ATA = 2
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

   debugPrint ("ATA: registered, entering message loop" & LF);

   --  Main IPC message loop
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
