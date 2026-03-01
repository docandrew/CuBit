------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace ATA PIO driver.
--
--  Provides block-level read access to ATA disks via IPC.
--  Clients send BLK_READ messages with a grant buffer; this driver
--  reads sectors from the disk and writes them into the grant.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with ATA_PIO;

procedure main is
   use ASCII;

   --  IPC labels for block device operations
   BLK_READ   : constant Unsigned_32 := 16#0200#;
   BLK_WRITE  : constant Unsigned_32 := 16#0201#;
   REPLY_OK   : constant Unsigned_32 := 16#F000#;
   REPLY_ERR  : constant Unsigned_32 := 16#F001#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 256 * 4096;

   --  Drive info
   drive : ATA_PIO.DriveInfo;

   --  IPC state
   sender : ProcessID;
   msg    : Message;

   --  Conversion helper
   function toAddr is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest  : ProcessID;
      label : Unsigned_32;
      word0 : Unsigned_64)
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

   --  Handle BLK_READ request
   --  words(0) = device_id (0 = primary master)
   --  words(1) = LBA (starting sector)
   --  words(2) = sector_count
   --  words(3) = grant_id (buffer to write data into)
   procedure handleRead (snd : ProcessID; m : Message) is
      lba       : constant Unsigned_64 := m.words (1);
      count     : constant Unsigned_16 := Unsigned_16 (m.words (2) and 16#FFFF#);
      grantId   : constant Unsigned_64 := m.words (3);
      grantAddr : constant System.Address :=
        toAddr (GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE);
      ok : Boolean;
   begin
      if count = 0 or count > 256 then
         sendReply (snd, REPLY_ERR, 0);
         return;
      end if;

      ok := ATA_PIO.readSectors (drive, lba, count, grantAddr);

      if ok then
         sendReply (snd, REPLY_OK, Unsigned_64 (count));
      else
         sendReply (snd, REPLY_ERR, 0);
      end if;
   end handleRead;

begin
   debugPrint ("ATA Driver: Starting..." & LF);

   --  Identify primary master drive
   ATA_PIO.identify (drive);

   if not drive.present then
      debugPrint ("ATA Driver: No drive found, exiting." & LF);
      return;
   end if;

   debugPrint ("ATA Driver: Drive found, entering message loop." & LF);

   --  Main IPC message loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when BLK_READ =>
            handleRead (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
