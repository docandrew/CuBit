------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace ATA PIO driver - body.
--  Uses port I/O syscalls for hardware access.
------------------------------------------------------------------------------
with CuBit.Messages; use CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;

package body ATA_PIO is

   --  Port I/O wrappers using syscalls
   procedure outb (port : Unsigned_16; val : Unsigned_8) is
      ignore : Unsigned_64;
   begin
      ignore := portOut8 (port, val);
   end outb;

   function inb (port : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (portIn8 (port) and 16#FF#);
   end inb;

   procedure ins16 (port : Unsigned_16;
                    addr : System.Address;
                    count : Unsigned_32) is
      ignore : Unsigned_64;
   begin
      ignore := portIns16 (port, addr, count);
   end ins16;

   --  Wait for BSY to clear
   procedure waitReady (drive : DriveInfo) is
      status : Unsigned_8;
   begin
      loop
         status := inb (drive.ioBase + OFFSET_STATUS);
         exit when (status and STATUS_BSY) = 0;
      end loop;
   end waitReady;

   --  Wait for BSY clear and DRQ or error
   function waitForData (drive : DriveInfo) return Boolean is
      status : Unsigned_8;
   begin
      --  Read alt status 4 times (~400ns delay)
      for i in 1 .. 4 loop
         status := inb (drive.ctrlBase);
      end loop;

      loop
         status := inb (drive.ioBase + OFFSET_STATUS);

         if (status and STATUS_ERR) /= 0 or
            (status and STATUS_DF) /= 0
         then
            return False;
         end if;

         exit when (status and STATUS_BSY) = 0 and
                   (status and STATUS_DRQ) /= 0;
      end loop;

      return True;
   end waitForData;

   procedure identify (drive : out DriveInfo) is
      status : Unsigned_8;
   begin
      drive.present  := False;
      drive.ioBase   := PRIMARY_IO_BASE;
      drive.ctrlBase := PRIMARY_CTRL_BASE;
      drive.isMaster := True;
      drive.maxLBA   := 0;

      --  Select master drive
      outb (drive.ioBase + OFFSET_DRIVE_SEL, SELECT_MASTER or SELECT_LBA);

      --  Clear sector count and LBA registers
      outb (drive.ioBase + OFFSET_SECTOR_CT, 0);
      outb (drive.ioBase + OFFSET_LBA_LOW, 0);
      outb (drive.ioBase + OFFSET_LBA_MID, 0);
      outb (drive.ioBase + OFFSET_LBA_HI, 0);

      --  Issue IDENTIFY command
      outb (drive.ioBase + OFFSET_CMD, CMD_IDENTIFY);

      --  Check for drive presence
      status := inb (drive.ioBase + OFFSET_STATUS);
      if status = 0 then
         return;  --  No drive
      end if;

      --  Wait for BSY to clear
      loop
         status := inb (drive.ioBase + OFFSET_STATUS);
         exit when (status and STATUS_BSY) = 0;

         --  If LBA_MID or LBA_HI are non-zero, this is not ATA
         if inb (drive.ioBase + OFFSET_LBA_MID) /= 0 or
            inb (drive.ioBase + OFFSET_LBA_HI) /= 0
         then
            return;  --  Not ATA (maybe ATAPI)
         end if;
      end loop;

      --  Check for errors
      if (status and STATUS_ERR) /= 0 then
         return;
      end if;

      --  Wait for DRQ
      loop
         status := inb (drive.ioBase + OFFSET_STATUS);
         exit when (status and STATUS_DRQ) /= 0 or
                   (status and STATUS_ERR) /= 0;
      end loop;

      if (status and STATUS_ERR) /= 0 then
         return;
      end if;

      --  Read 256 words of identify data
      declare
         idBuf : array (0 .. 255) of Unsigned_16
           with Alignment => 2;
      begin
         ins16 (drive.ioBase + OFFSET_DATA,
                idBuf'Address,
                256);

         --  Word 60-61: Total addressable sectors (LBA28)
         --  Word 100-103: Total addressable sectors (LBA48)
         drive.maxLBA := Unsigned_64 (idBuf (100)) or
                         Shift_Left (Unsigned_64 (idBuf (101)), 16) or
                         Shift_Left (Unsigned_64 (idBuf (102)), 32) or
                         Shift_Left (Unsigned_64 (idBuf (103)), 48);

         if drive.maxLBA = 0 then
            --  Fallback to LBA28
            drive.maxLBA := Unsigned_64 (idBuf (60)) or
                            Shift_Left (Unsigned_64 (idBuf (61)), 16);
         end if;
      end;

      drive.present := True;
   end identify;

   function readSectors
     (drive : DriveInfo;
      lba   : Unsigned_64;
      count : Unsigned_16;
      buf   : System.Address) return Boolean
   is
      selVal : Unsigned_8;
   begin
      if not drive.present then
         return False;
      end if;

      waitReady (drive);

      --  Select drive with LBA mode
      if drive.isMaster then
         selVal := SELECT_MASTER or SELECT_LBA;
      else
         selVal := SELECT_SLAVE or SELECT_LBA;
      end if;
      outb (drive.ioBase + OFFSET_DRIVE_SEL, selVal);

      --  LBA48: write high bytes first, then low bytes
      --  High sector count
      outb (drive.ioBase + OFFSET_SECTOR_CT,
            Unsigned_8 (Shift_Right (Unsigned_16 (count), 8) and 16#FF#));
      --  LBA bytes 3, 4, 5
      outb (drive.ioBase + OFFSET_LBA_LOW,
            Unsigned_8 (Shift_Right (lba, 24) and 16#FF#));
      outb (drive.ioBase + OFFSET_LBA_MID,
            Unsigned_8 (Shift_Right (lba, 32) and 16#FF#));
      outb (drive.ioBase + OFFSET_LBA_HI,
            Unsigned_8 (Shift_Right (lba, 40) and 16#FF#));

      --  Low sector count
      outb (drive.ioBase + OFFSET_SECTOR_CT,
            Unsigned_8 (count and 16#FF#));
      --  LBA bytes 0, 1, 2
      outb (drive.ioBase + OFFSET_LBA_LOW,
            Unsigned_8 (lba and 16#FF#));
      outb (drive.ioBase + OFFSET_LBA_MID,
            Unsigned_8 (Shift_Right (lba, 8) and 16#FF#));
      outb (drive.ioBase + OFFSET_LBA_HI,
            Unsigned_8 (Shift_Right (lba, 16) and 16#FF#));

      --  Issue READ PIO EXT command
      outb (drive.ioBase + OFFSET_CMD, CMD_READ_PIO_EXT);

      --  Read each sector
      for s in 0 .. Natural (count) - 1 loop
         if not waitForData (drive) then
            return False;
         end if;

         ins16 (drive.ioBase + OFFSET_DATA,
                buf + Storage_Offset (s * SECTOR_SIZE),
                SECTOR_SIZE / 2);
      end loop;

      return True;
   end readSectors;

end ATA_PIO;
