------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace ATA PIO driver - specification.
--  Provides block-level read/write via port I/O syscalls.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package ATA_PIO is

   --  ATA I/O port offsets from base
   OFFSET_DATA        : constant := 16#0#;
   OFFSET_ERROR       : constant := 16#1#;
   OFFSET_FEATURES    : constant := 16#1#;
   OFFSET_SECTOR_CT   : constant := 16#2#;
   OFFSET_LBA_LOW     : constant := 16#3#;
   OFFSET_LBA_MID     : constant := 16#4#;
   OFFSET_LBA_HI      : constant := 16#5#;
   OFFSET_DRIVE_SEL   : constant := 16#6#;
   OFFSET_STATUS      : constant := 16#7#;
   OFFSET_CMD         : constant := 16#7#;

   --  Control register offset from ctrl base
   OFFSET_ALT_STATUS  : constant := 16#2#;
   OFFSET_CTRL        : constant := 16#2#;

   --  Default port bases
   PRIMARY_IO_BASE    : constant := 16#1F0#;
   PRIMARY_CTRL_BASE  : constant := 16#3F6#;
   SECONDARY_IO_BASE  : constant := 16#170#;
   SECONDARY_CTRL_BASE : constant := 16#376#;

   --  Status register bits
   STATUS_ERR : constant Unsigned_8 := 16#01#;
   STATUS_DRQ : constant Unsigned_8 := 16#08#;
   STATUS_DF  : constant Unsigned_8 := 16#20#;
   STATUS_RDY : constant Unsigned_8 := 16#40#;
   STATUS_BSY : constant Unsigned_8 := 16#80#;

   --  Commands
   CMD_IDENTIFY       : constant Unsigned_8 := 16#EC#;
   CMD_READ_PIO_EXT   : constant Unsigned_8 := 16#24#;

   --  Drive select values
   SELECT_MASTER      : constant Unsigned_8 := 16#A0#;
   SELECT_SLAVE       : constant Unsigned_8 := 16#B0#;
   SELECT_LBA         : constant Unsigned_8 := 16#40#;

   SECTOR_SIZE : constant := 512;

   type DriveInfo is record
      present : Boolean;
      ioBase  : Unsigned_16;
      ctrlBase : Unsigned_16;
      isMaster : Boolean;
      maxLBA  : Unsigned_64;
   end record;

   --  Initialize and identify the primary master drive.
   --  Sets drive.present if the drive responds.
   procedure identify (drive : out DriveInfo);

   --  Read sectors from the drive using PIO LBA48.
   --  @param drive  - drive info (must be present)
   --  @param lba    - starting logical block address
   --  @param count  - number of sectors to read (max 256)
   --  @param buf    - destination address for sector data
   --  @return True on success, False on error
   function readSectors
     (drive : DriveInfo;
      lba   : Unsigned_64;
      count : Unsigned_16;
      buf   : System.Address) return Boolean;

end ATA_PIO;
