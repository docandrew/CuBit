------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block-device transport for an ext2 volume: byte-addressed reads and
--  writes over its Block.Device.V1 session (sector batching, read-modify-
--  write of partial sectors), barriers and FUA, and request counters.
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

private package Ext2.Device_IO is

   procedure deviceRequests (requests, flushes : out Unsigned_64);

   --  Read bytes from the device at a byte offset, bypassing the block cache.
   --  All storage, including RAM, uses a Block.Device.V1 session.
   procedure deviceRead
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count;
      status : out Read_Status);

   --  Write bytes to the device at a raw byte offset. Non-aligned writes
   --  read-modify-write partial sectors, taking the other bytes from cached
   --  blocks when present instead of reading the sector.
   procedure deviceWrite
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count;
      status : out Write_Status;
      fua    : Boolean := False);

   --  One device transfer of up to this many bytes (the staging buffer).
   Maximum_Transfer_Bytes : constant := 512 * 1024;

   subtype Staging_Index is Natural range 0 .. Maximum_Transfer_Bytes - 1;

   type Staging_Buffer is array (Staging_Index) of Unsigned_8 with Alignment => 8;

   --  Completed writes are durable: nothing to do without a volatile cache;
   --  otherwise the device's FLUSH. A failure quarantines the volume.
   --  Checkpoint writes issued since the last barrier (see Journal_State).
   Home_Writes_Pending : Boolean := False;

   procedure barrier (fs : in out Filesystem; ok : out Boolean);

   --  One block durable on its own: FUA where offered, else write + barrier.
   procedure writeDurable
     (fs : in out Filesystem; number : Unsigned_32; source : System.Address;
      ok : out Boolean);

end Ext2.Device_IO;
