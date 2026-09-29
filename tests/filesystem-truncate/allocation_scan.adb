with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;

-- Synthetic descriptor-table geometry isolates the allocation scan. This is
-- not a complete ext2 admission fixture; native Linux-created disks cover it.
procedure Allocation_Scan is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   table : array (Unsigned_32 range 0 .. 31) of BlockGroupDescriptor
     with Import, Address => Disk (2048)'Address;
   block : Unsigned_32;
   status : Write_Status;
   baseline : Bytes (Disk'Range);
   cases : Natural := 0;

   procedure Setup (Target : Unsigned_32) is
   begin
      Reset;
      Check_Reclamation := False;
      sb.blockCount := 64;
      sb.firstDataBlock := 1;
      sb.blocksPerBlockGroup := 2;
      sb.freeBlocks := 1;
      for Item of table loop
         Item := (blockBitmapAddr => 4, inodeBitmapAddr => 5,
           inodeTableAddr => 6, numFreeBlocks => 0,
           numFreeInodes => 0, numDirectories => 0,
           padding => 0, reserved => 0);
      end loop;
      table (Target).numFreeBlocks := 1;
      Disk (4096) := 254; -- First block in the selected group is free.
      fs := (sb => sb, blkSize => 1024,
        device => (endpointSlot => 1, grant => (slot => 1, generation => 1),
          grantBuffer => Grant_Buffer'Address, grantBytes => Grant_Buffer'Length,
          description => (blockCount => 128, maxTransferBlocks => 8,
            features => FEATURE_FLUSH or FEATURE_VOLATILE_CACHE, others => <>)),
        writeQuarantined => False, journal => <>);
      baseline := Disk;
      declare
         Discarded : Filesystem;
         Ignored : Admission_Result;
      begin
         --  End endpoint 1's previous block-cache lifetime. This synthetic
         --  geometry is deliberately not itself an admissible volume.
         initBlockDevice (Discarded, 1, (slot => 1, generation => 1),
                          Grant_Buffer'Address, Grant_Buffer'Length, Ignored);
      end;
      Calls := 0;
      Writes := 0;
   end Setup;
begin
   for Target in Unsigned_32 range 15 .. 31 loop
      Setup (Target);
      allocateBlock (fs, block, status);
      pragma Assert (status = Write_Complete and block = 1 + Target * 2);
      -- The cached descriptor block and bitmap block reads, then bitmap and
      -- descriptor writes completed from those cached blocks, then the
      -- superblock's read-modify-write: six calls whatever the group.
      pragma Assert (Calls = 6);
      pragma Assert (Writes = 3 and fs.sb.freeBlocks = 0);
      pragma Assert (table (Target).numFreeBlocks = 0 and Disk (4096) = 255);
      cases := cases + 1;
      for Failure in 1 .. 2 loop
         for Failure_Mode_Value in Failure_Mode loop
            for Style in Failure_Reply loop
               Setup (Target);
               Fail_At := Failure; CuBit.Messages.Mode := Failure_Mode_Value; Reply_Style := Style;
               allocateBlock (fs, block, status);
               pragma Assert (status = Write_Device_Error and block = 0);
               pragma Assert (Calls = Failure and Writes = 0 and Disk = baseline);
               cases := cases + 1;
            end loop;
         end loop;
      end loop;
   end loop;
   -- A second call sees the first call's own (written-through) metadata: the
   -- exhausted group is not allocated again from a stale snapshot, even
   -- when the in-memory superblock count still advertises a block.
   Setup (31);
   allocateBlock (fs, block, status);
   pragma Assert (status = Write_Complete);
   fs.sb.freeBlocks := 1;
   Calls := 0;
   allocateBlock (fs, block, status);
   pragma Assert (status = Write_No_Space and block = 0 and Writes = 3);
   pragma Assert (table (31).numFreeBlocks = 0 and Disk (4096) = 255);
   cases := cases + 1;
   Put_Line ("Ext2 allocation descriptor-sector scan: " & cases'Image & " checks PASS");
end Allocation_Scan;
