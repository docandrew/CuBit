with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
procedure Main is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
   ino : Inode with Import, Address => Disk (5120)'Address;
   pointers : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (21 * 1024)'Address;
   secondGroup : BlockGroupDescriptor
     with Import, Address => Disk (2048 + 32)'Address;
   parent : Inode with Import, Address => Disk (5120 + 128)'Address;
   dot : DirectoryEntry with Import, Address => Disk (20 * 1024)'Address;
   dotdot : DirectoryEntry
     with Import, Address => Disk (20 * 1024 + 12)'Address;
   result : Inode;
   status : Truncate_Status;
   goodCalls : Natural;
   original : Bytes (Disk'Range);
   candidate : Inode;
   written : Unsigned_64;
   writeStatus : Write_Status;
   payload : constant String := "test";
   inodeCall, pointerCall : Natural;
   allocated : Unsigned_32;
   found : Unsigned_32;
   lookupStatus : Directory_Lookup_Status;

   procedure Setup is
   begin
      Reset;
      sb.inodeCount := 8;
      sb.blockCount := 64;
      sb.freeBlocks := 40;
      sb.firstDataBlock := 1;
      sb.blocksPerBlockGroup := 64;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.inodeSize := 128;
      bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
              inodeTableAddr => 5, numFreeBlocks => 40,
              numFreeInodes => 0, numDirectories => 0,
              padding => 0, reserved => 0);
      --  Blocks 1..23 allocated, 24..63 free: matches freeBlocks = 40.
      --  The final bit represents a block beyond the volume and stays set.
      Disk (3072 .. 3073) := [others => 255];
      Disk (3074) := 127;
      Disk (3079) := 128;
      ino := NULL_INODE;
      ino.typeAndPermissions := 16#8000#;
      ino.sizeLo := 13 * 1024;
      ino.numDiskSectors := 6;
      ino.directBlocks (0) := 20;
      ino.singleIndirectBlock := 21;
      pointers (0) := 22;
      fs := (sb => sb, blkSize => 1024,
             device => (endpointSlot => 1,
                        grant => (slot => 1, generation => 1),
                        grantBuffer => Grant_Buffer'Address,
                        grantBytes => Grant_Buffer'Length,
                        description => (blockCount => 128,
                          maxTransferBlocks => 8, features => FEATURE_FLUSH,
                          others => <>)),
             writeQuarantined => False);
      Durable := Disk;
   end Setup;

   procedure Setup_Write is
   begin
      Setup;
      Check_Reclamation := False;
      candidate := ino;
   end Setup_Write;

   procedure Setup_Allocation is
   begin
      Setup_Write;
      sb.freeInodes := 6;
      sb.firstNonReservedInode := 3;
      bgd.numFreeInodes := 6;
      Disk (4096) := 3; -- Inodes 1 and 2 reserved, 3..8 free.
      fs.sb := sb;
   end Setup_Allocation;

   procedure Setup_Create (grow : Boolean) is
   begin
      Setup_Allocation;
      Check_Creation := True;
      Creation_Block := (if grow then 24 else 20);
      parent := NULL_INODE;
      parent.typeAndPermissions := 16#4000#;
      if not grow then
         parent.sizeLo := 1024;
         parent.numDiskSectors := 2;
         parent.directBlocks (0) := 20;
         dot := (inode => 2, length => 12, nameLength => 1,
                 fileType => FILETYPE_DIRECTORY);
         Disk (20 * 1024 + 8) := Character'Pos ('.');
         dotdot := (inode => 2, length => 1012, nameLength => 2,
                    fileType => FILETYPE_DIRECTORY);
         Disk (20 * 1024 + 20 .. 20 * 1024 + 21) :=
           [others => Character'Pos ('.')];
      end if;
   end Setup_Create;
begin
   Setup;
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Complete);
   pragma Assert (fileSize (result) = 0 and then fileSize (ino) = 0);
   pragma Assert (Reclaims = 3 and then fs.sb.freeBlocks = 43);
   pragma Assert (sb.freeBlocks = 43 and then bgd.numFreeBlocks = 43);
   pragma Assert ((Disk (3074) and 16#38#) = 0);
   goodCalls := Calls;

   for failure in 1 .. goodCalls loop
      for treatment in Failure_Mode loop
         Setup;
         original := Disk;
         Fail_At := failure;
         CuBit.Messages.Mode := treatment;
         truncateToEmpty (fs, 1, result, status);
         pragma Assert (Failed and then Calls = failure);
         pragma Assert (status /= Truncate_Complete);
         if Publication_Attempted or else Reclaims > 0 then
            pragma Assert (fs.writeQuarantined);
            pragma Assert (status = Truncate_Recovery_Required);
         elsif not fs.writeQuarantined then
            pragma Assert (Disk = original);
         end if;
         if fs.writeQuarantined then
            --  A second truncate must not issue any I/O.
            truncateToEmpty (fs, 1, result, status);
            pragma Assert (status = Truncate_Recovery_Required);
            pragma Assert (Calls = failure);
         end if;
      end loop;
   end loop;
   Put_Line ("Injected before/partial/after failure at" &
             goodCalls'Image & " transport boundaries: PASS");

   Setup;
   fs.device.description.features := 0;
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Durability_Unsupported and Writes = 0);
   Setup;
   fs.device.description.features := FEATURE_READ_ONLY;
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Read_Only and Writes = 0);
   Setup;
   ino.doubleIndirectBlock := 23;
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Unsupported and Writes = 0);
   Setup;
   pointers (0) := 20; -- duplicate direct/indirect pointer
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Invalid and Writes = 0);
   Setup;
   pointers (0) := 64; -- out of volume
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Invalid and Writes = 0);
   Setup;
   fs.device.description.features := FEATURE_VOLATILE;
   Check_Reclamation := False;
   truncateToEmpty (fs, 1, result, status);
   pragma Assert (status = Truncate_Complete and Barriers = 0);
   pragma Assert (fileSize (ino) = 0 and sb.freeBlocks = 43);

   Setup_Write;
   writeData (fs, 1, candidate, 13 * 1024, payload'Address,
              payload'Length, written, writeStatus);
   pragma Assert (writeStatus = Write_Complete and written = 4);
   inodeCall := Inode_Write_Call;
   pointerCall := Pointer_Write_Call;
   pragma Assert (inodeCall > pointerCall and pointerCall > 0);
   for boundary in 1 .. 2 loop
      for treatment in Failure_Mode loop
         Setup_Write;
         Fail_At := (if boundary = 1 then pointerCall else inodeCall);
         CuBit.Messages.Mode := treatment;
         writeData (fs, 1, candidate, 13 * 1024, payload'Address,
                    payload'Length, written, writeStatus);
         pragma Assert (Calls = Fail_At and fs.writeQuarantined);
         pragma Assert (writeStatus = Write_Recovery_Required and written = 0);
         --  Block 24 must stay allocated even when a failed pointer write
         --  published it. No rollback is allowed to make it reusable.
         pragma Assert ((Disk (3074) and 128) /= 0);
         writeData (fs, 1, candidate, 0, payload'Address,
                    payload'Length, written, writeStatus);
         pragma Assert (Calls = Fail_At and written = 0);
         pragma Assert (writeStatus = Write_Recovery_Required);
      end loop;
   end loop;
   Put_Line ("Indirect pointer/inode publication failure: PASS");

   for reserveInode in Boolean loop
      Setup_Allocation;
      if reserveInode then
         allocateInode (fs, allocated, writeStatus);
      else
         allocateBlock (fs, allocated, writeStatus);
      end if;
      pragma Assert (writeStatus = Write_Complete);
      pragma Assert (allocated = (if reserveInode then 3 else 24));
      pragma Assert
        ((if reserveInode then sb.freeInodes = 5 and bgd.numFreeInodes = 5
          else sb.freeBlocks = 39 and bgd.numFreeBlocks = 39));
      goodCalls := Calls;
      for failure in 1 .. goodCalls loop
         for treatment in Failure_Mode loop
            Setup_Allocation;
            original := Disk;
            Fail_At := failure;
            CuBit.Messages.Mode := treatment;
            if reserveInode then
               allocateInode (fs, allocated, writeStatus);
            else
               allocateBlock (fs, allocated, writeStatus);
            end if;
            pragma Assert (Failed and Calls = failure);
            pragma Assert (allocated = 0 and writeStatus /= Write_Complete);
            if Writes > 0 then
               pragma Assert (fs.writeQuarantined);
            end if;
            if fs.writeQuarantined then
               pragma Assert (writeStatus = Write_Recovery_Required);
               allocateBlock (fs, allocated, writeStatus);
               pragma Assert (allocated = 0 and Calls = failure);
               pragma Assert (writeStatus = Write_Recovery_Required);
               allocateInode (fs, allocated, writeStatus);
               pragma Assert (allocated = 0 and Calls = failure);
               pragma Assert (writeStatus = Write_Recovery_Required);
            else
               pragma Assert (Disk = original);
            end if;
         end loop;
      end loop;
      Put_Line ((if reserveInode then "Inode" else "Block") &
                " allocation failures at" & goodCalls'Image &
                " boundaries, three modes: PASS");

      Setup_Allocation;
      fs.device.description.features := FEATURE_READ_ONLY;
      if reserveInode then
         allocateInode (fs, allocated, writeStatus);
      else
         allocateBlock (fs, allocated, writeStatus);
      end if;
      pragma Assert (allocated = 0 and writeStatus = Write_Read_Only);
      pragma Assert (Calls = 0 and not fs.writeQuarantined);

      Setup_Allocation;
      if reserveInode then
         fs.sb.freeInodes := 0;
         allocateInode (fs, allocated, writeStatus);
      else
         fs.sb.freeBlocks := 0;
         allocateBlock (fs, allocated, writeStatus);
      end if;
      pragma Assert (allocated = 0 and writeStatus = Write_No_Space);
      pragma Assert (Calls = 0 and not fs.writeQuarantined);
   end loop;

   --  Last usable object in a partial final group; the clear padding bits
   --  beyond the volume must not be treated as allocatable objects.
   Setup_Allocation;
   fs.sb.blocksPerBlockGroup := 32;
   fs.sb.freeBlocks := 1;
   bgd.numFreeBlocks := 0;
   secondGroup := bgd;
   secondGroup.blockBitmapAddr := 6;
   secondGroup.numFreeBlocks := 1;
   Disk (6144 .. 6146) := [others => 255];
   Disk (6147) := 63;
   allocateBlock (fs, allocated, writeStatus);
   pragma Assert (allocated = 63 and writeStatus = Write_Complete);
   pragma Assert (fs.sb.freeBlocks = 0 and secondGroup.numFreeBlocks = 0);
   pragma Assert (Disk (6147) = 127);

   Setup_Allocation;
   fs.sb.inodesPerBlockGroup := 4;
   fs.sb.inodeCount := 7;
   fs.sb.freeInodes := 1;
   bgd.numFreeInodes := 0;
   secondGroup := bgd;
   secondGroup.inodeBitmapAddr := 6;
   secondGroup.inodeTableAddr := 7;
   secondGroup.numFreeInodes := 1;
   Disk (6144) := 3;
   allocateInode (fs, allocated, writeStatus);
   pragma Assert (allocated = 7 and writeStatus = Write_Complete);
   pragma Assert (fs.sb.freeInodes = 0 and secondGroup.numFreeInodes = 0);
   pragma Assert (Disk (6144) = 7);
   Put_Line ("Allocation admission and partial final groups: PASS");

   for grow in Boolean loop
      Setup_Create (grow);
      createFile (fs, 2, "sample", allocated, writeStatus);
      pragma Assert (writeStatus = Write_Complete and allocated = 3);
      goodCalls := Calls;
      lookupInDir (fs, parent, "sample", found, lookupStatus);
      pragma Assert (lookupStatus = Lookup_Found and found = 3);
      pragma Assert (parent.sizeLo = 1024);
      for failure in 1 .. goodCalls loop
         for treatment in Failure_Mode loop
            Setup_Create (grow);
            original := Disk;
            Fail_At := failure;
            CuBit.Messages.Mode := treatment;
            createFile (fs, 2, "sample", allocated, writeStatus);
            pragma Assert (Failed and Calls = failure);
            pragma Assert (allocated = 0 and writeStatus /= Write_Complete);
            if Writes > 0 then
               pragma Assert (fs.writeQuarantined);
            end if;
            if fs.writeQuarantined then
               pragma Assert (writeStatus = Write_Recovery_Required);
               createFile (fs, 2, "again", allocated, writeStatus);
               pragma Assert (allocated = 0 and Calls = failure);
               pragma Assert (writeStatus = Write_Recovery_Required);
            else
               pragma Assert (Disk = original);
            end if;
         end loop;
      end loop;
      Put_Line ((if grow then "Growing" else "Existing") &
                " directory creation failures at" & goodCalls'Image &
                " boundaries, three modes: PASS");
   end loop;
   Setup_Create (False);
   createFile (fs, 2, "sample", allocated, writeStatus);
   goodCalls := Writes;
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert (allocated = 0 and writeStatus = Write_Already_Exists);
   pragma Assert (Writes = goodCalls and not fs.writeQuarantined);

   Setup_Create (True);
   fs.sb.freeBlocks := 0;
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert (allocated = 0 and writeStatus = Write_No_Space);
   pragma Assert (Writes = 0 and not fs.writeQuarantined);

   Setup_Create (True);
   fs.sb.freeInodes := 0;
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert (allocated = 0 and writeStatus = Write_No_Space);
   pragma Assert (not fs.writeQuarantined and fs.sb.freeBlocks = 40);
   pragma Assert ((Disk (3074) and 128) = 0);
   goodCalls := Calls;
   for failure in 1 .. goodCalls loop
      for treatment in Failure_Mode loop
         Setup_Create (True);
         fs.sb.freeInodes := 0;
         Fail_At := failure;
         CuBit.Messages.Mode := treatment;
         createFile (fs, 2, "sample", allocated, writeStatus);
         pragma Assert (Failed and Calls = failure and allocated = 0);
         if Writes > 0 then
            pragma Assert
              (fs.writeQuarantined and writeStatus = Write_Recovery_Required);
         end if;
      end loop;
   end loop;
   Put_Line ("Create no-space cleanup faults at" & goodCalls'Image &
             " boundaries, three modes: PASS");

   Setup_Create (False);
   original := Disk;
   createFile (fs, 2, "../bad", allocated, writeStatus);
   pragma Assert (allocated = 0 and writeStatus = Write_Out_Of_Range);
   pragma Assert (Calls = 0 and Disk = original);
   Setup_Create (False);
   dot.length := 4; -- malformed serialized header, no reservation allowed
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert (allocated = 0 and writeStatus = Write_Device_Error);
   pragma Assert (Writes = 0 and not fs.writeQuarantined);
   Setup_Create (False);
   parent.flags := 16#1000#; -- indexed directory requires different updates
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert
     (allocated = 0 and writeStatus = Write_File_Range_Unsupported);
   pragma Assert (Writes = 0 and not fs.writeQuarantined);
   Setup_Create (False);
   fs.device.description.features := FEATURE_VOLATILE;
   Check_Reclamation := False;
   createFile (fs, 2, "sample", allocated, writeStatus);
   pragma Assert (allocated = 3 and writeStatus = Write_Complete and Barriers = 0);
   lookupInDir (fs, parent, "sample", found, lookupStatus);
   pragma Assert (lookupStatus = Lookup_Found and found = 3);
   Put_Line ("Create admission, duplicate name and no-space cleanup: PASS");
   Put_Line ("EXT2-TRUNCATE-CHECK: PASS");
end Main;
