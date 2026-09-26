with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
use type Ext2.Inode;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;

procedure Overwrites is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   ino, originalInode : Inode;
   originalDisk : Bytes (Disk'Range);
   payload : String (1 .. 4096) := [others => 'W'];
   written : Unsigned_64;
   status : Write_Status;
   admission : Admission_Result;
   dataStart : Natural;
   failures : Natural := 0;

   procedure Setup (Block_Bytes : Positive) is
      descriptorOffset : constant Natural :=
        (if Block_Bytes = 1024 then 2048 else Block_Bytes);
      bgd : BlockGroupDescriptor
        with Import, Address => Disk (descriptorOffset)'Address;
      diskInode : Inode
        with Import, Address => Disk (5 * Block_Bytes)'Address;
   begin
      Reset;
      Check_Reclamation := False;
      sb.signature := EXT2_SIGNATURE;
      sb.blockShift := (if Block_Bytes = 1024 then 0 else 2);
      sb.blockCount := Unsigned_32 (Disk'Length / Block_Bytes);
      sb.firstDataBlock := (if Block_Bytes = 1024 then 1 else 0);
      sb.blocksPerBlockGroup := sb.blockCount;
      sb.inodeCount := 8;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.incompatibleFeatures := 2; -- standard typed directory records
      sb.inodeSize := 128;
      bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
              inodeTableAddr => 5, numFreeBlocks => 0,
              numFreeInodes => 0, numDirectories => 0,
              padding => 0, reserved => 0);
      ino := NULL_INODE;
      ino.typeAndPermissions := 16#8000#;
      ino.numHardLinks := 1;
      ino.sizeLo := payload'Length;
      ino.numDiskSectors := payload'Length / 512;
      dataStart := (if Block_Bytes = 1024 then 20 * 1024 else 8 * 4096);
      for B in 0 .. payload'Length / Block_Bytes - 1 loop
         ino.directBlocks (B) := Unsigned_32 (dataStart / Block_Bytes + B);
      end loop;
      diskInode := ino;
      Disk (dataStart .. dataStart + payload'Length - 1) :=
        [others => Character'Pos ('A')];
      initBlockDevice
        (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
         Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      Writes := 0;
      originalInode := ino;
      originalDisk := Disk;
   end Setup;

   procedure Check_Contents (Offset, Length : Natural) is
   begin
      pragma Assert (ino = originalInode);
      for I in Disk'Range loop
         pragma Assert
           (Disk (I) =
              (if I >= dataStart + Offset and I < dataStart + Offset + Length
               then Character'Pos ('W') else originalDisk (I)));
      end loop;
   end Check_Contents;
begin
   for Large_Blocks in Boolean loop
      declare
         Block_Bytes : constant Positive := (if Large_Blocks then 4096 else 1024);
      begin
         Setup (Block_Bytes);
         writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
         pragma Assert (status = Write_Complete and written = payload'Length);
         pragma Assert (Calls = 1 and Writes = 1);
         Check_Contents (0, payload'Length);
         Ada.Text_IO.Put_Line
           ("OVERWRITE-IO block_bytes=" & Block_Bytes'Image &
            " payload_bytes=4096 calls=" & Calls'Image &
            " reads=" & Natural'Image (Calls - Writes) &
            " writes=" & Writes'Image);

         Setup (Block_Bytes);
         writeData (fs, 1, ino, 509, payload'Address, 7, written, status);
         pragma Assert (status = Write_Complete and written = 7);
         pragma Assert (Calls = 4 and Writes = 2);
         Check_Contents (509, 7);
         Ada.Text_IO.Put_Line
           ("UNALIGNED-OVERWRITE-IO block_bytes=" & Block_Bytes'Image &
            " payload_bytes=7 calls=" & Calls'Image &
            " reads=" & Natural'Image (Calls - Writes) &
            " writes=" & Writes'Image);

         --  Every completion in aligned and sector-crossing paths: neither a
         --  malformed reply nor a failed write may lead to another I/O request.
         for Grant_Limited in Boolean loop
            for Unaligned in Boolean loop
               declare
                  Boundaries : constant Positive :=
                    (if Unaligned then 4 elsif Grant_Limited then payload'Length / Block_Bytes
                     else 1);
               begin
                  for Boundary in 1 .. Boundaries loop
                     for Treatment in Failure_Mode loop
                        for Reply_Kind in Failure_Reply loop
                           Setup (Block_Bytes);
                           if Grant_Limited then
                              fs.device.grantBytes := Unsigned_32 (Block_Bytes);
                           end if;
                           Fail_At := Boundary;
                           Mode := Treatment;
                           Reply_Style := Reply_Kind;
                           writeData
                             (fs, 1, ino, (if Unaligned then 509 else 0),
                              payload'Address, (if Unaligned then 7 else payload'Length),
                              written, status);
                           pragma Assert (Failed and Calls = Boundary);
                           pragma Assert (status = Write_Device_Error);
                           pragma Assert (not fs.writeQuarantined and ino = originalInode);
                           pragma Assert
                             (written = (if Unaligned then 0 else
                                           Unsigned_64 ((Boundary - 1) *
                                             (if Grant_Limited then Block_Bytes else payload'Length))));
                           --  Failure may change data, but no metadata is touched.
                           pragma Assert
                             (Disk (0 .. dataStart - 1) = originalDisk (0 .. dataStart - 1));
                           failures := failures + 1;
                        end loop;
                     end loop;
                  end loop;
               end;
            end loop;
         end loop;

         --  Independent limits: a large grant does not override the provider's
         --  maximum transfer, nor may a large device limit override the grant.
         for Limit_Grant in Boolean loop
            Setup (Block_Bytes);
            if Limit_Grant then
               fs.device.grantBytes := 1024;
            else
               fs.device.description.maxTransferBlocks := 2;
            end if;
            writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
            pragma Assert (status = Write_Complete and written = payload'Length);
            pragma Assert (Calls = 4 and Writes = 4);
            Check_Contents (0, payload'Length);
         end loop;

         Setup (Block_Bytes);
         writeData (fs, 1, ino, 0, payload'Address, 3500, written, status);
         pragma Assert (status = Write_Complete and written = 3500);
         Check_Contents (0, 3500);
         Setup (Block_Bytes);
         writeData (fs, 1, ino, 512, payload'Address, 3072, written, status);
         pragma Assert (status = Write_Complete and written = 3072);
         Check_Contents (512, 3072);

         --  A single fragmented mapping ends a run, without skipping a block.
         if not Large_Blocks then
            Setup (Block_Bytes);
            ino.directBlocks (2) := 26;
            originalInode := ino;
            writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
            pragma Assert (status = Write_Complete and written = payload'Length);
            pragma Assert (Calls = 3 and Writes = 3 and ino = originalInode);
            for I in Disk'Range loop
               pragma Assert
                 (Disk (I) =
                    (if I in 20 * 1024 .. 22 * 1024 - 1 |
                             23 * 1024 .. 24 * 1024 - 1 |
                             26 * 1024 .. 27 * 1024 - 1
                     then Character'Pos ('W') else originalDisk (I)));
            end loop;

            --  A hole ends the run; the normal allocator is not bypassed.
            Setup (Block_Bytes);
            ino.directBlocks (2) := 0;
            originalInode := ino;
            writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
            pragma Assert (status = Write_No_Space and written = 2048);
            pragma Assert (Calls = 1 and Writes = 1 and ino = originalInode);

            --  Do not inspect unrequested mappings or batch across the old EOF.
            Setup (Block_Bytes);
            ino.directBlocks (1) := 64;
            originalInode := ino;
            writeData (fs, 1, ino, 0, payload'Address, 1024, written, status);
            pragma Assert (status = Write_Complete and written = 1024);
            pragma Assert (Calls = 1 and Writes = 1);
            Check_Contents (0, 1024);
            Setup (Block_Bytes);
            ino.sizeLo := 1024;
            ino.directBlocks (1) := 64;
            originalInode := ino;
            writeData (fs, 1, ino, 0, payload'Address, 2048, written, status);
            pragma Assert (status = Write_Out_Of_Range and written = 1024);
            pragma Assert (Calls = 1 and Writes = 1 and ino = originalInode);
            Check_Contents (0, 1024);

            --  Larger device sectors use the existing RMW path. Filesystem
            --  adjacency alone is not sufficient to form one sector write.
            Setup (Block_Bytes);
            Sector_Bytes := 4096;
            fs.device.description.logicalBlockSize := 4096;
            fs.device.description.physicalBlockSize := 4096;
            fs.device.description.blockCount := Disk'Length / 4096;
            fs.device.description.maxTransferBlocks := 1;
            writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
            pragma Assert (status = Write_Complete and written = payload'Length);
            pragma Assert (Calls = 8 and Writes = 4);
            Check_Contents (0, payload'Length);
         end if;

         Setup (Block_Bytes);
         fs.device.description.features := FEATURE_READ_ONLY;
         writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
         pragma Assert (status = Write_Read_Only and written = 0 and Calls = 0);
         pragma Assert (Disk = originalDisk);
         Setup (Block_Bytes);
         fs.writeQuarantined := True;
         writeData (fs, 1, ino, 0, payload'Address, payload'Length, written, status);
         pragma Assert (status = Write_Recovery_Required and written = 0 and Calls = 0);
         Setup (Block_Bytes);
         writeData (fs, 1, ino, 0, payload'Address, 0, written, status);
         pragma Assert (status = Write_Complete and written = 0 and Calls = 0);

         --  Existing allocated storage beyond EOF still requires size publication.
         Setup (Block_Bytes);
         declare
            diskInode : Inode
              with Import, Address => Disk (5 * Block_Bytes)'Address;
         begin
            ino.sizeLo := 4090;
            diskInode := ino;
            writeData (fs, 1, ino, 4090, payload'Address, 6, written, status);
            pragma Assert (status = Write_Complete and written = 6);
            pragma Assert (ino.sizeLo = 4096 and diskInode = ino);
            pragma Assert (Calls = 5 and Writes = 2);
         end;
      end;
   end loop;

   --  Filling a sparse hole does not extend EOF, but DOES change a mapping.
   --  The optimization must still publish that inode and zero the new block.
   Setup (1024);
   declare
      bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
      diskInode : Inode with Import, Address => Disk (5120)'Address;
   begin
      sb.freeBlocks := 41;
      bgd.numFreeBlocks := 41;
      Disk (3072 .. 3073) := [others => 255];
      Disk (3074) := 16#77#; -- blocks 1..23 reserved, except block 20
      Disk (3079) := 128; -- outside the final group
      fs.sb := sb;
      ino.directBlocks (0) := 0;
      ino.numDiskSectors := 6;
      diskInode := ino;
      writeData (fs, 1, ino, 0, payload'Address, 4, written, status);
      pragma Assert (status = Write_Complete and written = 4);
      pragma Assert (ino.sizeLo = 4096 and ino.directBlocks (0) = 20);
      pragma Assert (diskInode = ino and Inode_Write_Call /= 0);
      pragma Assert (Disk (20 * 1024 .. 20 * 1024 + 3) = [1 .. 4 => Character'Pos ('W')]);
      pragma Assert (Disk (20 * 1024 + 4 .. 21 * 1024 - 1) = [1 .. 1020 => 0]);
   end;
   Ada.Text_IO.Put_Line ("Overwrite failure cases:" & failures'Image & " PASS");
   Ada.Text_IO.Put_Line ("OVERWRITE-CHECK: PASS");
end Overwrites;
