with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Production Ext2 over the simulated sector device: per-request allocation
--  batching, contiguous placement, multi-group reservation, fault injection
--  at every request of a batched append, and the write-through block cache
--  (hits, and never newer than the device after a failed write).
procedure Batched_IO is
   use type Inode;
   Block_Bytes : constant := 1024;
   Pointers_Per_Block : constant := Block_Bytes / 4;
   Disk_Blocks : constant := Disk'Length / Block_Bytes;
   Payload_Bytes : constant := 40 * Block_Bytes; -- 12 direct + 28 single
   Metadata_Blocks : constant := 6; -- boot, superblock, table, bitmaps, inodes
   Fs : Filesystem;
   Sb : Superblock with Import, Address => Disk (Block_Bytes)'Address;
   Item : Inode with Import, Address => Disk (5 * Block_Bytes)'Address;
   Table : array (Unsigned_32 range 0 .. 3) of BlockGroupDescriptor
     with Import, Address => Disk (2 * Block_Bytes)'Address;
   Payload : String (1 .. Payload_Bytes);
   Output : String (1 .. Payload_Bytes);
   Written, Completed : Unsigned_64;
   Status : Write_Status;
   Read_Result : Read_Status;
   Candidate : Inode;
   Faults : Natural := 0;
   Append_Calls : Natural;

   type Geometry is (One_Group, Four_Groups);
   Group_Blocks : constant := 16;

   function Bit_Set (Bitmap_Block : Natural; Bit : Natural) return Boolean is
     ((Disk (Bitmap_Block * Block_Bytes + Bit / 8) and
       Shift_Left (Unsigned_8 (1), Bit mod 8)) /= 0);

   procedure Mark (Bitmap_Block, Bit : Natural; Used : Boolean) is
      Index : constant Natural := Bitmap_Block * Block_Bytes + Bit / 8;
      Mask : constant Unsigned_8 := Shift_Left (Unsigned_8 (1), Bit mod 8);
   begin
      Disk (Index) := (if Used then Disk (Index) or Mask else Disk (Index) and not Mask);
   end Mark;

   --  Four groups of 16 blocks keep their bitmaps in blocks 3, 6, 7 and 8
   --  (all inside group 0, as flex_bg places them).
   function Bitmap_Of (Group : Natural; Shape : Geometry) return Natural is
     (if Shape = One_Group then 3 else (case Group is when 0 => 3, when 1 => 6,
                                                         when 2 => 7, when others => 8));

   procedure Setup (Shape : Geometry) is
      Admission : Admission_Result;
      Groups : constant Natural := (if Shape = One_Group then 1 else 4);
      Per_Group : constant Natural :=
        (if Shape = One_Group then Disk_Blocks - 1 else Group_Blocks);
      Reserved_Last : constant Natural := (if Shape = One_Group then 5 else 8);
   begin
      Reset;
      Check_Reclamation := False;
      Sb.signature := EXT2_SIGNATURE;
      Sb.blockCount := Disk_Blocks; -- the last of four groups is partial
      Sb.firstDataBlock := 1;
      Sb.blocksPerBlockGroup := Unsigned_32 (Per_Group);
      Sb.inodeCount := 8 * Unsigned_32 (Groups);
      Sb.inodesPerBlockGroup := 8;
      Sb.majorVersion := 1;
      Sb.incompatibleFeatures := 2;
      Sb.inodeSize := 128;
      Sb.freeBlocks := 0;
      for G in 0 .. Groups - 1 loop
         declare
            Bitmap : constant Natural := Bitmap_Of (G, Shape);
            Free : Natural := 0;
         begin
            Disk (Bitmap * Block_Bytes .. (Bitmap + 1) * Block_Bytes - 1) := [others => 255];
            for Bit in 0 .. Per_Group - 1 loop
               declare
                  Block : constant Natural := 1 + G * Per_Group + Bit;
               begin
                  if Block > Reserved_Last and then Block < Natural (Sb.blockCount) then
                     Mark (Bitmap, Bit, False);
                     Free := Free + 1;
                  end if;
               end;
            end loop;
            Table (Unsigned_32 (G)) :=
              (blockBitmapAddr => Unsigned_32 (Bitmap), inodeBitmapAddr => 4,
               inodeTableAddr => 5, numFreeBlocks => Unsigned_16 (Free),
               numFreeInodes => 0, numDirectories => 0, padding => 0, reserved => 0);
            Sb.freeBlocks := Sb.freeBlocks + Unsigned_32 (Free);
         end;
      end loop;
      Item := NULL_INODE;
      Item.typeAndPermissions := 16#8000#;
      Item.numHardLinks := 1;
      initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                       Grant_Buffer'Address, Grant_Buffer'Length, Admission);
      pragma Assert (Admission = Admitted);
      Candidate := Item;
      Calls := 0;
      Writes := 0;
   end Setup;

   --  Independently walk the on-disk tree: every mapped block is allocated in
   --  its group's bitmap, every other data-area block is free, counts agree
   --  with the bitmaps, and the file reads back.
   procedure Check (Shape : Geometry; Length : Natural) is
      Groups : constant Natural := (if Shape = One_Group then 1 else 4);
      Per_Group : constant Natural := Natural (Sb.blocksPerBlockGroup);
      Referenced : array (0 .. Disk_Blocks - 1) of Boolean := [others => False];
      Pointers : array (0 .. Pointers_Per_Block - 1) of Unsigned_32
        with Import, Address => Disk (Natural (Item.singleIndirectBlock) * Block_Bytes)'Address;
      Mapped : Natural := 0;
      Free_Total : Natural := 0;
   begin
      pragma Assert (Item = Candidate);
      for B of Item.directBlocks loop
         if B /= 0 then Referenced (Natural (B)) := True; Mapped := Mapped + 1; end if;
      end loop;
      if Item.singleIndirectBlock /= 0 then
         Referenced (Natural (Item.singleIndirectBlock)) := True;
         Mapped := Mapped + 1;
         for B of Pointers loop
            if B /= 0 then
               pragma Assert (not Referenced (Natural (B)));
               Referenced (Natural (B)) := True;
               Mapped := Mapped + 1;
            end if;
         end loop;
      end if;
      pragma Assert (Item.numDiskSectors = Unsigned_32 (Mapped * Block_Bytes / 512));
      for G in 0 .. Groups - 1 loop
         declare
            Free : Natural := 0;
         begin
            for Bit in 0 .. Per_Group - 1 loop
               declare
                  Block : constant Natural := 1 + G * Per_Group + Bit;
               begin
                  if Block < Natural (Sb.blockCount) then
                     if Referenced (Block) then
                        pragma Assert (Bit_Set (Bitmap_Of (G, Shape), Bit));
                     elsif not Bit_Set (Bitmap_Of (G, Shape), Bit) then
                        Free := Free + 1;
                     end if;
                  end if;
               end;
            end loop;
            pragma Assert (Natural (Table (Unsigned_32 (G)).numFreeBlocks) = Free);
            Free_Total := Free_Total + Free;
         end;
      end loop;
      pragma Assert (Natural (Sb.freeBlocks) = Free_Total and Fs.sb.freeBlocks = Sb.freeBlocks);
      Output := [others => '?'];
      readData (Fs, Item, 0, Output'Address, Unsigned_64 (Length), Completed, Read_Result);
      pragma Assert (Read_Result = Read_Complete and Completed = Unsigned_64 (Length));
      pragma Assert (Output (1 .. Length) = Payload (1 .. Length));
   end Check;

   --  Data blocks of each run are physically consecutive.
   procedure Check_Contiguous is
      Pointers : array (0 .. Pointers_Per_Block - 1) of Unsigned_32
        with Import, Address => Disk (Natural (Item.singleIndirectBlock) * Block_Bytes)'Address;
   begin
      for I in 1 .. Item.directBlocks'Last loop
         pragma Assert (Item.directBlocks (I) = Item.directBlocks (I - 1) + 1);
      end loop;
      for I in 1 .. 27 loop
         pragma Assert (Pointers (I) = Pointers (I - 1) + 1);
      end loop;
   end Check_Contiguous;
begin
   for I in Payload'Range loop
      Payload (I) := Character'Val (Character'Pos ('A') + I mod 23);
   end loop;

   --  One request: two batches (the direct slots, then the single leaf),
   --  each publishing its bitmap, descriptor and superblock once.
   Setup (One_Group);
   writeData (Fs, 1, Candidate, 0, Payload'Address, Payload_Bytes, Written, Status);
   pragma Assert (Status = Write_Complete and Written = Payload_Bytes);
   Append_Calls := Calls;
   --  Reads: descriptor, inode-table, superblock and bitmap blocks, once.
   --  Writes: per batch bitmap + descriptor + superblock, then 12 KiB and
   --  28 KiB of payload in 4 KiB grant-sized transfers (3 + 7), the leaf,
   --  and finally the inode.
   pragma Assert (Calls - Writes = 4);
   pragma Assert (Writes = 3 + 3 + 3 + 7 + 1 + 1);
   Put_Line ("BATCHED-APPEND-IO 40 KiB at 1 KiB blocks: reads" &
             Natural'Image (Calls - Writes) & ", writes" & Writes'Image &
             " (was 40 allocations of 7 requests plus data/pointer/inode writes)");
   Check (One_Group, Payload_Bytes);
   Check_Contiguous;
   pragma Assert (Item.directBlocks (0) = Metadata_Blocks);
   pragma Assert (Natural (Sb.freeBlocks) = Disk_Blocks - Metadata_Blocks - 41);

   --  Every request of the batched append, failed before/partially/after I/O
   --  with every malformed reply: no follow-on request (the fixture asserts
   --  it), no completed-prefix report after uncertainty, quarantine once
   --  anything was written, and otherwise a disk identical to the start. A
   --  retry after an unquarantined failure produces a consistent volume.
   declare
      Baseline : constant Natural := Append_Calls;
      Original : Bytes (Disk'Range);
   begin
      for Boundary in 1 .. Baseline loop
         for Treatment in Failure_Mode loop
            for Reply_Kind in Failure_Reply loop
               Setup (One_Group);
               Original := Disk;
               Fail_At := Boundary;
               CuBit.Messages.Mode := Treatment;
               Reply_Style := Reply_Kind;
               writeData (Fs, 1, Candidate, 0, Payload'Address, Payload_Bytes,
                          Written, Status);
               pragma Assert (Failed and Calls = Boundary);
               pragma Assert (Status /= Write_Complete);
               if Writes > 0 then
                  pragma Assert (Fs.writeQuarantined and Status = Write_Recovery_Required);
                  pragma Assert (Written = 0);
                  --  Reservation metadata precedes anything referencing it:
                  --  every block the on-disk inode names is marked allocated.
                  for B of Item.directBlocks loop
                     pragma Assert (B = 0 or else Bit_Set (3, Natural (B) - 1));
                  end loop;
                  pragma Assert (Item.singleIndirectBlock = 0 or else
                                 Bit_Set (3, Natural (Item.singleIndirectBlock) - 1));
               else
                  pragma Assert (not Fs.writeQuarantined and Disk = Original);
                  Failed := False;
                  Fail_At := 0;
                  Candidate := Item;
                  writeData (Fs, 1, Candidate, 0, Payload'Address, Payload_Bytes,
                             Written, Status);
                  pragma Assert (Status = Write_Complete and Written = Payload_Bytes);
                  Check (One_Group, Payload_Bytes);
               end if;
               Faults := Faults + 1;
            end loop;
         end loop;
      end loop;
   end;

   --  A reservation spanning groups: each touched bitmap and descriptor is
   --  published once, and counts stay consistent group by group.
   Setup (Four_Groups);
   writeData (Fs, 1, Candidate, 0, Payload'Address, 30 * Block_Bytes, Written, Status);
   pragma Assert (Status = Write_Complete and Written = 30 * Block_Bytes);
   Check (Four_Groups, 30 * Block_Bytes);
   declare
      Resized : Inode;
      Result : Truncate_Status;
   begin
      Check_Resize_Reclamation := False;
      resizeFile (Fs, 1, 0, Resized, Result);
      pragma Assert (Result = Truncate_Complete);
      Candidate := Resized;
      Check (Four_Groups, 0);
      pragma Assert (Natural (Sb.freeBlocks) = Disk_Blocks - 1 - 8);
   end;

   --  The write-through cache: a repeated path lookup is served entirely
   --  from cached inode-table and directory blocks.
   Setup (One_Group);
   declare
      Number : Unsigned_32;
      Lookup : Directory_Lookup_Status;
      Root : Inode with Import, Address => Disk (5 * Block_Bytes + 128)'Address;
      Entry_Block : constant Natural := 20;
      Dirent : DirectoryEntry with Import, Address => Disk (Entry_Block * Block_Bytes)'Address;
      Name : String (1 .. 4) with Import, Address => Disk (Entry_Block * Block_Bytes + 8)'Address;
      Before : Natural;
   begin
      Root := NULL_INODE;
      Root.typeAndPermissions := 16#4000#;
      Root.sizeLo := Block_Bytes;
      Root.directBlocks (0) := Unsigned_32 (Entry_Block);
      Dirent := (inode => 1, length => Block_Bytes, nameLength => 4,
                 fileType => FILETYPE_REGULAR);
      Name := "file";
      resolvePath (Fs, "file", Number, Lookup);
      pragma Assert (Lookup = Lookup_Found and Number = 1);
      Before := Calls;
      resolvePath (Fs, "file", Number, Lookup);
      pragma Assert (Lookup = Lookup_Found and Number = 1 and Calls = Before);
      Put_Line ("CACHED-LOOKUP: first lookup" & Before'Image &
                " reads, repeated lookup 0 reads");
   end;

   --  A failed write never leaves the cache newer than the device: after an
   --  injected failure of the final inode write, the volume's cached blocks
   --  are dropped and the inode is re-read from the device.
   for Treatment in Failure_Mode loop
      Setup (One_Group);
      declare
         Final_Write : Natural;
         Cached : Inode;
      begin
         writeData (Fs, 1, Candidate, 0, Payload'Address, Block_Bytes, Written, Status);
         pragma Assert (Status = Write_Complete);
         Final_Write := Calls;
         Setup (One_Group);
         Fail_At := Final_Write;
         CuBit.Messages.Mode := Treatment;
         writeData (Fs, 1, Candidate, 0, Payload'Address, Block_Bytes, Written, Status);
         pragma Assert (Failed and Status = Write_Recovery_Required);
         Failed := False;
         Fail_At := 0;
         readInode (Fs, 1, Cached, Read_Result);
         pragma Assert (Read_Result = Read_Complete and Cached = Item);
         pragma Assert (Calls > Final_Write); -- re-read from the device
      end;
   end loop;
   Put_Line ("BATCHED-IO-CHECK: PASS" & Faults'Image & " injected failures");
end Batched_IO;
