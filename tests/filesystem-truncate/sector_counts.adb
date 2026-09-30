with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
use type Ext2.Inode;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;
with Sector_Accounting;
with Inode_Mappings;
with Double_Mappings;
with Triple_Mappings;
with Block_Paths;

procedure Sector_Counts is
   fs : Filesystem;
   candidate : Inode;
   payload : constant String := "test";
   longPayload : constant String (1 .. 8192) := [others => 'S'];
   written : Unsigned_64;
   status : Write_Status;
   admission : Admission_Result;
   initialFree : Unsigned_32;
   blockBytes : Positive := 1024;
   faultCases : Natural := 0;
   beforeFault : Bytes (Disk'Range);
   Fresh_Endpoint : Unsigned_64 := 100;

   procedure Setup (Size : Positive) is
      sb : Superblock with Import, Address => Disk (1024)'Address;
      bgd : BlockGroupDescriptor with Import,
        Address => Disk ((if Size = 1024 then 2 else 1) * Size)'Address;
      diskInode : Inode with Import, Address => Disk (5 * Size)'Address;
   begin
      Reset;
      Check_Reclamation := False;
      blockBytes := Size;
      sb.signature := EXT2_SIGNATURE;
      sb.blockShift := (case Size is when 1024 => 0, when 2048 => 1, when others => 2);
      sb.blockCount := Unsigned_32 (Disk'Length / Size);
      sb.firstDataBlock := (if Size = 1024 then 1 else 0);
      sb.blocksPerBlockGroup := sb.blockCount;
      sb.inodeCount := 8;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.incompatibleFeatures := 2; -- standard typed directory records
      sb.inodeSize := 128;
      sb.freeBlocks := sb.blockCount - 6;
      initialFree := sb.freeBlocks;
      bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
              inodeTableAddr => 5, numFreeBlocks => Unsigned_16 (sb.freeBlocks),
              numFreeInodes => 0, numDirectories => 0, padding => 0, reserved => 0);
      Disk (3 * Size .. 4 * Size - 1) := [others => 255];
      for B in 6 .. Natural (sb.blockCount) - 1 loop
         declare
            Relative : constant Natural := B - Natural (sb.firstDataBlock);
            Index : constant Natural := 3 * Size + Relative / 8;
         begin
            Disk (Index) := Disk (Index) and not Shift_Left (Unsigned_8'(1), Relative mod 8);
         end;
      end loop;
      candidate := NULL_INODE;
      candidate.typeAndPermissions := 16#8000#;
      candidate.numHardLinks := 1;
      diskInode := candidate;
      initBlockDevice (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                       Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      Writes := 0;
   end Setup;

   procedure Check (Allocated : Natural) is
      diskInode : Inode with Import, Address => Disk (5 * blockBytes)'Address;
      diskSuper : Superblock with Import, Address => Disk (1024)'Address;
      reached : Natural := 0;

      --  Count a pointer block and everything below it, Depth levels deep.
      procedure Count_Tree (Number : Unsigned_32; Depth : Positive) is
         entries : array (0 .. blockBytes / 4 - 1) of Unsigned_32
           with Import, Address => Disk (Natural (Number) * blockBytes)'Address;
      begin
         if Number = 0 then return; end if;
         reached := reached + 1;
         for Child of entries loop
            if Child /= 0 then
               if Depth = 1 then
                  reached := reached + 1;
               else
                  Count_Tree (Child, Depth - 1);
               end if;
            end if;
         end loop;
      end Count_Tree;
   begin
      --  Count actual pointers independently of the implementation's counter.
      for B of candidate.directBlocks loop
         if B /= 0 then reached := reached + 1; end if;
      end loop;
      if candidate.singleIndirectBlock /= 0 then
         declare
            pointers : array (0 .. blockBytes / 4 - 1) of Unsigned_32
              with Import, Address => Disk (Natural (candidate.singleIndirectBlock) * blockBytes)'Address;
         begin
            reached := reached + 1; -- the pointer block itself occupies storage
            for B of pointers loop
               if B /= 0 then reached := reached + 1; end if;
            end loop;
         end;
      end if;
      Count_Tree (candidate.doubleIndirectBlock, 2);
      Count_Tree (candidate.tripleIndirectBlock, 3);
      pragma Assert (reached = Allocated);
      pragma Assert (candidate.numDiskSectors = Unsigned_32 (reached * (blockBytes / 512)));
      pragma Assert (candidate = diskInode);
      pragma Assert (fs.sb.freeBlocks = initialFree - Unsigned_32 (Allocated));
      pragma Assert (diskSuper.freeBlocks = fs.sb.freeBlocks);
   end Check;

   procedure Write_At (Offset : Unsigned_64; Allocated : Natural) is
   begin
      writeData (fs, 1, candidate, Offset, payload'Address, payload'Length, written, status);
      pragma Assert (status = Write_Complete and written = payload'Length);
      Check (Allocated);
   end Write_At;


   procedure Check_Cache_Against_Disk (Offset : Unsigned_64) is
      Probe : Inode := candidate;
      Logical : constant Natural := Natural (Offset / Unsigned_64 (blockBytes));
      Per_Block : constant Natural := blockBytes / 4;
      Physical : Unsigned_32 := 0;
      Expected : String (1 .. 4) := [others => Character'Val (0)];
      Output : String (1 .. 4) := [others => '?'];
      Count : Unsigned_64;
      RStatus : Read_Status;

      function Pointer (Block_Number : Unsigned_32; Index : Natural)
         return Unsigned_32
      is
         Values : array (0 .. Per_Block - 1) of Unsigned_32
           with Import, Address => Disk (Natural (Block_Number) * blockBytes)'Address;
      begin
         return Values (Index);
      end Pointer;
   begin
      --  Inspect the candidate's mapped storage even when its size was not
      --  published. This is a test probe, NOT permission to reuse an uncertain
      --  inode in the service. Read quarantine still permits forensic reads.
      if Logical < 12 then
         Physical := Probe.directBlocks (Logical);
      elsif Logical < 12 + Per_Block then
         if Probe.singleIndirectBlock /= 0 then
            Physical := Pointer (Probe.singleIndirectBlock, Logical - 12);
         end if;
      elsif Logical < 12 + Per_Block + Per_Block * Per_Block then
         if Probe.doubleIndirectBlock /= 0 then
            declare
               Relative : constant Natural := Logical - 12 - Per_Block;
               Leaf : constant Unsigned_32 :=
                 Pointer (Probe.doubleIndirectBlock, Relative / Per_Block);
            begin
               if Leaf /= 0 then Physical := Pointer (Leaf, Relative mod Per_Block); end if;
            end;
         end if;
      elsif Probe.tripleIndirectBlock /= 0 then
         declare
            Relative : constant Natural :=
              Logical - 12 - Per_Block - Per_Block * Per_Block;
            Middle : constant Unsigned_32 := Pointer
              (Probe.tripleIndirectBlock, Relative / (Per_Block * Per_Block));
            Leaf : Unsigned_32 := 0;
         begin
            if Middle /= 0 then
               Leaf := Pointer (Middle, Relative / Per_Block mod Per_Block);
            end if;
            if Leaf /= 0 then Physical := Pointer (Leaf, Relative mod Per_Block); end if;
         end;
      end if;
      if Physical /= 0 then
         for I in Expected'Range loop
            Expected (I) := Character'Val (Disk (Natural (Physical) * blockBytes + I - 1));
         end loop;
      end if;
      Probe.sizeLo := Unsigned_32 ((Offset + 4) and 16#FFFF_FFFF#);
      Probe.sizeHi_DirACL := Unsigned_32 (Shift_Right (Offset + 4, 32));
      Failed := False;
      Fail_At := 0;
      --  Do not re-admit the volume or clear caches: this must catch a stale
      --  key after an ambiguous/partial pointer-block publication.
      readData (fs, Probe, Offset, Output'Address, 4, Count, RStatus);
      pragma Assert (RStatus = Read_Complete and Count = 4 and Output = Expected);
   end Check_Cache_Against_Disk;

   procedure Limited_Space (Free_Count : Positive := 1) is
      sb : Superblock with Import, Address => Disk (1024)'Address;
      bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
   begin
      Setup (1024);
      sb.freeBlocks := Unsigned_32 (Free_Count);
      fs.sb.freeBlocks := sb.freeBlocks;
      bgd.numFreeBlocks := Unsigned_16 (Free_Count);
      initialFree := sb.freeBlocks;
      Disk (3072 .. 4095) := [others => 255];
      --  Group-relative bits 5, 6, 7: physical blocks 6, 7, 8.
      Disk (3072) := (case Free_Count is when 1 => 16#DF#, when 2 => 16#9F#,
                        when others => 16#1F#);
   end Limited_Space;
begin
   for Shift in 0 .. 2 loop
      Setup (1024 * 2 ** Shift);
      Write_At (0, 1);
      Write_At (4, 1); -- extension within the same allocated block
      Write_At (Unsigned_64 (11 * blockBytes + 7), 2); -- sparse direct extent
      Write_At (Unsigned_64 (12 * blockBytes + 3), 4); -- data AND first indirect block
      Write_At (Unsigned_64 (14 * blockBytes + 3), 5); -- reuse indirect block
      Write_At (Unsigned_64 (14 * blockBytes + 3), 5); -- overwrite allocates nothing
      Write_At (Unsigned_64 (2 * blockBytes + 7), 6); -- hole below existing EOF
      declare
         empty : Inode;
         result : Truncate_Status;
      begin
         truncateToEmpty (fs, 1, empty, result);
         pragma Assert (result = Truncate_Complete);
         candidate := empty;
         Check (0);
      end;
   end loop;

   --  The production transformation is total: invalid/replacing mappings are
   --  rejected without changing even unrelated inode fields.
   declare
      Original, Updated : Inode := NULL_INODE;
      Accepted : Boolean;
   begin
      Original.flags := 16#A55A#;
      Original.sizeLo := 123;
      for Pattern in 0 .. 5 loop
         Inode_Mappings.Prepare_Attachment
           (Original,
            (case Pattern is when 0 | 1 => 0, when 2 | 3 => 12,
               when 4 => 268, when others => Unsigned_32'Last),
            (if Pattern = 0 then 0 else 6),
            (if Pattern in 1 | 3 then 6 else 0),
            2, Updated, Accepted);
         pragma Assert (not Accepted and Updated = Original);
      end loop;
      Original.directBlocks (0) := 6;
      Inode_Mappings.Prepare_Attachment (Original, 0, 7, 0, 2, Updated, Accepted);
      pragma Assert (not Accepted and Updated = Original);
      Original.singleIndirectBlock := 8;
      Inode_Mappings.Prepare_Attachment (Original, 12, 7, 9, 2, Updated, Accepted);
      pragma Assert (not Accepted and Updated = Original);
   end;

   declare
      Original, Updated : Inode := NULL_INODE;
      Accepted : Boolean;
   begin
      for Root_Present in Boolean loop
         Original.doubleIndirectBlock := (if Root_Present then 7 else 0);
         for New_Leaf in Boolean loop
            for Case_Number in 0 .. 5 loop
               Double_Mappings.Prepare
                 (Original, (if Case_Number = 1 then 0 else 7),
                  (case Case_Number is when 2 => 0, when 3 => 7, when others => 8),
                  (case Case_Number is when 4 => 0, when 5 => 8, when others => 9),
                  New_Leaf, 8, Updated, Accepted);
               pragma Assert (Accepted = (Case_Number = 0 and (Root_Present or New_Leaf)));
               if Accepted then
                  pragma Assert (Updated.numDiskSectors =
                    8 * (1 + (if Root_Present then 0 else 1) + (if New_Leaf then 1 else 0)));
                  pragma Assert (Updated.doubleIndirectBlock = 7);
               else
                  pragma Assert (Updated = Original);
               end if;
            end loop;
         end loop;
      end loop;
   end;

   --  Every validity rule of the triple transformation, with and without an
   --  existing root: zero/aliased blocks, a new middle without a new leaf and
   --  a new root without a new middle are rejected unchanged.
   declare
      Original, Updated : Inode := NULL_INODE;
      Accepted : Boolean;
      type Block_Set is array (0 .. 3) of Unsigned_32;
      Ids : constant Block_Set := [7, 8, 9, 10];
   begin
      Original.flags := 16#A55A#;
      for Root_Present in Boolean loop
         Original.tripleIndirectBlock := (if Root_Present then 7 else 0);
         for New_Middle in Boolean loop
            for New_Leaf in Boolean loop
               for Case_Number in 0 .. 8 loop
                  declare
                     Blocks : Block_Set := Ids;
                  begin
                     case Case_Number is
                        when 1 .. 4 => Blocks (Case_Number - 1) := 0;
                        when 5 => Blocks (1) := Blocks (0);
                        when 6 => Blocks (2) := Blocks (1);
                        when 7 => Blocks (3) := Blocks (2);
                        when 8 => Blocks (3) := Blocks (0);
                        when others => null;
                     end case;
                     Triple_Mappings.Prepare
                       (Original, Blocks (0), Blocks (1), Blocks (2), Blocks (3),
                        New_Middle, New_Leaf, 8, Updated, Accepted);
                     pragma Assert (Accepted =
                       (Case_Number = 0 and (if New_Middle then New_Leaf) and
                        (Root_Present or New_Middle)));
                     if Accepted then
                        pragma Assert (Updated.numDiskSectors = 8 *
                          (1 + (if Root_Present then 0 else 1) +
                           (if New_Middle then 1 else 0) + (if New_Leaf then 1 else 0)));
                        pragma Assert (Updated = (Original with delta
                          tripleIndirectBlock => 7,
                          numDiskSectors => Updated.numDiskSectors));
                     else
                        pragma Assert (Updated = Original);
                     end if;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
      --  An existing root must be the one supplied.
      Original.tripleIndirectBlock := 11;
      Triple_Mappings.Prepare (Original, 7, 8, 9, 10, True, True, 8, Updated, Accepted);
      pragma Assert (not Accepted and Updated = Original);
   end;

   --  Sparse double trees can be grown, shrunk across leaf boundaries, then
   --  reused. Both byte contents and exact metadata accounting must survive.
   for Geometry in 0 .. 2 loop
      Setup (1024 * 2 ** Geometry);
      declare
         First_Double : constant Unsigned_64 := Unsigned_64 ((12 + blockBytes / 4) * blockBytes);
         Next_Leaf : constant Unsigned_64 := First_Double + Unsigned_64 ((blockBytes / 4) * blockBytes);
         Resized : Inode;
         Result : Truncate_Status;
         Read_Result : Read_Status;
         Buffer : String (1 .. 20);
      begin
         Write_At (First_Double + 7, 3);
         Write_At (First_Double + 11, 3); -- same partial block extension
         Write_At (First_Double + Unsigned_64 (blockBytes) + 7, 4);
         Write_At (Next_Leaf + 7, 6);
         resizeFile (fs, 1, First_Double + 9, Resized, Result);
         pragma Assert (Result = Truncate_Complete);
         candidate := Resized;
         Check (3);
         Write_At (Next_Leaf + 7, 5);
         readData (fs, candidate, First_Double, Buffer'Address, Buffer'Length, written, Read_Result);
         pragma Assert (Read_Result = Read_Complete and written = Buffer'Length);
         pragma Assert (Buffer (1 .. 7) = [1 .. 7 => Character'Val (0)]);
         pragma Assert (Buffer (8 .. 9) = "te");
         pragma Assert (Buffer (10 .. 20) = [10 .. 20 => Character'Val (0)]);
         truncateToEmpty (fs, 1, Resized, Result);
         pragma Assert (Result = Truncate_Complete);
         candidate := Resized;
         Check (0);
      end;
   end loop;

   --  Sparse triple trees: grow, cross leaf and middle boundaries, shrink at
   --  each boundary (partial leaf, whole leaf, whole middle, whole root),
   --  regrow into a retained partial block, and reclaim everything.
   for Geometry in 0 .. 2 loop
      Setup (1024 * 2 ** Geometry);
      declare
         Sb : Superblock with Import, Address => Disk (1024)'Address;
         Per_Block : constant Unsigned_64 := Unsigned_64 (blockBytes / 4);
         Bytes : constant Unsigned_64 := Unsigned_64 (blockBytes);
         First_Triple : constant Unsigned_64 :=
           (12 + Per_Block + Per_Block * Per_Block) * Bytes;
         Next_Leaf : constant Unsigned_64 := First_Triple + Per_Block * Bytes;
         Next_Middle : constant Unsigned_64 :=
           First_Triple + Per_Block * Per_Block * Bytes;
         Resized : Inode;
         Result : Truncate_Status;
         Read_Result : Read_Status;
         Buffer : String (1 .. 20);

         procedure Shrink (Size : Unsigned_64; Allocated : Natural) is
         begin
            resizeFile (fs, 1, Size, Resized, Result);
            pragma Assert (Result = Truncate_Complete and fileSize (Resized) = Size);
            candidate := Resized;
            Check (Allocated);
         end Shrink;
      begin
         --  Triple offsets exceed 2 GiB only at 4 KiB; LARGE_FILE is required.
         Sb.readOnlyFeatures := 2;
         fs.sb.readOnlyFeatures := 2;
         Write_At (First_Triple + 7, 4);        -- root, middle, leaf, data
         Write_At (First_Triple + Bytes + 7, 5); -- same leaf
         Write_At (Next_Leaf + 7, 7);          -- new leaf, same middle
         Write_At (Next_Middle + 7, 10);       -- new middle, leaf and data
         Shrink (Next_Middle + 1, 10);          -- retains a partial block
         Shrink (Next_Middle, 7);               -- whole second middle retired
         Shrink (Next_Leaf, 5);                 -- second leaf retired
         Shrink (First_Triple + 9, 4);          -- partial first leaf
         Write_At (Next_Middle + 7, 7);        -- regrow a middle subtree
         readData (fs, candidate, First_Triple, Buffer'Address, Buffer'Length,
                   written, Read_Result);
         pragma Assert (Read_Result = Read_Complete and written = Buffer'Length);
         pragma Assert (Buffer (1 .. 7) = [1 .. 7 => Character'Val (0)]);
         pragma Assert (Buffer (8 .. 9) = "te");
         pragma Assert (Buffer (10 .. 20) = [10 .. 20 => Character'Val (0)]);
         Shrink (First_Triple, 0);              -- triple root retired
         pragma Assert (candidate.tripleIndirectBlock = 0);
         Write_At (First_Triple + 7, 4);
         Write_At (12 * Bytes, 6);              -- single root and data too
         Shrink (0, 0);
      end;
   end loop;

   --  The last 4 KiB double-tree slot crosses the 32-bit size field. Sparse
   --  growth needs only three real blocks, but must require LARGE_FILE and
   --  preserve the high word through publication and reclamation. The final
   --  triple slot then needs four more; the next byte is unsupported.
   Setup (4096);
   declare
      Limit : constant Unsigned_64 := Block_Paths.First_Triple (8) * 4096;
      Triple_Limit : constant Unsigned_64 := Block_Paths.Block_Limit (8) * 4096;
      Resized : Inode;
      Result : Truncate_Status;
      Sb : Superblock with Import, Address => Disk (1024)'Address;
   begin
      writeData (fs, 1, candidate, Limit - 1, payload'Address, 1, written, status);
      pragma Assert (status = Write_File_Range_Unsupported and Calls = 0 and Writes = 0);
      Sb.readOnlyFeatures := 2;
      fs.sb.readOnlyFeatures := 2;
      writeData (fs, 1, candidate, Limit - 1, payload'Address, 1, written, status);
      pragma Assert (status = Write_Complete and written = 1 and fileSize (candidate) = Limit);
      Check (3);
      --  The very last triple slot: root, middle, leaf and data.
      writeData (fs, 1, candidate, Triple_Limit - 1, payload'Address, 2, written, status);
      pragma Assert (status = Write_File_Range_Unsupported and written = 1);
      pragma Assert (fileSize (candidate) = Triple_Limit);
      Check (7);
      writeData (fs, 1, candidate, Triple_Limit, payload'Address, 1, written, status);
      pragma Assert (status = Write_File_Range_Unsupported and written = 0);
      resizeFile (fs, 1, Triple_Limit + 1, Resized, Result);
      pragma Assert (Result = Truncate_Unsupported);
      truncateToEmpty (fs, 1, Resized, Result);
      pragma Assert (Result = Truncate_Complete and fileSize (Resized) = 0);
      candidate := Resized;
      Check (0);
   end;

   --  Exhaust the representable boundary without needing a huge disk image.
   for Shift in 0 .. 2 loop
      for Blocks in Sector_Accounting.Attached_Blocks loop
         declare
            Sectors : constant Sector_Accounting.Block_Sectors := 2 * 2 ** Shift;
            Delta_Count : constant Unsigned_32 := Sectors * Unsigned_32 (Blocks);
            Updated : Unsigned_32;
            Fits : Boolean;
         begin
            Sector_Accounting.Plan_Addition
              (Unsigned_32'Last - Delta_Count, Sectors, Blocks, Updated, Fits);
            pragma Assert (Fits and Updated = Unsigned_32'Last);
            Sector_Accounting.Plan_Addition
              (Unsigned_32'Last - Delta_Count + 1, Sectors, Blocks, Updated, Fits);
            pragma Assert (not Fits and Updated = Unsigned_32'Last - Delta_Count + 1);
         end;
      end loop;
   end loop;

   --  Patterns 8 .. 10 grow triple trees: a new root/middle/leaf, a new leaf
   --  in an existing middle, and a new middle under an existing root.
   for Geometry in 0 .. 2 loop
      for Pattern in 0 .. 10 loop
         declare
            Size : constant Positive := 1024 * 2 ** Geometry;
            Per_Block : constant Unsigned_64 := Unsigned_64 (Size / 4);
            First_Triple : constant Unsigned_64 :=
              (12 + Per_Block + Per_Block * Per_Block) * Unsigned_64 (Size);
            Offset : constant Unsigned_64 :=
              (case Pattern is
                 when 0 | 3 => 0,
                 when 1 => Unsigned_64 (12 * Size),
                 when 2 => Unsigned_64 (14 * Size),
                 when 4 => Unsigned_64 (11 * Size),
                 when 5 => Unsigned_64 ((12 + Size / 4) * Size),
                 when 6 => Unsigned_64 ((13 + Size / 4) * Size),
                 when 7 => Unsigned_64 ((12 + 2 * (Size / 4)) * Size),
                 when 8 => First_Triple,
                 when 9 => First_Triple + Per_Block * Unsigned_64 (Size),
                 when others =>
                   First_Triple + Per_Block * Per_Block * Unsigned_64 (Size));
            Length : constant Unsigned_64 :=
              (if Pattern in 3 .. 4 then Unsigned_64 (Size * 2) else 4);
            Existing : constant Natural :=
              (case Pattern is when 2 => 2, when 6 | 7 => 3, when 9 | 10 => 4,
                 when others => 0);
            Added : constant Natural :=
              (case Pattern is when 1 | 3 | 7 | 9 => 2, when 4 | 5 | 10 => 3,
                 when 8 => 4, when others => 1);
            Baseline : Natural;

            procedure Prepare is
               Sb : Superblock with Import, Address => Disk (1024)'Address;
            begin
               Setup (Size);
               if Pattern >= 8 then
                  Sb.readOnlyFeatures := 2; -- 4 KiB triple offsets exceed 2 GiB
                  fs.sb.readOnlyFeatures := 2;
               end if;
               if Pattern = 2 then
                  Write_At (Unsigned_64 (12 * Size), 2);
               elsif Pattern in 6 .. 7 then
                  Write_At (Unsigned_64 ((12 + Size / 4) * Size), 3);
               elsif Pattern >= 9 then
                  Write_At (First_Triple, 4);
               end if;
               if Existing > 0 then
                  Calls := 0;
                  Writes := 0;
               end if;
            end Prepare;
         begin
            Prepare;
            writeData (fs, 1, candidate, Offset, longPayload'Address,
                       Length, written, status);
            pragma Assert (status = Write_Complete and written = Length);
            Check (Existing + Added);
            Baseline := Calls;
            if Pattern in 1 | 2 | 5 .. 10 then
               declare
                  Output : String (1 .. 4) := [others => '?'];
                  Before_Fresh : Natural;
                  Read_Count : Unsigned_64;
                  Read_Result : Read_Status;
               begin
                  --  Successful publication already supplies the complete
                  --  pointer block(s): the very next lookup needs data only.
                  readData (fs, candidate, Offset, Output'Address, 4,
                            Read_Count, Read_Result);
                  pragma Assert (Read_Result = Read_Complete and Read_Count = 4);
                  --  The payload too may already be cached (small
                  --  transfers go through the block cache).
                  pragma Assert (Output = "SSSS" and Calls <= Baseline + 1);
                  --  Same block numbers on another endpoint must not reuse
                  --  this volume's warm mappings. Reload each pointer level.
                  --  A never-used endpoint: the block cache outlives Setup.
                  Fresh_Endpoint := Fresh_Endpoint + 1;
                  fs.device.endpointSlot := Fresh_Endpoint;
                  Before_Fresh := Calls;
                  readData (fs, candidate, Offset, Output'Address, 4,
                            Read_Count, Read_Result);
                  pragma Assert (Read_Result = Read_Complete and Read_Count = 4);
                  --  Every pointer level again, and the payload.
                  pragma Assert (Output = "SSSS" and
                    Calls = Before_Fresh + (case Pattern is when 1 | 2 => 2,
                                              when 5 .. 7 => 3, when others => 4));
               end;
            end if;
            for Boundary in 1 .. Baseline loop
               for Treatment in Failure_Mode loop
                  for Reply_Kind in Failure_Reply loop
                     Prepare;
                     beforeFault := Disk;
                     Fail_At := Boundary;
                     Mode := Treatment;
                     Reply_Style := Reply_Kind;
                     writeData (fs, 1, candidate, Offset, longPayload'Address,
                                Length, written, status);
                     pragma Assert (Failed and Calls = Boundary and written = 0);
                     pragma Assert (status /= Write_Complete);
                     if fs.writeQuarantined then
                        pragma Assert (status = Write_Recovery_Required);
                        writeData (fs, 1, candidate, Offset, payload'Address,
                                   payload'Length, written, status);
                        pragma Assert (Calls = Boundary and status = Write_Recovery_Required);
                     else
                        --  A growth-gap zero write may finish before the
                        --  allocator's first metadata read fails. It changes
                        --  no visible data or reservation; this fixture's tail
                        --  was already zero, so require the WHOLE disk unchanged.
                        pragma Assert (Disk = beforeFault);
                        if Writes > 0 then
                           pragma Assert (Existing > 0 and status = Write_Device_Error);
                        end if;
                        Check (Existing);
                     end if;
                     if Pattern in 1 | 2 | 5 .. 10 then
                        Check_Cache_Against_Disk (Offset);
                     end if;
                     faultCases := faultCases + 1;
                  end loop;
               end loop;
            end loop;

            Prepare;
            candidate.numDiskSectors := Unsigned_32'Last -
              Unsigned_32 (Size / 512 * (case Pattern is when 1 | 7 | 9 => 2,
                 when 5 | 10 => 3, when 8 => 4, when others => 1)) + 1;
            writeData (fs, 1, candidate, Offset, payload'Address,
                       payload'Length, written, status);
            pragma Assert (status = Write_Out_Of_Range and written = 0 and Writes = 0);
            pragma Assert (fs.sb.freeBlocks = initialFree - Unsigned_32 (Existing));
         end;
      end loop;
   end loop;

   --  Data reservation succeeds, but no space remains for an indirect root.
   --  This is a definite, unpublished failure, so checked reclaim is allowed.
   --  Scenarios 4 .. 6 exhaust space before the triple root, middle or leaf:
   --  every earlier unpublished reservation is reclaimed in reverse.
   for Scenario in 1 .. 6 loop
   declare
      Free_Count : constant Positive :=
        (case Scenario is when 3 | 5 => 2, when 6 => 3, when others => 1);
      Offset : constant Unsigned_64 :=
        (case Scenario is when 1 => 12 * 1024, when 2 | 3 => 268 * 1024,
           when others => Block_Paths.First_Triple (2) * 1024);
   begin
   Limited_Space (Free_Count);
   writeData (fs, 1, candidate, Offset, payload'Address,
              payload'Length, written, status);
   pragma Assert (status = Write_No_Space and written = 0 and not fs.writeQuarantined);
   Check (0);
   declare
      Baseline : constant Natural := Calls;
   begin
      for Boundary in 1 .. Baseline loop
         for Treatment in Failure_Mode loop
            for Reply_Kind in Failure_Reply loop
               Limited_Space (Free_Count);
               Fail_At := Boundary;
               Mode := Treatment;
               Reply_Style := Reply_Kind;
               writeData (fs, 1, candidate, Offset, payload'Address,
                          payload'Length, written, status);
               pragma Assert (Failed and Calls = Boundary and written = 0);
               if Writes > 0 then
                  pragma Assert (fs.writeQuarantined and status = Write_Recovery_Required);
               else
                  Check (0);
               end if;
               faultCases := faultCases + 1;
            end loop;
         end loop;
      end loop;
   end;
   end;
   end loop;
   Ada.Text_IO.Put_Line ("Sector attachment fault cases:" & faultCases'Image & " PASS");
   Ada.Text_IO.Put_Line ("SECTOR-ACCOUNTING-CHECK: PASS");
end Sector_Counts;
