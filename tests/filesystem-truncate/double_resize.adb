with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;
with Block_Inventory;

procedure Double_Resize is
   use type Inode;
   Fs : Filesystem;
   Result : Inode;
   Status : Truncate_Status;
   Faults : Natural := 0;
begin
   --  Independent counting reference checks both ordering and permutation,
   --  including duplicates, arbitrary permutations and extreme block IDs.
   for Length in 0 .. 255 loop
      declare
         Values : Block_Inventory.Block_Array (1 .. Length);
         Expected : array (0 .. 16) of Natural := [others => 0];
         Actual : array (0 .. 16) of Natural := [others => 0];
         Unique : Boolean;
      begin
         for I in Values'Range loop
            Values (I) := Unsigned_32 ((I * 13 + Length * 7) mod 17);
            Expected (Natural (Values (I))) := Expected (Natural (Values (I))) + 1;
         end loop;
         Block_Inventory.Sort_And_Check (Values, Unique);
         for Value of Values loop
            Actual (Natural (Value)) := Actual (Natural (Value)) + 1;
         end loop;
         for I in Expected'Range loop
            pragma Assert (Expected (I) = Actual (I));
         end loop;
         pragma Assert (Unique = (Length <= 17));
      end;
   end loop;

   --  Exercise the largest admitted inventory, including IDs near U32'Last.
   --  The expected permutation is independent of the sorting implementation.
   declare
      Values : Block_Inventory.Block_Array (1 .. Block_Inventory.Maximum_Blocks);
      Unique : Boolean;
   begin
      for I in Values'Range loop
         Values (I) := Unsigned_32'Last - Unsigned_32 (I - 1);
      end loop;
      Block_Inventory.Sort_And_Check (Values, Unique);
      pragma Assert (Unique);
      for I in Values'Range loop
         pragma Assert (Values (I) = Unsigned_32'Last - Unsigned_32 (Values'Last - I));
      end loop;
      Values (1) := Values (Values'Last);
      Block_Inventory.Sort_And_Check (Values, Unique);
      pragma Assert (not Unique);
   end;

   for Geometry in 0 .. 2 loop
      declare
         Size : constant Positive := 1024 * 2 ** Geometry;
         Pointers : constant Unsigned_64 := Unsigned_64 (Size / 4);
         First_Double : constant Unsigned_64 := 12 + Pointers;
         Sb : Superblock with Import, Address => Disk (1024)'Address;
         Bgd : BlockGroupDescriptor with Import,
           Address => Disk ((if Size = 1024 then 2 else 1) * Size)'Address;
         Item : Inode with Import, Address => Disk (5 * Size)'Address;
         Root : array (0 .. Size / 4 - 1) of Unsigned_32
           with Import, Address => Disk (6 * Size)'Address;
         Leaf : array (Root'Range) of Unsigned_32
           with Import, Address => Disk (7 * Size)'Address;
         Other : array (Root'Range) of Unsigned_32
           with Import, Address => Disk (12 * Size)'Address;
         Single : array (Root'Range) of Unsigned_32
           with Import, Address => Disk (10 * Size)'Address;
         Sizes : constant array (Positive range <>) of Unsigned_64 :=
           [0, 17, 12 * Unsigned_64 (Size) + 17,
            First_Double * Unsigned_64 (Size) + 17,
            (First_Double + 1) * Unsigned_64 (Size) + 17,
            (First_Double + Pointers) * Unsigned_64 (Size),
            (First_Double + Pointers + 2) * Unsigned_64 (Size)];

         procedure Setup is
            Admission : Admission_Result;
         begin
            Reset;
            Check_Reclamation := False;
            Check_Resize_Reclamation := True;
            Reclamation_Block_Bytes := Size;
            Reclamation_First_Block := (if Size = 1024 then 1 else 0);
            Sb.signature := EXT2_SIGNATURE;
            Sb.blockShift := Unsigned_32 (Geometry);
            Sb.inodeCount := 8;
            Sb.blockCount := Unsigned_32 (Disk'Length / Size);
            Sb.freeBlocks := Sb.blockCount - 15;
            Sb.firstDataBlock := Unsigned_32 (Reclamation_First_Block);
            Sb.blocksPerBlockGroup := Sb.blockCount;
            Sb.inodesPerBlockGroup := 8;
            Sb.majorVersion := 1;
            Sb.incompatibleFeatures := 2;
            Sb.readOnlyFeatures := 2;
            Sb.inodeSize := 128;
            Bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
                    inodeTableAddr => 5, numFreeBlocks => Unsigned_16 (Sb.freeBlocks),
                    numFreeInodes => 0, numDirectories => 0, padding => 0, reserved => 0);
            Disk (3 * Size .. 4 * Size - 1) := [others => 255];
            for B in 15 .. Natural (Sb.blockCount) - 1 loop
               declare
                  Bit : constant Natural := B - Reclamation_First_Block;
               begin
                  Disk (3 * Size + Bit / 8) := Disk (3 * Size + Bit / 8) and
                    not Shift_Left (Unsigned_8 (1), Bit mod 8);
               end;
            end loop;
            Item := NULL_INODE;
            Item.typeAndPermissions := 16#8000#;
            Item.numHardLinks := 1;
            Item.sizeLo := Unsigned_32 ((First_Double + Pointers + 1) * Unsigned_64 (Size));
            Item.numDiskSectors := Unsigned_32 (9 * Size / 512);
            Item.directBlocks (0) := 9;
            Item.singleIndirectBlock := 10;
            Single (0) := 11;
            Item.doubleIndirectBlock := 6;
            Root (0) := 7;
            Root (1) := 12;
            Leaf (0) := 8;
            Leaf (1) := 14;
            Other (0) := 13;
            initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                             Grant_Buffer'Address, Grant_Buffer'Length, Admission);
            pragma Assert (Admission = Admitted);
            Calls := 0;
            Writes := 0;
            Durable := Disk;
         end Setup;
      begin
         for New_Size of Sizes loop
            Setup;
            resizeFile (Fs, 1, New_Size, Result, Status);
            pragma Assert (Status = Truncate_Complete and fileSize (Result) = New_Size);
            pragma Assert (Result = Item);
            declare
               Baseline : constant Natural := Calls;
            begin
               for Boundary in 1 .. Baseline loop
                  for Treatment in Failure_Mode loop
                     for Reply_Kind in Failure_Reply loop
                        if Reply_Kind /= Short_Transfer then
                           Setup;
                           Fail_At := Boundary;
                           CuBit.Messages.Mode := Treatment;
                           Reply_Style := Reply_Kind;
                           resizeFile (Fs, 1, New_Size, Result, Status);
                           pragma Assert (Failed and Calls = Boundary and Result = NULL_INODE);
                           pragma Assert (Status /= Truncate_Complete);
                           if Writes > 0 then
                              pragma Assert (Fs.writeQuarantined and Status = Truncate_Recovery_Required);
                              resizeFile (Fs, 1, 0, Result, Status);
                              pragma Assert (Calls = Boundary and Status = Truncate_Recovery_Required);
                           end if;
                           Faults := Faults + 1;
                        end if;
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
         Setup;
         resizeFile (Fs, 1, 0, Result, Status);
         pragma Assert (Status = Truncate_Complete and Item.numDiskSectors = 0);
         pragma Assert (Sb.freeBlocks = Sb.blockCount - 6);

         --  Aliases across leaves and tree levels are caught before mutation.
         for Corruption in 1 .. 5 loop
            Setup;
            case Corruption is
               when 1 => Other (0) := 8;
               when 2 => Other (0) := 7;
               when 3 => Root (1) := Root (0);
               when 4 => Item.numDiskSectors := Unsigned_32'Last;
               when others => Leaf (0) := Sb.blockCount;
            end case;
            resizeFile (Fs, 1, 0, Result, Status);
            pragma Assert (Status = Truncate_Invalid and Writes = 0);
         end loop;
      end;
   end loop;
   Put_Line ("DOUBLE-RESIZE-CHECK: PASS" & Faults'Image & " injected failures");
end Double_Resize;
