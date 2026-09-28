with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Sector_Accounting;
with Volume_Admission; use Volume_Admission;

procedure Resizing is
   use type Inode;
   Fs : Filesystem;
   Sb : Superblock with Import, Address => Disk (1024)'Address;
   Bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
   Item : Inode with Import, Address => Disk (5120)'Address;
   Pointers : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (21 * 1024)'Address;
   Result : Inode;
   Status : Truncate_Status;
   WStatus : Write_Status;
   RStatus : Read_Status;
   Completed : Unsigned_64;
   Buffer : String (1 .. 16 * 1024);
   Payload : constant String := "test";
   Faults : Natural := 0;
   Old_Disk : Bytes (Disk'Range);
   Sizes : constant array (Positive range <>) of Unsigned_64 :=
     [0, 17, 1024, 1025, 12 * 1024, 12 * 1024 + 17, 13 * 1024,
      14 * 1024, 15 * 1024 + 33];

   procedure Setup (Short : Boolean := False) is
      Admission : Admission_Result;
   begin
      Reset;
      Check_Reclamation := False;
      Check_Resize_Reclamation := True;
      Sb.signature := EXT2_SIGNATURE;
      Sb.inodeCount := 8;
      Sb.blockCount := 64;
      Sb.freeBlocks := 39;
      Sb.firstDataBlock := 1;
      Sb.blocksPerBlockGroup := 64;
      Sb.inodesPerBlockGroup := 8;
      Sb.majorVersion := 1;
      Sb.incompatibleFeatures := 2;
      Sb.inodeSize := 128;
      Bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
              inodeTableAddr => 5, numFreeBlocks => 39,
              numFreeInodes => 0, numDirectories => 0,
              padding => 0, reserved => 0);
      Disk (3072 .. 3074) := [others => 255];
      Disk (3079) := 128;
      Item := NULL_INODE;
      Item.typeAndPermissions := 16#8000#;
      Item.numHardLinks := 1;
      Item.sizeLo := (if Short then 17 else 14 * 1024);
      Item.numDiskSectors := 10;
      Item.directBlocks (0) := 20;
      Item.directBlocks (1) := 23;
      Item.singleIndirectBlock := 21;
      Pointers (0) := 22;
      Pointers (1) := 24;
      Disk (20 * 1024 .. 21 * 1024 - 1) := [others => Character'Pos ('A')];
      Disk (22 * 1024 .. 25 * 1024 - 1) := [others => Character'Pos ('B')];
      initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                       Grant_Buffer'Address, Grant_Buffer'Length, Admission);
      pragma Assert (Admission = Admitted);
      Calls := 0;
      Durable := Disk;
   end Setup;

   procedure Check_Contents (Length : Unsigned_64; Prefix : Positive) is
   begin
      readData (Fs, Result, 0, Buffer'Address, Length, Completed, RStatus);
      pragma Assert (RStatus = Read_Complete and Completed = Length);
      pragma Assert (Buffer (1 .. Prefix) = String'(1 .. Prefix => 'A'));
      for I in Prefix + 1 .. Natural (Length) loop
         pragma Assert (Buffer (I) = Character'Val (0));
      end loop;
   end Check_Contents;
begin
   --  Exhaustive supported subtraction counts, including rejected underflow.
   for Geometry in 0 .. 2 loop
      for Count in Sector_Accounting.Retired_Blocks loop
         declare
            S : constant Sector_Accounting.Block_Sectors := 2 ** (Geometry + 1);
            Removed : constant Unsigned_32 := S * Unsigned_32 (Count);
            Updated : Unsigned_32;
            Fits : Boolean;
         begin
            Sector_Accounting.Plan_Removal (Removed, S, Count, Updated, Fits);
            pragma Assert (Fits and Updated = 0);
            if Count > 0 then
               Sector_Accounting.Plan_Removal (Removed - 1, S, Count, Updated, Fits);
               pragma Assert (not Fits and Updated = Removed - 1);
            end if;
         end;
      end loop;
   end loop;

   for Short in Boolean loop
      for Size of Sizes loop
         Setup (Short);
         resizeFile (Fs, 1, Size, Result, Status);
         pragma Assert (Status = Truncate_Complete and fileSize (Result) = Size);
         pragma Assert (Result = Item);
         if Size = (if Short then 17 else 14 * 1024) then
            pragma Assert (Writes = 0 and Barriers = 0);
         end if;
         declare
            Baseline : constant Natural := Calls;
         begin
            for Boundary in 1 .. Baseline loop
               for Treatment in Failure_Mode loop
                  for Reply_Kind in Failure_Reply loop
                     --  FLUSH has no transfer count: zero is a valid reply,
                     --  so Short_Transfer is not an injectable flush error.
                     if Reply_Kind /= Short_Transfer then
                        Setup (Short);
                        Old_Disk := Disk;
                        Fail_At := Boundary;
                        CuBit.Messages.Mode := Treatment;
                        Reply_Style := Reply_Kind;
                        resizeFile (Fs, 1, Size, Result, Status);
                        pragma Assert (Failed and Calls = Boundary);
                        pragma Assert (Status /= Truncate_Complete and Result = NULL_INODE);
                        if Fs.writeQuarantined then
                           pragma Assert (Status = Truncate_Recovery_Required);
                           resizeFile (Fs, 1, 0, Result, Status);
                           pragma Assert (Calls = Boundary and Status = Truncate_Recovery_Required);
                        elsif Writes = 0 then
                           pragma Assert (Disk = Old_Disk);
                        else
                           --  A failed read after completed zeroing beyond EOF
                           --  changes no visible bytes and publishes no size.
                           pragma Assert (Status = Truncate_IO_Error);
                           pragma Assert (Item.sizeLo = (if Short then 17 else 14 * 1024));
                        end if;
                        Faults := Faults + 1;
                     end if;
                  end loop;
               end loop;
            end loop;
         end;
      end loop;
   end loop;

   Setup;
   resizeFile (Fs, 1, 17, Result, Status);
   pragma Assert (Status = Truncate_Complete and Result.numDiskSectors = 2);
   pragma Assert (Result.singleIndirectBlock = 0 and Sb.freeBlocks = 43);
   resizeFile (Fs, 1, 15 * 1024, Result, Status);
   pragma Assert (Status = Truncate_Complete and Result.numDiskSectors = 2);
   Check_Contents (15 * 1024, 17);

   Setup;
   resizeFile (Fs, 1, 17, Result, Status);
   writeData (Fs, 1, Result, 513, Payload'Address, Payload'Length, Completed, WStatus);
   pragma Assert (WStatus = Write_Complete and Completed = Payload'Length);
   Check_Contents (513, 17);
   readData (Fs, Result, 513, Buffer'Address, 4, Completed, RStatus);
   pragma Assert (RStatus = Read_Complete and Buffer (1 .. 4) = Payload);

   Setup (True);
   resizeFile (Fs, 1, 15 * 1024, Result, Status);
   pragma Assert (Status = Truncate_Complete and Result.numDiskSectors = 10);
   Check_Contents (15 * 1024, 17);

   --  Positioned writes share zero-exposure semantics, including failures
   --  between zeroing the gap and publishing payload/size. Nothing may issue
   --  another transport command after a rejected completion.
   Setup (True);
   Result := Item;
   writeData (Fs, 1, Result, 513, Payload'Address, 4, Completed, WStatus);
   pragma Assert (WStatus = Write_Complete and Completed = 4);
   declare
      Baseline : constant Natural := Calls;
   begin
      for Boundary in 1 .. Baseline loop
         for Treatment in Failure_Mode loop
            for Reply_Kind in Failure_Reply loop
               Setup (True);
               Result := Item;
               Fail_At := Boundary;
               CuBit.Messages.Mode := Treatment;
               Reply_Style := Reply_Kind;
               writeData (Fs, 1, Result, 513, Payload'Address, 4, Completed, WStatus);
               pragma Assert (Failed and Calls = Boundary and Completed = 0);
               pragma Assert (WStatus /= Write_Complete);
               pragma Assert (Disk (20 * 1024 .. 20 * 1024 + 16) =
                 Bytes'(0 .. 16 => Character'Pos ('A')));
               if Fs.writeQuarantined then
                  pragma Assert (WStatus = Write_Recovery_Required);
                  writeData (Fs, 1, Result, 0, Payload'Address, 4, Completed, WStatus);
                  pragma Assert (Calls = Boundary and WStatus = Write_Recovery_Required);
               else
                  pragma Assert (Item.sizeLo = 17 and Item.numDiskSectors = 10);
               end if;
               Faults := Faults + 1;
            end loop;
         end loop;
      end loop;
   end;

   Setup;
   resizeFile (Fs, 1, (12 + 256) * 1024, Result, Status);
   pragma Assert (Status = Truncate_Complete and Result.numDiskSectors = 10);
   pragma Assert (Sb.freeBlocks = 39);
   readData (Fs, Result, (12 + 256) * 1024 - 1, Buffer'Address, 1, Completed, RStatus);
   pragma Assert (RStatus = Read_Complete and Completed = 1 and Buffer (1) = Character'Val (0));

   Setup;
   Fs.device.description.features := FEATURE_READ_ONLY;
   resizeFile (Fs, 1, 17, Result, Status);
   pragma Assert (Status = Truncate_Read_Only and Calls = 0);
   Result := Item;
   writeData (Fs, 1, Result, 16 * 1024, Payload'Address, 4, Completed, WStatus);
   pragma Assert (WStatus = Write_Read_Only and Calls = 0 and not Fs.writeQuarantined);
   Setup;
   Fs.device.description.features := 0;
   resizeFile (Fs, 1, 17, Result, Status);
   pragma Assert (Status = Truncate_Durability_Unsupported and Calls = 0);
   Setup;
   Fs.device.description.features := FEATURE_VOLATILE;
   Check_Resize_Reclamation := False;
   resizeFile (Fs, 1, 17, Result, Status);
   pragma Assert (Status = Truncate_Complete and Barriers = 0);

   Setup;
   Old_Disk := Disk;
   resizeFile (Fs, 1, Unsigned_64'Last, Result, Status);
   pragma Assert (Status = Truncate_Unsupported and Writes = 0 and Disk = Old_Disk);
   Setup;
   Item.numDiskSectors := 9;
   resizeFile (Fs, 1, 0, Result, Status);
   pragma Assert (Status = Truncate_Invalid and Writes = 0);
   Put_Line ("EXT2-RESIZE-CHECK: PASS" & Faults'Image & " injected failures");
end Resizing;
