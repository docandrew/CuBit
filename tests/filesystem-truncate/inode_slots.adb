with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with Volume_Admission; use Volume_Admission;
with CuBit.Messages; use CuBit.Messages;

procedure Inode_Slots is
   Fs : Filesystem;
   SB : Superblock with Import, Address => Disk (1024)'Address;
   BGD : BlockGroupDescriptor with Import, Address => Disk (4096)'Address;
   Number : Unsigned_32;
   Status : Write_Status;
   Admission : Admission_Result;
   Original : Bytes (Disk'Range);
   Baseline : Natural;
   Cases : Natural := 0;
   Table : constant := 5 * 4096;

   procedure Setup (Stride : Positive) is
   begin
      Reset;
      Check_Reclamation := False;
      SB.signature := EXT2_SIGNATURE;
      SB.blockShift := 2;
      SB.blockCount := 16;
      SB.blocksPerBlockGroup := 16;
      SB.inodeCount := 8;
      SB.inodesPerBlockGroup := 8;
      SB.freeInodes := 6;
      SB.majorVersion := 1;
      SB.inodeSize := Unsigned_16 (Stride);
      SB.firstNonReservedInode := 3;
      SB.incompatibleFeatures := 2;
      BGD := (blockBitmapAddr => 3, inodeBitmapAddr => 4, inodeTableAddr => 5,
              numFreeBlocks => 0, numFreeInodes => 6, numDirectories => 0,
              padding => 0, reserved => 0);
      Disk (4 * 4096) := 3;
      Disk (Table .. Table + 8 * Stride - 1) := [others => 16#A5#];
      Original := Disk;
      initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                       Grant_Buffer'Address, Grant_Buffer'Length, Admission);
      pragma Assert (Admission = Admitted);
      Calls := 0;
      Writes := 0;
   end Setup;
begin
   for Power in 7 .. 12 loop
      declare
         Stride : constant Positive := 2 ** Power;
         First : constant Natural := Table + 2 * Stride;
         Last : constant Natural := First + Stride - 1;
      begin
         Setup (Stride);
         allocateInode (Fs, Number, Status);
         pragma Assert (Status = Write_Complete and Number = 3);
         pragma Assert (Disk (First .. Last) = Bytes'[First .. Last => 0]);
         pragma Assert (Disk (Table .. First - 1) = Original (Table .. First - 1));
         pragma Assert
           (Disk (Last + 1 .. Table + 8 * Stride - 1) =
            Original (Last + 1 .. Table + 8 * Stride - 1));
         Baseline := Calls;
         for Boundary in 1 .. Baseline loop
            for Treatment in Failure_Mode loop
               for Reply_Kind in Failure_Reply loop
                  Setup (Stride);
                  Fail_At := Boundary;
                  Mode := Treatment;
                  Reply_Style := Reply_Kind;
                  allocateInode (Fs, Number, Status);
                  pragma Assert (Failed and Calls = Boundary and Number = 0);
                  if Writes > 0 then
                     pragma Assert (Status = Write_Recovery_Required and Fs.writeQuarantined);
                     allocateInode (Fs, Number, Status);
                     pragma Assert (Status = Write_Recovery_Required and Calls = Boundary);
                  else
                     pragma Assert (Disk = Original);
                  end if;
                  Cases := Cases + 1;
               end loop;
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("INODE-SLOT-CHECK: PASS" & Cases'Image & " injected failures");
end Inode_Slots;
