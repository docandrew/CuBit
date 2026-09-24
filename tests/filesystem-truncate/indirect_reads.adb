with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with Volume_Admission; use Volume_Admission;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
procedure Indirect_Reads is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   ino : Inode := NULL_INODE;
   single : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (21 * 1024)'Address;
   first : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (23 * 1024)'Address;
   second : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (24 * 1024)'Address;
   alternate : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (26 * 1024)'Address;
   alternateLeaf : array (0 .. 255) of Unsigned_32
     with Import, Address => Disk (27 * 1024)'Address;
   output : String (1 .. 4096) := [others => '?'];
   avolume : Unsigned_64;
   status : Read_Status;
   writeStatus : Write_Status;
   admission : Admission_Result;
   original : Bytes (Disk'Range);
   baseline, previous : Natural;
   Single_Offset : constant Unsigned_64 := 12 * 1024;
   Double_Offset : constant Unsigned_64 := (12 + 256) * 1024;

   procedure Setup is
   begin
      Reset;
      sb.signature := EXT2_SIGNATURE;
      sb.blockCount := 64;
      sb.firstDataBlock := 1;
      sb.blocksPerBlockGroup := 64;
      sb.inodeCount := 8;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.inodeSize := 128;
      --  Use the real volume admission path to reset volume/cache lifetime.
      initBlockDevice
        (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
         Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      ino := NULL_INODE;
      ino.typeAndPermissions := 16#8000#;
      ino.sizeLo := Unsigned_32 (Double_Offset + 1024);
      ino.directBlocks (11) := 20;
      ino.singleIndirectBlock := 21;
      single (0) := 22;
      ino.doubleIndirectBlock := 23;
      first (0) := 24;
      second (0) := 25;
      Disk (20 * 1024 .. 21 * 1024 - 1) := [others => Character'Pos ('D')];
      Disk (22 * 1024 .. 23 * 1024 - 1) := [others => Character'Pos ('S')];
      Disk (25 * 1024 .. 26 * 1024 - 1) := [others => Character'Pos ('T')];
      output := [others => '?'];
      Check_Reclamation := False;
   end Setup;

   procedure Retry is
   begin
      --  Clear only the transport fault: do not reset the Ext2 caches.
      Failed := False;
      Fail_At := 0;
      output := [others => '?'];
   end Retry;

   procedure Check_Data (offset : Unsigned_64; value : Character) is
   begin
      readData (fs, ino, offset, output'Address, 1024, avolume, status);
      pragma Assert (status = Read_Complete and avolume = 1024);
      pragma Assert (output (1 .. 1024) = [1 .. 1024 => value]);
      pragma Assert (output (1025 .. 4096) = [1025 .. 4096 => '?']);
   end Check_Data;
begin
   for doubleIndirect in Boolean loop
      declare
         offset : constant Unsigned_64 :=
           (if doubleIndirect then Double_Offset else Single_Offset);
         value : constant Character := (if doubleIndirect then 'T' else 'S');
      begin
         Setup;
         Check_Data (offset, value);
         baseline := Calls;
         pragma Assert (baseline = (if doubleIndirect then 3 else 2));
         Check_Data (offset, value);
         pragma Assert (Calls = baseline + 1); -- warm cache: only payload I/O
         for boundary in 1 .. baseline loop
            for treatment in Failure_Mode loop
               for style in Failure_Reply loop
                  Setup;
                  Fail_At := boundary;
                  CuBit.Messages.Mode := treatment;
                  Reply_Style := style;
                  readData (fs, ino, offset, output'Address, 1024, avolume, status);
                  pragma Assert (Calls = boundary and Failed);
                  pragma Assert (status = Read_Device_Error and avolume = 0);
                  pragma Assert (output = [output'Range => '?']);
                  pragma Assert (Writes = 0 and not fs.writeQuarantined);
                  Retry;
                  Check_Data (offset, value);
               end loop;
            end loop;
         end loop;
      end;
   end loop;
   Put_Line ("Single/double lookup and payload failures: 75 cases PASS");

   --  Evict a warm cache with a two-transfer pointer-block read; fail AFTER
   --  its first half overwrites the buffer, then revisit the old cache key.
   --  Test single, double-root and double-leaf caches independently.
   for level in 1 .. 3 loop
      Setup;
      fs.device.grantBytes := 512;
      fs.device.description.maxTransferBlocks := 1;
      if level = 1 then
         Check_Data (Single_Offset, 'S');
         alternate (0) := 28;
         ino.singleIndirectBlock := 26;
      else
         Check_Data (Double_Offset, 'T');
         alternate (0) := 27;
         alternateLeaf (0) := 28;
         ino.doubleIndirectBlock := 26;
      end if;
      Disk (28 * 1024 .. 29 * 1024 - 1) := [others => Character'Pos ('U')];
      output := [others => '?'];
      Fail_At := Calls + (if level = 3 then 4 else 2);
      readData (fs, ino, (if level = 1 then Single_Offset else Double_Offset),
                output'Address, 1024, avolume, status);
      pragma Assert (status = Read_Device_Error and avolume = 0);
      pragma Assert (output = [output'Range => '?']);
      Retry;
      ino.singleIndirectBlock := 21;
      ino.doubleIndirectBlock := 23;
      Check_Data ((if level = 1 then Single_Offset else Double_Offset),
                  (if level = 1 then 'S' else 'T'));
      Retry;
      if level = 1 then
         ino.singleIndirectBlock := 26;
      else
         ino.doubleIndirectBlock := 26;
      end if;
      Check_Data ((if level = 1 then Single_Offset else Double_Offset), 'U');
   end loop;
   Put_Line ("Failed partial-fill eviction never retains either cache key: PASS");

   for speculative in Boolean loop
      Setup;
      if not speculative then
         fs.device.grantBytes := 1024; -- complete direct block before lookup
      end if;
      Fail_At := (if speculative then 1 else 2);
      readData (fs, ino, Single_Offset - 1024, output'Address, 2048, avolume, status);
      pragma Assert (status = Read_Device_Error and Calls = Fail_At);
      pragma Assert (avolume = (if speculative then 0 else 1024));
      if speculative then
         pragma Assert (output = [output'Range => '?']);
      else
         pragma Assert (output (1 .. 1024) = [1 .. 1024 => 'D']);
         pragma Assert (output (1025 .. 4096) = [1025 .. 4096 => '?']);
      end if;
   end loop;
   Put_Line ("Completed-prefix and speculative lookup failures: PASS");

   Setup;
   Fail_At := 1;
   original := Disk;
   writeData (fs, 1, ino, Single_Offset, output'Address, 1, avolume, writeStatus);
   pragma Assert (writeStatus = Write_Device_Error and avolume = 0);
   pragma Assert (Calls = 1 and Writes = 0 and Disk = original);
   pragma Assert (not fs.writeQuarantined);

   --  A real zero pointer is a hole. Failed/invalid pointers are not.
   for hole in 1 .. 4 loop
      Setup;
      case hole is
         when 1 => ino.singleIndirectBlock := 0;
         when 2 => single (0) := 0;
         when 3 => first (0) := 0;
         when 4 => second (0) := 0;
         when others => null;
      end case;
      Check_Data ((if hole <= 2 then Single_Offset else Double_Offset),
                  Character'Val (0));
   end loop;
   for invalid in 1 .. 5 loop
      Setup;
      case invalid is
         when 1 => ino.directBlocks (11) := 64;
         when 2 => single (0) := 64;
         when 3 => ino.doubleIndirectBlock := 64;
         when 4 => first (0) := 64;
         when 5 => second (0) := 64;
         when others => null;
      end case;
      readData (fs, ino, (if invalid = 1 then Single_Offset - 1024
                         elsif invalid = 2 then Single_Offset
                         else Double_Offset),
                output'Address, 1024, avolume, status);
      pragma Assert (status = Read_Out_Of_Range and avolume = 0);
      pragma Assert (output = [output'Range => '?']);
   end loop;
   Setup;
   single (0) := 64;
   original := Disk;
   writeData (fs, 1, ino, Single_Offset, output'Address, 1, avolume, writeStatus);
   pragma Assert (writeStatus = Write_Out_Of_Range and avolume = 0);
   pragma Assert (Writes = 0 and Disk = original);
   Put_Line ("Sparse holes, malformed mappings and no-allocation-on-read-error: PASS");

   Setup;
   single (255) := 22;
   Check_Data (Double_Offset - 1024, 'S');
   first (255) := 24;
   second (255) := 25;
   --  Reset volume caches after deliberately changing the fixture in place.
   initBlockDevice
     (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
      Grant_Buffer'Length, admission);
   pragma Assert (admission = Admitted);
   ino.sizeLo := Unsigned_32 ((12 + 256 + 256 * 256 + 1) * 1024);
   Check_Data (Unsigned_64 (ino.sizeLo) - 2048, 'T');
   output := [others => '?'];
   readData (fs, ino, Unsigned_64 (ino.sizeLo) - 1024,
             output'Address, 1024, avolume, status);
   pragma Assert (status = Read_File_Range_Unsupported and avolume = 0);
   pragma Assert (output = [output'Range => '?']);

   Setup;
   Check_Data (Single_Offset, 'S');
   single (0) := 25;
   fs.device.endpointSlot := 2;
   Check_Data (Single_Offset, 'T');
   Setup;
   Check_Data (Single_Offset, 'S');
   single (0) := 25;
   initBlockDevice (fs, 1, (slot => 1, generation => 1),
                    Grant_Buffer'Address, Grant_Buffer'Length, admission);
   pragma Assert (admission = Admitted);
   previous := Calls;
   Check_Data (Single_Offset, 'T');
   pragma Assert (Calls = previous + 2); -- revolume reloaded metadata
   Put_Line ("Mapping boundaries, volume isolation and same-endpoint revolume: PASS");
   Put_Line ("INDIRECT-READ-CHECK: PASS");
end Indirect_Reads;
