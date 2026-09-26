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
      sb.incompatibleFeatures := 2; -- standard typed directory records
      sb.inodeSize := 128;
      --  Use the real volume admission path to reset volume/cache lifetime.
      initBlockDevice
        (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
         Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      ino := NULL_INODE;
      ino.typeAndPermissions := 16#8000#;
      ino.numHardLinks := 1;
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

   procedure Setup_Contiguous_Write is
   begin
      Setup;
      ino.directBlocks (11) := 30;
      single (0) := 31;
      single (1) := 32;
      single (2) := 33;
      original := Disk;
   end Setup_Contiguous_Write;
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

   Setup_Contiguous_Write;
   writeData (fs, 1, ino, Single_Offset - 1024, output'Address,
              output'Length, avolume, writeStatus);
   pragma Assert (writeStatus = Write_Complete and avolume = output'Length);
   pragma Assert (Calls = 2 and Writes = 1); -- one pointer read, one data write
   for I in Disk'Range loop
      pragma Assert
        (Disk (I) = (if I in 30 * 1024 .. 34 * 1024 - 1
                     then Character'Pos ('?') else original (I)));
   end loop;

   --  Include both speculative pointer reads and data-batch completions. A
   --  smaller grant produces two data batches and a meaningful completed prefix.
   for Grant_Limited in Boolean loop
      for Boundary in 1 .. (if Grant_Limited then 3 else 2) loop
         for Treatment in Failure_Mode loop
            for Reply_Kind in Failure_Reply loop
               Setup_Contiguous_Write;
               if Grant_Limited then
                  fs.device.grantBytes := 2048;
               end if;
               Fail_At := Boundary;
               CuBit.Messages.Mode := Treatment;
               Reply_Style := Reply_Kind;
               writeData (fs, 1, ino, Single_Offset - 1024, output'Address,
                          output'Length, avolume, writeStatus);
               pragma Assert (Failed and Calls = Boundary);
               pragma Assert (writeStatus = Write_Device_Error);
               pragma Assert (avolume = (if Boundary = 3 then 2048 else 0));
               pragma Assert (not fs.writeQuarantined);
               pragma Assert (Disk (0 .. 30 * 1024 - 1) =
                              original (0 .. 30 * 1024 - 1));
            end loop;
         end loop;
      end loop;
   end loop;

   Setup_Contiguous_Write;
   single (1) := 64; -- corrupt mapping in the not-yet-written batch
   original := Disk;
   writeData (fs, 1, ino, Single_Offset - 1024, output'Address,
              output'Length, avolume, writeStatus);
   pragma Assert (writeStatus = Write_Out_Of_Range and avolume = 0);
   pragma Assert (Calls = 1 and Writes = 0 and Disk = original);

   --  Existing adjacent mappings batch across single/double tree boundaries.
   Setup_Contiguous_Write;
   single (255) := 30;
   second (0) := 31;
   writeData (fs, 1, ino, Double_Offset - 1024, output'Address,
              2048, avolume, writeStatus);
   pragma Assert (writeStatus = Write_Complete and avolume = 2048);
   pragma Assert (Calls = 4 and Writes = 1);

   --  Warm double-indirect overwrites have the same one-request payload path
   --  as direct blocks. Faults never allocate or modify inode/pointer metadata.
   for Failure in 0 .. 3 loop
      for Treatment in Failure_Mode loop
         for Reply_Kind in Failure_Reply loop
            Setup;
            ino.sizeLo := Unsigned_32 (Double_Offset + 4096);
            second (0 .. 3) := [30, 31, 32, 33];
            original := Disk;
            Fail_At := Failure;
            CuBit.Messages.Mode := Treatment;
            Reply_Style := Reply_Kind;
            output := [others => 'W'];
            writeData (fs, 1, ino, Double_Offset, output'Address,
                       4096, avolume, writeStatus);
            pragma Assert (Disk (0 .. 30 * 1024 - 1) = original (0 .. 30 * 1024 - 1));
            pragma Assert (not fs.writeQuarantined);
            if Failure = 0 then
               pragma Assert (Calls = 3 and Writes = 1 and avolume = 4096);
               pragma Assert (writeStatus = Write_Complete);
               previous := Calls;
               writeData (fs, 1, ino, Double_Offset, output'Address,
                          4096, avolume, writeStatus);
               pragma Assert (Calls = previous + 1 and writeStatus = Write_Complete);
            else
               pragma Assert (Failed and Calls = Failure and avolume = 0);
               pragma Assert (writeStatus /= Write_Complete);
               Retry;
               output := [others => 'W'];
               writeData (fs, 1, ino, Double_Offset, output'Address,
                          4096, avolume, writeStatus);
               pragma Assert (writeStatus = Write_Complete and avolume = 4096);
            end if;
            pragma Assert (Disk (30 * 1024 .. 34 * 1024 - 1) =
              Bytes'(0 .. 4095 => Character'Pos ('W')));
         end loop;
      end loop;
   end loop;

   --  Coalescing also crosses double-indirect leaves. A failed lookahead
   --  must not submit the partially assembled data batch.
   for Failure in 0 .. 4 loop
      for Treatment in Failure_Mode loop
         for Reply_Kind in Failure_Reply loop
            Setup;
            ino.sizeLo := Unsigned_32 (Double_Offset + 257 * 1024);
            first (1) := 27;
            second (255) := 30;
            alternateLeaf (0) := 31;
            original := Disk;
            Fail_At := Failure;
            CuBit.Messages.Mode := Treatment;
            Reply_Style := Reply_Kind;
            writeData (fs, 1, ino, Double_Offset + 255 * 1024, output'Address,
                       2048, avolume, writeStatus);
            if Failure = 0 then
               pragma Assert (Calls = 4 and Writes = 1 and avolume = 2048);
               pragma Assert (writeStatus = Write_Complete);
            else
               pragma Assert (Failed and Calls = Failure and avolume = 0);
               pragma Assert (writeStatus /= Write_Complete);
               if Failure < 4 then
                  pragma Assert (Writes = 0 and Disk = original);
               end if;
            end if;
         end loop;
      end loop;
   end loop;

   --  Double allocation and extension use real bitmap/inode fixtures in
   --  sector_counts. This data-only fixture checks the triple boundary.
   Setup;
   first (255) := 24;
   second (255) := 25;
   ino.sizeLo := (12 + 256 + 256 * 256) * 1024;
   writeData (fs, 1, ino, Unsigned_64 (ino.sizeLo) - 1024,
              output'Address, 2048, avolume, writeStatus);
   pragma Assert (writeStatus = Write_File_Range_Unsupported and avolume = 1024);
   Put_Line ("DOUBLE-OVERWRITE-CHECK: PASS 105 faults, cache/batching and triple boundary");
   Put_Line ("Coalesced indirect writes: 75 fault cases and boundary checks PASS");
   Put_Line ("INDIRECT-READ-CHECK: PASS");
end Indirect_Reads;
