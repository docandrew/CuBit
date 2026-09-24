with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with Volume_Admission; use Volume_Admission;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
procedure Path_Reads is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
   root : Inode with Import, Address => Disk (5120 + 128)'Address;
   folder : Inode with Import, Address => Disk (5120 + 256)'Address;
   file : Inode with Import, Address => Disk (5120 + 384)'Address;
   rootEntry : DirectoryEntry with Import, Address => Disk (20 * 1024)'Address;
   leafEntry : DirectoryEntry with Import, Address => Disk (21 * 1024)'Address;
   rootName : String (1 .. 6) with Import, Address => Disk (20 * 1024 + 8)'Address;
   leafName : String (1 .. 4) with Import, Address => Disk (21 * 1024 + 8)'Address;
   found : Unsigned_32;
   lookup : Directory_Lookup_Status;
   readStatus : Read_Status;
   renameStatus : Rename_Status;
   candidate : Inode;
   original : Bytes (Disk'Range);
   baseline : Natural;
   admission : Admission_Result;

   procedure Setup is
   begin
      Reset;
      sb.signature := EXT2_SIGNATURE;
      sb.inodeCount := 8;
      sb.blockCount := 64;
      sb.firstDataBlock := 1;
      sb.blocksPerBlockGroup := 64;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.inodeSize := 128;
      bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4, inodeTableAddr => 5,
              numFreeBlocks => 0, numFreeInodes => 0, numDirectories => 2,
              padding => 0, reserved => 0);
      root := NULL_INODE;
      root.typeAndPermissions := 16#4000#;
      root.sizeLo := 1024;
      root.directBlocks (0) := 20;
      folder := root;
      folder.directBlocks (0) := 21;
      file := NULL_INODE;
      file.typeAndPermissions := 16#8000#;
      rootEntry := (inode => 3, length => 1024, nameLength => 6,
                    fileType => FILETYPE_DIRECTORY);
      leafEntry := (inode => 4, length => 1024, nameLength => 4,
                    fileType => FILETYPE_REGULAR);
      rootName := "folder";
      leafName := "file";
      initBlockDevice
        (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
         Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      Check_Reclamation := False;
   end Setup;
begin
   Setup;
   resolvePath (fs, "/folder/file", found, lookup);
   pragma Assert (lookup = Lookup_Found and found = 4);
   baseline := Calls;
   pragma Assert (baseline = 6);
   for boundary in 1 .. baseline loop
      for treatment in Failure_Mode loop
         for style in Failure_Reply loop
            Setup;
            original := Disk;
            Fail_At := boundary;
            CuBit.Messages.Mode := treatment;
            Reply_Style := style;
            resolvePath (fs, "/folder/file", found, lookup);
            pragma Assert (lookup = Lookup_Device_Error and found = 0);
            pragma Assert (Failed and Calls = boundary and Writes = 0);
            pragma Assert (Disk = original);
         end loop;
      end loop;
   end loop;
   Put_Line ("Nested path read/reply failures: 90 cases PASS");

   for boundary in 1 .. 2 loop
      for treatment in Failure_Mode loop
         for style in Failure_Reply loop
            Setup;
            candidate := root; -- ensure failure clears any previous value
            Fail_At := boundary;
            CuBit.Messages.Mode := treatment;
            Reply_Style := style;
            readInode (fs, 4, candidate, readStatus);
            pragma Assert (readStatus = Read_Device_Error);
            pragma Assert (candidate = NULL_INODE and Calls = boundary);
         end loop;
      end loop;
   end loop;
   Put_Line ("Final inode read/reply failures: 30 cases PASS");

   for boundary in 1 .. 3 loop
      for treatment in Failure_Mode loop
         for style in Failure_Reply loop
            Setup;
            original := Disk;
            Fail_At := boundary;
            CuBit.Messages.Mode := treatment;
            Reply_Style := style;
            renamePath (fs, "folder/file", "folder/renamed", renameStatus);
            pragma Assert (renameStatus = Rename_IO_Error);
            pragma Assert (Calls = boundary and Writes = 0 and Disk = original);
         end loop;
      end loop;
   end loop;
   Put_Line ("Rename parent resolution failures: 45 cases PASS");

   Setup;
   resolvePath (fs, "/folder/missing", found, lookup);
   pragma Assert (lookup = Lookup_Not_Found and found = 0);
   resolvePath (fs, "/absent/file", found, lookup);
   pragma Assert (lookup = Lookup_Not_Found and found = 0);
   renamePath (fs, "absent/file", "absent/renamed", renameStatus);
   pragma Assert (renameStatus = Rename_Source_Not_Found and Writes = 0);
   Setup;
   rootEntry.length := 4;
   resolvePath (fs, "folder/file", found, lookup);
   pragma Assert (lookup = Lookup_Malformed and found = 0);
   renamePath (fs, "folder/file", "folder/renamed", renameStatus);
   pragma Assert (renameStatus = Rename_Malformed and Writes = 0);
   Setup;
   root.directBlocks (0) := 64;
   resolvePath (fs, "folder/file", found, lookup);
   pragma Assert (lookup /= Lookup_Not_Found and lookup /= Lookup_Found);
   pragma Assert (found = 0 and Writes = 0);
   Setup;
   bgd.inodeTableAddr := 64;
   resolvePath (fs, "folder/file", found, lookup);
   pragma Assert (lookup = Lookup_Out_Of_Range and found = 0);
   renamePath (fs, "folder/file", "folder/renamed", renameStatus);
   pragma Assert (renameStatus = Rename_Out_Of_Range and Writes = 0);
   Setup;
   resolvePath (fs, "../file", found, lookup);
   pragma Assert (lookup = Lookup_Malformed and found = 0 and Calls = 0);
   resolvePath (fs, "folder/file/child", found, lookup);
   pragma Assert (lookup = Lookup_Malformed and found = 0);
   Setup;
   resolvePath (fs, "", found, lookup);
   pragma Assert (lookup = Lookup_Found and found = ROOT_INODE);
   resolvePath (fs, "///", found, lookup);
   pragma Assert (lookup = Lookup_Found and found = ROOT_INODE);
   resolvePath (fs, "//folder///file", found, lookup);
   pragma Assert (lookup = Lookup_Found and found = 4);
   fs.device.description.features := FEATURE_VOLATILE;
   resolvePath (fs, "folder/file", found, lookup);
   pragma Assert (lookup = Lookup_Found and found = 4);
   Put_Line ("PATH-READ-CHECK: PASS");
end Path_Reads;
