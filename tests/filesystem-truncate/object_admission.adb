with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with Ext2_Support; use Ext2_Support;
with Volume_Admission; use Volume_Admission;
with CuBit.Messages; use CuBit.Messages;

procedure Object_Admission is
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   bgd : BlockGroupDescriptor with Import, Address => Disk (2048)'Address;
   root : Inode with Import, Address => Disk (5120 + 128)'Address;
   target : Inode with Import, Address => Disk (5120 + 256)'Address;
   dent : DirectoryEntry with Import, Address => Disk (20 * 1024)'Address;
   name : String (1 .. 4) with Import, Address => Disk (20 * 1024 + 8)'Address;
   candidate : Inode;
   original : Bytes (Disk'Range);
   output : String (1 .. 4) := "????";
   payload : constant String := "NOPE";
   completed : Unsigned_64;
   readStatus : Read_Status;
   writeStatus : Write_Status;
   truncateStatus : Truncate_Status;
   admission : Admission_Result;
   entries : Listed_Records;
   entryCount : Natural;
   cursor : Unsigned_64;
   pageStatus : Directory_Read_Status;
   found : Unsigned_32;
   lookup : Directory_Lookup_Status;

   procedure Setup is
   begin
      Reset;
      Check_Reclamation := False;
      sb.signature := EXT2_SIGNATURE;
      sb.inodeCount := 8;
      sb.blockCount := 64;
      sb.firstDataBlock := 1;
      sb.blocksPerBlockGroup := 64;
      sb.inodesPerBlockGroup := 8;
      sb.majorVersion := 1;
      sb.inodeSize := 128;
      sb.incompatibleFeatures := Incompat_Directory_Types;
      bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4, inodeTableAddr => 5,
              numFreeBlocks => 0, numFreeInodes => 0, numDirectories => 1,
              padding => 0, reserved => 0);
      root := NULL_INODE;
      root.typeAndPermissions := 16#4000#;
      root.numHardLinks := 7; -- normal directory link counts aren't file aliases
      root.sizeLo := 1024;
      root.directBlocks (0) := 20;
      target := NULL_INODE;
      target.typeAndPermissions := 16#8000#;
      target.numHardLinks := 1;
      target.sizeLo := 4;
      target.directBlocks (0) := 21;
      target.numDiskSectors := 2;
      Disk (21 * 1024 .. 21 * 1024 + 3) :=
        [Character'Pos ('D'), Character'Pos ('A'), Character'Pos ('T'), Character'Pos ('A')];
      dent := (inode => 3, length => 1024, nameLength => 4, fileType => FILETYPE_REGULAR);
      name := "item";
      initBlockDevice (fs, 1, (slot => 1, generation => 1),
                       Grant_Buffer'Address, Grant_Buffer'Length, admission);
      pragma Assert (admission = Admitted);
      Calls := 0;
      Writes := 0;
      output := "????";
   end Setup;

   procedure Rejected is
   begin
      original := Disk;
      candidate := target;
      pragma Assert (Check_File (candidate) /= File_Allowed);
      readData (fs, candidate, 0, output'Address, 4, completed, readStatus);
      pragma Assert (readStatus = Read_Object_Unsupported and completed = 0);
      pragma Assert (Calls = 0 and output = "????");
      writeData (fs, 3, candidate, 0, payload'Address, 4, completed, writeStatus);
      pragma Assert (writeStatus = Write_Object_Unsupported and completed = 0 and Calls = 0);
      truncateToEmpty (fs, 3, candidate, truncateStatus);
      pragma Assert (truncateStatus = Truncate_Unsupported and Writes = 0);
      pragma Assert (Disk = original);
      --  Unsupported objects remain visible without opening or following them.
      readDirectoryPage (fs, root, 0, entries, entryCount, cursor, pageStatus);
      pragma Assert (pageStatus = Directory_End and entryCount = 1);
      pragma Assert (entries (0).inode = 3 and entries (0).length = 4);
   end Rejected;
begin
   for Kind in Unsigned_16 range 0 .. 15 loop
      if Kind /= 8 then
         Setup;
         target.typeAndPermissions := Shift_Left (Kind, 12);
         --  The directory record still falsely advertises a regular file.
         Rejected;
      end if;
   end loop;
   for Links in Unsigned_16 range 2 .. 3 loop
      Setup;
      target.numHardLinks := Links;
      Rejected;
   end loop;
   --  No links: never opened by name, but a file unlinked while open stays
   --  usable through its handles until the last close reclaims it.
   Setup;
   target.numHardLinks := 0;
   pragma Assert (Check_File (target) = Not_A_Single_Link);
   pragma Assert (Check_File (target, Unlinked_Allowed => True) = File_Allowed);
   readData (fs, target, 0, output'Address, 4, completed, readStatus);
   pragma Assert (readStatus = Read_Complete and completed = 4 and output = "DATA");
   Setup;
   target.numHardLinks := Unsigned_16'Last;
   Rejected;
   for Bit in 0 .. 31 loop
      Setup;
      target.flags := Shift_Left (Unsigned_32'(1), Bit);
      Rejected;
   end loop;
   Setup;
   target.deletedTime := 1;
   Rejected;
   Setup;
   target.fragmentBlockAddr := 1;
   Rejected;

   Setup;
   target.typeAndPermissions := 16#A000#;
   resolvePath (fs, "item/child", found, lookup);
   pragma Assert (lookup = Lookup_Malformed and found = 0 and Writes = 0);

   --  Authority does not derive from UNIX uid/gid/mode or setuid bits.
   Setup;
   target.typeAndPermissions := 16#8FFF#;
   target.uid := 123;
   target.gid := 456;
   target.fileACL := 25; -- opaque standard xattrs are not interpreted as grants
   pragma Assert (Check_File (target) = File_Allowed);
   readData (fs, target, 0, output'Address, 4, completed, readStatus);
   pragma Assert (readStatus = Read_Complete and completed = 4 and output = "DATA");
   Ada.Text_IO.Put_Line ("OBJECT-ADMISSION-CHECK: PASS (52 rejected inode cases, unlinked-open admitted)");
end Object_Admission;
