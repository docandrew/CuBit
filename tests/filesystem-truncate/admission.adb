with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;
with Ext2_Support;

procedure Admission is
   use type System.Address;
   fs : Filesystem;
   sb : Superblock with Import, Address => Disk (1024)'Address;
   result : Admission_Result;
   cases : Natural := 0;

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
   end Setup;

   procedure Check (Expected : Admission_Result; Expected_Calls : Natural) is
   begin
      initBlockDevice
        (fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
         Grant_Buffer'Length, result);
      pragma Assert (result = Expected);
      pragma Assert (Calls = Expected_Calls);
      pragma Assert (Writes = 0 and Barriers = 0);
      if result /= Admitted then
         pragma Assert (fs.blkSize = 0 and fs.sb.signature = 0);
         pragma Assert (fs.device.endpointSlot = 0);
         pragma Assert (fs.device.grantBuffer = System.Null_Address);
      end if;
      cases := cases + 1;
   end Check;

   procedure Describe is
   begin
      Override_Description := True;
      Description_Reply :=
        ((REPLY_OK, 4, 0, 0), 0,
         [0 => Disk'Length / 512, 1 => Pack_Sizes (512, 512),
          2 => 8, 3 => Pack_Properties (FEATURE_FLUSH, Fixed_Media)]);
   end Describe;
begin
   Setup;
   Check (Admitted, 2);
   pragma Assert (fs.blkSize = 1024 and fs.device.endpointSlot = 1);

   --  Failure must clear a previously successful output as well.
   Setup;
   Fail_At := 1;
   Mode := Before_IO;
   Check (Device_Error, 1);

   --  Both failed describe and failed superblock reads are terminal. The
   --  fixture raises if production code retries after a rejected completion.
   for stage in 1 .. 2 loop
      for transport in Failure_Mode loop
         for replyStyle in Failure_Reply loop
            Setup;
            Fail_At := stage;
            Mode := transport;
            Reply_Style := replyStyle;
            Check
              ((if stage = 1 and replyStyle /= Error_Label
                then Invalid_Description else Device_Error), stage);
         end loop;
      end loop;
   end loop;

   Setup;
   Describe;
   Description_Reply := ((REPLY_NO_DEVICE, 1, 0, 0), 0, [others => 0]);
   Check (No_Device, 1);
   for malformed in 1 .. 4 loop
      Setup;
      Describe;
      Description_Reply := ((REPLY_NO_DEVICE, 1, 0, 0), 0, [others => 0]);
      case malformed is
         when 1 => Description_Reply.tag.length := 0;
         when 2 => Description_Reply.tag.flags := 1;
         when 3 => Description_Reply.tag.reserved := 1;
         when 4 => Description_Reply.words (0) := 1;
      end case;
      Check (Invalid_Description, 1);
   end loop;

   for malformed in 1 .. 7 loop
      Setup;
      Describe;
      case malformed is
         when 1 => Description_Reply.words (0) := 0;
         when 2 => Description_Reply.words (1) := 123;
         when 3 => Description_Reply.words (2) := 0;
         when 4 => Description_Reply.words (3) := 0; -- wrong protocol version
         when 5 => Description_Reply.tag.length := 3;
         when 6 => Description_Reply.tag.flags := 1;
         when 7 => Description_Reply.tag.reserved := 1;
      end case;
      Check (Invalid_Description, 1);
   end loop;

   Setup;
   Describe;
   Description_Reply.words (1) := Pack_Sizes (8192, 8192);
   Check (Invalid_Session, 1); -- grant cannot hold one device block
   Setup;
   Describe;
   Description_Reply.words (0) := 2; -- cannot read superblock
   Check (Invalid_Description, 1);

   Setup;
   sb.signature := 0;
   Check (Unsupported_Filesystem, 2);
   Setup;
   sb.blockShift := 3;
   Check (Unsupported_Filesystem, 2);

   --  Sweep every individual feature bit, including future unknown bits.
   --  Unknown RO_COMPAT flags cannot accidentally get a writable session.
   for Bit in 0 .. 31 loop
      declare
         Flag : constant Unsigned_32 := Shift_Left (Unsigned_32'(1), Bit);
      begin
         Setup;
         sb.compatibleFeatures := Flag;
         Check ((if (Flag and Ext2_Support.Supported_Compatible) /= 0
                 then Admitted else Unsupported_Filesystem), 2);
         Setup;
         sb.incompatibleFeatures := sb.incompatibleFeatures or Flag;
         Check ((if Flag = Ext2_Support.Incompat_Directory_Types
                 then Admitted else Unsupported_Filesystem), 2);
         Setup;
         sb.readOnlyFeatures := Flag;
         Check ((if (Flag and Ext2_Support.Supported_Read_Only) /= 0
                 then Admitted else Unsupported_Filesystem), 2);
      end;
   end loop;
   Setup;
   sb.compatibleFeatures := Ext2_Support.Supported_Compatible;
   sb.readOnlyFeatures := Ext2_Support.Supported_Read_Only;
   Check (Admitted, 2); -- common Linux mke2fs ext2 profile
   Setup;
   sb.incompatibleFeatures := 0; -- legacy untyped directory records
   Check (Unsupported_Filesystem, 2);
   Setup;
   sb.majorVersion := 0;
   Check (Unsupported_Filesystem, 2);
   Setup;
   sb.majorVersion := 2;
   Check (Unsupported_Filesystem, 2);
   Setup;
   sb.creatorOS := 1;
   Check (Unsupported_Filesystem, 2);

   --  Independently address the standard on-disk offsets, not Ada fields.
   Setup;
   Disk (1024 + 92) := 4; -- HAS_JOURNAL
   Check (Unsupported_Filesystem, 2);
   Setup;
   Disk (1024 + 96) := 6; -- FILETYPE | RECOVER
   Check (Unsupported_Filesystem, 2);
   Setup;
   Disk (1024 + 100) := 16#40#; -- EXTRA_ISIZE
   Check (Unsupported_Filesystem, 2);
   for malformed in 1 .. 7 loop
      Setup;
      case malformed is
         when 1 => sb.blocksPerBlockGroup := 0;
         when 2 => sb.inodesPerBlockGroup := 0;
         when 3 => sb.blockCount := 65; -- exceeds offered device
         when 4 => sb.firstDataBlock := 64;
         when 5 => sb.inodeSize := 64;
         when 6 => sb.freeBlocks := 64;
         when 7 => sb.freeInodes := 9;
      end case;
      Check (Invalid_Filesystem, 2);
   end loop;

   Setup;
   sb.inodeSize := 132; -- aligned is insufficient: slots must be powers of two
   Check (Invalid_Filesystem, 2);

   Setup;
   initBlockDevice (fs, 1, (slot => 1, generation => 1),
                    System.Null_Address, 4096, result);
   pragma Assert (result = Invalid_Session and Calls = 0);
   initBlockDevice (fs, 0, (slot => 1, generation => 1),
                    Grant_Buffer'Address, 4096, result);
   pragma Assert (result = Invalid_Session and Calls = 0);
   initBlockDevice (fs, 1, (slot => 1, generation => 1),
                    Grant_Buffer'Address, 0, result);
   pragma Assert (result = Invalid_Session and Calls = 0);

   --  Exhaustive fallback policy, including resource/authority failures which
   --  occur in the native session adapter rather than the Ext2 parser.
   for outcome in Admission_Result loop
      pragma Assert
        (May_Search_Next (outcome) =
           (outcome = Provider_Not_Ready or outcome = No_Device));
   end loop;
   Ada.Text_IO.Put_Line ("VOLUME-ADMISSION-CHECK: PASS" & cases'Image & " cases");
end Admission;
