------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  installer.svc's protocol (SYS-001): system.disks and system.install.
--  The installer alone holds disk-install; its clients (the CCL console)
--  ask it, and an install happens only when the request's confirmation
--  names the target disk's identity exactly (model and serial).
--
--  Every request carries a one-page grant of the client's memory (words
--  0-1 of Install, 1-2 of Disk_Info: its slot and generation). The records
--  below sit at its start, little-endian.
--
--  Disk_Info (words [disk, slot, generation, 0]): REPLY_OK writes a
--  Disk_Record; REPLY_NO_DEVICE when no disk has that number.
--  Install (words [slot, generation, 0, 0]): the grant holds an
--  Install_Request. The reply comes at once (word 0 a Start_Result); the
--  install then runs while the service keeps answering.
--  Install_Status (words [0, 0, 0, 0]): how the current or last install
--  stands (Encode_Status): running with files and bytes done of the
--  totals, or finished with its Install_Outcome and Identity_Proof.
--  Disk_Info during an install: Reply_Busy (the disk is held for writing).
--
--  The confirmation names the disk's model, serial, block count and block
--  size, all of which must equal the disk's own. A disk that reports no
--  serial is then identified by model and geometry alone, and the reply
--  says so (Model_And_Geometry): never silently.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Device_Identities;
with CuBit.Disk_Install;

package CuBit.Installer_Protocol with Pure, SPARK_Mode is

   package DId renames CuBit.Device_Identities;

   --  The catalog's installer role (userspace/ccl/catalogs).
   Service_Role : constant := 31;

   Disk_Info : constant Unsigned_32 := 16#0260#;
   Install   : constant Unsigned_32 := 16#0261#;
   Install_Status : constant Unsigned_32 := 16#0262#;
   Request_Words : constant := 4;
   Reply_OK        : constant Unsigned_32 := 16#F000#;
   Reply_Error     : constant Unsigned_32 := 16#F001#;
   Reply_No_Device : constant Unsigned_32 := 16#F002#;
   Reply_Busy      : constant Unsigned_32 := 16#F003#;

   Page_Bytes : constant := 4096;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Page is Byte_Array (1 .. Page_Bytes);

   subtype Disk_Ordinal is CuBit.Disk_Install.Disk_Ordinal;

   type Disk_Record is record
      Blocks     : Unsigned_64 := 0;
      Block_Size : Unsigned_32 := 0;
      Identity   : DId.Identity;
   end record;

   --  Bytes of volume per inode on the root (Ext2_Image.Bytes_Per_Inode)
   --  and the ESP's size in MiB (Disk_Layout.ESP_MiB): both explicit.
   subtype Inode_Ratio is Unsigned_32 range 8192 .. 2 ** 24;
   subtype ESP_MiB is Unsigned_32 range 260 .. 65_536;

   Maximum_Source : constant := 255;
   subtype Source_Length is Natural range 1 .. Maximum_Source;

   type Install_Request is record
      Disk         : Disk_Ordinal := 0;
      --  Must equal the disk's identity: model, serial, size.
      Confirmation : DId.Identity;
      Confirm_Blocks : Unsigned_64 := 0;
      Confirm_Block_Size : Unsigned_32 := 0;
      Ratio        : Inode_Ratio := 262_144;
      ESP_Size     : ESP_MiB := 260;
      --  The payload directory: its esp/ and root/ trees are installed.
      Length       : Source_Length := 1;
      Source       : String (1 .. Maximum_Source) := [others => ' '];
   end record;

   type Install_Outcome is
     (Installed, No_Such_Disk, Disk_Unidentified, Confirmation_Mismatch,
      Disk_Too_Small, Unsupported_Block_Size, Source_Unreadable, Payload_Too_Large,
      Files_Do_Not_Fit, Device_Failed, Random_Unavailable, Bad_Request);
   for Install_Outcome use
     (Installed => 0, No_Such_Disk => 1, Disk_Unidentified => 2,
      Confirmation_Mismatch => 3, Disk_Too_Small => 4, Unsupported_Block_Size => 5,
      Source_Unreadable => 6, Payload_Too_Large => 7, Files_Do_Not_Fit => 8,
      Device_Failed => 9, Random_Unavailable => 10, Bad_Request => 11);

   --  What identified the disk: its serial number (with model and
   --  geometry), or, for a disk without one, model and geometry alone.
   type Identity_Proof is (Serial_Number, Model_And_Geometry);
   for Identity_Proof use (Serial_Number => 0, Model_And_Geometry => 1);

   --  Install's answer: started, refused because one is running, or the
   --  request did not decode.
   type Start_Result is (Started, Busy, Rejected);
   for Start_Result use (Started => 0, Busy => 1, Rejected => 2);

   type Install_State is (Idle, Running, Finished);
   for Install_State use (Idle => 0, Running => 1, Finished => 2);

   --  Files fit 32 bits of a status word (Install_Tree holds far fewer).
   subtype File_Count is Unsigned_64 range 0 .. 2 ** 32 - 1;

   type Install_Progress is record
      State      : Install_State := Idle;
      --  Meaningful once Finished.
      Outcome    : Install_Outcome := Bad_Request;
      Proof      : Identity_Proof := Serial_Number;
      Files_Done : File_Count := 0;
      Files      : File_Count := 0;
      Bytes_Done : Unsigned_64 := 0;
      Bytes      : Unsigned_64 := 0;
   end record;

   type Status_Words is array (0 .. 3) of Unsigned_64;

   function Encode_Status (Item : Install_Progress) return Status_Words;
   --  Valid: every field in range, done never past the totals.
   procedure Decode_Status (Words : Status_Words; Item : out Install_Progress; Valid : out Boolean)
     with Post => (if Valid then Item.Files_Done <= Item.Files and then Item.Bytes_Done <= Item.Bytes);

   function Encode_Disk (Item : Disk_Record) return Page
     with Pre => DId.Well_Formed (Item.Identity.Model)
                 and then DId.Well_Formed (Item.Identity.Serial);
   procedure Decode_Disk (Data : Page; Item : out Disk_Record; Valid : out Boolean);

   function Encode_Install (Item : Install_Request) return Page
     with Pre => DId.Well_Formed (Item.Confirmation.Model)
                 and then DId.Well_Formed (Item.Confirmation.Serial);
   --  Valid: every field in range, the identity printable, the source path
   --  printable ASCII.
   procedure Decode_Install (Data : Page; Item : out Install_Request; Valid : out Boolean);

end CuBit.Installer_Protocol;
