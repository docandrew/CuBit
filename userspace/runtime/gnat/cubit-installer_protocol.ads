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
--  Install_Request; the reply's word 0 is an Install_Outcome, word 1 the
--  files written, word 2 their bytes, word 3 the Identity_Proof matched.
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
   Request_Words : constant := 4;
   Reply_OK        : constant Unsigned_32 := 16#F000#;
   Reply_Error     : constant Unsigned_32 := 16#F001#;
   Reply_No_Device : constant Unsigned_32 := 16#F002#;

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
