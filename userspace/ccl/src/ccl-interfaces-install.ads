with Interfaces;
with CCL.Catalog;
with CCL.Objects;

--  system.disks and system.install (SYS-001): the installer as CCL
--  operations, answered by installer.svc (CuBit.Installer_Protocol).
--    (system.disks)  List<Disk>: each disk's number, identity (model and
--                      serial, as the disk reports them) and size.
--    (system.install (Install_Request disk => 0 confirm => (Disk_Identity
--      "UUHMGKH128DBB" "...") inode_ratio => 262144 esp_mib => 1024
--      source => "@cd:0/install"))
--                      Install_Report: erases the disk and installs, only
--                      when confirm equals that disk's identity exactly.
--  Nothing is defaulted: the disk, its identity, the inode ratio, the ESP
--  size and the payload are all spelled out. The types are CCL source,
--  checked by the CCL type checker when the interface is published; the
--  digest and keys are SHA-256 of it (then "#" and the type's name), and
--  tests/ccl-console checks them.
package CCL.Interfaces.Install with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 4;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Disk_Number (range 0 15)) (type Inode_Ratio (range 8192 16777216)) " &
     "(type ESP_Size (range 260 65536)) (type Disk_Identity (record (model String) " &
     "(serial String) (blocks Integer) (block_bytes Integer))) (type Disk (record " &
     "(number Disk_Number) (identity Disk_Identity))) (type Install_Request (record (disk " &
     "Disk_Number) (confirm Disk_Identity) (inode_ratio Inode_Ratio) (esp_mib ESP_Size) " &
     "(source String))) (type Install_Outcome (enum Installed No_Such_Disk " &
     "Unidentified Wrong_Disk Too_Small Bad_Block_Size Unreadable Too_Large " &
     "Does_Not_Fit Device_Failed No_Randomness Bad_Request)) (type Identity_Proof (enum " &
     "Serial_Number Model_Geometry)) (type Install_Report (record (outcome " &
     "Install_Outcome) (proof Identity_Proof) (files Integer) (bytes Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#4CE2_84FE_046C_D70B#,
      16#F562_02D9_BC97_57DC#,
      16#9895_CA49_CB09_685D#,
      16#5834_8C3E_2014_ADD2#];
   DISK_KEY : constant CCL.Objects.Schema_Key :=
     [16#CEAF_27CF_3010_1CFB#,
      16#D99B_6C18_49E0_9488#,
      16#4B0A_B9DD_00E2_58DE#,
      16#2ED6_4A93_2B19_5F9F#];
   DISKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#D337_326C_F321_99DE#,
      16#2B82_36BE_DF4D_4EBF#,
      16#24CB_2234_C441_2DC9#,
      16#7D2A_43D2_18FE_CD8D#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#14F2_E11E_1F5A_3F47#,
      16#CAE9_8BC2_2856_FB56#,
      16#35C3_C79B_87FF_59F4#,
      16#6B19_DBC2_2563_3529#];
   REPORT_KEY : constant CCL.Objects.Schema_Key :=
     [16#DEB8_DCED_474B_D340#,
      16#0548_683F_DEAA_886C#,
      16#38F8_C4E7_150A_EBDF#,
      16#11BE_3F21_71F7_6454#];

   --  Install_Outcome's members are CuBit.Installer_Protocol.Install_Outcome's,
   --  in the same order, spelled short: a qualified name is one token of at
   --  most 32 characters.

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   DISK_FIELDS : constant := 2;
   IDENTITY_FIELDS : constant := 4;
   REPORT_FIELDS : constant := 4;
   REQUEST_DISK : constant := 1;
   REQUEST_CONFIRM : constant := 2;
   REQUEST_RATIO : constant := 3;
   REQUEST_ESP : constant := 4;
   REQUEST_SOURCE : constant := 5;
   IDENTITY_MODEL : constant := 1;
   IDENTITY_SERIAL : constant := 2;
   IDENTITY_BLOCKS : constant := 3;
   IDENTITY_BLOCK_BYTES : constant := 4;

   type Operation is (Disks, Install);
   function Name (Item : Operation) return String is
     (case Item is when Disks => "disks", when Install => "install");
   FIRST_BINDING : constant Unsigned_32 := 16#000D_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Disk, Disks, Request, Report : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Install;
