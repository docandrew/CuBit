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
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Disk_Number (range 0 15)) (type Inode_Ratio (range 8192 16777216)) " &
     "(type ESP_Size (range 260 65536)) (type Disk_Identity (record (model String) " &
     "(serial String))) (type Disk (record (number Disk_Number) (identity Disk_Identity) " &
     "(blocks Integer) (block_bytes Integer))) (type Install_Request (record (disk " &
     "Disk_Number) (confirm Disk_Identity) (inode_ratio Inode_Ratio) (esp_mib ESP_Size) " &
     "(source String))) (type Install_Outcome (enum Installed No_Such_Disk " &
     "Unidentified Wrong_Disk Too_Small Bad_Block_Size Unreadable Too_Large " &
     "Does_Not_Fit Device_Failed No_Randomness Bad_Request)) (type Install_Report (record (outcome " &
     "Install_Outcome) (files Integer) (bytes Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#8EE4_96C8_B5ED_8B00#,
      16#AA90_44B5_9853_4659#,
      16#E8D1_353A_6A04_A927#,
      16#3B27_B514_51C5_533D#];
   DISK_KEY : constant CCL.Objects.Schema_Key :=
     [16#F1E0_42A4_204A_7EE3#,
      16#8A11_C94C_2978_1293#,
      16#6351_4551_C870_DABF#,
      16#A9AE_AB0F_2D58_541F#];
   DISKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#FCEA_46EC_4E80_A2F2#,
      16#4462_E680_5442_B513#,
      16#876D_6FE4_DE73_74DB#,
      16#24AC_53CA_5672_9101#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#3CBB_3545_366D_49A0#,
      16#1E2C_BF70_D80C_085B#,
      16#B913_5C0C_7455_BE4E#,
      16#96E3_0076_7FF7_8BF2#];
   REPORT_KEY : constant CCL.Objects.Schema_Key :=
     [16#236A_9267_22BC_7551#,
      16#D3EB_9C13_E987_8BD2#,
      16#C6DD_DB46_6E6A_9F04#,
      16#F1A5_77CA_612C_922F#];

   --  Install_Outcome's members are CuBit.Installer_Protocol.Install_Outcome's,
   --  in the same order, spelled short: a qualified name is one token of at
   --  most 32 characters.

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   DISK_FIELDS : constant := 4;
   IDENTITY_FIELDS : constant := 2;
   REPORT_FIELDS : constant := 3;
   REQUEST_DISK : constant := 1;
   REQUEST_CONFIRM : constant := 2;
   REQUEST_RATIO : constant := 3;
   REQUEST_ESP : constant := 4;
   REQUEST_SOURCE : constant := 5;
   IDENTITY_MODEL : constant := 1;
   IDENTITY_SERIAL : constant := 2;

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
