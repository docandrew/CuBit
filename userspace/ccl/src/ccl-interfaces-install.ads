with Interfaces;
with CCL.Catalog;
with CCL.Objects;

--  system.disks and system.install (SYS-001): the installer as CCL
--  operations, answered by installer.svc (CuBit.Installer_Protocol).
--    (system.disks)  List<Disk>: each disk's number, identity (model and
--                      serial, as the disk reports them) and size.
--    (system.install (Install_Request disk => 0 confirm => (Disk_Identity
--      "UUHMGKH128DBB" "..." 31246336 4096) inode_ratio => 262144
--      esp_mib => 1024 source => "@cd:0/install"))
--                      Task<Install_Report>: starts erasing the disk and
--                      installing, only when confirm equals that disk's
--                      identity exactly, and returns at once; the task
--                      completes with the report. (wait t) is the
--                      blocking form.
--    (system.progress) Install_Progress: the current or last install's
--                      state, files and bytes done of the totals.
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
   PUBLISHED_SCHEMAS : constant := 5;
   --  Declared types Publish adds to the catalog's registry (its named
   --  types and the list types it specializes).
   PUBLISHED_TYPES : constant := 12;
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
     "Install_Outcome) (proof Identity_Proof) (files Integer) (bytes Integer))) (type " &
     "Install_State (enum Idle Running Finished)) (type Install_Progress (record (state " &
     "Install_State) (files_done Integer) (files Integer) (bytes_done Integer) (bytes " &
     "Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#B6E2_D5AC_A1A6_83D9#,
      16#27F5_F8CE_ED65_531A#,
      16#26F3_1994_9A46_DA2E#,
      16#6A3C_9F44_7E3B_9DE0#];
   DISK_KEY : constant CCL.Objects.Schema_Key :=
     [16#1E2F_000F_2AC2_AF7C#,
      16#8AEA_3FD5_C576_2367#,
      16#59B7_C18D_ED20_920F#,
      16#398B_F189_B387_F3C6#];
   DISKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#5F24_A660_6ACC_0E37#,
      16#AF64_6817_057A_2BD5#,
      16#5272_52B9_1E3C_A31F#,
      16#9002_B3EA_9C46_2FC6#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#57BD_81E3_9EBC_82CB#,
      16#199E_7894_AB80_1C51#,
      16#5763_5CB3_3E10_F796#,
      16#2D90_58DB_E3FD_A50B#];
   REPORT_KEY : constant CCL.Objects.Schema_Key :=
     [16#BA39_1C0E_6012_0BA7#,
      16#6C1A_F232_F4F2_B46B#,
      16#30A2_9013_3AD2_E0BC#,
      16#8E99_691D_8E2A_E335#];

   PROGRESS_KEY : constant CCL.Objects.Schema_Key :=
     [16#AA4E_EC32_138E_0F2D#,
      16#5B38_2515_A57B_CB1F#,
      16#8742_9188_1F0B_2BB1#,
      16#669F_031E_4F1E_5536#];

   --  Install_Outcome's members are CuBit.Installer_Protocol.Install_Outcome's,
   --  in the same order, spelled short: a qualified name is one token of at
   --  most 32 characters.

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   DISK_FIELDS : constant := 2;
   IDENTITY_FIELDS : constant := 4;
   REPORT_FIELDS : constant := 4;
   PROGRESS_FIELDS : constant := 5;
   REQUEST_DISK : constant := 1;
   REQUEST_CONFIRM : constant := 2;
   REQUEST_RATIO : constant := 3;
   REQUEST_ESP : constant := 4;
   REQUEST_SOURCE : constant := 5;
   IDENTITY_MODEL : constant := 1;
   IDENTITY_SERIAL : constant := 2;
   IDENTITY_BLOCKS : constant := 3;
   IDENTITY_BLOCK_BYTES : constant := 4;

   type Operation is (Disks, Install, Progress);
   function Name (Item : Operation) return String is
     (case Item is when Disks => "disks", when Install => "install", when Progress => "progress");
   FIRST_BINDING : constant Unsigned_32 := 16#000D_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Disk, Disks, Request, Report, Progress : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Install;
