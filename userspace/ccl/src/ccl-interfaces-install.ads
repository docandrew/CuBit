with Interfaces;
with CCL.Binding_Blocks;
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
--                      state, its step (Install_Step: partitioning,
--                      formatting, filling the storehouse, generation 1,
--                      verifying, the ESP, making it bootable), files and
--                      bytes done of the totals. A report's step is the
--                      last one reached: a disk is bootable only once an
--                      install got past Making_Bootable.
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
   PUBLISHED_TYPES : constant := 13;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Disk_Number (range 0 15)) (type Inode_Ratio (range 8192 16777216)) (type " &
     "ESP_Size (range 260 65536)) (type Disk_Identity (record (model String) (serial " &
     "String) (blocks Integer) (block_bytes Integer))) (type Disk (record (number " &
     "Disk_Number) (identity Disk_Identity))) (type Install_Request (record (disk " &
     "Disk_Number) (confirm Disk_Identity) (inode_ratio Inode_Ratio) (esp_mib ESP_Size) " &
     "(source String))) (type Install_Outcome (enum Installed No_Such_Disk Unidentified " &
     "Wrong_Disk Too_Small Bad_Block_Size Unreadable Too_Large Does_Not_Fit Device_Failed " &
     "No_Randomness Bad_Request Verify_Failed Device_Timed_Out Source_Timed_Out " &
     "Source_Changed)) (type Identity_Proof (enum Serial_Number Model_Geometry)) (type " &
     "Install_Step (enum Checking Partitioning Formatting_Root Filling_Storehouse " &
     "Writing_Generation Verifying_Root Writing_ESP Verifying_ESP Making_Bootable " &
     "Flushing Done)) (type Install_Report (record (outcome Install_Outcome) (proof " &
     "Identity_Proof) (step Install_Step) (files Integer) (bytes Integer))) (type " &
     "Install_State (enum Idle Running Finished)) (type Install_Progress (record (state " &
     "Install_State) (step Install_Step) (files_done Integer) (files Integer) (bytes_done " &
     "Integer) (bytes Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#2984_A6CA_9CE4_035E#,
      16#7F7D_3C97_7A63_C53F#,
      16#902C_5CF1_FE93_62D2#,
      16#67CB_5688_6D9B_A13C#];
   DISK_KEY : constant CCL.Objects.Schema_Key :=
     [16#73DD_CB5A_AD2F_9348#,
      16#3934_50DB_44A3_082C#,
      16#6D33_CEF2_DA87_A323#,
      16#E282_C872_6A18_049C#];
   DISKS_KEY : constant CCL.Objects.Schema_Key :=
     [16#08C9_E32E_7837_8295#,
      16#8E4B_8431_70A3_FA56#,
      16#DD89_38EF_E9C1_DFD2#,
      16#7E00_9D94_5832_E359#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#FC99_4E5E_44AE_4F3F#,
      16#6D68_376A_E16B_56A3#,
      16#AF8B_CAC6_33EE_648E#,
      16#6D1E_E1EF_1164_5195#];
   REPORT_KEY : constant CCL.Objects.Schema_Key :=
     [16#18F5_7DA7_8835_8E97#,
      16#B0A9_7479_C6B2_E2EA#,
      16#7F18_37CA_62C8_99F2#,
      16#091F_FE18_3405_55B5#];

   PROGRESS_KEY : constant CCL.Objects.Schema_Key :=
     [16#0EDA_9593_E989_7C2A#,
      16#8953_EBC2_2939_3035#,
      16#4A7D_582F_90C6_65BE#,
      16#434E_6DA3_309E_DB45#];

   --  Install_Outcome's members are CuBit.Installer_Protocol.Install_Outcome's,
   --  in the same order, spelled short: a qualified name is one token of at
   --  most 32 characters.

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   DISK_FIELDS : constant := 2;
   IDENTITY_FIELDS : constant := 4;
   REPORT_FIELDS : constant := 5;
   PROGRESS_FIELDS : constant := 6;
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
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Install);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Disk, Disks, Request, Report, Progress : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Install;
