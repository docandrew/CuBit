------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Who built this machine's firmware: the one typed platform identity both
--  quirk tables key on (devmgr's hardware-discovery quirks, Platform_Quirks;
--  acpi.svc's firmware-interpretation quirks).
--
--  @description
--  Derived once per boot from the sealed firmware snapshot: the header of
--  the FADT (signature "FACP", the one fixed table every ACPI machine has).
--  ACPI 6.5 section 5.2.6 "System Description Table Header": OEMID at byte
--  10 (6 bytes), OEM Table ID at byte 16 (8 bytes), OEM Revision at byte 24
--  (4 bytes, little-endian). The IdeaPad Slim 3i's FADT says OEMID
--  "LENOVO", OEM Table ID "CB-01   ", OEM Revision 1 (saved laptop tables,
--  .acpi-proof-work/laptop-acpi-1-tables/facp.dat).
--
--  SMBIOS (vendor / product / version) is not part of the sealed snapshot:
--  the kernel seals ACPI tables only (CuBit.Firmware_Snapshot), so neither
--  devmgr nor acpi.svc can read SMBIOS today. Matching on it needs the
--  kernel to seal the SMBIOS entry point and structure table too; until
--  then a quirk names its firmware by these ACPI fields.
--
--  Pure and proved (tests/platform-quirks): no I/O, no allocation, nothing
--  a ZFP runtime lacks.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Platform_Identity with Pure, SPARK_Mode is

   --  ACPI 6.5 table 5.4, the System Description Table Header.
   Header_Bytes        : constant := 36;
   Signature_Offset    : constant := 0;
   Signature_Bytes     : constant := 4;
   OEM_ID_Offset       : constant := 10;
   OEM_ID_Bytes        : constant := 6;
   OEM_Table_ID_Offset : constant := 16;
   OEM_Table_ID_Bytes  : constant := 8;
   OEM_Revision_Offset : constant := 24;
   OEM_Revision_Bytes  : constant := 4;

   --  ACPI 6.5 table 5.6: the FADT's signature.
   FADT_Signature : constant String := "FACP";

   subtype OEM_ID_Text is String (1 .. OEM_ID_Bytes);
   subtype OEM_Table_ID_Text is String (1 .. OEM_Table_ID_Bytes);

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  Where the identity came from.
   type Identity_Source is
     (Unknown,        --  no FADT in the snapshot (or not read yet)
      FADT_Header);   --  the sealed FADT's header

   type Identity is record
      Source       : Identity_Source := Unknown;
      OEM_ID       : OEM_ID_Text := [others => ' '];
      OEM_Table_ID : OEM_Table_ID_Text := [others => ' '];
      OEM_Revision : Unsigned_32 := 0;
   end record;

   No_Identity : constant Identity := (others => <>);

   --  The identity in a table's first Header_Bytes bytes: FADT_Header when
   --  they carry the FADT signature, No_Identity otherwise.
   function From_Header (Header : Byte_Array) return Identity
   with Pre  => Header'First = 0 and then Header'Length >= Header_Bytes,
        Post => (if From_Header'Result.Source = FADT_Header then
                   (for all I in 1 .. Signature_Bytes =>
                      Header (Signature_Offset + I - 1) =
                        Character'Pos (FADT_Signature (FADT_Signature'First + I - 1)))
                 else From_Header'Result = No_Identity);

   ---------------------------------------------------------------------------
   --  How a quirk names the firmware it applies to.
   ---------------------------------------------------------------------------
   type Firmware_Match_Kind is
     (Any_Firmware,       --  hardware-keyed only: the firmware does not matter
      OEM,                --  this OEMID
      OEM_Table,          --  this OEMID and OEM Table ID
      OEM_Table_Revised); --  ... and an OEM Revision in [Low, High]

   type Firmware_Match is record
      Kind         : Firmware_Match_Kind := Any_Firmware;
      OEM_ID       : OEM_ID_Text := [others => ' '];
      OEM_Table_ID : OEM_Table_ID_Text := [others => ' '];
      Revision_Low, Revision_High : Unsigned_32 := 0;
   end record;

   Any : constant Firmware_Match := (others => <>);

   function Matches (M : Firmware_Match; Firmware : Identity) return Boolean is
     (case M.Kind is
        when Any_Firmware      => True,
        when OEM               => Firmware.Source = FADT_Header
                                  and then Firmware.OEM_ID = M.OEM_ID,
        when OEM_Table         => Firmware.Source = FADT_Header
                                  and then Firmware.OEM_ID = M.OEM_ID
                                  and then Firmware.OEM_Table_ID = M.OEM_Table_ID,
        when OEM_Table_Revised => Firmware.Source = FADT_Header
                                  and then Firmware.OEM_ID = M.OEM_ID
                                  and then Firmware.OEM_Table_ID = M.OEM_Table_ID
                                  and then Firmware.OEM_Revision in
                                             M.Revision_Low .. M.Revision_High);

end Platform_Identity;
