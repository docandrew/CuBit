------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The device-memory tag of table-named firmware data, as INSPECT_CAPABILITY
--  reports it (kernel Capabilities.FIRMWARE_DATA_TAG, Physical_Ranges
--  Data_Memory).
--
--  @description
--  A sibling of Capability_ABI's firmware-memory tags (Firmware_NVS_Tag,
--  Firmware_Control_Tag, Firmware_PCI_Config_Tag), kept in its own unit so
--  the many hosted ACPI checks that pin Capability_ABI's bytes are not
--  disturbed by the new kind. Firmware_Data_Tag: whole pages of ACPI NVS or
--  firmware-reserved spans that are no other firmware kind's and hold no
--  ACPI table, mapped uncached, readable and writable. devmgr holds the
--  kernel's roots of it and narrows one envelope at a time
--  (CuBit.Firmware_Data_Exchange); no derivation sets or changes the tag.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Firmware_Data_ABI with Pure, SPARK_Mode is

   Firmware_Data_Tag : constant Unsigned_64 := 16#4657_4D45_4D00_0005#;  --  "FWMEM", 5

end Firmware_Data_ABI;
