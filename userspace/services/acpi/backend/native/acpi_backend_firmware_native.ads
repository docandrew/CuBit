pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Firmware_Memory;
-- Facts always come from the held handle's kernel inspection, not startup
-- class assertions or an IPC payload. No mapping, copying or delegation.
package ACPI_Backend_Firmware_Native is
   package Policy renames ACPI_Backend_Firmware_Memory;
   function Inspect (Held : CuBit.Handles.Device_Memory) return Policy.Report;
   function Inspect_Claim
     (Held : CuBit.Handles.Device_Memory; First_Page, Pages : Interfaces.Unsigned_64;
      Kind : Policy.M.Memory_Kind; Cache : Policy.M.Cache_Mode) return Policy.Report;
end ACPI_Backend_Firmware_Native;
