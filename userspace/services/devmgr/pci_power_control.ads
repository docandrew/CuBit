pragma Ada_2022;
with Interfaces; use Interfaces;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Bringing a PCI function from D3hot to D0 before its driver uses it
--  (PCI-BAR-001; PCI Bus Power Management Interface 1.2, section 5.4).
--
--  @description
--  devmgr must put a function in D0 before it can place or use the
--  function's BARs. Linux's pci_enable_device does the same. The IdeaPad's
--  LPSS I2C controllers arrive in D3hot with BAR 0 cleared by the firmware's
--  _PS3. This package only decides; devmgr does the configuration I/O:
--  - Locate finds the one Power Management capability in a type-0 header,
--    and refuses a malformed chain (a cycle, a duplicate, a misaligned
--    pointer), as Intel_GPU_PCI_Power.Decode does.
--  - To_D0 is the PMCSR value to write. PowerState (bits 1:0) becomes 00,
--    PME_En is kept, and PME_Status (bit 15, write-1-to-clear) is written
--    as 0, so the write never clears a pending wake.
--  - Resets_On_D0 says whether No_Soft_Reset (bit 3) is clear. If so, the
--    function resets on the way to D0 and loses its configuration, so a BAR
--    is placed after the transition, never before.
--  The caller waits the D3hot-to-D0 recovery time (10 ms, PCI PM 1.2 table
--  5-1) before touching the function again, and then reads the state back.
------------------------------------------------------------------------------
package PCI_Power_Control with SPARK_Mode, Pure is
   type Configuration is array (Natural range 0 .. 255) of Unsigned_8;

   type Location_Status is (Found, Absent, Malformed);
   subtype PM_Offset is Natural range 16#40# .. 16#F8#;

   type Location is record
      Status : Location_Status := Absent;
      Offset : PM_Offset := PM_Offset'First;
   end record;

   function Locate (Data : Configuration) return Location
     with Post => (if Locate'Result.Status = Found then
                     Locate'Result.Offset mod 4 = 0
                     and then Data (Locate'Result.Offset) = 1);

   PMCSR_Offset_From_Capability : constant := 4;
   Power_State_Mask : constant Unsigned_16 := 16#0003#;
   No_Soft_Reset    : constant Unsigned_16 := 16#0008#;
   PME_Status       : constant Unsigned_16 := 16#8000#;
   D3hot_Recovery_Ms : constant := 10;

   type Power_State is (D0, D1, D2, D3_Hot);
   function State_Of (PMCSR : Unsigned_16) return Power_State is
     (case PMCSR and Power_State_Mask is
        when 0 => D0, when 1 => D1, when 2 => D2, when others => D3_Hot);

   function To_D0 (PMCSR : Unsigned_16) return Unsigned_16 is
     (PMCSR and not (Power_State_Mask or PME_Status))
   with Post => (To_D0'Result and Power_State_Mask) = 0
                and then (To_D0'Result and PME_Status) = 0;

   function Resets_On_D0 (PMCSR : Unsigned_16) return Boolean is
     ((PMCSR and No_Soft_Reset) = 0);
end PCI_Power_Control;
