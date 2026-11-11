------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's sideband subsystem (ACPI-POOL-001): where the chipset's
--  sideband (PCR) window is, and the block of any port in it. It alone
--  touches the P2SB (00:1F.1); the rest of devmgr asks it for a port's
--  block or the GPIO communities.
--
--  @description
--  Its place in the platform-register pool's flow (ACPI-POOL-001; the
--  whole flow is in the kernel's Platform_Pool spec):
--    1. Discover_Before_ACPI: before any ACPI program starts, briefly
--       unhide the P2SB (00:1F.1), read the sideband base (SBREG_BAR),
--       hide it again, and derive the GPIO community ranges from it
--       (Community_Range). devmgr's pool builder ADDS those ranges to the
--       platform pool; nobody maps them here. It has to be before ACPI:
--       _INI methods touch these registers (the IdeaPad's TPD0/TPL1._INI
--       read GPIO PDW registers through SystemMemory).
--    2. The ACPI backend then maps the pieces AML touches (kernel-checked
--       once per mapping).
--    3. After _INI the ACPI service reports the platform devices' _CRS
--       resources (GPIO controller ranges, I2C controllers, the touchpad's
--       address and GpioInt pin), Windows-style. devmgr cross-checks
--       Community_Range against that report (a mismatch is logged) and
--       binds drivers from it. Future work: the hand-off point is marked
--       "ACPI-POOL-001 step 3" in devmgr's main.
--    4. A GPIO driver's claim of a community is SHARED with firmware (AML
--       keeps its mappings, the kernel logs the overlap); only PCI device
--       BARs are revoked from ACPI when a driver binds.
--
--  Sources of the window (Window_Source):
--    * P2SB_BAR: Discover_Before_ACPI, Linux p2sb.c's protocol (see
--      Devmgr_Sideband_Layout), once, before any other process can reach
--      PCI configuration, and checked hidden afterwards.
--    * Test_Profile: the headless test profile's stand-in in edu's BAR0.
--  The pool never sees how the window was found: it asks for Window,
--  Port_Block and the community ranges. If the step-3 _CRS report proves
--  reliable, a source fed from it (Windows-style: the GPIO device's _CRS
--  ranges) could replace the unhide behind this interface without a change
--  to the callers.
--
--  PCI configuration access is devmgr's (the formal subprograms), so the
--  P2SB writes go through the same port and the same freeze rules as
--  every other configuration write.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Devmgr_Platform_Pool;
with Devmgr_Sideband_Layout;

generic
   with function Read_Config_16 (Bus, Slot, Func, Offset : Unsigned_8) return Unsigned_16;
   with function Read_Config_32 (Bus, Slot, Func, Offset : Unsigned_8) return Unsigned_32;
   with procedure Write_Config_32 (Bus, Slot, Func, Offset : Unsigned_8; Value : Unsigned_32);
   with procedure Inform (Text : String);
   with procedure Warn (Text : String);
   with function Hex (Value : Unsigned_64) return String;
package Devmgr_Sideband is
   package PP renames Devmgr_Platform_Pool;
   package SL renames Devmgr_Sideband_Layout;

   type Window_Source is
     (No_Window,      --  not found (yet): every port block is Not_Placed
      P2SB_BAR,       --  the P2SB's SBREG_BAR (Discover_Before_ACPI)
      Test_Profile);  --  the headless test profile's stand-in

   --  Step 1, Alder Lake-N: the P2SB's window and the four GPIO community
   --  ranges, read before any ACPI program starts. A window not found
   --  leaves No_Window, with the reason logged.
   procedure Discover_Before_ACPI;

   --  The test profile (edu's BAR0 at BAR_Base): its window and its one
   --  community (Devmgr_Sideband_Layout.Test_Window, Test_Community).
   procedure Adopt_Test_Window (BAR_Base : Unsigned_64);

   function Source return Window_Source;

   --  The whole window (Status Found only with a source).
   function Window return PP.Block;

   --  A port's PCR block: Not_Placed without a P2SB_BAR window.
   function Port_Block (Port : SL.Port_ID) return PP.Block;

   --  The GPIO community ranges in the window, each a Found block (none
   --  without a window): what the pool builder adds as Chipset_Shared
   --  members, and what step 3 will cross-check against ACPI's _CRS report.
   function Community_Range_Count return Natural;
   function Community_Range (Index : Positive) return PP.Block
   with Pre => Index <= Community_Range_Count;

end Devmgr_Sideband;
