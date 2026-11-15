------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel GPIO communities in the chipset's sideband (PCR) window: where
--  Alder Lake-N's four are, the GPIO controller that describes them in
--  ACPI, and the headless test profile's stand-in (ACPI-POOL-001).
--
--  @description
--  Pure, proved (tests/platform-pool, tests/platform-quirks).
--
--  * PCR address = SBREG + (port ID << 16) + register (coreboot
--      soc/intel/common/block/pcr; also lab.whitequark.org "Accessing Intel
--      ICH/PCH GPIOs", 2017-11-08). One port's window is 64 KiB.
--  * GPIO communities: 64 KiB each (coreboot "GPIO_BASE_SIZE 0x10000").
--      Alder Lake-N has communities 0, 1, 4 and 5 (Linux
--      pinctrl-alderlake.c adln_communities: "ADL_N_COMMUNITY(0, 0, 66,
--      adln_community0_gpps)", (1, 67, 168, ..1), (2, 169, 248, ..4),
--      (3, 249, 256, ..5)), port IDs "PID_GPIOCOM0 0x6e", "PID_GPIOCOM1
--      0x6d", "PID_GPIOCOM4 0x6a", "PID_GPIOCOM5 0x69" (coreboot
--      alderlake/include/soc/pcr_ids.h). The IdeaPad's DSDT GPI0._CRS
--      agrees: "CML0 = (SBRG + 0x006E0000)", "CML1 = (SBRG + 0x006D0000)",
--      "CML4 = (SBRG + 0x006A0000)", "CML5 = (SBRG + 0x00690000)", each
--      Memory32Fixed length 0x10000, and its PNP0C02 _CRS reserves the rest
--      of the window around exactly those four.
--  * The ACPI GPIO controller for Alder Lake-N is "INTC1057" (Linux
--      pinctrl-alderlake.c: '{ "INTC1057", (kernel_ulong_t)&adln_soc_data }';
--      the IdeaPad's GPI0._HID returns it when PCHS is 3, "PNP0C02" when
--      firmware hides the controller with GPHD). Linux's pinctrl driver
--      binds by that _HID and takes its MMIO from _CRS, never from the P2SB.
--  Each community is its own pool member (Chipset_Shared), so a future
--  GPIO driver bound to one claims just that community: the claim is
--  shared, ACPI's mappings there stay and the kernel logs the overlap.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Devmgr_Platform_Pool; use Devmgr_Platform_Pool;
with Platform_Quirks.Intel_P2SB;

package Platform_Quirks.Intel_GPIO_Communities with Pure, SPARK_Mode is

   Port_Bytes  : constant := 16#1_0000#;                 --  one port's PCR window
   GPIO_Community_Bytes : constant := 16#1_0000#;        --  GPIO_BASE_SIZE

   --  Port IDs (8 bits) and their PCR windows: SBREG + (port << 16).
   subtype Port_ID is Unsigned_64 range 0 .. 255;
   ADL_N_GPIO_Communities : constant := 4;
   subtype ADL_N_Community_Index is Positive range 1 .. ADL_N_GPIO_Communities;
   ADL_N_GPIO_Ports : constant array (ADL_N_Community_Index) of Port_ID :=
     [16#6E#, 16#6D#, 16#6A#, 16#69#];   --  communities 0, 1, 4, 5

   --  The ACPI _HID of Alder Lake-N's GPIO controller.
   ADL_N_GPIO_Controller_HID : constant String := "INTC1057";

   --  The gpio-launch test profile's controller _HID (a CuBit test ID,
   --  no real device's): what its stand-in for acpi.svc reports, and what
   --  its fixture driver declares (tests/gpio-launch).
   Test_GPIO_Controller_HID : constant String := "CBIT1057";

   function Port_Block (Window : Block; Port : Port_ID) return Block
   with Pre  => Window.Status = Found and then Whole_Pages (Window)
                and then Window.Bytes = Intel_P2SB.SBREG_Bytes,
        Post => Port_Block'Result.Status = Found
                and then Whole_Pages (Port_Block'Result)
                and then Port_Block'Result.Base = Window.Base + Port * Port_Bytes
                and then Port_Block'Result.Bytes = Port_Bytes
                and then Port_Block'Result.Base >= Window.Base
                and then Port_Block'Result.Base + Port_Block'Result.Bytes
                           <= Window.Base + Window.Bytes;

   ---------------------------------------------------------------------------
   --  The test profile's sideband stand-in: edu's BAR0 past the first
   --  128 KiB, one 64 KiB community at Test_Community_Offset into BAR0.
   ---------------------------------------------------------------------------
   Test_Community_Offset : constant := 16#C_0000#;

   function Test_Window (BAR_Base : Unsigned_64) return Block
   with Post => (if Test_Window'Result.Status = Found then
                   Whole_Pages (Test_Window'Result)
                   and then Test_Window'Result.Base = BAR_Base + MCHBAR_Bytes
                   and then Test_Window'Result.Bytes = Edu_BAR0_Bytes - MCHBAR_Bytes);

   function Test_Community (BAR_Base : Unsigned_64) return Block
   with Post => (if Test_Community'Result.Status = Found then
                   Whole_Pages (Test_Community'Result)
                   and then Test_Community'Result.Base = BAR_Base + Test_Community_Offset
                   and then Test_Community'Result.Bytes = GPIO_Community_Bytes);

end Platform_Quirks.Intel_GPIO_Communities;
