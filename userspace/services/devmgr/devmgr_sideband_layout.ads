------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The layout devmgr's sideband subsystem (Devmgr_Sideband) works from:
--  the P2SB (00:1F.1), its SBREG_BAR sideband window, the port blocks in
--  it, and Alder Lake-N's GPIO communities (ACPI-POOL-001).
--
--  @description
--  Pure decoding, proved (tests/platform-pool). Only Devmgr_Sideband
--  touches 00:1F.1; the rest of devmgr asks it for a port's block.
--
--  * P2SB (00:1F.1) SBREG_BAR: BAR0/BAR1 of the P2SB, which firmware
--      hides. The protocol is Linux drivers/platform/x86/p2sb.c, which
--      every Linux boot of this laptop runs (fs_initcall_sync): "#define
--      P2SBC 0xe0", "#define P2SBC_HIDE BIT(8)"; leave the function alone
--      when "its device class is not PCI_CLASS_MEMORY_OTHER"; read P2SBC,
--      and only if HIDE is set "pci_bus_write_config_dword(bus, devfn,
--      P2SBC, 0)", read BAR0, "pci_bus_write_config_dword(bus, devfn,
--      P2SBC, P2SBC_HIDE)". coreboot's p2sb_dev_is_hidden reads the vendor
--      ID (0xFFFF hidden, 0x8086 visible). There is no static-table
--      source: the laptop names SBREG only in AML (GNVS field SBRG, used
--      by GPI0._CRS and a PNP0C02 _CRS), which devmgr must not trust for
--      the pool. The window is 16 MiB: coreboot alderlake/Kconfig
--      PCR_BASE_ADDRESS "default 0xfd000000" (0xe0000000 only for PCH-S),
--      and the laptop's own PNP0C02 _CRS "LEN3 = ((SBRG + 0x01000000) -
--      BAS3)" for a non-S PCH. Not verified on the laptop: the base the
--      firmware programs and that the hide bit clears there.
--  * GPIO communities: 64 KiB each (coreboot "GPIO_BASE_SIZE 0x10000"),
--      at SBREG + (port ID << 16) (coreboot pcr.h PCR address scheme).
--      Alder Lake-N has communities 0, 1, 4 and 5 (Linux
--      pinctrl-alderlake.c adln_communities: "ADL_N_COMMUNITY(0, 0, 66,
--      adln_community0_gpps)", (1, 67, 168, ..1), (2, 169, 248, ..4),
--      (3, 249, 256, ..5)), port IDs "PID_GPIOCOM0 0x6e", "PID_GPIOCOM1
--      0x6d", "PID_GPIOCOM4 0x6a", "PID_GPIOCOM5 0x69" (coreboot
--      alderlake/include/soc/pcr_ids.h). The laptop's DSDT GPI0._CRS
--      agrees: "CML0 = (SBRG + 0x006E0000)", "CML1 = (SBRG + 0x006D0000)",
--      "CML4 = (SBRG + 0x006A0000)", "CML5 = (SBRG + 0x00690000)", each
--      Memory32Fixed length 0x10000.
--  Each community is its own pool member (Chipset_Shared), so a future
--  GPIO driver bound to one claims just that community: the claim is
--  shared, ACPI's mappings there stay and the kernel logs the overlap. The
--  rest of the sideband window is carved into the members between them.
--
--  The window's source is the P2SB today. A Windows-style source (the
--  community ranges the ACPI service reads from the GPIO device's _CRS)
--  could replace it behind Devmgr_Sideband's interface: the pool only asks
--  for the window and the communities.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Devmgr_Platform_Pool; use Devmgr_Platform_Pool;

package Devmgr_Sideband_Layout with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  P2SB (00:1F.1) and its sideband window.
   ---------------------------------------------------------------------------
   P2SB_Bus : constant := 0;
   P2SB_Slot : constant := 31;
   P2SB_Function : constant := 1;
   P2SB_Control_Offset : constant := 16#E0#;            --  P2SBC
   P2SB_Hide : constant Unsigned_32 := 2 ** 8;          --  P2SBC_HIDE
   PCI_Class_Offset : constant := 16#0A#;               --  PCI_CLASS_DEVICE
   PCI_Class_Memory_Other : constant Unsigned_16 := 16#0580#;
   PCI_Absent : constant Unsigned_16 := 16#FFFF#;
   PCI_BAR0_Offset : constant := 16#10#;
   PCI_BAR1_Offset : constant := 16#14#;
   SBREG_Bytes : constant := 16#100_0000#;               --  16 MiB
   Port_Bytes  : constant := 16#1_0000#;                 --  one port's PCR window
   GPIO_Community_Bytes : constant := 16#1_0000#;        --  GPIO_BASE_SIZE

   type P2SB_Action is
     (Leave_Alone,     --  something else answers at 00:1F.1: never touched
      Read_Visible,    --  firmware left it visible: read BAR0 directly
      Unhide_Read);    --  hidden: unhide, read BAR0, hide again

   --  Linux p2sb_cache_resources' decision from the class word and P2SBC.
   function Action_Of (Class : Unsigned_16; Control : Unsigned_32) return P2SB_Action is
     (if Class /= PCI_Absent and then Class /= PCI_Class_Memory_Other then Leave_Alone
      elsif (Control and P2SB_Hide) /= 0 then Unhide_Read
      elsif Class = PCI_Absent then Leave_Alone
      else Read_Visible);

   --  SBREG_BAR from BAR0/BAR1: a memory BAR (32- or 64-bit), nonzero,
   --  16 MiB aligned and below the limit: Found, 16 MiB. A hidden device's
   --  all-ones read is Not_Placed.
   PCI_BAR_IO        : constant Unsigned_32 := 2 ** 0;
   PCI_BAR_Type_Mask : constant Unsigned_32 := 2#110#;
   PCI_BAR_Type_32   : constant Unsigned_32 := 2#000#;
   PCI_BAR_Type_64   : constant Unsigned_32 := 2#100#;
   PCI_BAR_Flag_Mask : constant Unsigned_32 := 16#F#;

   function Decode_SBREG (BAR0, BAR1 : Unsigned_32) return Block
   with Post => (if Decode_SBREG'Result.Status = Found then
                   Whole_Pages (Decode_SBREG'Result)
                   and then Decode_SBREG'Result.Bytes = SBREG_Bytes
                   and then Decode_SBREG'Result.Base mod SBREG_Bytes = 0
                   and then Decode_SBREG'Result.Base /= 0
                   and then (BAR0 and PCI_BAR_IO) = 0);

   --  Port IDs (8 bits) and their PCR windows: SBREG + (port << 16).
   subtype Port_ID is Unsigned_64 range 0 .. 255;
   ADL_N_GPIO_Communities : constant := 4;
   ADL_N_GPIO_Ports : constant array (1 .. ADL_N_GPIO_Communities) of Port_ID :=
     [16#6E#, 16#6D#, 16#6A#, 16#69#];   --  communities 0, 1, 4, 5

   function Port_Block (Window : Block; Port : Port_ID) return Block
   with Pre  => Window.Status = Found and then Whole_Pages (Window)
                and then Window.Bytes = SBREG_Bytes,
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

end Devmgr_Sideband_Layout;
