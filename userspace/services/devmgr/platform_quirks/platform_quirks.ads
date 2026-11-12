------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's platform quirks: the hardware-discovery facts that hold only
--  on some platforms (the Intel P2SB sideband window, chipset sideband
--  layouts, GPIO communities, PCI fixups), selected by one typed table.
--
--  @description
--  Every quirk is a row of The_Table (Platform_Quirk_Tables.Tables): a key
--  (the host bridge family, from PCI 00:00.0's vendor and device IDs, and
--  the firmware's Platform_Identity), an effect, and the source that
--  justifies it (Citation). devmgr asks the table what applies to the
--  platform it observed (Observed_Platform) and acts on the effect; it
--  never tests platform IDs itself. The firmware-interpretation quirks are
--  acpi.svc's own table, on the same shared pattern and identity.
--
--  Children:
--    * Intel_P2SB: the P2SB's registers and the typed outcome of one
--      discovery (pure, proved);
--    * Intel_GPIO_Communities: GPIO community layouts and the ACPI GPIO
--      controller that describes them (pure, proved);
--    * Sideband: the config-space protocol that reads the P2SB (the only
--      code that touches 00:1F.1);
--    * GPIO_Bootstrap_Admission: the fallback when the P2SB cannot be
--      read: devmgr's decision on the GPIO controller spans acpi.svc
--      observed before _INI (pure, proved).
--
--  Adding a platform: add a Quirk literal, its row and its Citation. A row
--  whose effect is not yet acted on is not added.
------------------------------------------------------------------------------
pragma Ada_2022;
with Devmgr_Platform_Pool;
with Platform_Identity;
with Platform_Quirk_Tables;

package Platform_Quirks with Pure, SPARK_Mode is

   package PP renames Devmgr_Platform_Pool;
   package PI renames Platform_Identity;
   use type PP.Bridge_Kind;

   --  The platform as devmgr observed it this boot.
   type Observed_Platform is record
      Bridge   : PP.Bridge_Kind := PP.Unknown;  --  PP.Bridge_Of (00:00.0's IDs, edu present)
      Firmware : PI.Identity := PI.No_Identity; --  the sealed FADT's header
   end record;

   --  A row's key: a host bridge family and the firmware it applies to.
   type Platform_Key is record
      Bridge   : PP.Bridge_Kind := PP.Unknown;
      Firmware : PI.Firmware_Match := PI.Any;
   end record;

   function Matches (K : Platform_Key; Platform : Observed_Platform) return Boolean is
     (K.Bridge /= PP.Unknown and then K.Bridge = Platform.Bridge
      and then PI.Matches (K.Firmware, Platform.Firmware));

   ---------------------------------------------------------------------------
   --  Effects.
   ---------------------------------------------------------------------------

   --  Where the chipset's sideband (PCR) window comes from.
   type Sideband_Source is
     (No_Sideband,      --  this row says nothing about the sideband
      P2SB_SBREG,       --  the P2SB's SBREG_BAR (Intel_P2SB, Sideband)
      Test_Stand_In);   --  the headless test profile's edu BAR0 stand-in

   --  Which GPIO communities lie in it.
   type Community_Layout is
     (No_Communities,
      ADL_N_Communities,   --  Intel_GPIO_Communities.ADL_N_GPIO_Ports
      Test_Community);     --  Intel_GPIO_Communities.Test_Community

   --  Which ACPI GPIO controller may describe the communities when the
   --  sideband window could not be read (GPIO_Bootstrap_Admission).
   type GPIO_Controller is
     (No_Controller,
      ADL_N_INTC1057);     --  Intel_GPIO_Communities.ADL_N_GPIO_Controller_HID

   type Effect is record
      Sideband    : Sideband_Source := No_Sideband;
      Communities : Community_Layout := No_Communities;
      Controller  : GPIO_Controller := No_Controller;
   end record;

   ---------------------------------------------------------------------------
   --  The table.
   ---------------------------------------------------------------------------
   type Quirk is
     (Intel_ADL_N_P2SB_Sideband,     --  read the sideband window from the P2SB
      Intel_ADL_N_GPIO_Bootstrap,    --  ... or admit GPI0's _CRS spans before _INI
      QEMU_Q35_Test_Sideband);       --  the headless test profile

   package Tables is new Platform_Quirk_Tables.Tables
     (Quirk => Quirk, Key => Platform_Key, Effect => Effect,
      Observed => Observed_Platform, Matches => Matches);

   The_Table : constant Tables.Table :=
     [Intel_ADL_N_P2SB_Sideband =>
        (Applies_To => (Bridge => PP.Alder_Lake_N, Firmware => (Kind => PI.Any_Firmware, others => <>)),
         Changes    => (Sideband => P2SB_SBREG, Communities => ADL_N_Communities,
                        Controller => No_Controller),
         Source     => Platform_Quirk_Tables.Linux_Source),
      Intel_ADL_N_GPIO_Bootstrap =>
        (Applies_To => (Bridge => PP.Alder_Lake_N, Firmware => (Kind => PI.Any_Firmware, others => <>)),
         Changes    => (Sideband => No_Sideband, Communities => ADL_N_Communities,
                        Controller => ADL_N_INTC1057),
         Source     => Platform_Quirk_Tables.Firmware_Tables),
      QEMU_Q35_Test_Sideband =>
        (Applies_To => (Bridge => PP.QEMU_Q35_Test, Firmware => (Kind => PI.Any_Firmware, others => <>)),
         Changes    => (Sideband => Test_Stand_In, Communities => Test_Community,
                        Controller => No_Controller),
         Source     => Platform_Quirk_Tables.CuBit_Hardware_Log)];

   function Citation (Q : Quirk) return String is
     (case Q is
        when Intel_ADL_N_P2SB_Sideband =>
          "Linux drivers/platform/x86/p2sb.c (P2SBC 0xe0, HIDE bit 8, devfn 31.1); " &
          "coreboot soc/intel/alderlake pcr_ids.h, Kconfig PCR_BASE_ADDRESS",
        when Intel_ADL_N_GPIO_Bootstrap =>
          "IdeaPad Slim 3i DSDT GPI0._HID INTC1057 / _CRS four Memory32Fixed 64 KiB at " &
          "SBRG+0x6E/6D/6A/69_0000; Linux pinctrl-alderlake.c binds INTC1057 from _CRS; " &
          "laptop images 11/12: P2SB unhide ignored",
        when QEMU_Q35_Test_Sideband =>
          "headless platform-pool test profile: QEMU q35 with the edu device (docs/specs/edu.rst)");

   --  The sideband source, community layout and bootstrap controller the
   --  table gives Platform (the first applicable row that names one).
   function Sideband_Of (Platform : Observed_Platform) return Sideband_Source;
   function Communities_Of (Platform : Observed_Platform) return Community_Layout;
   function Controller_Of (Platform : Observed_Platform) return GPIO_Controller;

end Platform_Quirks;
