------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The controller's PCI identity and interrupt mode, as devmgr packs them
--  into word 3 of OP_XHCI_CONFIGURE, and the controller quirks Linux
--  applies to that identity.
--
--  @description
--  Word 3: bits 0..7 the interrupt mode (XHCI.Runtime_Interrupt_Mode),
--  8..15 the PCI revision, 16..31 the device ID, 32..47 the vendor ID,
--  48..63 zero.
--
--  Quirks: drivers/usb/host/xhci-pci.c, xhci_pci_quirks() (Linux 6.x).
--  For Intel's Alder Lake-N PCH controller (8086:54ED) Linux sets:
--    * XHCI_LPM_SUPPORT, XHCI_INTEL_HOST, XHCI_AVOID_BEI (every Intel host);
--    * XHCI_RESET_TO_DEFAULT (Tiger Lake / Alder Lake PCH, 0x51ED, 0x54ED);
--    * XHCI_DEFAULT_PM_RUNTIME_ALLOW (every host with HCIVERSION >= 1.2).
--  None changes what this driver does at run time: it enables no link
--  power management (LPM), queues no isochronous TRBs (AVOID_BEI), has no
--  runtime suspend (PM_RUNTIME_ALLOW) and no hibernation path yet
--  (RESET_TO_DEFAULT: at S4 power-off Linux disables ports left in U3
--  without remote wake, so boot firmware is not delayed by them;
--  xhci_pci_poweroff_late; PWR-003 must do this). INTEL_HOST gates EHCI port
--  switching on Panther/Lynx Point only. XHCI_SPURIOUS_PME_STALL,
--  XHCI_PME_STUCK_QUIRK and XHCI_MISSING_CAS do not apply to 0x54ED.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package XHCI_Controller_Identity with SPARK_Mode => On, Pure is

   MODE_MASK       : constant Unsigned_64 := 16#FF#;
   REVISION_SHIFT  : constant := 8;
   DEVICE_SHIFT    : constant := 16;
   VENDOR_SHIFT    : constant := 32;
   BYTE_MASK       : constant Unsigned_64 := 16#FF#;
   ID_MASK         : constant Unsigned_64 := 16#FFFF#;
   RESERVED_MASK   : constant Unsigned_64 := 16#FFFF_0000_0000_0000#;

   type Identity is record
      Vendor   : Unsigned_16 := 0;
      Device   : Unsigned_16 := 0;
      Revision : Unsigned_8 := 0;
   end record;

   function Encode (Mode : Unsigned_8; Id : Identity) return Unsigned_64 is
     (Unsigned_64 (Mode) or Shift_Left (Unsigned_64 (Id.Revision), REVISION_SHIFT)
      or Shift_Left (Unsigned_64 (Id.Device), DEVICE_SHIFT)
      or Shift_Left (Unsigned_64 (Id.Vendor), VENDOR_SHIFT));

   function Well_Formed (Word : Unsigned_64) return Boolean is
     ((Word and RESERVED_MASK) = 0);
   function Mode_Of (Word : Unsigned_64) return Unsigned_8 is
     (Unsigned_8 (Word and MODE_MASK));
   function Identity_Of (Word : Unsigned_64) return Identity is
     ((Vendor   => Unsigned_16 (Shift_Right (Word, VENDOR_SHIFT) and ID_MASK),
       Device   => Unsigned_16 (Shift_Right (Word, DEVICE_SHIFT) and ID_MASK),
       Revision => Unsigned_8 (Shift_Right (Word, REVISION_SHIFT) and BYTE_MASK)));

   VENDOR_INTEL : constant Unsigned_16 := 16#8086#;
   DEVICE_TIGER_LAKE_PCH    : constant Unsigned_16 := 16#A0ED#;
   DEVICE_ALDER_LAKE_PCH    : constant Unsigned_16 := 16#51ED#;
   DEVICE_ALDER_LAKE_N_PCH  : constant Unsigned_16 := 16#54ED#;
   HCI_VERSION_1_2 : constant Unsigned_16 := 16#0120#;

   type Quirk is
     (Intel_Host, LPM_Support, Avoid_BEI, Reset_To_Default, Default_PM_Runtime_Allow);
   type Quirk_Set is array (Quirk) of Boolean;

   function Quirks (Id : Identity; HCI_Version : Unsigned_16) return Quirk_Set is
     ([Intel_Host | LPM_Support | Avoid_BEI => Id.Vendor = VENDOR_INTEL,
       Reset_To_Default => Id.Vendor = VENDOR_INTEL and then
         Id.Device in DEVICE_TIGER_LAKE_PCH | DEVICE_ALDER_LAKE_PCH | DEVICE_ALDER_LAKE_N_PCH,
       Default_PM_Runtime_Allow => HCI_Version >= HCI_VERSION_1_2]);

   --  Which quirks change this driver's run-time behaviour (see above:
   --  none does today), and whether Set holds one.
   Runtime_Effect : constant Quirk_Set := [others => False];
   function Changes_Runtime (Set : Quirk_Set) return Boolean is
     (for some Q in Quirk => Set (Q) and then Runtime_Effect (Q));

   function Quirk_Name (Q : Quirk) return String is
     (case Q is
         when Intel_Host => "intel-host",
         when LPM_Support => "lpm",
         when Avoid_BEI => "avoid-bei",
         when Reset_To_Default => "reset-to-default",
         when Default_PM_Runtime_Allow => "pm-runtime");

end XHCI_Controller_Identity;
