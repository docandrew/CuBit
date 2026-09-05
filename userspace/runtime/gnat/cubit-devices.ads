------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed device-inventory protocol shared by devmgr and inspection clients
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Devices is
   --  Device inspection is deliberately separate from reset, rebind, policy,
   --  and disable authority.  These messages return bounded snapshots only.
   OP_INVENTORY_COUNT    : constant Unsigned_32 := 16#0240#;
   OP_INVENTORY_ITEM     : constant Unsigned_32 := 16#0241#;
   OP_XHCI_DIAGNOSTICS   : constant Unsigned_32 := 16#0242#;
   OP_PUBLISH_XHCI_STATS : constant Unsigned_32 := 16#0243#;

   REPLY_OK              : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR           : constant Unsigned_32 := 16#F001#;

   type Device_Kind is
     (Other_Device,
      Storage_Controller,
      Network_Controller,
      Display_Controller,
      Audio_Controller,
      USB_Controller);
   for Device_Kind use
     (Other_Device       => 0,
      Storage_Controller => 1,
      Network_Controller => 2,
      Display_Controller => 3,
      Audio_Controller   => 4,
      USB_Controller     => 5);

   type Driver_State is (Unclaimed, Driver_Starting, Driver_Active,
                         Driver_Failed);
   for Driver_State use
     (Unclaimed       => 0,
      Driver_Starting => 1,
      Driver_Active   => 2,
      Driver_Failed   => 3);

   type Interrupt_Mode is (Interrupt_Polling, Interrupt_MSI, Interrupt_MSIX);
   for Interrupt_Mode use
     (Interrupt_Polling => 0,
      Interrupt_MSI     => 1,
      Interrupt_MSIX    => 2);

   --  A deliberately conservative bound.  Inventory overflow is observable
   --  through the count reply rather than causing allocation during boot.
   MAX_PCI_DEVICES : constant Positive := 128;
end CuBit.Devices;
