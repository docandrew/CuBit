------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The virtio 1.0 PCI transport (virtio 1.0 4.1.4): the common, notify and
--  device configuration structures in a memory BAR devmgr has mapped at
--  CuBit.Virtio_Net_Control.Modern_Virtual_Address. Every offset devmgr
--  sends is checked against the bytes it mapped before any access.
------------------------------------------------------------------------------
package Virtio.Modern is

   --  Features (virtio 1.0 6, 5.1.3).
   F_VERSION_1 : constant Unsigned_64 := Shift_Left (1, 32);
   F_NET_MAC   : constant Unsigned_64 := Shift_Left (1, 5);
   F_EVENT_IDX : constant Unsigned_64 := Shift_Left (1, 29);

   --  The layout devmgr found; False if it does not fit what was mapped.
   procedure Bind
     (Common, Device, Notify, Multiplier, Mapped : Unsigned_64; OK : out Boolean);

   procedure Reset;

   --  Reset, acknowledge, and agree on Wanted and the device's features;
   --  VERSION_1 must be among them. OK once the device accepts (FEATURES_OK).
   procedure Negotiate (Wanted : Unsigned_64; Agreed : out Unsigned_64; OK : out Boolean);

   --  Queue Index: Size entries (at most the device's), its three parts'
   --  physical addresses, MSI-X vector Vector; enabled if OK.
   procedure Setup_Queue
     (Index : Unsigned_16; Size : Unsigned_16; Desc, Driver, Device : Unsigned_64;
      Vector : Unsigned_16; OK : out Boolean);

   procedure Set_Config_Vector (Vector : Unsigned_16; OK : out Boolean);

   --  Everything is set up: DRIVER_OK.
   procedure Start;

   procedure Notify (Index : Unsigned_16);

   --  Device configuration byte I (virtio-net: the MAC is bytes 0 .. 5).
   function Device_Byte (I : Natural) return Unsigned_8;

end Virtio.Modern;
