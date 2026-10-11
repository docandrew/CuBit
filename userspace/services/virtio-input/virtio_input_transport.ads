------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The virtio 1.0 PCI transport (virtio 1.0 4.1.4) for one virtio-input
--  device: common, notification and device configuration structures in the
--  memory BAR devmgr mapped at CuBit.Virtio_Input_Control's address. Every
--  offset devmgr sends is checked against the bytes it mapped before use.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Virtio_Input_Transport is

   F_VERSION_1 : constant Unsigned_64 := Shift_Left (1, 32);
   NO_VECTOR   : constant Unsigned_16 := 16#FFFF#;

   --  The layout devmgr found; False if it does not fit what was mapped.
   procedure Bind
     (Common, Device, Notify, Multiplier, Mapped : Unsigned_64; OK : out Boolean);

   procedure Reset;

   --  Reset, acknowledge, and agree on VERSION_1 only (virtio-input defines
   --  no device features). OK once the device accepts (FEATURES_OK).
   procedure Negotiate (OK : out Boolean);

   --  The device's maximum size for queue Index (0: none).
   function Queue_Limit (Index : Unsigned_16) return Unsigned_16;

   --  Queue Index: Size entries, its three parts' physical addresses and
   --  MSI-X table entry Vector; enabled if OK.
   procedure Setup_Queue
     (Index : Unsigned_16; Size : Unsigned_16; Desc, Driver, Device : Unsigned_64;
      Vector : Unsigned_16; OK : out Boolean);

   procedure Set_Config_Vector (Vector : Unsigned_16; OK : out Boolean);

   --  Everything is set up: DRIVER_OK.
   procedure Start;
   procedure Fail;
   procedure Notify (Index : Unsigned_16);

   --  virtio-input configuration (virtio 1.2 5.8.4): select a field, then
   --  read its Size bytes. Size zero means the device has no such field.
   Config_Union_Bytes : constant := 128;
   subtype Config_Index is Natural range 0 .. Config_Union_Bytes - 1;
   procedure Select_Config (Selector, Subselector : Unsigned_8; Size : out Unsigned_8);
   function Config_Byte (Index : Config_Index) return Unsigned_8;
   function Config_Word (Index : Config_Index) return Unsigned_32
     with Pre => Index <= Config_Index'Last - 3;

end Virtio_Input_Transport;
