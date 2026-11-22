------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv's view of its launch: HW-002's generic PCI launch
--  (docs/device-matching.md, "Generic PCI launch"). The only unit that
--  knows how devmgr hands over the device.
--
--  @description
--  Authority arrives as startup capabilities (Startup.Registers, the
--  interrupt, devmgr's endpoint); Launch_Configuration.Generic_PCI says
--  where things are. (The firmware is not part of the launch: it is
--  embedded in the driver, RTW89_Firmware_Blob.)
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Launch is

   type Interrupt_Kind is (Polled, Line, MSI, MSI_X);
   function Name (Kind : Interrupt_Kind) return String is
     (case Kind is
        when Polled => "polled", when Line => "intx",
        when MSI => "msi", when MSI_X => "msi-x");

   type Resources is record
      Location          : Unsigned_64 := 0;   --  bus << 8 | dev << 3 | fn
      Vendor, Device    : Unsigned_64 := 0;
      Revision          : Unsigned_64 := 0;
      Registers_Physical : Unsigned_64 := 0;
      Registers_Bytes   : Unsigned_64 := 0;
      Registers_Virtual : Unsigned_64 := 0;
      DMA_Physical      : Unsigned_64 := 0;
      DMA_Bytes         : Unsigned_64 := 0;
      DMA_Virtual       : Unsigned_64 := 0;
      Interrupt_Mode    : Interrupt_Kind := Polled;
      Vectors           : Unsigned_64 := 0;
   end record;

   type Acquire_Status is
     (Acquired, No_Configuration, No_Register_Window, Window_Too_Small,
      No_DMA_Region, DMA_Too_Small, Map_Refused);

   function Name (Status : Acquire_Status) return String is
     (case Status is
        when Acquired           => "acquired",
        when No_Configuration   => "no-launch-configuration",
        when No_Register_Window => "no-register-window",
        when Window_Too_Small   => "register-window-too-small",
        when No_DMA_Region      => "no-dma-region",
        when DMA_Too_Small      => "dma-region-too-small",
        when Map_Refused        => "map-refused");

   --  Read the launch configuration and map the register window.
   procedure Acquire (Device : out Resources; Status : out Acquire_Status);

   --  Tell devmgr we drive the device (it unmasks interrupts), or decline.
   procedure Report_Ready;
   procedure Report_Not_Present;

end RTW89_Launch;
