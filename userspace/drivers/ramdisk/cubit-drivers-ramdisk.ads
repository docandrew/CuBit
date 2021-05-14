-------------------------------------------------------------------------------
-- CuBit
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Ramdisk driver
--
-- @description
-- The ramdisk driver is loaded as a module at boot time by GRUB. It expects
-- the kernel to map the ramdisk image (also loaded as a module at boot time)
-- into this process' memory.
-------------------------------------------------------------------------------
with System;

package CuBit.Drivers.Ramdisk is

    procedure setup (rdAddr : System.Address) with SPARK_Mode => On;

end CuBit.Drivers.Ramdisk;
