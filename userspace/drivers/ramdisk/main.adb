-------------------------------------------------------------------------------
-- CuBit
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Ramdisk driver
--
-- @description
-- The ramdisk driver is loaded as a module at boot time.
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Drivers.Ramdisk;

procedure main is
    use ASCII;

    MAGIC_RAMDISK_ADDRESS : constant Unsigned_64 := 1000;
    rdAddr : System.Address;

    function toAddr is new Ada.Unchecked_Conversion (Unsigned_64, System.Address);
    msg : Unsigned_64;
begin

    debugPrint ("Ramdisk: Loading" & LF);
    -- Get address of ramdisk from kernel
    rdAddr := toAddr (CuBit.Messages.getInfo (MAGIC_RAMDISK_ADDRESS, 0));
    CuBit.Drivers.Ramdisk.setup (rdAddr);

    loop
        msg := CuBit.Messages.recvMsg;

        debugPrint ("Ramdisk: got message " & msg'Image & LF);

    end loop;

end main;
