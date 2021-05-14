-------------------------------------------------------------------------------
-- CuBit
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Keyboard upper-half driver
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with System; use System;

with Cubit.Drivers.Keyboard;

procedure main is
    use ASCII;

    function toAddr is new Ada.Unchecked_Conversion (Unsigned_64, System.Address);
    msg : Unsigned_64;
begin

    debugPrint ("Keyboard Driver: Loading" & LF);

    if CuBit.Messages.registerDriver /= 0 then
        debugPrint ("Keyboard Driver: Registered.");
    else
        debugPrint ("Keyboard Driver: Failed to register. Exiting.");
        return;
    end if;

    Cubit.Drivers.Keyboard.eventLoop;

end main;
