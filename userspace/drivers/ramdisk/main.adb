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
with CuBit.Messages;

procedure main is
begin

    CuBit.Messages.sendMsg (1, 2);

end main;