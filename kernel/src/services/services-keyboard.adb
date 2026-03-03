-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Process.IPC;
with TextIO; use TextIO;
with Strings; use Strings;
with Sysinfo;
with Time;
with x86; use x86;

package body Services.Keyboard
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Start the keyboard handling service
    ---------------------------------------------------------------------------
    procedure start with SPARK_Mode => On is
        use Time;

        event     : Process.Message;
        driverPID : Process.ProcessID := Process.NO_PROCESS;
        code      : Unsigned_8 := 0;
    begin
        -- @TODO turn off caps lock to start.
        println ("Services.Keyboard: Started, waiting for upper-half driver to register.");

        while driverPID = Process.NO_PROCESS loop
            -- poll until the upper-half driver is registered.
            Process.sleep (1 * Seconds);
            driverPID := Process.ProcessID(
                Sysinfo.getInfo (query  => Sysinfo.REGISTERED_DRIVER,
                                 detail => Sysinfo.DRIVER_KEYBOARD));
        end loop;

        println ("Services.Keyboard: found upper-half driver!");

        loop
            -- get notified of new keypress
            event := Process.IPC.receiveEvent;

            -- read it from the keyboard
            in8 (16#60#, code);

            -- Fire-and-forget event to upper-half driver (non-blocking).
            -- No reply needed — eliminates keyboard lag from blocking send().
            Process.IPC.sendEvent (driverPID,
                (tag      => (label  => 1,
                              length => 1,
                              flags  => 0,
                              badge  => 0),
                 capBadge => 0,
                 words    => (0 => Unsigned_64(code), others => 0)));
        end loop;
    end start;

end Services.Keyboard;
