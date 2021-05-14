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

        event     : Unsigned_64;
        driverPID : Process.ProcessID := Process.NO_PROCESS;
        code      : Unsigned_8 := 0;
        reply     : Unsigned_64;
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

            -- send it to the upper-half driver and get reply
            reply := Process.IPC.send (driverPID, Unsigned_64(code));
        end loop;
    end start;

end Services.Keyboard;
