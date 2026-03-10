-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with Process.IPC;
with TextIO; use TextIO;
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
            Process.sleep (100 * Milliseconds);
            driverPID := Process.ProcessID(
                Sysinfo.getInfo (query  => Sysinfo.REGISTERED_DRIVER,
                                 detail => Sysinfo.DRIVER_KEYBOARD));
        end loop;

        println ("Services.Keyboard: found upper-half driver!");

        --  Flush any stale bytes left in the PS/2 output buffer from GRUB
        --  or mouse initialization.
        flushPS2 : declare
            status : Unsigned_8;
            ignore : Unsigned_8;
        begin
            loop
                in8 (16#64#, status);
                exit when (status and 16#01#) = 0;
                in8 (16#60#, ignore);
            end loop;
        end flushPS2;

        loop
            -- get notified of new keypress
            event := Process.IPC.receiveEvent;

            -- Check PS/2 status register before reading data port.
            -- Bit 0: output buffer full (data available)
            -- Bit 5: auxiliary output (1 = data from mouse, 0 = keyboard)
            -- Skip if no data or if data is from the mouse port.
            checkAndRead : declare
                status : Unsigned_8;
            begin
                in8 (16#64#, status);

                if (status and 16#01#) /= 0
                    and then (status and 16#20#) = 0
                then
                    in8 (16#60#, code);

                    -- Re-read the registered driver PID each keystroke so
                    -- that when a new process (e.g. DOOM) calls
                    -- registerDriver, we start routing to it without
                    -- restarting the service.
                    driverPID := Process.ProcessID(
                        Sysinfo.getInfo (
                            query  => Sysinfo.REGISTERED_DRIVER,
                            detail => Sysinfo.DRIVER_KEYBOARD));

                    if driverPID /= Process.NO_PROCESS then
                        Process.IPC.sendEvent (driverPID,
                            (tag      => (label  => 1,
                                          length => 1,
                                          flags  => 0,
                                          badge  => 0),
                             capBadge => 0,
                             words    => (0 => Unsigned_64(code),
                                          others => 0)));
                    end if;
                end if;
            end checkAndRead;
        end loop;
    end start;

end Services.Keyboard;
