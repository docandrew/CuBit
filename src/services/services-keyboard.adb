-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------

--with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces; use Interfaces;
with Process;
with TextIO; use TextIO;
with Strings; use Strings;
with x86; use x86;

package body Services.Keyboard
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Start the keyboard handling service
    ---------------------------------------------------------------------------
    procedure start with SPARK_Mode => On is
        msg : Unsigned_64;
    begin
        loop
            msg := Process.receive;
            println ("Keyboard: received msg");
            readKey;
        end loop;
    end start;

    ---------------------------------------------------------------------------
    -- Read a single character from the keyboard,
    -- to be called after interrupt is received.
    ---------------------------------------------------------------------------
    procedure readKey is
        scanCode : Unsigned_8 := 0;
        --index : Integer;
    begin
        in8 (16#60#, scanCode);
        print (toHexString (Unsigned_32(scanCode)));

        if scanCode < 128 then
            -- Key Press
            case scanCode is
                when LSHIFT | RSHIFT =>
                    shifted := True;
                when CAPSLOCK =>
                    null;
                when others =>
                    if (shifted or caps) then
                        null; --return asciiOf(scanCode);
                    else
                        null; --return asciiOf(scanCode);
                    end if;
            end case;
        else
            -- Key Release
            scanCode := scanCode - 128;

            case scanCode is
                when CAPSLOCK =>
                    caps := not caps;
                when LSHIFT | RSHIFT =>
                    shifted := False;    -- TODO: figure out if there's some weirdness with both shifts being held down
                when others =>
                    null;
            end case;
        end if;

        --return NUL;
    end readKey;

end Services.Keyboard;