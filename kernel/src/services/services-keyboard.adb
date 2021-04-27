-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------

--with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces; use Interfaces;
-- with ASCII;

with Process;
with TextIO; use TextIO;
with Services.Keyboard.Scancodes; use Services.Keyboard.Scancodes;
with Strings; use Strings;
with x86; use x86;

package body Services.Keyboard
    with SPARK_Mode => On
is
    shifted : Boolean := False;
    caps    : Boolean := False;

    type AsciiChar is
    record
        char    : Character;
        shifted : Character;
        caps    : Character;
    end record;

    ---------------------------------------------------------------------------
    -- For basic keyboard testing. Apps will want to manually do conversion and
    -- listen for events.
    ---------------------------------------------------------------------------
    type ScanToCharArr is array (ScanCode) of AsciiChar;
    scanChars : ScanToCharArr := (
        (char => ASCII.ESC, shifted => ASCII.ESC, caps => ASCII.ESC),  -- SCAN_ESC
        (char => '1',       shifted => '!',       caps => '1'),        -- SCAN_1
        (char => '2',       shifted => '@',       caps => '2'),        -- SCAN_2
        (char => '3',       shifted => '#',       caps => '3'),        -- SCAN_3
        (char => '4',       shifted => '4',       caps => '4'),        -- SCAN_4
        (char => '5',       shifted => '%',       caps => '5'),        -- SCAN_5
        (char => '6',       shifted => '^',       caps => '6'),        -- SCAN_6
        (char => '7',       shifted => '&',       caps => '7'),        -- SCAN_7
        (char => '8',       shifted => '*',       caps => '8'),        -- SCAN_8
        (char => '9',       shifted => '(',       caps => '9'),        -- SCAN_9
        (char => '0',       shifted => ')',       caps => '0'),        -- SCAN_0
        (char => '-',       shifted => '_',       caps => '-'),        -- SCAN_MINUS
        (char => '=',       shifted => '+',       caps => '='),        -- SCAN_EQUALS
        (char => ASCII.BS,  shifted => ASCII.BS,  caps => ASCII.BS),   -- SCAN_BACKSPACE
        (char => ASCII.HT,  shifted => ASCII.HT,  caps => ASCII.HT),   -- SCAN_TAB
        (char => 'q',       shifted => 'Q',       caps => 'Q'),        -- SCAN_Q
        (char => 'w',       shifted => 'W',       caps => 'W'),        -- SCAN_W
        (char => 'e',       shifted => 'E',       caps => 'E'),        -- SCAN_E
        (char => 'r',       shifted => 'R',       caps => 'R'),        -- SCAN_R
        (char => 't',       shifted => 'T',       caps => 'T'),        -- SCAN_T
        (char => 'y',       shifted => 'Y',       caps => 'Y'),        -- SCAN_Y
        (char => 'u',       shifted => 'U',       caps => 'U'),        -- SCAN_U
        (char => 'i',       shifted => 'I',       caps => 'I'),        -- SCAN_I
        (char => 'o',       shifted => 'O',       caps => 'O'),        -- SCAN_O
        (char => 'p',       shifted => 'P',       caps => 'P'),        -- SCAN_P
        (char => '[',       shifted => '{',       caps => '['),        -- SCAN_LEFT_BRACKET
        (char => ']',       shifted => '}',       caps => ']'),        -- SCAN_RIGHT_BRACKET
        (char => ASCII.LF,  shifted => ASCII.LF,  caps => ASCII.LF),   -- SCAN_ENTER
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_CTRL
        (char => 'a',       shifted => 'A',       caps => 'A'),        -- SCAN_A
        (char => 's',       shifted => 'S',       caps => 'S'),        -- SCAN_S
        (char => 'd',       shifted => 'D',       caps => 'D'),        -- SCAN_D
        (char => 'f',       shifted => 'F',       caps => 'F'),        -- SCAN_F
        (char => 'g',       shifted => 'G',       caps => 'G'),        -- SCAN_G
        (char => 'h',       shifted => 'H',       caps => 'H'),        -- SCAN_H
        (char => 'j',       shifted => 'J',       caps => 'J'),        -- SCAN_J
        (char => 'k',       shifted => 'K',       caps => 'K'),        -- SCAN_K
        (char => 'l',       shifted => 'L',       caps => 'L'),        -- SCAN_L
        (char => ';',       shifted => ':',       caps => ';'),        -- SCAN_SEMICOLON
        (char => ''',       shifted => '"',       caps => '''),        -- SCAN_APOSTROPHE
        (char => '`',       shifted => '~',       caps => '`'),        -- SCAN_TILDE
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_LEFT_SHIFT
        (char => '\',       shifted => '|',       caps => '\'),        -- SCAN_BACKSLASH
        (char => 'z',       shifted => 'Z',       caps => 'Z'),        -- SCAN_Z
        (char => 'x',       shifted => 'X',       caps => 'X'),        -- SCAN_X
        (char => 'c',       shifted => 'C',       caps => 'C'),        -- SCAN_C
        (char => 'v',       shifted => 'V',       caps => 'V'),        -- SCAN_V
        (char => 'b',       shifted => 'B',       caps => 'B'),        -- SCAN_B
        (char => 'n',       shifted => 'N',       caps => 'N'),        -- SCAN_N
        (char => 'm',       shifted => 'M',       caps => 'M'),        -- SCAN_M
        (char => ',',       shifted => '<',       caps => ','),        -- SCAN_COMMA
        (char => '.',       shifted => '>',       caps => '.'),        -- SCAN_PERIOD
        (char => '/',       shifted => '?',       caps => '/'),        -- SCAN_FORWARD_SLASH
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_RIGHT_SHIFT
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_PRINT_SCREEN
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_ALT
        (char => ' ',       shifted => ' ',       caps => ' '),        -- SCAN_SPACE
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_CAPS_LOCK
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F1
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F2
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F3
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F4
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F5
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F6
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F7
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F8
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F9
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F10
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F11
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_F12
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_NUM_LOCK
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_SCROLL_LOCK
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_HOME
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_UP
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_PAGE_UP
        (char => '-',       shifted => '-',       caps => '-'),        -- SCAN_NUMPAD_MINUS
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_LEFT_ARROW
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_CENTER
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_RIGHT_ARROW
        (char => '+',       shifted => '+',       caps => '+'),        -- SCAN_NUMPAD_PLUS
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_END
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_DOWN_ARROW
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_PAGE_DOWN
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL),  -- SCAN_INSERT
        (char => ASCII.NUL, shifted => ASCII.NUL, caps => ASCII.NUL)   -- SCAN_DELETE
    );

    ---------------------------------------------------------------------------
    -- Start the keyboard handling service
    ---------------------------------------------------------------------------
    procedure start with SPARK_Mode => On is
        msg : Unsigned_64;
    begin
        -- @TODO turn off caps lock to start.
        loop
            msg := Process.receive;
            -- println ("Keyboard: received msg");
            readKey;
        end loop;
    end start;

    ---------------------------------------------------------------------------
    -- asciiOf - for basic testing of keyboard scancodes and functionality
    ---------------------------------------------------------------------------
    function asciiOf (code : ScanCode; shifted : Boolean; caps : Boolean) return Character is
    begin
        if shifted then
            return scanChars(code).shifted;
        elsif caps then
            return scanChars(code).caps;
        else
            return scanChars(code).char;
        end if;
    end asciiOf;

    ---------------------------------------------------------------------------
    -- Read a single character from the keyboard,
    -- to be called after interrupt is received.
    ---------------------------------------------------------------------------
    procedure readKey is
        code  : Unsigned_8 := 0;
        scode : ScanCode;
    begin
        in8 (16#60#, code);
        -- print (toHexString (Unsigned_32(scanCode)));

        if code < 128 then
            scode := ScanCode'Enum_Val(code);
            -- Key Press
            case scode is
                when SCAN_LEFT_SHIFT | SCAN_RIGHT_SHIFT =>
                    shifted := True;
                when SCAN_CAPS_LOCK =>
                    null;
                when others =>
                    print (asciiOf (scode, shifted, caps));
            end case;
        else
            -- Key Release
            code := code - 128;
            scode := ScanCode'Enum_Val(code);
            case scode is
                when SCAN_CAPS_LOCK =>
                    caps := not caps;
                when SCAN_LEFT_SHIFT | SCAN_RIGHT_SHIFT =>
                    shifted := False;
                when others =>
                    null; --print (asciiOf (scode, shifted, caps));
            end case;
        end if;

        --return NUL;
    end readKey;

end Services.Keyboard;