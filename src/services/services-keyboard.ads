-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------
with Textmode;

package Services.Keyboard with 
    SPARK_Mode => On
is
    -- TODO: figure out how best to model the keyboard,
    -- map scan codes to specific key presses, then map those
    -- key presses to specific chars.
    -- Maybe ScanCode -> Position (row, #of keys to the right)
    -- Position -> Key
    -- then somewhere else -> Key to Character
    --type Keymap is array range 1..255 of Boolean;
    --USKeys : Keymap is (

    caps    : Boolean := False;
    shifted : Boolean := False;

    CAPSLOCK    : constant := 16#3a#;
    LSHIFT      : constant := 16#2a#;
    RSHIFT      : constant := 16#36#;


    ---------------------------------------------------------------------------
    -- start
    -- Entry for keyboard service
    ---------------------------------------------------------------------------
    procedure start;

    ---------------------------------------------------------------------------
    -- Read a single character from the keyboard,
    -- to be called after interrupt is received.
    ---------------------------------------------------------------------------
    procedure readKey with
        Global => (In_Out => (caps, shifted, Textmode.ScreenState, Textmode.cursor));

end Services.Keyboard;
