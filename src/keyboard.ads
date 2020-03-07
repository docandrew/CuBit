with textmode;

package keyboard with 
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

    caps : Boolean := False;
    shifted : Boolean := False;

    CAPSLOCK    : constant := 16#3a#;
    LSHIFT      : constant := 16#2a#;
    RSHIFT      : constant := 16#36#;

    ---------------------------------------------------------------------------
    -- Read a single character from the keyboard,
    -- to be called after interrupt is received.
    ---------------------------------------------------------------------------
    procedure readKey with
        Global => (In_Out => (caps, shifted, textmode.ScreenState, textmode.cursor));
end keyboard;