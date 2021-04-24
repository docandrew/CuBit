-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service
-------------------------------------------------------------------------------
with TextIO;

package Services.Keyboard with 
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- start
    -- Entry for keyboard service
    ---------------------------------------------------------------------------
    procedure start;

    ---------------------------------------------------------------------------
    -- Read a single character from the keyboard,
    -- to be called after interrupt is received.
    ---------------------------------------------------------------------------
    procedure readKey;

end Services.Keyboard;
