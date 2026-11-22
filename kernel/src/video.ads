-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- VESA Video
--
-- @description
-- This package contains types for the various framebuffer pixel formats we
-- expect to see. For now, running on virtualized hardware, there isn't a whole
-- lot of diversity.
-------------------------------------------------------------------------------
package Video is
    pragma No_Elaboration_Code_All;
    pragma Pure;

    type ColorFormat is (Format_RGB332, 
                         Format_RGB565,
                         Format_RGB8,
                         Format_RGBA8);

    -- 8-bit color
    type RGB332 is
    record
        b : Natural range 0..3;
        g : Natural range 0..7;
        r : Natural range 0..7;
    end record with Size => 8;

    for RGB332 use
    record
        b at 0 range 0..1;
        g at 0 range 2..4;
        r at 0 range 5..7;
    end record;

    -- 16-bit color
    type RGB565 is
    record
        b : Natural range 0..31;
        g : Natural range 0..63;
        r : Natural range 0..31;
    end record with Size => 16;

    for RGB565 use
    record
        b at 0  range 0..4;
        g at 0  range 5..10;
        r at 0  range 11..15;
    end record;
    
    -- 24-bit color
    type RGB8 is
    record
        r : Natural range 0..255;
        g : Natural range 0..255;
        b : Natural range 0..255;
    end record with Size => 32;

    for RGB8 use
    record
        b at 0 range 0..7;
        g at 1 range 0..7;
        r at 2 range 0..7;
    end record;

    -- 32-bit color
    type RGBA8 is
    record
        b : Natural range 0..255;
        g : Natural range 0..255;
        r : Natural range 0..255;
        a : Natural range 0..255;
    end record with Size => 32;

    for RGBA8 use
    record
        b at 0 range 0..7;
        g at 1 range 0..7;
        r at 2 range 0..7;
        a at 3 range 0..7;
    end record;

end Video;
