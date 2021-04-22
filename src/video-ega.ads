-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- EGA Textmode Console Routines
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with TextIO;
with Video;
with Virtmem; use Virtmem;
with x86;

package Video.EGA with
    SPARK_Mode => On
is
    type EGAColor is (
        BLACK, 
        BLUE, 
        GREEN, 
        CYAN, 
        RED, 
        MAGENTA, 
        BROWN, 
        LT_GRAY, 
        DK_GRAY,
        LT_BLUE,
        LT_GREEN,
        LT_CYAN,
        LT_RED,
        LT_MAGENTA,
        YELLOW,
        WHITE);
    
    for EGAColor'Size use 4;       -- 4 bits
    
    for EGAColor use (
        BLACK       => 0, 
        BLUE        => 1, 
        GREEN       => 2, 
        CYAN        => 3, 
        RED         => 4, 
        MAGENTA     => 5, 
        BROWN       => 6, 
        LT_GRAY     => 7, 
        DK_GRAY     => 8,
        LT_BLUE     => 9,
        LT_GREEN    => 10,
        LT_CYAN     => 11,
        LT_RED      => 12,
        LT_MAGENTA  => 13,
        YELLOW      => 14,
        WHITE       => 15);

    type TextBufferChar is
    record
        ch : Character;
        fg : EGAColor;
        bg : EGAColor;
    end record;

    -- two bytes for each character
    for TextBufferChar use
    record
        ch at 0 range 0 .. 7;
        fg at 1 range 0 .. 3;
        bg at 1 range 4 .. 7;
    end record;

    EGA_OFFSET  : constant := 16#000B_8000#;
    COLS        : constant := 80;
    ROWS        : constant := 25;
    SCREEN_ADDR : constant := LINEAR_BASE + EGA_OFFSET;
    SCREEN_END  : constant := SCREEN_ADDR + (2 * COLS * ROWS);
    CURSOR_END  : constant := COLS * ROWS - 1;

    -- our current cursor position, and acceptable range
    -- of cursor positions.
    type CursorType is new Natural range 0 .. CURSOR_END;
    cursor : CursorType;

    type Col is new Natural range 0 .. COLS - 1;
    type Row is new Natural range 0 .. ROWS - 1;

    function "*" (Left : Row; Right : Integer) return CursorType;
    pragma Convention (Intrinsic, "*");
    pragma Inline_Always ("*");
    pragma Pure_Function ("*");

    function "+" (Left : CursorType; Right : Col) return CursorType;
    pragma Convention (Intrinsic, "+");
    pragma Inline_Always ("+");
    pragma Pure_Function ("+");

    type TextBuffer is array (CursorType) of TextBufferChar with Volatile_Components;

    ---------------------------------------------------------------------------
    -- getTextInterface
    -- @return TextIOInterface with the information needed by TextIO to
    --  use this video driver for console output.
    ---------------------------------------------------------------------------
    function getTextInterface return TextIO.TextIOInterface;
       
    ---------------------------------------------------------------------------
    -- Print a single character at a specific location and color
    ---------------------------------------------------------------------------
    procedure put (c, r : Natural; fg,bg : in TextIO.Color; ch : in Character);

    ---------------------------------------------------------------------------
    -- Clear screen
    ---------------------------------------------------------------------------
    procedure clear (bg : in TextIO.Color);

    -- ---------------------------------------------------------------------------
    -- -- Get the row of our cursor
    -- ---------------------------------------------------------------------------
    -- function getRow return Row;

    -- ---------------------------------------------------------------------------
    -- -- Get column of our cursor
    -- ---------------------------------------------------------------------------
    -- function getCol return Col;

    ---------------------------------------------------------------------------
    -- Scroll all text up one line
    ---------------------------------------------------------------------------
    procedure scrollUp;

end Video.EGA;
