-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- EGA Textmode Console Routines
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Config;
with Strings; use Strings;

package body Video.EGA with
    SPARK_Mode => On
is

    function "*" (Left : Row; Right : Integer) return CursorType is
    begin
        return CursorType(left) * CursorType(right);
    end "*";

    function "+" (Left : CursorType; Right : Col) return CursorType is
    begin
        return Left + CursorType(Right);
    end "+";

    ---------------------------------------------------------------------------
    -- getEGAColor
    -- Convert generic color type to an EGA color type.
    ---------------------------------------------------------------------------
    function getEGAColor (color : in TextIO.Color) return EGAColor is
    begin
        case color is
            when TextIO.BLACK      => return BLACK;
            when TextIO.BLUE       => return BLUE;
            when TextIO.GREEN      => return GREEN;
            when TextIO.CYAN       => return CYAN;
            when TextIO.RED        => return RED;
            when TextIO.MAGENTA    => return MAGENTA;
            when TextIO.BROWN      => return BROWN;
            when TextIO.LT_GRAY    => return LT_GRAY;
            when TextIO.DK_GRAY    => return DK_GRAY;
            when TextIO.LT_BLUE    => return LT_BLUE;
            when TextIO.LT_GREEN   => return LT_GREEN;
            when TextIO.LT_CYAN    => return LT_CYAN;
            when TextIO.LT_RED     => return LT_RED;
            when TextIO.LT_MAGENTA => return LT_MAGENTA;
            when TextIO.YELLOW     => return YELLOW;
            when TextIO.WHITE      => return WHITE;
        end case;
    end getEGAColor;

    ---------------------------------------------------------------------------
    -- The actual screen we are blitting chars to.
    ---------------------------------------------------------------------------
    screen : TextBuffer with
        Address => System.Storage_Elements.To_Address (SCREEN_ADDR),
        Async_Readers => True,
        Effective_Writes => True;

    -- get the value at a specific coordinate
    --function get (x : in Col; y : in Row) return TextBufferChar is
    --begin
    --    return screen(CursorType(y) * COLS + CursorType(x));
    --end get;

    ---------------------------------------------------------------------------
    -- getTextInterface
    ---------------------------------------------------------------------------
    function getTextInterface return TextIO.TextIOInterface is
    begin
        return (rows   => Natural(ROWS),
                cols   => Natural(COLS),
                put    => put'Access,
                scroll => scrollUp'Access, 
                clear  => clear'Access);
    end getTextInterface;

    ---------------------------------------------------------------------------
    -- print character to EGA buffer
    ---------------------------------------------------------------------------
    procedure put (c, r : Natural; fg,bg : in TextIO.Color; ch : in Character)
    is
        egaFG : constant EGAColor := getEGAColor (fg);
        egaBG : constant EGAColor := getEGAColor (bg);
    begin
        screen(Row(r) * COLS + Col(c)) := (ch, egaFG, egaBG);
    end put;

    ---------------------------------------------------------------------------
    -- clear EGA text buffer
    ---------------------------------------------------------------------------
    procedure clear (bg : TextIO.Color) is
    begin
        for c in Col'range loop
            for r in Row'range loop
                put (Natural(c), Natural(r), bg, bg, ' ');
            end loop;
        end loop;
    end clear;

    ---------------------------------------------------------------------------
    -- Scroll all text up one line.
    ---------------------------------------------------------------------------
    procedure scrollUp is
    begin
        for c in Col'range loop
            for r in 0 .. (Row'last - 1) loop
                screen(r * COLS + c) := screen((r + 1) * COLS + c);
            end loop;
        end loop;

        -- clear line at end
        for c in Col'range loop
            screen (Row'Last * COLS + c) := (' ', BLACK, BLACK);
        end loop;
    end scrollUp;

end Video.EGA;