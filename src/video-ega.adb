-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- EGA Textmode Console Routines
--
-- TODO: use generics to reduce some of the redundancy here
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Config;
with Strings; use Strings;
with Video;

package body Video.EGA with
    Refined_State => (ScreenState => screen),
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- getEGAColor
    -- Convert generic color type to an EGA color type.
    ---------------------------------------------------------------------------
    function getEGAColor (color : in TextIO.Color) return EGAColor is
    begin
        case color is
            when Color.BLACK      => return EGAColor.BLACK;
            when Color.BLUE       => return EGAColor.BLUE;
            when Color.GREEN      => return EGAColor.GREEN;
            when Color.CYAN       => return EGAColor.CYAN;
            when Color.RED        => return EGAColor.RED;
            when Color.MAGENTA    => return EGAColor.MAGENTA;
            when Color.BROWN      => return EGAColor.BROWN;
            when Color.LT_GRAY    => return EGAColor.LT_GRAY;
            when Color.DK_GRAY    => return EGAColor.DK_GRAY;
            when Color.LT_BLUE    => return EGAColor.LT_BLUE;
            when Color.LT_GREEN   => return EGAColor.LT_GREEN;
            when Color.LT_CYAN    => return EGAColor.LT_CYAN;
            when Color.LT_RED     => return EGAColor.LT_RED;
            when Color.LT_MAGENTA => return EGAColor.LT_MAGENTA;
            when Color.YELLOW     => return EGAColor.YELLOW;
            when Color.WHITE      => return EGAColor.WHITE;
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
    procedure put (x : in Col; y : in Row; fg,bg : in TextIO.Color; ch : in Character) with
        Refined_Global => (Output => screen)
    is
        pragma Annotate (GNATProve, Intentional, "might not be written", 
                         "SPARK does not recognize writes to individual elements as affecting the whole.");
    
        egaFG : EGAColor := getEGAColor (fg);
        egaBG : EGAColor := getEGAColor (bg);
    begin
        screen (CursorType(y) * COLS + CursorType(x)) := (ch, egaFG, egaBG);
    end put;

    ---------------------------------------------------------------------------
    -- clear EGA text buffer
    ---------------------------------------------------------------------------
    procedure clear (bg : TextIO.Color) with
        Refined_Global => (Output => (screen, cursor))
    is
        egaBG : EGAColor := getEGAColor (bg);
    begin
        for x in Col'range loop
            for y in Row'range loop
                put (x, y, egaBG, egaBG, ' ');
            end loop;
        end loop;

        setCursor(0,0);
    end clear;

    -- Scroll all text up one line. Set cursor to bottom left.
    procedure scrollUp 
    is
    begin
        for y in 0 .. (Row'last - 1) loop
            for x in Col'range loop
                screen (CursorType(y) * COLS + CursorType(x)) := 
                    screen(CursorType(y + Row(1)) * COLS + CursorType(x));
            end loop;
        end loop;

        -- clear line at end
        for x in Col'range loop
            put (x, Row'last, BLACK, BLACK, ' ');
        end loop;
        
        -- move cursor to bottom left of screen done in TextIO now.
        -- setCursor(0, Row'last);
    end scrollUp;

end Video.EGA;