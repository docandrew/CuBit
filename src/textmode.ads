-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- EGA Textmode Console Routines
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with Virtmem; use Virtmem;
with x86;

package textmode with
    SPARK_Mode => On,
    Abstract_State => (ScreenState with External => (Async_Readers => True,
                                                     Effective_Writes => True))
is
    type Color is (
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
    
    for Color'Size use 4;       -- 4 bits
    
    for Color use (
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
            fg : Color;
            bg : Color;
        end record;

    -- two bytes for each character
    for TextBufferChar use
        record
            ch at 0 range 0 .. 7;
            fg at 1 range 0 .. 3;
            bg at 1 range 4 .. 7;
        end record;

    --CONSOLE_BUFSIZE : constant  := 16#0000_4000#;
    EGA_OFFSET : constant       := 16#000B_8000#;
    COLS : constant             := 80;
    ROWS : constant             := 25;
    SCREEN_ADDR : constant      := LINEAR_BASE + EGA_OFFSET;
    SCREEN_END : constant       := SCREEN_ADDR + (2 * COLS * ROWS);
    CURSOR_END : constant       := COLS * ROWS - 1;

    -- our current cursor position, and acceptable range
    -- of cursor positions.
    type CursorType is new Natural range 0 .. CURSOR_END;
    cursor : CursorType;

    type Col is new Natural range 0 .. COLS - 1;
    type Row is new Natural range 0 .. ROWS - 1;

    -- TODO: define a much larger buffer here that we can use for scrolling later on.
    type TextBuffer is array (CursorType) of TextBufferChar with Volatile_Components;
       
    ---------------------------------------------------------------------------
    -- Return the TextBufferChar at a specific Column and Row
    ---------------------------------------------------------------------------
    --function get (x : in Col; y : in Row) return TextBufferChar;

    ---------------------------------------------------------------------------
    -- Print a single character at a specific location and color
    ---------------------------------------------------------------------------
    procedure put (x : in Col; y : in Row; fg,bg : in Color; ch : in Character) with
        Global => (Output => ScreenState);

    ---------------------------------------------------------------------------
    -- Clear screen
    ---------------------------------------------------------------------------
    procedure clear (bg : in Color) with
        Global => (Output => (ScreenState, cursor));

    ---------------------------------------------------------------------------
    -- Print a single char at the current cursor and update cursor.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (ch : in Character; fg,bg : in Color) with
        Global => (In_Out => (ScreenState, cursor, x86.IOPortState));

    ---------------------------------------------------------------------------
    -- Print a single char with LT_GRAY text, black background.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (ch : in Character);

    ---------------------------------------------------------------------------
    -- Print a string at the current cursor and update cursor.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (str : in String; fg,bg : in Color);

    ---------------------------------------------------------------------------
    -- Print a string with LT_GRAY text, black background.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (str : in String);

    ---------------------------------------------------------------------------
    -- Print a string with a line-feed at end.
    ---------------------------------------------------------------------------
    procedure println (str : in String; fg,bg : in Color);

    ---------------------------------------------------------------------------
    -- Print a string with LT_GRAY text, line-feed at end.
    ---------------------------------------------------------------------------
    procedure println (str : in String);

    ---------------------------------------------------------------------------
    -- Just print a newline on its own
    ---------------------------------------------------------------------------
    procedure println;

    ---------------------------------------------------------------------------
    -- printd (print decimal)
    ---------------------------------------------------------------------------
    procedure printd(n : in Unsigned_32; fg,bg : in Color);
    procedure printd(n : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- printdln (print decimal w/ newline)
    ---------------------------------------------------------------------------
    procedure printdln(n : in Unsigned_32; fg,bg : in Color);
    procedure printdln(n : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- printd (print decimal)
    ---------------------------------------------------------------------------
    procedure printd(n : in Unsigned_64; fg,bg : in Color);
    procedure printd(n : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- printdln (print decimal w/ newline)
    ---------------------------------------------------------------------------
    procedure printdln(n : in Unsigned_64; fg,bg : in Color);
    procedure printdln(n : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- Print an integer to the screen
    ---------------------------------------------------------------------------
    procedure print(i : in Integer; fg,bg : in Color);
    procedure print(i : in Integer);

    ---------------------------------------------------------------------------
    -- Print an integer to the screen with terminating newline
    ---------------------------------------------------------------------------
    procedure println(i : in Integer; fg,bg : in Color);
    procedure println(i : in Integer);

    ---------------------------------------------------------------------------
    -- Print an Unsigned_8/16/32/64
    ---------------------------------------------------------------------------
    procedure print(u8 : Unsigned_8);
    procedure println(u8 : Unsigned_8);
    procedure print(u16 : Unsigned_16);
    procedure println(u16 : Unsigned_16);
    procedure print(u32 : Unsigned_32);
    procedure println(u32 : Unsigned_32);
    procedure print(u64 : Unsigned_64);
    procedure println(u64 : Unsigned_64);

    ---------------------------------------------------------------------------
    -- Print a boolean to the screen
    ---------------------------------------------------------------------------
    procedure print(b : in Boolean; fg,bg : in Color);
    procedure print(b : in Boolean);
    procedure println(b : in Boolean; fg,bg : in Color);
    procedure println(b : in Boolean);
    
    ---------------------------------------------------------------------------
    -- Print an address
    ---------------------------------------------------------------------------
    procedure print(addr : System.Address);
    procedure println(addr : System.Address);
    
    ---------------------------------------------------------------------------
    -- Print an address
    ---------------------------------------------------------------------------
    procedure print(addr : Integer_Address);
    procedure println(addr : Integer_Address);

    ---------------------------------------------------------------------------
    -- Print a null-terminated C string. This shouldn't normally be used by
    -- CuBit code, but some compiler-inserted strings (i.e. exception
    -- locations) are null-terminated and should be printed with this function.
    ---------------------------------------------------------------------------
    procedure printz(addr : System.Address);

    ---------------------------------------------------------------------------
    -- Set cursor to a specific x,y coordinate
    ---------------------------------------------------------------------------
    procedure setCursor (x : Col; y : Row) with
        Global => (Output => cursor);

    ---------------------------------------------------------------------------
    -- Get the row of our cursor
    ---------------------------------------------------------------------------
    function getRow return Row with
        Global => (Input => cursor),
        Depends => (getRow'Result => cursor),
        Post => (if cursor = CursorType'last then getRow'Result = Row'last);

    ---------------------------------------------------------------------------
    -- Get column of our cursor
    ---------------------------------------------------------------------------
    function getCol return Col with
        Global => (Input => cursor),
        Depends => (getCol'Result => cursor),
        Post => (if cursor = CursorType'last then getCol'Result = Col'last);

    ---------------------------------------------------------------------------
    -- Scroll all text up one line
    ---------------------------------------------------------------------------
    procedure scrollUp with
        Global => (In_Out => ScreenState, Output => cursor);

private

end textmode;