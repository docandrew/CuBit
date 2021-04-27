-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Text I/O
--
-- @description
-- Routines for outputting text to the screen and/or serial port.
-- The term "Text IO" in CuBit refers to basic console output.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Config;
with Serial;

package TextIO 
    with SPARK_Mode => On
is
    
    -- Basic colors for Text IO. Individual drivers may map these to a device-
    -- specific internal color format.
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

    ---------------------------------------------------------------------------
    -- enableSerial
    -- Turn on serial port mirroring of any text output by TextIO
    ---------------------------------------------------------------------------
    procedure enableSerial;

    ---------------------------------------------------------------------------
    -- disableVideo
    -- When serial-only output should be used, even if a video driver has been
    -- enabled. Can be useful for debugging video driver issues.
    ---------------------------------------------------------------------------
    procedure disableVideo;

    ---------------------------------------------------------------------------
    -- Procedure types that a video driver must supply to support Text IO
    ---------------------------------------------------------------------------

    -- Put a single character to the string at a particular row & column.
    type PutCharProc is access procedure (col,row : Natural;
                                          fg, bg  : TextIO.Color;
                                          c       : Character);

    -- Scroll up by one row of characters
    type ScrollUpProc is access procedure;

    -- Clear the screen with a particular color
    type ClearProc is access procedure (bg : TextIO.Color);

    ---------------------------------------------------------------------------
    -- TextIOInterface
    --
    -- Video drivers that support basic console output need to provide the
    -- TextIO package with basic information about the size of the screen (for
    -- monospaced font output) and routines for drawing a character, clearing
    -- the screen and scrolling up by one line.
    -- 
    -- This record contains that information.
    ---------------------------------------------------------------------------
    type TextIOInterface is
    record
        rows   : Natural;
        cols   : Natural;
        put    : PutCharProc;
        scroll : ScrollUpProc;
        clear  : ClearProc;
    end record;
    
    ---------------------------------------------------------------------------
    -- setVideo
    -- Tell TextIO what video driver routines to use.
    ---------------------------------------------------------------------------
    procedure setVideo (driver : TextIOInterface);

    ---------------------------------------------------------------------------
    -- clear
    -- Clears the screen with the given background color.
    ---------------------------------------------------------------------------
    procedure clear (bg : TextIO.Color);

    ---------------------------------------------------------------------------
    -- Print a single char at the current cursor and update cursor.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (ch : in Character; fg,bg : in TextIO.Color);

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
    procedure print (str : in String; fg,bg : in TextIO.Color);

    ---------------------------------------------------------------------------
    -- Print a string with LT_GRAY text, black background.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary.
    ---------------------------------------------------------------------------
    procedure print (str : in String);

    ---------------------------------------------------------------------------
    -- Print a string with a line-feed at end.
    ---------------------------------------------------------------------------
    procedure println (str : in String; fg,bg : in TextIO.Color);

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
    procedure printd (n : in Unsigned_32; fg,bg : in TextIO.Color);
    procedure printd (n : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- printdln (print decimal w/ newline)
    ---------------------------------------------------------------------------
    procedure printdln (n : in Unsigned_32; fg,bg : in TextIO.Color);
    procedure printdln (n : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- printd (print decimal)
    ---------------------------------------------------------------------------
    procedure printd (n : in Unsigned_64; fg,bg : in TextIO.Color);
    procedure printd (n : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- printdln (print decimal w/ newline)
    ---------------------------------------------------------------------------
    procedure printdln (n : in Unsigned_64; fg,bg : in TextIO.Color);
    procedure printdln (n : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- Print an integer to the screen
    ---------------------------------------------------------------------------
    procedure print (i : in Integer; fg,bg : in TextIO.Color);
    procedure print (i : in Integer);

    ---------------------------------------------------------------------------
    -- Print an integer to the screen with terminating newline
    ---------------------------------------------------------------------------
    procedure println (i : in Integer; fg,bg : in TextIO.Color);
    procedure println (i : in Integer);

    ---------------------------------------------------------------------------
    -- Print an Unsigned_8/16/32/64
    ---------------------------------------------------------------------------
    procedure print (u8 : Unsigned_8);
    procedure println (u8 : Unsigned_8);
    procedure print (u16 : Unsigned_16);
    procedure println (u16 : Unsigned_16);
    procedure print (u32 : Unsigned_32);
    procedure println (u32 : Unsigned_32);
    procedure print (u64 : Unsigned_64);
    procedure println (u64 : Unsigned_64);

    ---------------------------------------------------------------------------
    -- Print a boolean to the screen
    ---------------------------------------------------------------------------
    procedure print (b : in Boolean; fg,bg : in TextIO.Color);
    procedure print (b : in Boolean);
    procedure println (b : in Boolean; fg,bg : in TextIO.Color);
    procedure println (b : in Boolean);
    
    ---------------------------------------------------------------------------
    -- Print an address
    ---------------------------------------------------------------------------
    procedure print (addr : System.Address);
    procedure println (addr : System.Address);
    
    ---------------------------------------------------------------------------
    -- Print an address
    ---------------------------------------------------------------------------
    procedure print (addr : Integer_Address);
    procedure println (addr : Integer_Address);

    ---------------------------------------------------------------------------
    -- Print a null-terminated C string. This shouldn't normally be used by
    -- CuBit code, but some compiler-inserted strings (i.e. exception
    -- locations) are null-terminated and should be printed with this function.
    ---------------------------------------------------------------------------
    procedure printz (addr : System.Address);

    ---------------------------------------------------------------------------
    -- Set cursor to a specific x,y coordinate
    ---------------------------------------------------------------------------
    procedure setCursor (x : Natural; y : Natural);

    ---------------------------------------------------------------------------
    -- Get the row of our cursor
    ---------------------------------------------------------------------------
    function getRow return Natural;

    ---------------------------------------------------------------------------
    -- Get column of our cursor
    ---------------------------------------------------------------------------
    function getCol return Natural;

    ---------------------------------------------------------------------------
    -- Scroll all text up one line
    ---------------------------------------------------------------------------
    -- procedure scrollUp with
    --     Global => (In_Out => ScreenState, Output => cursor);
end TextIO;
