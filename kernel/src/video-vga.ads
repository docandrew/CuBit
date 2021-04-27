-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- VGA Driver
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

with Multiboot;
with TextIO;

package Video.VGA is

    VideoException : exception;

    HDIST       : constant := 1;    -- horizontal distance between letters
    VDIST       : constant := 0;    -- vertical distance between rows of letters
    FONT_WIDTH  : constant := 8;
    FONT_HEIGHT : constant := 13;

    -- Width/height of the screen
    w    : Natural;
    h    : Natural;

    format : Video.ColorFormat;

    -- For mono-spaced bitmap font rendering
    rows     : Natural;
    cols     : Natural;

    framebufferAddr  : System.Address;

    -- bytes per row, though in VirtualBox this seems to be incorrect.
    framebufferPitch : System.Storage_Elements.Storage_Offset;

    -- bits per pixel
    framebufferDepth : System.Storage_Elements.Storage_Offset;
    
    -- in bytes
    framebufferSize  : System.Storage_Elements.Storage_Offset;

    ---------------------------------------------------------------------------
    -- setup
    -- Given a MultibootInfo record containing framebuffer details, initialize
    -- the VGA driver.
    ---------------------------------------------------------------------------
    procedure setup (mbInfo : Multiboot.MultibootInfo);

    ---------------------------------------------------------------------------
    -- getTextInterface
    -- @return Video.TextIOInterface with the information needed by TextIO to
    --  use this video driver for console output.
    ---------------------------------------------------------------------------
    function getTextInterface return TextIO.TextIOInterface;

    ---------------------------------------------------------------------------
    -- putPixel
    ---------------------------------------------------------------------------
    procedure putPixel (pix : Video.RGBA8; x,y : Natural);
    
    ---------------------------------------------------------------------------
    -- getPixel
    ---------------------------------------------------------------------------
    -- function getPixel (x      : XCoord;
    --                    y      : YCoord) return PixelType;

    ---------------------------------------------------------------------------
    -- clear
    ---------------------------------------------------------------------------
    procedure clear (color : Video.RGBA8);
    procedure clear (color : TextIO.Color);

    ---------------------------------------------------------------------------
    -- drawChar
    -- draw bitmap character at a specific coord (top-left corner of the glyph)
    -- with a colored background.
    ---------------------------------------------------------------------------
    procedure drawChar (c : Character; x,y : Natural; fg,bg : Video.RGBA8);

    ---------------------------------------------------------------------------
    -- put
    -- Procedure for Text I/O
    ---------------------------------------------------------------------------
    procedure put (col, row : Natural; fg, bg : TextIO.Color; ch : Character);

    ---------------------------------------------------------------------------
    -- scrollUp
    -- Procedure for Text I/O
    -- Scroll up one line.
    ---------------------------------------------------------------------------
    procedure scrollUp;
    
end Video.VGA;
