-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Video.VGA
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with BuddyAllocator;
with Multiboot;
with TextIO; use TextIO;
with Util;
with Video;
with Virtmem;
with x86;

package body Video.VGA is

    type BitmapCharacter is array (Natural range 1..13) of Unsigned_8;
    type FontMapT is array (Natural range 1..95) of BitmapCharacter;

    -- Basic ASCII character map, starting at ASCII 32
    fontMap : FontMapT := (
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#), -- space
        (16#00#, 16#00#, 16#18#, 16#18#, 16#00#, 16#00#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#), -- !
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#36#, 16#36#, 16#36#, 16#36#), 
        (16#00#, 16#00#, 16#00#, 16#66#, 16#66#, 16#ff#, 16#66#, 16#66#, 16#ff#, 16#66#, 16#66#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#18#, 16#7e#, 16#ff#, 16#1b#, 16#1f#, 16#7e#, 16#f8#, 16#d8#, 16#ff#, 16#7e#, 16#18#), 
        (16#00#, 16#00#, 16#0e#, 16#1b#, 16#db#, 16#6e#, 16#30#, 16#18#, 16#0c#, 16#76#, 16#db#, 16#d8#, 16#70#), 
        (16#00#, 16#00#, 16#7f#, 16#c6#, 16#cf#, 16#d8#, 16#70#, 16#70#, 16#d8#, 16#cc#, 16#cc#, 16#6c#, 16#38#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#18#, 16#1c#, 16#0c#, 16#0e#), 
        (16#00#, 16#00#, 16#0c#, 16#18#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#18#, 16#0c#), 
        (16#00#, 16#00#, 16#30#, 16#18#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#18#, 16#30#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#99#, 16#5a#, 16#3c#, 16#ff#, 16#3c#, 16#5a#, 16#99#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#00#, 16#18#, 16#18#, 16#18#, 16#ff#, 16#ff#, 16#18#, 16#18#, 16#18#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#30#, 16#18#, 16#1c#, 16#1c#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#ff#, 16#ff#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#00#, 16#38#, 16#38#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#60#, 16#60#, 16#30#, 16#30#, 16#18#, 16#18#, 16#0c#, 16#0c#, 16#06#, 16#06#, 16#03#, 16#03#), 
        (16#00#, 16#00#, 16#3c#, 16#66#, 16#c3#, 16#e3#, 16#f3#, 16#db#, 16#cf#, 16#c7#, 16#c3#, 16#66#, 16#3c#), 
        (16#00#, 16#00#, 16#7e#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#78#, 16#38#, 16#18#), 
        (16#00#, 16#00#, 16#ff#, 16#c0#, 16#c0#, 16#60#, 16#30#, 16#18#, 16#0c#, 16#06#, 16#03#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#03#, 16#03#, 16#07#, 16#7e#, 16#07#, 16#03#, 16#03#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#ff#, 16#cc#, 16#6c#, 16#3c#, 16#1c#, 16#0c#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#03#, 16#03#, 16#07#, 16#fe#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#ff#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#c7#, 16#fe#, 16#c0#, 16#c0#, 16#c0#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#30#, 16#30#, 16#30#, 16#30#, 16#18#, 16#0c#, 16#06#, 16#03#, 16#03#, 16#03#, 16#ff#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#e7#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#03#, 16#03#, 16#03#, 16#7f#, 16#e7#, 16#c3#, 16#c3#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#00#, 16#38#, 16#38#, 16#00#, 16#00#, 16#38#, 16#38#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#30#, 16#18#, 16#1c#, 16#1c#, 16#00#, 16#00#, 16#1c#, 16#1c#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#06#, 16#0c#, 16#18#, 16#30#, 16#60#, 16#c0#, 16#60#, 16#30#, 16#18#, 16#0c#, 16#06#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#ff#, 16#ff#, 16#00#, 16#ff#, 16#ff#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#60#, 16#30#, 16#18#, 16#0c#, 16#06#, 16#03#, 16#06#, 16#0c#, 16#18#, 16#30#, 16#60#), 
        (16#00#, 16#00#, 16#18#, 16#00#, 16#00#, 16#18#, 16#18#, 16#0c#, 16#06#, 16#03#, 16#c3#, 16#c3#, 16#7e#), 
        (16#00#, 16#00#, 16#3f#, 16#60#, 16#cf#, 16#db#, 16#d3#, 16#dd#, 16#c3#, 16#7e#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#ff#, 16#c3#, 16#c3#, 16#c3#, 16#66#, 16#3c#, 16#18#), 
        (16#00#, 16#00#, 16#fe#, 16#c7#, 16#c3#, 16#c3#, 16#c7#, 16#fe#, 16#c7#, 16#c3#, 16#c3#, 16#c7#, 16#fe#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#fc#, 16#ce#, 16#c7#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c7#, 16#ce#, 16#fc#), 
        (16#00#, 16#00#, 16#ff#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#fc#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#ff#), 
        (16#00#, 16#00#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#fc#, 16#c0#, 16#c0#, 16#c0#, 16#ff#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#cf#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#ff#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#), 
        (16#00#, 16#00#, 16#7e#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#7e#), 
        (16#00#, 16#00#, 16#7c#, 16#ee#, 16#c6#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#, 16#06#), 
        (16#00#, 16#00#, 16#c3#, 16#c6#, 16#cc#, 16#d8#, 16#f0#, 16#e0#, 16#f0#, 16#d8#, 16#cc#, 16#c6#, 16#c3#), 
        (16#00#, 16#00#, 16#ff#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#), 
        (16#00#, 16#00#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#db#, 16#ff#, 16#ff#, 16#e7#, 16#c3#), 
        (16#00#, 16#00#, 16#c7#, 16#c7#, 16#cf#, 16#cf#, 16#df#, 16#db#, 16#fb#, 16#f3#, 16#f3#, 16#e3#, 16#e3#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#fe#, 16#c7#, 16#c3#, 16#c3#, 16#c7#, 16#fe#), 
        (16#00#, 16#00#, 16#3f#, 16#6e#, 16#df#, 16#db#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#66#, 16#3c#), 
        (16#00#, 16#00#, 16#c3#, 16#c6#, 16#cc#, 16#d8#, 16#f0#, 16#fe#, 16#c7#, 16#c3#, 16#c3#, 16#c7#, 16#fe#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#03#, 16#03#, 16#07#, 16#7e#, 16#e0#, 16#c0#, 16#c0#, 16#e7#, 16#7e#), 
        (16#00#, 16#00#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#ff#), 
        (16#00#, 16#00#, 16#7e#, 16#e7#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#), 
        (16#00#, 16#00#, 16#18#, 16#3c#, 16#3c#, 16#66#, 16#66#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#), 
        (16#00#, 16#00#, 16#c3#, 16#e7#, 16#ff#, 16#ff#, 16#db#, 16#db#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#), 
        (16#00#, 16#00#, 16#c3#, 16#66#, 16#66#, 16#3c#, 16#3c#, 16#18#, 16#3c#, 16#3c#, 16#66#, 16#66#, 16#c3#), 
        (16#00#, 16#00#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#3c#, 16#3c#, 16#66#, 16#66#, 16#c3#), 
        (16#00#, 16#00#, 16#ff#, 16#c0#, 16#c0#, 16#60#, 16#30#, 16#7e#, 16#0c#, 16#06#, 16#03#, 16#03#, 16#ff#), 
        (16#00#, 16#00#, 16#3c#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#3c#), 
        (16#00#, 16#03#, 16#03#, 16#06#, 16#06#, 16#0c#, 16#0c#, 16#18#, 16#18#, 16#30#, 16#30#, 16#60#, 16#60#), 
        (16#00#, 16#00#, 16#3c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#3c#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#c3#, 16#66#, 16#3c#, 16#18#), 
        (16#ff#, 16#ff#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#18#, 16#38#, 16#30#, 16#70#), 
        (16#00#, 16#00#, 16#7f#, 16#c3#, 16#c3#, 16#7f#, 16#03#, 16#c3#, 16#7e#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#fe#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#fe#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#), 
        (16#00#, 16#00#, 16#7e#, 16#c3#, 16#c0#, 16#c0#, 16#c0#, 16#c3#, 16#7e#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#7f#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#7f#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#), 
        (16#00#, 16#00#, 16#7f#, 16#c0#, 16#c0#, 16#fe#, 16#c3#, 16#c3#, 16#7e#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#30#, 16#30#, 16#30#, 16#30#, 16#30#, 16#fc#, 16#30#, 16#30#, 16#30#, 16#33#, 16#1e#), 
        (16#7e#, 16#c3#, 16#03#, 16#03#, 16#7f#, 16#c3#, 16#c3#, 16#c3#, 16#7e#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#fe#, 16#c0#, 16#c0#, 16#c0#, 16#c0#), 
        (16#00#, 16#00#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#00#, 16#00#, 16#18#, 16#00#), 
        (16#38#, 16#6c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#0c#, 16#00#, 16#00#, 16#0c#, 16#00#), 
        (16#00#, 16#00#, 16#c6#, 16#cc#, 16#f8#, 16#f0#, 16#d8#, 16#cc#, 16#c6#, 16#c0#, 16#c0#, 16#c0#, 16#c0#), 
        (16#00#, 16#00#, 16#7e#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#78#), 
        (16#00#, 16#00#, 16#db#, 16#db#, 16#db#, 16#db#, 16#db#, 16#db#, 16#fe#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#fc#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#7c#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#7c#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#c0#, 16#c0#, 16#c0#, 16#fe#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#fe#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#03#, 16#03#, 16#03#, 16#7f#, 16#c3#, 16#c3#, 16#c3#, 16#c3#, 16#7f#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#c0#, 16#e0#, 16#fe#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#fe#, 16#03#, 16#03#, 16#7e#, 16#c0#, 16#c0#, 16#7f#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#1c#, 16#36#, 16#30#, 16#30#, 16#30#, 16#30#, 16#fc#, 16#30#, 16#30#, 16#30#, 16#00#), 
        (16#00#, 16#00#, 16#7e#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#c6#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#18#, 16#3c#, 16#3c#, 16#66#, 16#66#, 16#c3#, 16#c3#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c3#, 16#e7#, 16#ff#, 16#db#, 16#c3#, 16#c3#, 16#c3#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#c3#, 16#66#, 16#3c#, 16#18#, 16#3c#, 16#66#, 16#c3#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#c0#, 16#60#, 16#60#, 16#30#, 16#18#, 16#3c#, 16#66#, 16#66#, 16#c3#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#ff#, 16#60#, 16#30#, 16#18#, 16#0c#, 16#06#, 16#ff#, 16#00#, 16#00#, 16#00#, 16#00#), 
        (16#00#, 16#00#, 16#0f#, 16#18#, 16#18#, 16#18#, 16#38#, 16#f0#, 16#38#, 16#18#, 16#18#, 16#18#, 16#0f#), 
        (16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#, 16#18#), 
        (16#00#, 16#00#, 16#f0#, 16#18#, 16#18#, 16#18#, 16#1c#, 16#0f#, 16#1c#, 16#18#, 16#18#, 16#18#, 16#f0#), 
        (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#06#, 16#8f#, 16#f1#, 16#60#, 16#00#, 16#00#, 16#00#)
    );

    ---------------------------------------------------------------------------
    -- getVGAColor (RGB565)
    ---------------------------------------------------------------------------
    -- function getVGAColor (color : in Video.Color) return Video.RGB565 is
    -- begin
    --     case color is
    --         when Color.BLACK      => return (r => 0,  g => 0,  b => 0);
    --         when Color.BLUE       => return (r => 0,  g => 0,  b => 20);
    --         when Color.GREEN      => return (r => 0,  g => 40, b => 0);
    --         when Color.CYAN       => return (r => 0,  g => 40, b => 20);
    --         when Color.RED        => return (r => 20, g => 0,  b => 0);
    --         when Color.MAGENTA    => return (r => 20, g => 0,  b => 20);
    --         when Color.BROWN      => return (r => 20, g => 10, b => 5);
    --         when Color.LT_GRAY    => return (r => 20, g => 40, b => 20);
    --         when Color.DK_GRAY    => return (r => 10, g => 20, b => 10);
    --         when Color.LT_BLUE    => return (r => 0,  g => 0,  b => 31);
    --         when Color.LT_GREEN   => return (r => 0,  g => 63, b => 0);
    --         when Color.LT_CYAN    => return (r => 0,  g => 63, b => 31);
    --         when Color.LT_RED     => return (r => 31, g => 0,  b => 0);
    --         when Color.LT_MAGENTA => return (r => 31, g => 0,  b => 31);
    --         when Color.YELLOW     => return (r => 31, g => 63, b => 0);
    --         when Color.WHITE      => return (r => 31, g => 63, b => 31);
    --     end case;
    -- end getVGAColor;

    -- function getVGAColor (color : in Video.Color) return Video.RGB8 is
    -- begin
    --     case color is
    --         when Color.BLACK      => return (r => 0,   g => 0,   b => 0);
    --         when Color.BLUE       => return (r => 0,   g => 0,   b => 127);
    --         when Color.GREEN      => return (r => 0,   g => 192, b => 0);
    --         when Color.CYAN       => return (r => 0,   g => 192, b => 192);
    --         when Color.RED        => return (r => 192, g => 0,   b => 0);
    --         when Color.MAGENTA    => return (r => 192, g => 0,   b => 192);
    --         when Color.BROWN      => return (r => 84,  g => 40,  b => 10);
    --         when Color.LT_GRAY    => return (r => 192, g => 192, b => 192);
    --         when Color.DK_GRAY    => return (r => 127, g => 127, b => 127);
    --         when Color.LT_BLUE    => return (r => 0,   g => 0,   b => 255);
    --         when Color.LT_GREEN   => return (r => 0,   g => 255, b => 0);
    --         when Color.LT_CYAN    => return (r => 0,   g => 255, b => 255);
    --         when Color.LT_RED     => return (r => 255, g => 0,   b => 0);
    --         when Color.LT_MAGENTA => return (r => 255, g => 0,   b => 255);
    --         when Color.YELLOW     => return (r => 255, g => 255, b => 0);
    --         when Color.WHITE      => return (r => 255, g => 255, b => 255);
    --     end case;
    -- end getVGAColor;

    function getVGAColor (color : in TextIO.Color) return Video.RGBA8 is
    begin
        case color is
            when TextIO.BLACK      => return (r => 0,   g => 0,   b => 0,   a => 255);
            when TextIO.BLUE       => return (r => 0,   g => 0,   b => 127, a => 255);
            when TextIO.GREEN      => return (r => 0,   g => 192, b => 0,   a => 255);
            when TextIO.CYAN       => return (r => 0,   g => 192, b => 192, a => 255);
            when TextIO.RED        => return (r => 192, g => 0,   b => 0,   a => 255);
            when TextIO.MAGENTA    => return (r => 192, g => 0,   b => 192, a => 255);
            when TextIO.BROWN      => return (r => 84,  g => 40,  b => 10,  a => 255);
            when TextIO.LT_GRAY    => return (r => 192, g => 192, b => 192, a => 255);
            when TextIO.DK_GRAY    => return (r => 127, g => 127, b => 127, a => 255);
            when TextIO.LT_BLUE    => return (r => 0,   g => 0,   b => 255, a => 255);
            when TextIO.LT_GREEN   => return (r => 0,   g => 255, b => 0,   a => 255);
            when TextIO.LT_CYAN    => return (r => 0,   g => 255, b => 255, a => 255);
            when TextIO.LT_RED     => return (r => 255, g => 0,   b => 0,   a => 255);
            when TextIO.LT_MAGENTA => return (r => 255, g => 0,   b => 255, a => 255);
            when TextIO.YELLOW     => return (r => 255, g => 255, b => 0,   a => 255);
            when TextIO.WHITE      => return (r => 255, g => 255, b => 255, a => 255);
        end case;
    end getVGAColor;

    backbufferAddr : System.Address;

    ---------------------------------------------------------------------------
    -- waitForVSync
    -- wait for start of next retrace
    ---------------------------------------------------------------------------
    procedure waitForVSync is
        -- Register 1, bit 3 is vsync. 1 = retrace in progress.
        VGA_INPUT_STATUS : constant x86.IOPort := 16#03DA#;
        VGA_VSYNC_MASK   : Unsigned_8 := 8;
        val : Unsigned_8;
    begin
        loop
            x86.in8 (VGA_INPUT_STATUS, val);
            -- VGA is in retrace
            -- print ("VGA in retrace? "); println (val);
            exit when (val and VGA_VSYNC_MASK) /= 0;
        end loop;

        loop
            x86.in8 (VGA_INPUT_STATUS, val);
            -- wait for start of retrace
            -- print ("VGA start retrace? "); println (val);
            exit when (val and VGA_VSYNC_MASK) = 0;
        end loop;
    end waitForVSync;

    ---------------------------------------------------------------------------
    -- swapBuffers
    -- Copy everything in the backbuffer to video memory.
    ---------------------------------------------------------------------------
    procedure swapBuffers is
        use System.Storage_Elements;
    begin
        -- waitForVSync;
        x86.rep_movsb (framebufferAddr, backbufferAddr, framebufferSize);
    end swapBuffers;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (mbInfo : Multiboot.MultibootInfo) is
        use System.Storage_Elements;
    begin
        w                := Natural(mbInfo.framebuffer_width);
        h                := Natural(mbInfo.framebuffer_height);

        framebufferAddr  := To_Address (mbInfo.framebuffer_addr + Virtmem.LINEAR_BASE);
        framebufferDepth := Storage_Offset(mbInfo.framebuffer_bpp);
        framebufferPitch := Storage_Offset(mbInfo.framebuffer_pitch);
        framebufferSize  := Storage_Offset(h * w) * (framebufferDepth / 8);

        case mbInfo.framebuffer_bpp is
            when 8 =>
                format := Video.Format_RGB332;
            when 16 =>
                format := Video.Format_RGB565;
            when 24 =>
                format := Video.Format_RGB8;
            when 32 =>
                format := Video.Format_RGBA8;
            when others =>
                raise VideoException with "Unsupported bit depth";
        end case;

        rows := Natural(h / (FONT_HEIGHT + VDIST)) - 1;
        cols := Natural(w / (FONT_WIDTH + HDIST));

        -- Allocate memory for a back-buffer so we can do double-buffering.
        BuddyAllocator.alloc (ord  => BuddyAllocator.getOrder (framebufferSize),
                              addr => backbufferAddr);
    end setup;

    ---------------------------------------------------------------------------
    -- getTextInterface
    ---------------------------------------------------------------------------
    function getTextInterface return TextIO.TextIOInterface is
    begin
        return (rows   => rows,
                cols   => cols,
                put    => put'Access,
                scroll => scrollUp'Access, 
                clear  => clear'Access);
    end getTextInterface;

    ---------------------------------------------------------------------------
    -- to8bpp
    -- Downsample 32-bit color to an 8-bit color. There are probably more
    -- sophisticated ways to do this.
    ---------------------------------------------------------------------------
    function to8bpp (c : RGBA8) return Unsigned_8 is
        ret : Video.RGB332;
        function toU8 is new Ada.Unchecked_Conversion (Source => Video.RGB332,
                                                       Target => Unsigned_8);
    begin
        ret.b := c.b / 85;
        ret.g := c.g / 36;
        ret.r := c.r / 36;

        return toU8(ret);
    end to8bpp;

    ---------------------------------------------------------------------------
    -- to16bpp
    ---------------------------------------------------------------------------
    function to16bpp (c : RGBA8) return Unsigned_16 is
        ret : Video.RGB565;
        function toU16 is new Ada.Unchecked_Conversion (Source => Video.RGB565,
                                                        Target => Unsigned_16);
    begin
        ret.b := c.b / 8;
        ret.g := c.g / 4;
        ret.r := c.r / 8;

        return toU16(ret);
    end to16bpp;

    ---------------------------------------------------------------------------
    -- bytesPerPixel
    ---------------------------------------------------------------------------
    function bytesPerPixel (cf : Video.ColorFormat) return System.Storage_Elements.Storage_Offset is
    begin
        case cf is
            when Video.Format_RGB332 => return 1;
            when Video.Format_RGB565 => return 2;
            when Video.Format_RGB8   => return 3;
            when Video.Format_RGBA8  => return 4;
        end case;
    end bytesPerPixel;

    ---------------------------------------------------------------------------
    -- copyBuffer
    -- @TODO we'll eventually want to render to a backbuffer and then copy it
    -- to the video framebuffer all at once when things aren't busy.
    ---------------------------------------------------------------------------

    -- Given x,y coords of a pixel, return the offset into the framebuffer.
    function getOffset (x, y : Natural) return System.Storage_Elements.Storage_Offset is
        use System.Storage_Elements;
    begin
        return Storage_Offset((w * y) + (x)) * (framebufferDepth / 8);
    end getOffset;

    ---------------------------------------------------------------------------
    -- putPixel
    -- @TODO these routines need some work when color format isn't 32-bits.
    ---------------------------------------------------------------------------
    procedure putPixel (pix : Video.RGBA8; x, y : Natural) is
        use System.Storage_Elements;

        -- bpp : Storage_Offset := bytesPerPixel (format);
        -- bytesPerRow : Storage_Offset := bpp * Storage_Offset(w);

        bb : Storage_Array(0..framebufferSize-1)
            with Import, Address => backbufferAddr;

        i  : Storage_Offset := getOffset (x, y);
    begin
        -- downgrade color
        case format is
            when Video.Format_RGB332 =>
                bb(i) := Storage_Element(to8bpp (pix));

            when Video.Format_RGB565 =>
                declare
                    color  : Unsigned_16 := to16bpp (pix);
                    loByte : Unsigned_8 := Util.getByte (color, 0);
                    hiByte : Unsigned_8 := Util.getByte (color, 1);
                begin
                    bb(i)   := Storage_Element(loByte);
                    bb(i+1) := Storage_Element(hiByte);
                end;

            when Video.Format_RGB8 =>
                bb(i)   := Storage_Element(pix.b);
                bb(i+1) := Storage_Element(pix.g);
                bb(i+2) := Storage_Element(pix.r);

            when Video.Format_RGBA8 =>
                bb(i)   := Storage_Element(pix.b);
                bb(i+1) := Storage_Element(pix.g);
                bb(i+2) := Storage_Element(pix.r);
                bb(i+3) := Storage_Element(pix.a);
        end case;
    end putPixel;

    ---------------------------------------------------------------------------
    -- getPixel
    ---------------------------------------------------------------------------
    -- function getPixel (x : Natural;
    --                    y : Natural) return Video.RGBA8 is
    -- begin
    --     return buffer((y * w) + x);
    -- end getPixel;

    ---------------------------------------------------------------------------
    -- clear
    ---------------------------------------------------------------------------
    procedure clear (color : Video.RGBA8) is
        use System.Storage_Elements;

        bb : Storage_Array(0..framebufferSize-1)
            with Import, Address => backbufferAddr;

        idx : Storage_Offset := 0;
    begin
        loop
            bb(idx..idx+3) := (Storage_Element(color.b), 
                               Storage_Element(color.g), 
                               Storage_Element(color.r),
                               Storage_Element(color.a));
            idx := idx + 4;
            exit when idx >= frameBufferSize;
        end loop;

        swapBuffers;
    end clear;

    procedure clear (color : TextIO.Color) is
    begin
        clear (getVGAColor (color));
    end clear;

    ---------------------------------------------------------------------------
    -- drawChar
    -- draw bitmap character at a specific coord (top-left corner of the glyph)
    -- with a transparent background.
    --
    -- @TODO probably faster to render these ahead of time and just blit from a
    -- texture atlas.
    ---------------------------------------------------------------------------
    procedure drawChar (c : Character; x,y : Natural; fg : Video.RGBA8) is
    begin
        if Character'Pos(c) < 32 or Character'Pos(c) > 126 then
            return;
        end if;

        declare
            bitmap : BitmapCharacter := fontMap(Character'Pos(c) - 31);
        begin
            for i in 0..bitmap'Last - 1 loop                 -- each element is one row, starting at bottom
                for j in 0..FONT_WIDTH - 1 loop
                    if Util.isBitSet(bitmap(i), j - 1) then
                        putPixel (fg, x + (FONT_WIDTH+1-j), y + (FONT_HEIGHT + 1 - i));
                    end if;
                end loop;
            end loop;
        end;
    end drawChar;

    ---------------------------------------------------------------------------
    -- drawChar
    -- draw bitmap character at a specific coord (top-left corner of the glyph)
    -- with a colored background.
    ---------------------------------------------------------------------------
    procedure drawChar (c : Character; x, y : Natural; fg, bg : Video.RGBA8) is
    begin
        if Character'Pos(c) < 32 or Character'Pos(c) > 126 then
            return;
        end if;

        declare
            bitmap : BitmapCharacter := fontMap(Character'Pos(c) - 31);
        begin
            for i in 0..bitmap'Last - 1 loop                 -- each element is one row, starting at bottom
                for j in 0..FONT_WIDTH - 1 loop
                    if Util.isBitSet(bitmap(i+1), j) then
                        putPixel (fg, x + (FONT_WIDTH + 1-j), y + (FONT_HEIGHT + 1-i));
                    else
                        putPixel (bg, x + (FONT_WIDTH + 1-j), y + (FONT_HEIGHT + 1-i));
                    end if;
                end loop;
            end loop;
        end;
    end drawChar;

    ---------------------------------------------------------------------------
    -- put
    ---------------------------------------------------------------------------
    procedure put (col, row : Natural; fg, bg : TextIO.Color; ch : Character) is
        x : Natural := col * (FONT_WIDTH + HDIST);
        y : Natural := row * (FONT_HEIGHT + VDIST);
    begin
        drawChar (ch, x, y, getVGAColor(fg), getVGAColor(bg));
    end put;

    ---------------------------------------------------------------------------
    -- scrollUp
    -- Scroll up by just shifting bytes to the left in our backbuffer.
    ---------------------------------------------------------------------------
    procedure scrollUp is
        pragma Suppress (All_Checks);

        use System.Storage_Elements;
        
        shift : Storage_Offset := framebufferPitch * (FONT_HEIGHT + VDIST);
        len   : Storage_Offset := framebufferSize - shift;

        dst   : System.Address := backbufferAddr;
        src   : System.Address := backbufferAddr + shift;
    begin
        x86.rep_movsb (dst, src, len);

        swapBuffers;
    end scrollUp;

end Video.VGA;
