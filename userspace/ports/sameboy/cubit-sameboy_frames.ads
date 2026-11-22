------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SameBoy's picture and pacing (docs/c-removal.md): the 160x144 screen
--  scaled 3x into the window buffer, and emulated cycles to wall time.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.SameBoy_Frames with SPARK_Mode, Pure is

   Screen_Width  : constant := 160;
   Screen_Height : constant := 144;
   Scale         : constant := 3;
   View_Width    : constant := Screen_Width * Scale;
   View_Height   : constant := Screen_Height * Scale;

   type Screen_Pixels is array (0 .. Screen_Width * Screen_Height - 1)
     of Unsigned_32 with Convention => C;
   type View_Pixels is array (0 .. View_Width * View_Height - 1)
     of Unsigned_32 with Convention => C;

   --  The screen pixel shown at a view pixel.
   function Source_Of (Index : Natural) return Natural is
     ((Index / View_Width / Scale) * Screen_Width
      + (Index mod View_Width) / Scale)
     with Pre => Index < View_Width * View_Height,
          Post => Source_Of'Result < Screen_Width * Screen_Height;

   --  Each screen pixel becomes a Scale x Scale block.
   procedure Enlarge (Screen : Screen_Pixels; View : out View_Pixels)
     with Relaxed_Initialization => View,
          Post => View'Initialized and then
                  (for all I in View'Range => View (I) = Screen (Source_Of (I)));

   --  GB_run reports 8 MHz cycle units (2 per clock at the emulated clock
   --  rate, including CGB double speed): their duration in nanoseconds.
   --  Implausibly long runs saturate.
   Maximum_Cycles : constant := 2 ** 34;
   function Nanoseconds (Cycles : Unsigned_64; Clock_Rate : Unsigned_32)
     return Unsigned_64
     with Pre => Clock_Rate > 0;

end CuBit.SameBoy_Frames;
