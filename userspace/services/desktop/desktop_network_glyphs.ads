------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The network items' glyphs (UI-027) as square parts within a 24-pixel
--  icon box: signal bars (Wi-Fi), a wired port, a padlock. One geometry for
--  the status button (Desktop's renderer) and the panel (the toolkit).
------------------------------------------------------------------------------
pragma Ada_2022;

package Desktop_Network_Glyphs with SPARK_Mode, Pure is
   type Part is record
      X, Y, W, H : Natural := 0;   --  from the box's top left
      Outline    : Boolean := False;
   end record;

   Bars : constant := 4;
   subtype Bar_Index is Positive range 1 .. Bars;
   Bar_Width : constant := 4;
   Bar_Gap   : constant := 1;
   Bar_Step  : constant := 4;
   Bar_Base  : constant := 5;
   Bottom    : constant := 21;
   Left      : constant := 2;

   function Bar (B : Bar_Index) return Part is
     ((X => Left + (B - 1) * (Bar_Width + Bar_Gap),
       Y => Bottom - (Bar_Base + (B - 1) * Bar_Step),
       W => Bar_Width, H => Bar_Base + (B - 1) * Bar_Step, Outline => False));

   --  The port: outline, latch notch, three contacts.
   Port_Parts : constant := 5;
   subtype Port_Index is Positive range 1 .. Port_Parts;
   Port_Left : constant := 3;
   Port_Top : constant := 5;
   Port_Width : constant := 18;
   Port_Height : constant := 13;
   Notch_Width : constant := 6;
   Notch_Height : constant := 3;
   Pin_Width : constant := 2;
   Pin_Height : constant := 4;
   Pin_Pitch : constant := 4;
   Pin_Top : constant := 3;
   Pins : constant := 3;
   First_Pin_X : constant := Port_Left + (Port_Width - (Pins - 1) * Pin_Pitch - Pin_Width) / 2;

   function Port (P : Port_Index) return Part is
     (case P is
         when 1 => (Port_Left, Port_Top, Port_Width, Port_Height, True),
         when 2 => (Port_Left + (Port_Width - Notch_Width) / 2, Port_Top + Port_Height,
                    Notch_Width, Notch_Height, False),
         when others => (First_Pin_X + (P - 3) * Pin_Pitch, Port_Top + Pin_Top, Pin_Width, Pin_Height, False));

   --  The padlock (a secured network), Lock_Width wide.
   Lock_Width : constant := 8;
   Lock_Parts : constant := 2;
   subtype Lock_Index is Positive range 1 .. Lock_Parts;
   Shackle_Inset : constant := 2;
   Shackle_Height : constant := 6;
   Lock_Body_Top : constant := 5;
   Lock_Body_Height : constant := 6;

   function Lock (L : Lock_Index) return Part is
     (case L is
         when 1 => (Shackle_Inset, 0, Lock_Width - 2 * Shackle_Inset, Shackle_Height, True),
         when 2 => (0, Lock_Body_Top, Lock_Width, Lock_Body_Height, False));
end Desktop_Network_Glyphs;
