with System;
with CuBit.Appearance;
--  Retained wallpaper layers (docs/assets.md): one private, pre-scaled copy
--  of the backdrop per output, so repainting damage is row copies instead of
--  resampling the wallpaper for every damaged pixel. A layer is rebuilt only
--  when what it shows changes (the style drawn, or the output's size) and is
--  released with its output. If a layer cannot be allocated, painting falls
--  back to resampling (Desktop_Wallpaper.Paint); the pixels are the same.
package Desktop_Wallpaper_Layers is
   Maximum_Layers : constant := 2;   --  one per display output
   subtype Layer_Index is Natural range 0 .. Maximum_Layers - 1;

   --  As Desktop_Wallpaper.Paint, through Layer's retained copy: Target
   --  holds Height rows of Pitch bytes; X, Y, W, H is the damage to paint.
   procedure Paint
     (Layer : Layer_Index; Target : System.Address;
      Width, Height, Pitch : Positive; X, Y, W, H : Natural;
      Style : CuBit.Appearance.Preferences);

   --  Free Layer's memory (output teardown). Painting rebuilds it.
   procedure Release (Layer : Layer_Index);
end Desktop_Wallpaper_Layers;
