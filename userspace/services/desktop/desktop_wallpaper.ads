with Desktop_Backdrop_Style;
with System;
with CuBit.Display_Geometry;

package Desktop_Wallpaper is
   --  Render a decoded wallpaper (Desktop_Wallpaper_Store, whatever its
   --  size) into a private, validated display buffer. Aspect-fill scaling
   --  crops centrally without stretching or bars. An image that is not
   --  Ready, or whose raster is not the size the style says, is drawn as the
   --  flat colour. No file I/O or additional full-screen allocation here.
   procedure Render
     (Target : System.Address;
      Width, Height, Pitch : Positive);

   --  Paint only a caller-clipped rectangle. Existing compositor cursor and
   --  drag-layer caches retain their pixels; no fourth framebuffer is needed.
   procedure Paint
     (Target : System.Address;
      Width, Height, Pitch : Positive;
      X, Y, W, H : Natural;
      Style : Desktop_Backdrop_Style.Backdrop := Desktop_Backdrop_Style.Flat);
   -- Caller owns Pitch * Screen.Height writable bytes in the acquired target.
   -- Samples the decoded raster directly; no intermediate preview allocation.
   procedure Paint_Output
     (Target : System.Address; Pitch : Positive;
      Screen : CuBit.Display_Geometry.Output;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : Desktop_Backdrop_Style.Backdrop := Desktop_Backdrop_Style.Flat)
     with Pre => Pitch / 4 >= Natural (Screen.Width);
end Desktop_Wallpaper;
