with System;
with CuBit.Appearance;
with CuBit.Display_Geometry;

package Desktop_Wallpaper is
   Source_Width : constant := 2048;
   Source_Height : constant := 576;
   Cubie_Width : constant := 2048;
   Cubie_Height : constant := 1152;
   --  Render the immutable embedded asset into a private, validated display
   --  buffer. Aspect-fill scaling crops centrally without stretching or bars.
   --  No runtime image parser, file I/O or additional full-screen allocation.
   procedure Render
     (Target : System.Address;
      Width, Height, Pitch : Positive);

   --  Paint only a caller-clipped rectangle. Existing compositor cursor and
   --  drag-layer caches retain their pixels; no fourth framebuffer is needed.
   procedure Paint
     (Target : System.Address;
      Width, Height, Pitch : Positive;
      X, Y, W, H : Natural;
      Style : CuBit.Appearance.Preferences := CuBit.Appearance.Default);
   -- Caller owns Pitch * Screen.Height writable bytes in the acquired target.
   -- Samples the immutable asset directly; no intermediate preview allocation.
   procedure Paint_Output
     (Target : System.Address; Pitch : Positive;
      Screen : CuBit.Display_Geometry.Output;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences := CuBit.Appearance.Default)
     with Pre => Pitch / 4 >= Natural (Screen.Width);
end Desktop_Wallpaper;
