with System;

package Desktop_Wallpaper is
   Source_Width : constant := 2048;
   Source_Height : constant := 576;
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
      X, Y, W, H : Natural);
end Desktop_Wallpaper;
