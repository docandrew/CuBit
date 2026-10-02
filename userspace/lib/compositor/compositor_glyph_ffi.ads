with Interfaces;
with System;
with Compositor_Glyph_Layout;
-- Trusted font parsing/rasterization boundary. The caller owns exclusive pixel
-- storage until return and must not publish it unless Completed is true.
package Compositor_Glyph_FFI with SPARK_Mode => Off is
   procedure Rasterize
     (Font, Code : Interfaces.Unsigned_32;
      Layout : Compositor_Glyph_Layout.Layout;
      Pixels : System.Address; Capacity : Interfaces.Unsigned_64;
      Advance : out Natural; Completed : out Boolean);
end Compositor_Glyph_FFI;
