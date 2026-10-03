with Interfaces;
with System;
with Compositor_Glyph_Layout;
-- Trusted font parsing/rasterization boundary. The caller owns exclusive pixel
-- storage until return and must not publish it unless Completed is true.
-- The audited body is outside proof: the foreign call is synchronous, retains
-- no mapping, and writes only the validated caller-owned capacity. Global null
-- describes named Ada state, not an absence of writes through Pixels.
package Compositor_Glyph_FFI with SPARK_Mode is
   use type System.Address, Interfaces.Unsigned_64;
   procedure Rasterize
     (Font, Code : Interfaces.Unsigned_32;
      Layout : Compositor_Glyph_Layout.Layout;
      Pixels : System.Address; Capacity : Interfaces.Unsigned_64;
      Advance : out Natural; Completed : out Boolean)
     with Global => null,
       Post => (if Completed then Advance in 1 .. Layout.Width and
         Compositor_Glyph_Layout.Valid (Layout) and Pixels /= System.Null_Address and
         Capacity >= Interfaces.Unsigned_64 (Layout.Bytes));
end Compositor_Glyph_FFI;
