with Interfaces;
-- Texel coordinates within one retained source image. This is geometry only:
-- it does not create an image view or grant authority over an atlas backing.
package Compositor_Source_Region with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_32;
   use type Word;
   type Rectangle is record
      X, Y, Width, Height, Image_Width, Image_Height : Word := 0;
   end record with Convention => C, Size => 192;
   for Rectangle use record
      X at 0 range 0 .. 31;
      Y at 4 range 0 .. 31;
      Width at 8 range 0 .. 31;
      Height at 12 range 0 .. 31;
      Image_Width at 16 range 0 .. 31;
      Image_Height at 20 range 0 .. 31;
   end record;
   function Valid (R : Rectangle) return Boolean is
     (R.Image_Width in 1 .. 65535 and then R.Image_Height in 1 .. 65535 and then
      R.X < R.Image_Width and then R.Y < R.Image_Height and then
      R.Width in 1 .. R.Image_Width - R.X and then
      R.Height in 1 .. R.Image_Height - R.Y);
end Compositor_Source_Region;
