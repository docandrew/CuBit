--  The hosted harness's window memory (library level: too big for a stack).
with Interfaces;
package Host_Pixels is
   Window_Width : constant := 960;
   Window_Height : constant := 744;
   type Pixel_Array is array (Natural range <>) of Interfaces.Unsigned_32;
   Pixels : Pixel_Array (0 .. Window_Width * Window_Height - 1) := [others => 0];
   Snapshot : Pixel_Array (Pixels'Range) := [others => 0];
   --  The same logical window at density 2.
   Density_2 : constant := 2;
   Pixels_2x : Pixel_Array (0 .. Window_Width * Window_Height * Density_2 * Density_2 - 1) :=
     [others => 0];
end Host_Pixels;
