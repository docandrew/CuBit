with Interfaces; use Interfaces;
with CuBit.QOI; use CuBit.QOI;
with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Store;
package body Wallpaper_Loader is
   package Store renames Desktop_Wallpaper_Store;
   function Load (Asset : CuBit.Appearance.Background) return System.Address is
      W : constant Positive := Desktop_Backdrop_Style.Width (Asset);
      H : constant Positive := Desktop_Backdrop_Style.Height (Asset);
      Left : Natural := W * H - 1;
      Result : Store.Load_Result;
      function Big (Value : Natural) return Byte_Array is
        [Unsigned_8 (Value / 2**24), Unsigned_8 (Value / 2**16 mod 256),
         Unsigned_8 (Value / 2**8 mod 256), Unsigned_8 (Value mod 256)];
   begin
      Store.Begin_Load (Asset);
      Store.Feed (Asset, Byte_Array'[Magic_Q, Magic_O, Magic_I, Magic_F] & Big (W) & Big (H) &
                         Byte_Array'[Channels_RGBA, Colorspace_SRGB]);
      --  (0,0,0,0) is index slot 0's initial value: the first pixel, then runs.
      Store.Feed (Asset, [1 => Op_Index]);
      while Left > 0 loop
         declare
            Run : constant Positive := Natural'Min (Left, 62);
         begin
            Store.Feed (Asset, [1 => Op_Run or Unsigned_8 (Run - Run_Bias)]);
            Left := Left - Run;
         end;
      end loop;
      Store.Feed (Asset, [0, 0, 0, 0, 0, 0, 0, Marker_Last]);
      Store.End_Load (Asset, False, Result);
      pragma Assert (Result.Loaded);
      return Store.Pixels (Asset);
   end Load;
end Wallpaper_Loader;
