with Interfaces; use Interfaces;
with CuBit.QOI; use CuBit.QOI;
with Desktop_Wallpaper_Store;
package body Wallpaper_Loader is
   package Store renames Desktop_Wallpaper_Store;
   function Load (Asset : Positive; Width, Height : Positive) return System.Address is
      Left : Natural := Width * Height - 1;
      Result : Store.Load_Result;
      Reserved : Boolean := True;
      function Big (Value : Natural) return Byte_Array is
        [Unsigned_8 (Value / 2**24), Unsigned_8 (Value / 2**16 mod 256),
         Unsigned_8 (Value / 2**8 mod 256), Unsigned_8 (Value mod 256)];
   begin
      if Store.Capacity = 0 then
         Store.Reserve (Test_Images, Reserved);
      end if;
      pragma Assert (Reserved);
      Store.Begin_Load (Asset);
      Store.Feed (Asset, Byte_Array'[Magic_Q, Magic_O, Magic_I, Magic_F] & Big (Width) & Big (Height) &
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
