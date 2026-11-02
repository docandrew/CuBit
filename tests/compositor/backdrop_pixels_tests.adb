with Ada.Text_IO;
with Compositor_Upload;
with Desktop_Backdrop_Pixels;
with Wallpaper_Assets;
with Interfaces; use Interfaces;
with System;
procedure Backdrop_Pixels_Tests is
   package U renames Compositor_Upload;
   package WA renames Wallpaper_Assets;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Buffer, Before : aliased Pixels (0 .. 16_385);
   Sentinel : constant Unsigned_32 := 16#DEADBEEF#;
   Plan : U.Plan; OK, Complete : Boolean;
   First, Stride, Cases : Natural := 0;
   function Expected (N : Natural; Asset : WA.Test_Image) return Unsigned_32 is
     (16#FF000000# or Unsigned_32 ((N * 31 + 17 * WA.Test_Image'Pos (Asset)) mod 16#1000000#));
begin
   for I in WA.Wallpaper'Range loop WA.Wallpaper (I) := Expected (I, WA.Wide); end loop;
   for I in WA.Tall_Image'Range loop WA.Tall_Image (I) := Expected (I, WA.Tall); end loop;
   for I in WA.Third_Image'Range loop WA.Third_Image (I) := Expected (I, WA.Third); end loop;
   --  Every test wallpaper, each at its own decoded size: no fixed slots.
   for Asset in WA.Test_Image loop
      for Padding in 0 .. 1 loop
         First := 0; Stride := WA.Widths (Asset) + Padding * 5;
         while First < WA.Heights (Asset) loop
            U.Row_Chunk (WA.Widths (Asset), WA.Heights (Asset), First, WA.Heights (Asset), 65536,
              U.BGRA8, Plan, OK, (if Padding = 0 then 0 else Stride)); pragma Assert (OK);
            Buffer := (others => Sentinel);
            Desktop_Backdrop_Pixels.Copy_Chunk
              (WA.Index (Asset), WA.Widths (Asset), WA.Heights (Asset), Buffer (1)'Address, 65536, Plan, Complete);
            pragma Assert (Complete and Buffer (0) = Sentinel and Buffer (Buffer'Last) = Sentinel);
            for I in 0 .. 16_383 loop
               if I / Stride < U.Area (Plan).Height and I mod Stride < WA.Widths (Asset) then
                  pragma Assert (Buffer (I + 1) = Expected ((First + I / Stride) * WA.Widths (Asset) + I mod Stride, Asset));
               else pragma Assert (Buffer (I + 1) = Sentinel); end if;
            end loop;
            First := First + U.Area (Plan).Height; Cases := Cases + 1;
         end loop;
      end loop;
   end loop;
   for Fault in 1 .. 7 loop
      U.Row_Chunk (2048, 576, 0, 576, 65536, U.BGRA8, Plan, OK); pragma Assert (OK);
      if Fault = 5 then U.Row_Chunk (2048, 576, 0, 576, 65536, U.R8, Plan, OK); pragma Assert (OK); end if;
      if Fault = 6 then U.Make (0, 0, 0, (others => 0), 0, 0, U.BGRA8, Plan, OK); pragma Assert (not OK); end if;
      Buffer := (others => Sentinel); Before := Buffer;
      --  1: no image; 2: a wallpaper of another size than the plan's; 7: a
      --  size that is not the decoded raster's.
      Desktop_Backdrop_Pixels.Copy_Chunk
        ((case Fault is when 1 => 0, when 2 => WA.Index (WA.Tall), when others => WA.Index (WA.Wide)),
         2048, (if Fault = 7 then 575 else 576),
         (if Fault = 3 then System.Null_Address else Buffer (1)'Address),
         (if Fault = 4 then 65535 else 65536), Plan, Complete);
      pragma Assert (not Complete and Buffer = Before);
   end loop;
   Ada.Text_IO.Put_Line ("PASS immutable wallpaper staging:" & Cases'Image & " chunks, three wallpapers of their own sizes, padded rows and seven rejection/no-write cases");
end Backdrop_Pixels_Tests;
