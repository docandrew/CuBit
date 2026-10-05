with Ada.Text_IO;
with CuBit.Appearance;
with Compositor_Upload;
with Desktop_Backdrop_Pixels;
with Desktop_Backdrop_Style;
with Wallpaper_Assets;
with Interfaces; use Interfaces;
with System;
procedure Backdrop_Pixels_Tests is
   package U renames Compositor_Upload;
   package P renames Desktop_Backdrop_Style;
   package A renames CuBit.Appearance;
   use type A.Background;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Buffer, Before : aliased Pixels (0 .. 16_385);
   Sentinel : constant Unsigned_32 := 16#DEADBEEF#;
   Plan : U.Plan; OK, Complete : Boolean;
   First, Stride, Cases : Natural := 0;
   Assets : constant array (1 .. 2) of A.Background := (A.Wallpaper, A.Cubie);
   function Expected (N : Natural; Asset : A.Background) return Unsigned_32 is
     (16#FF000000# or Unsigned_32 ((N * 31 + (if Asset = A.Cubie then 17 else 0)) mod 16#1000000#));
begin
   for I in Wallpaper_Assets.Wallpaper'Range loop Wallpaper_Assets.Wallpaper (I) := Expected (I, A.Wallpaper); end loop;
   for I in Wallpaper_Assets.Cubie'Range loop Wallpaper_Assets.Cubie (I) := Expected (I, A.Cubie); end loop;
   for Asset of Assets loop
      for Padding in 0 .. 1 loop
         First := 0; Stride := P.Width (Asset) + Padding * 5;
         while First < P.Height (Asset) loop
            U.Row_Chunk (P.Width (Asset), P.Height (Asset), First, 65536,
              U.BGRA8, Plan, OK, (if Padding = 0 then 0 else Stride)); pragma Assert (OK);
            Buffer := (others => Sentinel);
            Desktop_Backdrop_Pixels.Copy_Chunk (Asset, Buffer (1)'Address, 65536, Plan, Complete);
            pragma Assert (Complete and Buffer (0) = Sentinel and Buffer (Buffer'Last) = Sentinel);
            for I in 0 .. 16_383 loop
               if I / Stride < U.Area (Plan).Height and I mod Stride < P.Width (Asset) then
                  pragma Assert (Buffer (I + 1) = Expected ((First + I / Stride) * P.Width (Asset) + I mod Stride, Asset));
               else pragma Assert (Buffer (I + 1) = Sentinel); end if;
            end loop;
            First := First + U.Area (Plan).Height; Cases := Cases + 1;
         end loop;
      end loop;
   end loop;
   for Fault in 1 .. 6 loop
      U.Row_Chunk (2048, 576, 0, 65536, U.BGRA8, Plan, OK); pragma Assert (OK);
      if Fault = 5 then U.Row_Chunk (2048, 576, 0, 65536, U.R8, Plan, OK); pragma Assert (OK); end if;
      if Fault = 6 then U.Make (0, 0, 0, (others => 0), 0, 0, U.BGRA8, Plan, OK); pragma Assert (not OK); end if;
      Buffer := (others => Sentinel); Before := Buffer;
      Desktop_Backdrop_Pixels.Copy_Chunk
        ((if Fault = 1 then A.Slate elsif Fault = 2 then A.Cubie else A.Wallpaper),
         (if Fault = 3 then System.Null_Address else Buffer (1)'Address),
         (if Fault = 4 then 65535 else 65536), Plan, Complete);
      pragma Assert (not Complete and Buffer = Before);
   end loop;
   Ada.Text_IO.Put_Line ("PASS immutable wallpaper staging:" & Cases'Image & " chunks, both complete atlases, padded rows and six rejection/no-write cases");
end Backdrop_Pixels_Tests;
