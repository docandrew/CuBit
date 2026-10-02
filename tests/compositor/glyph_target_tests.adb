with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
with Mesa_Cache;
with Compositor_Glyph_Target;
procedure Glyph_Target_Tests is
   package B renames Compositor_Glyph_Target;
   package R renames B.R;
   package SW renames R.Software;
   package G renames R.P.G;
   use type SW.Pixels;
   use System.Storage_Elements;
   S : R.State;
   Views : Mesa_Cache.State;
   Screen : constant G.Output := (24, 20, G.Unrotated, (1, 1), 0, 0);
   Target : aliased SW.Pixels (0 .. 28 * 20 + 7);
   Expected : SW.Pixels (Target'Range);
   Mask : SW.Bytes (0 .. R.P.L.Maximum_Bytes - 1) := (others => 127);
   Image : B.F.Image;
   Capacity : Unsigned_64;
   OK : Boolean;
   procedure Reset with Import, Convention => C, External_Name => "glyph_mock_reset";
   function Stat (Index : Unsigned_32) return Unsigned_32 with Import, Convention => C, External_Name => "glyph_mock_stat";
begin
   Reset; R.Use_Software (S, Views, OK); pragma Assert (OK);
   for Case_Number in 0 .. 6 loop
      Target := (others => 16#FF12_3456#); Expected := Target;
      Image := (Target'Address, 24, 20, 112, 1); Capacity := Target'Length * 4;
      case Case_Number is
         when 1 => Image.Writable := 0;
         when 2 => Image.Pitch := 92;
         when 3 => Capacity := 112 * 20 - 1;
         when 4 => Image.Width := 25;
         when 5 => Image.Pixels := System.Null_Address;
         when 6 => Image.Pitch := 113;
         when others => SW.Paint (Screen, (0, 0), (3, 5, 21, 16), Mask, Expected, 28, 16#8031_AF07#);
      end case;
      B.Paint (S, Views, Image, Capacity, (0, 65, Screen.Scale), Screen,
               (0, 0), (3, 5, 21, 16), 16#8031_AF07#, OK);
      pragma Assert (OK = (Case_Number = 0) and Target = Expected and Stat (0) = 1 and Stat (4) = 0);
   end loop;
   R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0);
   declare
      Store : R.Storage.State;
      Small : G.Output := (4, 4, G.Unrotated, (1, 1), 0, 0);
      Layout : constant R.P.L.Layout := R.P.L.Plan ((1, 1));
      Advance : Natural;
   begin
      R.Storage.Allocate (Store, 1, Layout, OK); pragma Assert (OK);
      R.Storage.Rasterize (Store, 1, 0, 65, Layout, Advance, OK); pragma Assert (OK);
      declare
         Alias : SW.Pixels (0 .. 15) with Import, Address => R.Storage.Pixels (Store, 1) + Storage_Offset'(4);
         Before : constant SW.Pixels := Alias;
      begin
         R.Storage.Paint (Store, 1, Small, (0, 0), (0, 0, 4, 4), Alias, 4, 16#FFFF_FFFF#, OK);
         pragma Assert (not OK and Alias = Before);
      end;
      Small.Scale := (5, 4); pragma Assert (not R.Storage.Can_Paint (Store, 1, Small));
      R.Storage.Release (Store, 1, OK); pragma Assert (OK);
      pragma Assert (not R.Storage.Can_Paint (Store, 1, Screen));
   end;
   Ada.Text_IO.Put_Line ("GLYPH-TARGET: PASS mapped target, stride/tail guards, six descriptor rejections, alias and stale backing rejection");
end Glyph_Target_Tests;
