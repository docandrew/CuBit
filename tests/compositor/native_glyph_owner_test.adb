with Interfaces; use Interfaces;
with Compositor_Glyph_Renderer;
with Compositor_Glyph_Target;
with Compositor_Glyph_FFI;
with Mesa_Cache;
function Native_Glyph_Owner_Test return Boolean is
   package R renames Compositor_Glyph_Renderer;
   package G renames R.P.G;
   use type R.P.A.Signed;
   function Run (Mode : Positive) return Boolean is
      S : R.State;
      Views : Mesa_Cache.State;
      type Pixels is array (Natural range <>) of Unsigned_32;
      type Bytes is array (Natural range <>) of Unsigned_8;
      Target : aliased Pixels (0 .. 84 * 72 - 1) with Alignment => 64;
      Reference : aliased Bytes (0 .. 4095) with Alignment => 64;
      Screen : G.Output := (80, 72, G.Unrotated, (1, 1), 0, 0);
      Key : R.C.Key;
      Origin : G.Logical_Point;
      Damage : G.Physical_Rectangle;
      Sentinel : constant Unsigned_32 := 16#FF12_3456#;
      OK : Boolean;
      Advance : Natural;
      function Byte (Pixel : Unsigned_32; Channel : Natural) return Natural is
        (Natural (Shift_Right (Pixel, Channel * 8) and 255));
      procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
        with Import, Convention => C, External_Name => "compositor_test_mismatch";
   begin
      Mesa_Cache.Initialize (Views, True);
      Mesa_Cache.Ensure (Views, 0, (Target'Address, 80, 72, 84 * 4, 1), Target'Length * 4, OK);
      if not OK then return False; end if;
      Target := (others => Sentinel);
      if Mode = 5 then
         for I in 1 .. 32 loop
            R.Queue (S, Views, 0, (0, 31 + I, Screen.Scale), Screen, (0, 0),
                     (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
            if not OK then return False; end if;
         end loop;
         R.Use_Software (S, Views, OK);
         if not OK or not R.Software_Active (S) or R.Queued (S) /= 0 or R.Charged (S) /= 32 * 544 then return False; end if;
         for Pixel of Target loop if Pixel /= Sentinel then return False; end if; end loop;
         Mesa_Cache.Shutdown (Views);
         if not Mesa_Cache.Can_Retire (Views) or not Mesa_Cache.Views_Clear (Views) then return False; end if;
      end if;
      if Mode = 1 or Mode = 5 then
         for Frame in 0 .. 259 loop
            Screen.Scale := (if Frame < 190 then (1, 1) else (5, 4));
            Screen.Rotation := G.Orientation'Val (Frame mod 4);
            Key := (Frame / 95 mod 2, 32 + Frame mod 95, Screen.Scale);
            Origin := (G.Logical_Coordinate (Frame mod 7 - 5), G.Logical_Coordinate (Frame mod 5 - 3));
            Damage := (if Frame mod 2 = 0 then (0, 0, 80, 72) else (7, 9, 73, 64));
            declare
               Layout : constant R.P.L.Layout := R.P.L.Plan (Screen.Scale);
               Left : constant R.P.A.Signed := R.P.Snap (Origin.X, 0, Screen.Scale);
               Top : constant R.P.A.Signed := R.P.Snap (Origin.Y, 0, Screen.Scale);
            begin
               Target := (others => Sentinel);
               Reference := (others => 16#A5#);
               Compositor_Glyph_FFI.Rasterize (Unsigned_32 (Key.Face), Unsigned_32 (Key.Code), Layout,
                  Reference'Address, Reference'Length, Advance, OK);
               if not OK then return False; end if;
               if Mode = 5 then
                  Compositor_Glyph_Target.Paint (S, Views,
                    (Target'Address, 80, 72, 336, 1), Target'Length * 4,
                    Key, Screen, Origin, Damage, 16#FFFF_FFFF#, OK);
               else
                  R.Queue (S, Views, 0, Key, Screen, Origin, Damage, 16#FFFF_FFFF#, OK);
                  if not OK then return False; end if;
                  R.Flush (S, Views, OK);
               end if;
               if not OK or R.Queued (S) /= 0 or R.Charged (S) > 524_288 then return False; end if;
               for I in Target'Range loop
                  declare
                     X : constant Natural := I mod 84;
                     Y : constant Natural := I / 84;
                     UX, UY : R.P.A.Signed;
                     Coverage : Natural := 0;
                  begin
                     if X < 80 then
                        case Screen.Rotation is
                           when G.Unrotated => UX := R.P.A.Signed (X); UY := R.P.A.Signed (Y);
                           when G.Clockwise_90 => UX := R.P.A.Signed (Y); UY := R.P.A.Signed (79 - X);
                           when G.Clockwise_180 => UX := R.P.A.Signed (79 - X); UY := R.P.A.Signed (71 - Y);
                           when G.Clockwise_270 => UX := R.P.A.Signed (71 - Y); UY := R.P.A.Signed (X);
                        end case;
                        if UX >= Left and then UX < Left + R.P.A.Signed (Layout.Width) and then
                          UY >= Top and then UY < Top + R.P.A.Signed (Layout.Height) and then
                          X >= Natural (Damage.Left) and then X < Natural (Damage.Right) and then
                          Y >= Natural (Damage.Top) and then Y < Natural (Damage.Bottom)
                        then Coverage := Natural (Reference (Natural (UY - Top) * Layout.Pitch + Natural (UX - Left))); end if;
                     end if;
                     for C in 0 .. 3 loop
                        declare Expected : constant Natural := (255 * Coverage + Byte (Sentinel, C) * (255 - Coverage) + 127) / 255; begin
                           if abs (Byte (Target (I), C) - Expected) > (if Mode = 5 or Coverage = 0 then 0 else 1) then
                              Mismatch (Unsigned_32 (Frame), Unsigned_32 (I), Target (I), Unsigned_32 (Expected)); return False;
                           end if;
                        end;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
      elsif Mode = 2 then
         for I in 1 .. 32 loop
            R.Queue (S, Views, 0, (0, 65, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
            if not OK or R.Queued (S) /= I or R.Charged (S) /= 544 then return False; end if;
         end loop;
         for Pixel of Target loop if Pixel /= Sentinel then return False; end if; end loop;
      elsif Mode = 3 then
         Screen.Scale := (16, 1);
         for I in 0 .. 2 loop
            R.Queue (S, Views, 0, (0, 65 + I, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
            if not OK then return False; end if;
         end loop;
         R.Queue (S, Views, 0, (0, 68, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
         if OK or R.Queued (S) /= 3 or R.Charged (S) /= 3 * 139_264 then return False; end if;
         R.Flush (S, Views, OK); if not OK then return False; end if;
         R.Queue (S, Views, 0, (0, 68, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
         if not OK then return False; end if;
      else
         declare Densities : constant array (1 .. 7) of G.UI_Scale :=
           ((16, 1), (16, 1), (16, 1), (13, 1), (5, 1), (1, 1), (3, 16));
         begin
            for I in Densities'Range loop
               Screen.Scale := Densities (I);
               R.Queue (S, Views, 0, (0, 65 + I, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
               if not OK then return False; end if;
            end loop;
            if R.Queued (S) /= 7 or R.Charged (S) /= 523_936 then return False; end if;
            R.Queue (S, Views, 0, (0, 90, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
            if OK or R.Queued (S) /= 7 or R.Charged (S) /= 523_936 then return False; end if;
            R.Flush (S, Views, OK); if not OK then return False; end if;
            R.Queue (S, Views, 0, (0, 90, Screen.Scale), Screen, (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
            if not OK then return False; end if;
         end;
      end if;
      R.Shutdown (S, Views, OK);
      if not OK or R.Queued (S) /= 0 or R.Charged (S) /= 0 then return False; end if;
      if Mode = 2 then
         for Pixel of Target loop if Pixel /= Sentinel then return False; end if; end loop;
      end if;
      for Y in 0 .. 71 loop
         for X in 80 .. 83 loop if Target (Y * 84 + X) /= Sentinel then return False; end if; end loop;
      end loop;
      Mesa_Cache.Shutdown (Views);
      return Mesa_Cache.Can_Retire (Views) and Mesa_Cache.Views_Clear (Views);
   end Run;
   procedure Report with Import, Convention => C, External_Name => "compositor_glyph_owner_report";
begin
   for Mode in 1 .. 5 loop if not Run (Mode) then return False; end if; end loop;
   Report;
   return True;
end Native_Glyph_Owner_Test;
