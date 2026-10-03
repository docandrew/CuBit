with Interfaces; use Interfaces;
with System;
with Compositor_Formats;
with Desktop_Compositor;
with Compositor_Affine;
with Compositor_Sampling;
function Native_Output_Test return Boolean is
   package A renames Compositor_Affine;
   package S renames Compositor_Sampling;
   package G renames S.G;
   use type System.Address;
   use type G.Orientation;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Source : aliased Pixels (0 .. 143) with Alignment => 64;
   Target : aliased Pixels (0 .. 159) with Alignment => 64;
   SI : aliased Compositor_Formats.Image := (Source'Address, 12, 12, 48, 0);
   TI : aliased Compositor_Formats.Image := (Target'Address, 12, 10, 64, 1);
   Drawn, Restart : Boolean;
   Target_Result : Desktop_Compositor.Target_Release;
   use type Desktop_Compositor.Target_Release;
   Source_Result : Desktop_Compositor.Source_Release;
   use type Desktop_Compositor.Source_Release;
   Screen : G.Output := (Width => 12, Height => 10, X => 4, others => <>);
   Surface : G.Logical_Rectangle := (2, 0, 10, 8);
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   Expected : Unsigned_32;
   Damage : G.Physical_Rectangle;
   function Blend_Pixel (Source, Background : Unsigned_32; Straight : Boolean) return Unsigned_32 is
      Alpha : constant Unsigned_32 := Shift_Right (Source, 24);
      Inverse : constant Unsigned_32 := 255 - Alpha;
      Result : Unsigned_32 := 0;
   begin
      for Byte in 0 .. 3 loop
         declare
            S : constant Unsigned_32 := Shift_Right (Source, Byte * 8) and 255;
            B : constant Unsigned_32 := Shift_Right (Background, Byte * 8) and 255;
            C : constant Unsigned_32 := (if Straight and Byte < 3 then (S * Alpha + B * Inverse + 127) / 255
               else Unsigned_32'Min (255, S + (B * Inverse + 127) / 255));
         begin
            Result := Result or Shift_Left (C, Byte * 8);
         end;
      end loop;
      return Result;
   end Blend_Pixel;
   function Matches (Actual, Expected : Unsigned_32; Tolerance : Natural) return Boolean is
   begin
      for Byte in 0 .. 3 loop
         if abs (Integer (Shift_Right (Actual, Byte * 8) and 255) -
                 Integer (Shift_Right (Expected, Byte * 8) and 255)) > Tolerance then return False; end if;
      end loop;
      return True;
   end Matches;
   procedure Report with Import, Convention => C, External_Name => "compositor_output_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
begin
   for Frame in 1 .. 32 loop
      Target := (others => Sentinel);
      Damage := (G.Pixel_Edge (Frame mod 12), G.Pixel_Edge (Frame mod 10), 12, 10);
      if Frame mod 7 = 0 then Damage := (3, 3, 3, 9); end if;
      declare Color : constant Unsigned_32 := Shift_Left (Unsigned_32 (Frame * 7), 24) or
         Shift_Left (Unsigned_32 (Frame * 5), 16) or Shift_Left (Unsigned_32 (Frame * 3), 8) or Unsigned_32 (Frame);
      begin
         Desktop_Compositor.Draw_Fill
           (TI, Unsigned_64 (Target'Size / 8), Damage, Color, Frame mod 2 = 0, Drawn, Restart);
         if not Drawn or Restart then return False; end if;
         for I in Target'Range loop
            Expected := Sentinel;
            if I mod 16 >= Natural (Damage.Left) and I mod 16 < Natural (Damage.Right) and
              I / 16 >= Natural (Damage.Top) and I / 16 < Natural (Damage.Bottom) then Expected := Color; end if;
            if Target (I) /= Expected then
               Mismatch (Unsigned_32 (800 + Frame), Unsigned_32 (I), Target (I), Expected); return False;
            end if;
         end loop;
      end;
   end loop;
   for Mode in 0 .. 2 loop
   Screen := (Width => 12, Height => 10, X => 4, others => <>);
   Surface := (2, 0, 10, 8); SI.Width := 12; SI.Height := 12;
   for Scale in 1 .. 4 loop
      case Scale is
         when 1 => Screen.Scale := (1, 1);
         when 2 => Screen.Scale := (3, 2);
         when 3 =>
            Screen.Scale := (5, 4); Surface := (0, 0, 8, 8);
            SI.Width := 10; SI.Height := 10;
         when others =>
            Screen.Scale := (3, 2); Screen.X := 0; Surface := (2, 0, 10, 8);
            SI.Width := 12; SI.Height := 12;
      end case;
      for Rotation in G.Orientation loop
         Screen.Rotation := Rotation;
         declare P : constant A.Result := A.Plan (Screen, Surface);
         begin
            if not P.Visible then return False; end if;
            for Frame in 1 .. 8 loop
               for I in Source'Range loop
                  if Mode /= 0 then
                     declare Alpha : constant Unsigned_32 :=
                       (if I = 0 then 0 elsif I = 1 then 255
                        else Unsigned_32 ((I * 37 + Frame * 19) mod 256));
                     begin
                        Source (I) := Shift_Left (Alpha, 24) or
                          (if Mode = 2 then 16#00F08040# else
                           Shift_Left (Alpha, 16) or Shift_Left (Alpha / 2, 8) or (Alpha / 3));
                     end;
                  else
                  Source (I) := 16#FF00_0000# or Shift_Left (Unsigned_32 (Frame), 16) or
                    Shift_Left (Unsigned_32 (I / 12), 8) or Unsigned_32 (I mod 12);
                  end if;
               end loop;
               Target := (others => Sentinel);
               Damage := (case Frame is
                 when 1 => (0, 0, 12, 10), when 2 => (3, 2, 10, 8),
                 when 3 => (4, 0, 5, 10), when 4 => (0, 5, 12, 6),
                 when 5 => (12, 0, 20, 10), when 6 => (8, 7, 2, 1),
                 when 7 => (0, 0, 0, 0), when others => (2, 3, 20, 20));
               Desktop_Compositor.Draw_Output
                 (TI, SI, Unsigned_64 (Target'Size / 8), Unsigned_64 (Source'Size / 8),
                  Screen, Surface, Damage, Frame mod 2 = 0, Drawn, Restart, Over => Mode /= 0, Straight_Alpha => Mode = 2);
               if not Drawn or Restart then return False; end if;
               for I in Target'Range loop
                  Expected := Sentinel;
                  if I mod 16 < 12 then
                     declare M : constant S.Sample := S.Map
                       (Screen, (G.Pixel_Index (I mod 16), G.Pixel_Index (I / 16)),
                        Surface, G.Physical_Extent (SI.Width), G.Physical_Extent (SI.Height));
                     begin
                        if M.Valid and then I mod 16 >= Natural (Damage.Left) and then
                          I mod 16 < Natural (Damage.Right) and then I / 16 >= Natural (Damage.Top) and then
                          I / 16 < Natural (Damage.Bottom) then
                           Expected := Source (Natural (M.Y) * 12 + Natural (M.X));
                           if Mode /= 0 then Expected := Blend_Pixel (Expected, Sentinel, Mode = 2); end if;
                        end if;
                     end;
                  end if;
                  if not Matches (Target (I), Expected, (if Mode = 2 and Expected /= Sentinel then 1 else 0)) then
                     Mismatch (Unsigned_32 (Scale * 100 + G.Orientation'Pos (Rotation) * 10 + Frame),
                       Unsigned_32 (I), Target (I), Expected);
                     return False;
                  end if;
               end loop;
            end loop;
         end;
      end loop;

   end loop;
   end loop;
   -- A target/geometry mismatch must fail before pixel writes and retire the
   -- cache safely, including both output slots and retained source views.
   Target := (others => Sentinel);
   Screen.Width := 13;
   Desktop_Compositor.Draw_Output
     (TI, SI, Unsigned_64 (Target'Size / 8), Unsigned_64 (Source'Size / 8),
      Screen, Surface, (0, 0, 12, 10), False, Drawn, Restart);
   if Drawn or Restart then return False; end if;
   for Pixel of Target loop
      if Pixel /= Sentinel then return False; end if;
   end loop;
   Desktop_Compositor.Forget_Source (Source'Address, Source_Result);
   if Source_Result /= Desktop_Compositor.Source_Retired then return False; end if;
   Desktop_Compositor.Forget_Targets (Target_Result);
   if Target_Result /= Desktop_Compositor.Targets_Retired then return False; end if;
   Report;
   return True;
end Native_Output_Test;
