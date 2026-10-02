with Interfaces; use Interfaces;
with System;
with Mesa_FFI;
with Mesa_Affine_FFI;
with Compositor_Affine;
with Compositor_Sampling;
function Native_Affine_Test return Boolean is
   package A renames Compositor_Affine;
   package S renames Compositor_Sampling;
   package G renames S.G;
   use type System.Address;
   use type G.Orientation;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Source : aliased Pixels (0 .. 143) with Alignment => 64;
   Target : aliased Pixels (0 .. 159) with Alignment => 64;
   SI : aliased Mesa_FFI.Image := (Source'Address, 12, 12, 48, 0);
   TI : aliased Mesa_FFI.Image := (Target'Address, 12, 10, 64, 1);
   Context, Src, Dst : System.Address;
   Screen : G.Output := (Width => 12, Height => 10, X => 4, others => <>);
   Surface : G.Logical_Rectangle := (2, 0, 10, 8);
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   Expected : Unsigned_32;
   procedure Report with Import, Convention => C, External_Name => "compositor_affine_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
begin
   Context := Mesa_FFI.Create;
   if Context = System.Null_Address then return False; end if;
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
      Src := Mesa_FFI.Import_Image (Context, SI'Access);
      Dst := Mesa_FFI.Import_Image (Context, TI'Access);
      if Src = System.Null_Address or Dst = System.Null_Address then return False; end if;
      for Rotation in G.Orientation loop
         Screen.Rotation := Rotation;
         declare P : constant A.Result := A.Plan (Screen, Surface);
         begin
            if not P.Visible then return False; end if;
            if Scale = 1 and Rotation = G.Unrotated then
               for Fault in 1 .. 13 loop
                  Target := (others => Sentinel);
                  declare D : aliased A.Draw := P.Value;
                  begin
                     case Fault is
                        when 1 => D.Numerator := 0;
                        when 2 => D.Numerator := 17;
                        when 3 => D.Denominator := 0;
                        when 4 => D.Rotation := 4;
                        when 5 => D.Over := 2;
                        when 6 => D.Clip_X := TI.Width;
                        when 7 => D.Clip_Y := TI.Height;
                        when 8 => D.Clip_W := 0;
                        when 9 => D.Clip_W := Unsigned_32'Last;
                        when 10 => D.Logical_W := 0;
                        when 11 => D.Logical_H := 2 ** 31 + 1;
                        when 12 => D.Origin_X := Integer_64'Last;
                        when others => D.Origin_Y := Integer_64'First;
                     end case;
                     if Mesa_Affine_FFI.Render (Context, Dst, Src, D'Access, Screen.Width, Screen.Height) /= 1 then return False; end if;
                     for Pixel of Target loop
                        if Pixel /= Sentinel then return False; end if;
                     end loop;
                  end;
               end loop;
            end if;
            for Frame in 1 .. 8 loop
               for I in Source'Range loop
                  Source (I) := 16#FF00_0000# or Shift_Left (Unsigned_32 (Frame), 16) or
                    Shift_Left (Unsigned_32 (I / 12), 8) or Unsigned_32 (I mod 12);
               end loop;
               Target := (others => Sentinel);
               declare D : aliased A.Draw := P.Value;
               begin
                  D.Over := Unsigned_32 (Frame mod 2);
                  if Mesa_Affine_FFI.Render (Context, Dst, Src, D'Access, Screen.Width, Screen.Height) /= 0 then return False; end if;
               end;
               for I in Target'Range loop
                  Expected := Sentinel;
                  if I mod 16 < 12 then
                     declare M : constant S.Sample := S.Map
                       (Screen, (G.Pixel_Index (I mod 16), G.Pixel_Index (I / 16)),
                        Surface, G.Physical_Extent (SI.Width), G.Physical_Extent (SI.Height));
                     begin
                        if M.Valid then Expected := Source (Natural (M.Y) * 12 + Natural (M.X)); end if;
                     end;
                  end if;
                  if Target (I) /= Expected then
                     Mismatch (Unsigned_32 (Scale * 100 + G.Orientation'Pos (Rotation) * 10 + Frame),
                       Unsigned_32 (I), Target (I), Expected);
                     return False;
                  end if;
               end loop;
            end loop;
         end;
      end loop;
      if Mesa_FFI.Release (Context, Src) /= 0 or else Mesa_FFI.Release (Context, Dst) /= 0 then return False; end if;
   end loop;
   Mesa_FFI.Destroy (Context);
   Report;
   return True;
end Native_Affine_Test;
