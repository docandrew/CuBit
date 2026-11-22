with Ada.Text_IO;
with Compositor_Sampling; use Compositor_Sampling;
procedure Sampling_Tests is
   use type G.Pixel_Edge;
   Cases : Natural := 0;
   Screen : G.Output := (Width => 6, Height => 6, X => 4, Scale => (3, 2), others => <>);
   Surface : constant G.Logical_Rectangle := (2, 0, 6, 4);
begin
   -- Independent floating reference is exact for these small rational cases
   -- away from rounding ambiguity; division is done once at the final sample.
   for N in G.Scale_Component loop
      for D in G.Scale_Component loop
         for Origin in -8 .. 8 loop
            for P in G.Pixel_Index range 0 .. 31 loop
               declare
                  R : constant Axis_Result := Axis (P, (N, D), 0,
                    G.Logical_Coordinate (Origin), 7, 13);
                  Fine : constant Fine_Axis_Result := Fine_Axis (P, (N, D), 0,
                    G.Logical_Coordinate (Origin), 7, 13 * 256);
                  Position : constant Long_Long_Float :=
                    (Long_Long_Float (P) + 0.5) * Long_Long_Float (D) /
                      Long_Long_Float (N) - Long_Long_Float (Origin);
               begin
                  pragma Assert (R.Valid = (Position >= 0.0 and Position < 7.0));
                  pragma Assert (Fine.Valid = R.Valid);
                  if R.Valid then
                     pragma Assert (Fine.Index / 256 = Natural (R.Index));
                     pragma Assert (Fine.Index = Integer
                       (Long_Long_Float'Floor (Position * (13.0 * 256.0) / 7.0 + 1.0E-9)));
                     pragma Assert (Integer (R.Index) =
                       Integer (Long_Long_Float'Floor (Position * 13.0 / 7.0 + 1.0E-12)));
                  end if;
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for Y in G.Pixel_Index range 0 .. 5 loop
      for X in G.Pixel_Index range 0 .. 2 loop
         declare R : constant Sample := Map (Screen, (X, Y), Surface, 6, 6);
         begin pragma Assert (R.Valid and then R.X = X + 3 and then R.Y = Y); end;
      end loop;
   end loop;
   -- Rotation must change output coordinates only, never source sampling phase.
   for Rotation in G.Orientation loop
      Screen.Rotation := Rotation;
      for U in G.Pixel_Index range 0 .. 5 loop
         for V in G.Pixel_Index range 0 .. 5 loop
            declare
               P : G.Physical_Point;
               R : Sample;
               Fine : Fine_Sample;
            begin
               case Rotation is
                  when G.Unrotated => P := (U, V);
                  when G.Clockwise_90 => P := (5 - V, U);
                  when G.Clockwise_180 => P := (5 - U, 5 - V);
                  when G.Clockwise_270 => P := (V, 5 - U);
               end case;
               R := Map (Screen, P, Surface, 6, 6);
               Fine := Fine_Map (Screen, P, Surface, 6 * 256, 6 * 256);
               pragma Assert (Fine.Valid = R.Valid);
               if Fine.Valid then
                  pragma Assert (Fine.X = Natural (U + 3) * 256 + 128);
                  pragma Assert (Fine.Y = Natural (V) * 256 + 128);
               end if;
               pragma Assert (R.Valid = (U < 3));
               if R.Valid then pragma Assert (R.X = U + 3 and R.Y = V); end if;
            end;
         end loop;
      end loop;
   end loop;
   pragma Assert (not Map (Screen, (6, 0), Surface, 6, 6).Valid);
   pragma Assert (not Map (Screen, (0, 0), (0, 0, 0, 1), 6, 6).Valid);
   Screen.Height := 4;
   Screen.Rotation := G.Clockwise_90;
   pragma Assert (Map (Screen, (5, 2), Surface, 6, 6) = (True, 5, 0));
   Screen.Rotation := G.Clockwise_180;
   pragma Assert (Map (Screen, (3, 3), Surface, 6, 6) = (True, 5, 0));
   Screen.Rotation := G.Clockwise_270;
   pragma Assert (Map (Screen, (0, 1), Surface, 6, 6) = (True, 5, 0));
   declare R : constant Axis_Result := Axis
     (G.Pixel_Index'Last, (16, 1), G.Output_Origin'Last,
      G.Logical_Coordinate'First, Logical_Size'Last, G.Physical_Extent'Last);
   begin pragma Assert (R.Valid); end;
   declare R : constant Fine_Axis_Result := Fine_Axis
     (G.Pixel_Index'Last, (16, 1), G.Output_Origin'Last,
      G.Logical_Coordinate'First, Logical_Size'Last, Fine_Extent'Last);
   begin pragma Assert (R.Valid and then R.Index < Fine_Extent'Last); end;
   pragma Assert (not Fine_Map (Screen, (6, 0), Surface, 1536, 1536).Valid);
   pragma Assert (not Fine_Map (Screen, (0, 0), (0, 0, 0, 1), 1536, 1536).Valid);
   Ada.Text_IO.Put_Line ("SAMPLING: PASS" & Cases'Image & " rational/offset pixel + 1/256 subpixel cases plus clipped high-density seam, rotations and extremes");
end Sampling_Tests;
