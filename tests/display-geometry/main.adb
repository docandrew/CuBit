with Ada.Text_IO;
with CuBit.Display_Geometry;

procedure Main is
   use CuBit.Display_Geometry;
   Screen : Output := (Width => 17, Height => 11, others => <>);
   Cases : Natural := 0;
   procedure Check is
      B : constant Logical_Rectangle := Bounds (Screen);
      Full : constant Physical_Rectangle :=
        (0, 0, Screen.Width, Screen.Height);
   begin
      pragma Assert (Damage (Screen, B) = Full);
      pragma Assert (Damage (Screen, (others => 0)) = Empty);
      pragma Assert (not Contains (Screen, (B.Right, B.Bottom)));
      pragma Assert (Contains (Screen, (B.Left, B.Top)));
      pragma Assert (Damage (Screen,
        (Logical_Coordinate'First, Logical_Coordinate'First,
         Logical_Coordinate'Last, Logical_Coordinate'Last)) = Full);
      for Y in Pixel_Index range 0 .. Screen.Height - 1 loop
         for X in Pixel_Index range 0 .. Screen.Width - 1 loop
            declare
               P : constant Point_Mapping := To_Desktop (Screen, (X, Y));
               A : Physical_Rectangle;
            begin
               pragma Assert (P.Valid);
               -- Every native pixel's input position must be included when
               -- its logical unit is redrawn: rotation/input/damage agree.
               A := Damage (Screen,
                 (P.Value.X, P.Value.Y, P.Value.X + 1, P.Value.Y + 1));
               pragma Assert (X >= A.Left and X < A.Right);
               pragma Assert (Y >= A.Top and Y < A.Bottom);
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end Check;
begin
   for Rotation in Orientation loop
      Screen.Rotation := Rotation;
      for N in Scale_Component loop
         for D in Scale_Component loop
            Screen.Scale := (N, D);
            Screen.X := -1920;
            Screen.Y := -1080;
            Check;
         end loop;
      end loop;
   end loop;
   -- Asymmetric corners establish clockwise direction independently of the
   -- forward/inverse agreement test (which alone could share a wrong rotation).
   Screen := (Width => 3, Height => 2, Rotation => Clockwise_90, others => <>);
   pragma Assert (To_Desktop (Screen, (0, 0)).Value = Logical_Point'(0, 2));
   pragma Assert (To_Desktop (Screen, (2, 1)).Value = Logical_Point'(1, 0));
   pragma Assert (Damage (Screen, (0, 0, 1, 1)) = Physical_Rectangle'(2, 0, 3, 1));
   pragma Assert (not To_Desktop (Screen, (3, 0)).Valid);
   Screen.Rotation := Unrotated;
   pragma Assert (Damage (Screen, (0, 0, 1, 1)) = Physical_Rectangle'(0, 0, 1, 1));
   Screen.Rotation := Clockwise_180;
   pragma Assert (Damage (Screen, (0, 0, 1, 1)) = Physical_Rectangle'(2, 1, 3, 2));
   Screen.Rotation := Clockwise_270;
   pragma Assert (Damage (Screen, (0, 0, 1, 1)) = Physical_Rectangle'(0, 1, 1, 2));
   Screen := (Width => 5, Height => 3, Scale => (3, 2), others => <>);
   pragma Assert (Bounds (Screen) = Logical_Rectangle'(0, 0, 4, 2));
   pragma Assert (Damage (Screen, (-2, -3, 1, 1)) = Physical_Rectangle'(0, 0, 2, 2));
   pragma Assert (Damage (Screen, (1, 0, 4, 2)) = Physical_Rectangle'(1, 0, 5, 3));
   pragma Assert (Damage (Screen, (4, 0, 8, 3)) = Empty);
   pragma Assert (Damage (Screen, (2, 2, 1, 1)) = Empty);
   -- Shared boundary: left/above displays use negative desktop coordinates.
   Screen := (Width => 3840, Height => 2160, Scale => (2, 1), others => <>);
   pragma Assert (Bounds (Screen) = Logical_Rectangle'(0, 0, 1920, 1080));
   Screen := (Width => 1920, Height => 1080, Rotation => Clockwise_90,
              X => -1080, Y => -500, others => <>);
   pragma Assert (Bounds (Screen) = Logical_Rectangle'(-1080, -500, 0, 1420));
   pragma Assert (not Contains (Screen, (0, 0)));
   -- Maximum modes/extreme origins; no backing allocation for desktop gaps.
   for Rotation in Orientation loop
      Screen := (Width => Physical_Extent'Last, Height => Physical_Extent'Last,
                 Rotation => Rotation, Scale => (1, 16),
                 X => Output_Origin'First, Y => Output_Origin'Last);
      pragma Assert (Damage (Screen, Bounds (Screen)) =
        Physical_Rectangle'(0, 0, Screen.Width, Screen.Height));
      pragma Assert (To_Desktop (Screen, (Pixel_Index'Last, Pixel_Index'Last)).Valid);
   end loop;
   Ada.Text_IO.Put_Line ("PASS display transforms:" & Cases'Image & " pixel round trips plus boundaries");
end Main;
