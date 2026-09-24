pragma Ada_2022;
with Ada.Text_IO;
with CuBit.Display_Arrangement;
procedure Arrangement_Test is
   package A renames CuBit.Display_Arrangement;
   package L renames A.L;
   package G renames A.G;
   use type L.Layout, L.Admission_Status, L.Named_Display_ID;
   use type G.Logical_Coordinate, G.Pixel_Edge, G.UI_Scale, G.Orientation;
   Current, Candidate : L.Layout;
   Accepted : Boolean;
   Cases : Natural := 0;
   procedure Check (Moving : L.Viewport_Index; X, Y : G.Output_Origin) is
      Min_X, Min_Y : G.Output_Origin := G.Output_Origin'Last;
   begin
      A.Move (Current, Moving, X, Y, Candidate, Accepted);
      Cases := Cases + 1;
      if Accepted then
         pragma Assert (L.Validate (Candidate).Status = L.Accepted);
         pragma Assert (Candidate.Count = Current.Count);
         for I in 1 .. Current.Count loop
            pragma Assert (Candidate.Items (I).Display = Current.Items (I).Display);
            pragma Assert (Candidate.Items (I).Geometry.Width = Current.Items (I).Geometry.Width);
            pragma Assert (Candidate.Items (I).Geometry.Height = Current.Items (I).Geometry.Height);
            pragma Assert (Candidate.Items (I).Geometry.Scale = Current.Items (I).Geometry.Scale);
            pragma Assert (Candidate.Items (I).Geometry.Rotation = Current.Items (I).Geometry.Rotation);
            pragma Assert (Candidate.Items (I).Geometry.X >= 0 and Candidate.Items (I).Geometry.Y >= 0);
            Min_X := G.Output_Origin'Min (Min_X, Candidate.Items (I).Geometry.X);
            Min_Y := G.Output_Origin'Min (Min_Y, Candidate.Items (I).Geometry.Y);
         end loop;
         pragma Assert (Min_X = 0 and Min_Y = 0);
      else
         pragma Assert (Candidate = Current);
      end if;
   end;
begin
   Check (1, 0, 0);
   Current.Count := 1;
   Check (1, 0, 0);
   Current.Count := 2;
   Current.Items (1) := (1, (Width => 1024, Height => 768, others => <>));
   Current.Items (2) := (2, (Width => 1280, Height => 720, X => 1024, others => <>));
   for Moving in L.Viewport_Index range 1 .. 2 loop
      for X in -20 .. 20 loop
         for Y in -20 .. 20 loop
            Check (Moving, G.Output_Origin (X * 100), G.Output_Origin (Y * 100));
            pragma Assert (Accepted);
         end loop;
      end loop;
   end loop;
   Check (2, -1280, 0);
   pragma Assert (Candidate.Items (2).Geometry.X = 0 and Candidate.Items (1).Geometry.X = 1280);
   Check (2, 0, -720);
   pragma Assert (Candidate.Items (2).Geometry.Y = 0 and Candidate.Items (1).Geometry.Y = 720);
   Check (2, 0, 768);
   pragma Assert (Candidate.Items (2).Geometry.Y = 768 and Candidate.Items (1).Geometry.Y = 0);
   Check (2, G.Output_Origin'First, G.Output_Origin'Last);
   Check (2, G.Output_Origin'Last, G.Output_Origin'First);
   Check (16, 0, 0);
   pragma Assert (not Accepted);
   Current.Items (2).Display := 1;
   Check (2, 1024, 0);
   pragma Assert (not Accepted);
   Current.Items (2).Display := 2;
   Current.Items (1).Geometry.Rotation := G.Clockwise_90;
   Current.Items (2).Geometry.Scale := (3, 2);
   for X in -10 .. 10 loop
      for Y in -10 .. 10 loop
         Check (2, G.Output_Origin (X * 100), G.Output_Origin (Y * 100));
         pragma Assert (Accepted);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS arrangement:" & Cases'Image & " candidates; modes preserved, connected and normalized");
   Cases := 0;
   Current.Count := 2;
   Current.Items (1) := (1, (Width => 1920, Height => 1080, others => <>));
   Current.Items (2) := (2, (Width => 1280, Height => 720, X => 1920, others => <>));
   for Side in 1 .. 4 loop
      A.Move (Current, 2,
        (case Side is when 1 => 1920, when 2 => -1280, when others => 0),
        (case Side is when 3 => -720, when 4 => 1080, when others => 0),
        Candidate, Accepted);
      pragma Assert (Accepted);
      Current := Candidate;
      for Moving in L.Viewport_Index range 1 .. 2 loop
         for Preset in A.Scale_Preset loop
            A.Rescale (Current, Moving, Preset, Candidate, Accepted);
            Cases := Cases + 1;
            if Accepted then
               pragma Assert (L.Validate (Candidate).Status = L.Accepted);
               pragma Assert (Candidate.Count = Current.Count);
               for I in 1 .. Current.Count loop
                  pragma Assert (Candidate.Items (I).Display = Current.Items (I).Display);
                  pragma Assert (Candidate.Items (I).Geometry.Width = Current.Items (I).Geometry.Width);
                  pragma Assert (Candidate.Items (I).Geometry.Height = Current.Items (I).Geometry.Height);
                  pragma Assert (Candidate.Items (I).Geometry.Rotation = Current.Items (I).Geometry.Rotation);
                  pragma Assert (Candidate.Items (I).Geometry.Scale =
                    (if I = Moving then A.Factor (Preset) else Current.Items (I).Geometry.Scale));
               end loop;
            else
               pragma Assert (Candidate = Current);
            end if;
         end loop;
      end loop;
   end loop;
   Current.Count := 0;
   A.Rescale (Current, 1, A.Scale_125, Candidate, Accepted);
   pragma Assert (not Accepted and Candidate = Current);
   Current.Count := 1;
   A.Rescale (Current, 1, A.Scale_200, Candidate, Accepted);
   pragma Assert (Accepted and Candidate.Items (1).Geometry.Scale = (2, 1));
   A.Rescale (Current, 16, A.Scale_125, Candidate, Accepted);
   pragma Assert (not Accepted and Candidate = Current);
   Ada.Text_IO.Put_Line ("PASS scale policy:" & Cases'Image & " combinations plus empty/single/invalid selection");
end Arrangement_Test;
