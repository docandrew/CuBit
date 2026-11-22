with Ada.Text_IO;
with CuBit.Display_Layouts;

procedure Main is
   use CuBit.Display_Layouts;
   use type G.Logical_Coordinate;
   Candidate : Layout;
   Cases : Natural := 0;
   procedure Check (Expected : Admission_Status) is
      Result : constant Validation_Result := Validate (Candidate);
   begin
      if Result.Status /= Expected then
         raise Program_Error with "Expected " & Expected'Image &
           " got " & Result.Status'Image;
      end if;
      if Result.Status = Accepted then
         --  Independently follow each parent chain to the root.
         for Start in 1 .. Candidate.Count loop
            declare
               Node : Viewport_Index := Start;
            begin
               for Step in 1 .. Candidate.Count loop
                  exit when Node = 1;
                  pragma Assert (Result.Tree (Node).Depth > 0);
                  pragma Assert (Result.Tree (Node).Parent <= Candidate.Count);
                  pragma Assert
                    (Adjacent (G.Bounds (Candidate.Items (Node).Geometry),
                     G.Bounds
                       (Candidate.Items (Result.Tree (Node).Parent).Geometry)));
                  Node := Result.Tree (Node).Parent;
               end loop;
               pragma Assert (Node = 1);
            end;
         end loop;
      elsif Result.Status in Repeated_Display | Overlapping_Displays then
         pragma Assert (Result.First in 1 .. Candidate.Count);
         pragma Assert (Result.Second in 1 .. Result.First - 1);
      elsif Result.Status = Disconnected then
         pragma Assert (Result.First in 1 .. Candidate.Count);
         pragma Assert (Result.Tree (Result.First).Depth = 0);
      end if;
      Cases := Cases + 1;
   end Check;

   -- Independent oracle for unit cells: overlap is equal grid coordinates;
   -- adjacency is Manhattan distance one, without calling the model helpers.
   procedure Check_Grid is
      Seen : array (1 .. 4) of Boolean := [True, False, False, False];
      Expected : Admission_Status := Accepted;
   begin
      for I in 1 .. 4 loop
         for J in 1 .. I - 1 loop
            if Candidate.Items (I).Geometry.X = Candidate.Items (J).Geometry.X
              and then Candidate.Items (I).Geometry.Y =
                Candidate.Items (J).Geometry.Y
            then
               Check (Overlapping_Displays);
               return;
            end if;
         end loop;
      end loop;
      for Pass in 1 .. 4 loop
         for I in 1 .. 4 loop
            for J in 1 .. 4 loop
               if Seen (J) and then
                 abs (Candidate.Items (I).Geometry.X -
                      Candidate.Items (J).Geometry.X) +
                 abs (Candidate.Items (I).Geometry.Y -
                      Candidate.Items (J).Geometry.Y) = 1
               then
                  Seen (I) := True;
               end if;
            end loop;
         end loop;
      end loop;
      for Item of Seen loop
         if not Item then Expected := Disconnected; end if;
      end loop;
      Check (Expected);
   end Check_Grid;
begin
   Check (Empty_Not_Allowed);
   pragma Assert (Validate (Candidate, Permit_Headless).Status = Accepted);
   Candidate.Count := 1;
   Check (Accepted);
   Candidate.Count := 2;
   Check (Repeated_Display);
   pragma Assert
     (Validate (Candidate, Permit_Headless).Status = Repeated_Display);
   Candidate.Items (2).Display := 2;
   Check (Overlapping_Displays);
   Candidate.Items (2).Geometry.X := 1;
   Check (Accepted);
   Candidate.Items (2).Geometry.Y := 1;
   Check (Disconnected); -- Corner only.
   Candidate.Items (2).Geometry.X := 2;
   Check (Disconnected); -- Real gap.
   -- A reversed enumeration chain needs all breadth passes, not one scan.
   Candidate.Count := Max_Viewports;
   for I in Viewport_Index loop
      Candidate.Items (I) :=
        (Display => Named_Display_ID (I),
         Geometry => (Width => 1, Height => 1,
           X => G.Output_Origin (Max_Viewports - I), others => <>));
   end loop;
   Check (Accepted);
   -- Remove a middle/bridging viewport without changing the desired origins.
   Candidate.Items (8) := Candidate.Items (Max_Viewports);
   Candidate.Count := Max_Viewports - 1;
   Check (Disconnected);
   -- Mixed modes, scale and rotation are evaluated in logical coordinates.
   Candidate := (Count => 2, others => <>);
   Candidate.Items (1).Geometry :=
     (Width => 3840, Height => 2160, Scale => (2, 1), others => <>);
   Candidate.Items (2) := (Display => 2, Geometry =>
     (Width => 1920, Height => 1080, Rotation => G.Clockwise_90,
      X => -1080, Y => -500, others => <>));
   Check (Accepted);
   -- Enumerate every ordered four-cell arrangement on a 3x3 grid, including
   -- detection-order permutations, overlaps, islands and corner contacts.
   Candidate := (Count => 4, others => <>);
   for Encoding in 0 .. 9 ** 4 - 1 loop
      declare
         Digits_Left : Natural := Encoding;
         Cell : Natural;
      begin
         for I in 1 .. 4 loop
            Cell := Digits_Left mod 9;
            Digits_Left := Digits_Left / 9;
            Candidate.Items (I) := (Display => Named_Display_ID (I),
              Geometry => (Width => 1, Height => 1,
                X => G.Output_Origin (Cell mod 3) - 1,
                Y => G.Output_Origin (Cell / 3) - 1, others => <>));
         end loop;
         Check_Grid;
      end;
   end loop;
   -- A ring is connected even though its center is not a usable viewport.
   Candidate := (Count => 8, others => <>);
   declare
      Points : constant array (1 .. 8) of G.Logical_Point :=
        [(-1, -1), (0, -1), (1, -1), (1, 0),
         (1, 1), (0, 1), (-1, 1), (-1, 0)];
   begin
      for I in Points'Range loop
         Candidate.Items (I) := (Display => Named_Display_ID (I),
           Geometry => (Width => 1, Height => 1,
             X => Points (I).X, Y => Points (I).Y, others => <>));
      end loop;
   end;
   Check (Accepted);
   Ada.Text_IO.Put_Line ("PASS layout admission cases:" & Cases'Image);
end Main;
