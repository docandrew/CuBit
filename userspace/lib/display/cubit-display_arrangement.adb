pragma Ada_2022;
package body CuBit.Display_Arrangement with SPARK_Mode is
   use type L.Admission_Status;
   use type G.Logical_Coordinate;
   type Wide is range -2 ** 40 .. 2 ** 40;
   -- Bounds/extent arithmetic fits this domain; distances use the wider base.
   subtype Coordinate is Wide range -2 ** 32 .. 2 ** 32;
   Edge_Snap_Distance : constant := 32;
   type Side is (Left_Edge, Right_Edge, Top_Edge, Bottom_Edge);
   type Candidate_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : L.Layout;
         when False => null;
      end case;
   end record;

   procedure Rescale
     (Current : L.Layout; Moving : L.Viewport_Index; Preset : Scale_Preset;
      Proposed : out L.Layout; Accepted : out Boolean)
   is
      Candidate, Adjusted : L.Layout := Current;
      B : G.Logical_Rectangle;
   begin
      Proposed := Current;
      Accepted := False;
      if Moving > Current.Count then return; end if;
      Candidate.Items (Moving).Geometry.Scale := Factor (Preset);
      B := G.Bounds (Candidate.Items (Moving).Geometry);
      if Wide (B.Right) - Wide (B.Left) < Minimum_Width or
        Wide (B.Bottom) - Wide (B.Top) < Minimum_Height then
         return;
      end if;
      if L.Validate (Candidate).Status = L.Accepted then
         Proposed := Candidate;
         Accepted := True;
      else
         -- Changing logical extent may open a gap or introduce overlap. Keep
         -- the selected display attached to its nearest admissible neighbor.
         Move (Candidate, Moving, Candidate.Items (Moving).Geometry.X,
               Candidate.Items (Moving).Geometry.Y, Adjusted, Accepted);
         if Accepted then Proposed := Adjusted; end if;
      end if;
   end Rescale;

   function Clamp (Value, Low, High : Coordinate) return Coordinate is
     (Coordinate'Max (Low, Coordinate'Min (High, Value)));
   function Align_Edge (Value, First, Last : Coordinate) return Coordinate is
     (if abs (Value - First) <= Edge_Snap_Distance then First
      elsif abs (Value - Last) <= Edge_Snap_Distance then Last else Value);

   function Prepare
     (Current : L.Layout; Moving : L.Viewport_Index; PX, PY : Coordinate)
      return Candidate_Result
   is
      Candidate : L.Layout := Current;
      Min_X, Min_Y : G.Output_Origin := G.Output_Origin'Last;
   begin
      if PX not in Wide (G.Output_Origin'First) .. Wide (G.Output_Origin'Last) or else
        PY not in Wide (G.Output_Origin'First) .. Wide (G.Output_Origin'Last)
      then return (Valid => False); end if;
      Candidate.Items (Moving).Geometry.X := G.Output_Origin (PX);
      Candidate.Items (Moving).Geometry.Y := G.Output_Origin (PY);
      for I in 1 .. Candidate.Count loop
         Min_X := G.Output_Origin'Min (Min_X, Candidate.Items (I).Geometry.X);
         Min_Y := G.Output_Origin'Min (Min_Y, Candidate.Items (I).Geometry.Y);
      end loop;
      for I in 1 .. Candidate.Count loop
         declare
            NX : constant G.Logical_Coordinate := Candidate.Items (I).Geometry.X - Min_X;
            NY : constant G.Logical_Coordinate := Candidate.Items (I).Geometry.Y - Min_Y;
         begin
            if NX not in G.Output_Origin or NY not in G.Output_Origin then
               return (Valid => False);
            end if;
            Candidate.Items (I).Geometry.X := NX;
            Candidate.Items (I).Geometry.Y := NY;
         end;
      end loop;
      -- Validate the FINAL coordinates, not a pre-normalization surrogate.
      if L.Validate (Candidate).Status /= L.Accepted then return (Valid => False); end if;
      return (True, Candidate);
   end Prepare;

   procedure Move
     (Current : L.Layout; Moving : L.Viewport_Index;
      X, Y : G.Output_Origin; Proposed : out L.Layout; Accepted : out Boolean)
   is
      Best : Wide := Wide'Last;
      Moving_Bounds : G.Logical_Rectangle;
      Width, Height : Coordinate;
   begin
      Proposed := Current;
      Accepted := False;
      if Current.Count < 2 or Moving > Current.Count then return; end if;
      Moving_Bounds := G.Bounds (Current.Items (Moving).Geometry);
      Width := Wide (Moving_Bounds.Right) - Wide (Moving_Bounds.Left);
      Height := Wide (Moving_Bounds.Bottom) - Wide (Moving_Bounds.Top);
      if Width <= 0 or Height <= 0 then return; end if;
      for I in 1 .. Current.Count loop
         pragma Loop_Invariant (Accepted or else Proposed = Current);
         if I /= Moving then
            declare
               B : constant G.Logical_Rectangle := G.Bounds (Current.Items (I).Geometry);
               Left : constant Coordinate := Wide (B.Left);
               Top : constant Coordinate := Wide (B.Top);
               Right : constant Coordinate := Wide (B.Right);
               Bottom : constant Coordinate := Wide (B.Bottom);
               -- Positive edge overlap, not a corner-only connection.
               SX : constant Coordinate := Clamp (Align_Edge (Wide (X), Left, Right - Width),
                                                   Left - Width + 1, Right - 1);
               SY : constant Coordinate := Clamp (Align_Edge (Wide (Y), Top, Bottom - Height),
                                                   Top - Height + 1, Bottom - 1);
            begin
               for Edge in Side loop
                  pragma Loop_Invariant (Accepted or else Proposed = Current);
                  declare
                     PX : constant Coordinate :=
                       (case Edge is when Left_Edge => Left - Width,
                        when Right_Edge => Right, when others => SX);
                     PY : constant Coordinate :=
                       (case Edge is when Top_Edge => Top - Height,
                        when Bottom_Edge => Bottom, when others => SY);
                     Candidate : constant Candidate_Result := Prepare (Current, Moving, PX, PY);
                     Distance : constant Wide := abs (PX - Wide (X)) + abs (PY - Wide (Y));
                  begin
                     if Candidate.Valid and then Distance < Best then
                        Proposed := Candidate.Value;
                        Accepted := True;
                        Best := Distance;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Move;
end CuBit.Display_Arrangement;
