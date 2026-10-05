package body Intel_GPU_Scanout_Inventory with SPARK_Mode is
   use type Intel_GPU_Plane_Decode.Status;
   use type Intel_GPU_Cursor_Decode.Status;
   use type Intel_GPU_Display_Presence.Presence;
   function Collect (P : Planes; C : Cursors; Table_Bytes : Unsigned_64;
     Presence : Intel_GPU_Display_Presence.Snapshot) return Inventory is
      Result : Inventory;
      package DP renames Intel_GPU_Display_Presence;
   begin
      if not Presence.Known or else
        (for some P in DP.Pipe => Presence.Pipes (P) = DP.Unknown) or else
        Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608
      then return Result; end if;
      for I in Plane_Index loop
         pragma Loop_Invariant (Result.Count <= I - 1);
         if Presence.Pipes (DP.Pipe'Val ((I - 1) / 5)) = DP.Present then
         if not P (I).Collected then return Result; end if;
         declare
            D : constant Intel_GPU_Plane_Decode.Decoded :=
              Intel_GPU_Plane_Decode.Decode (P (I).Before, P (I).After, Table_Bytes);
         begin
            if D.State = Intel_GPU_Plane_Decode.Linear_Ready then
               Result.Count := Result.Count + 1; Result.Ranges (Result.Count) := D.Memory;
            elsif D.State /= Intel_GPU_Plane_Decode.Disabled then
               Result.Status := Unsupported_Plane; return Result;
            end if;
         end;
         end if;
      end loop;
      for I in Cursor_Index loop
         pragma Loop_Invariant (Result.Count <= 20 + I - 1);
         if Presence.Pipes (DP.Pipe'Val (I - 1)) = DP.Present then
         if not C (I).Collected then return Result; end if;
         declare
            D : constant Intel_GPU_Cursor_Decode.Decoded :=
              Intel_GPU_Cursor_Decode.Decode (C (I).Before, C (I).After, Table_Bytes);
         begin
            if D.State = Intel_GPU_Cursor_Decode.Ready then
               Result.Count := Result.Count + 1; Result.Ranges (Result.Count) := D.Memory;
            elsif D.State /= Intel_GPU_Cursor_Decode.Disabled then
               Result.Status := Unsupported_Cursor; return Result;
            end if;
         end;
         end if;
      end loop;
      Result.Status := Complete;
      return Result;
   end Collect;
   function Plan_Linear_Flip
     (P : Planes; C : Cursors;
      Presence : Intel_GPU_Display_Presence.Snapshot;
      Selected : Plane_Index;
      Table_Bytes, Target_First, Target_Bytes : Unsigned_64)
      return Intel_GPU_Plane_Decode.Flip_Plan
   is
      package DP renames Intel_GPU_Display_Presence;
      State : constant Inventory := Collect (P, C, Table_Bytes, Presence);
      Rejected : constant Intel_GPU_Plane_Decode.Flip_Plan := (others => <>);
   begin
      if not Presence.Known or else
        Presence.Pipes (DP.Pipe'Val ((Selected - 1) / 5)) /= DP.Present or else
        not P (Selected).Collected or else
        not No_Scanout_Overlap (State, (True, Target_First, Target_Bytes))
      then
         return Rejected;
      end if;
      return Intel_GPU_Plane_Decode.Plan_Linear_Flip
        (P (Selected).Before, P (Selected).After,
         Table_Bytes, Target_First, Target_Bytes);
   end Plan_Linear_Flip;
   function No_Scanout_Overlap (State : Inventory; Candidate : Intel_GPU_Scanout_Range.Extent)
     return Boolean is
   begin
      if State.Status /= Complete or else not Canonical (Candidate) then return False; end if;
      for I in 1 .. State.Count loop
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 => Canonical (State.Ranges (J)) and then
              Disjoint (Candidate, State.Ranges (J)));
         declare E : constant Intel_GPU_Scanout_Range.Extent := State.Ranges (I); begin
            if not Canonical (E) then return False; end if;
            if Candidate.First >= E.First then
               if Candidate.First - E.First < E.Bytes then return False; end if;
            elsif E.First - Candidate.First < Candidate.Bytes then return False;
            end if;
         end;
      end loop;
      return True;
   end No_Scanout_Overlap;
end Intel_GPU_Scanout_Inventory;
