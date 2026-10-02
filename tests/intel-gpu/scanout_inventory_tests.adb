with Intel_GPU_Display_Presence;
with Ada.Text_IO; with Interfaces;
with Intel_GPU_Scanout_Inventory; with Intel_GPU_Scanout_Range;
procedure Scanout_Inventory_Tests is
   use Interfaces; use Intel_GPU_Scanout_Inventory;
   package DP renames Intel_GPU_Display_Presence;
   Present_Pipes : constant DP.Snapshot := (True, [others => DP.Present]);
   P : Planes; C : Cursors; R : Inventory;
   Candidate : Intel_GPU_Scanout_Range.Extent := (True, 0, 4096);
   procedure Reset is
   begin
      for I in Plane_Index loop
         P (I) := (Collected => True, others => <>);
         P (I).Before := (16#84000000#, 1, 15, 0,
           Unsigned_32 (I) * 65536, Unsigned_32 (I) * 65536);
         P (I).After := P (I).Before;
      end loop;
      for I in Cursor_Index loop
         C (I) := (Collected => True, others => <>);
         C (I).Before := (16#27#, Unsigned_32 (I + 20) * 65536,
           Unsigned_32 (I + 20) * 65536, 0);
         C (I).After := C (I).Before;
      end loop;
   end Reset;
begin
   declare
      Presence : DP.Snapshot;
      Expected : Natural;
   begin
      for Mask in Unsigned_32 range 0 .. 15 loop
         Reset;
         Presence := (True, [others => DP.Absent]);
         Expected := 0;
         for Pipe in DP.Pipe loop
            if (Mask and Shift_Left (1, DP.Pipe'Pos (Pipe))) /= 0 then
               Presence.Pipes (Pipe) := DP.Present;
               Expected := Expected + 6;
            else
               for N in 1 .. 5 loop
                  P (DP.Pipe'Pos (Pipe) * 5 + N).Collected := False;
               end loop;
               C (DP.Pipe'Pos (Pipe) + 1).Collected := False;
            end if;
         end loop;
         R := Collect (P, C, 8_388_608, Presence);
         pragma Assert (R.Status = Complete and R.Count = Expected);
         for Pipe in DP.Pipe loop
            declare Unknown_Pipe : DP.Snapshot := Presence; begin
               Unknown_Pipe.Pipes (Pipe) := DP.Unknown;
               R := Collect (P, C, 8_388_608, Unknown_Pipe);
               pragma Assert (R.Status = Incomplete and not No_Scanout_Overlap (R, Candidate));
            end;
         end loop;
         Presence.Known := False;
         pragma Assert (Collect (P, C, 8_388_608, Presence).Status = Incomplete);
      end loop;
   end;
   Reset; R := Collect (P, C, 8_388_608, Present_Pipes);
   pragma Assert (R.Status = Complete and R.Count = 24);
   for I in 1 .. 24 loop
      pragma Assert (R.Ranges (I).First = Unsigned_64 (I) * 65536);
      Candidate.First := R.Ranges (I).First;
      pragma Assert (not No_Scanout_Overlap (R, Candidate));
      Candidate.First := R.Ranges (I).First - 4096;
      pragma Assert (No_Scanout_Overlap (R, Candidate));
      Candidate.Bytes := 4097;
      pragma Assert (not No_Scanout_Overlap (R, Candidate));
      Candidate.Bytes := 4096;
      Candidate.First := R.Ranges (I).First + R.Ranges (I).Bytes;
      pragma Assert (No_Scanout_Overlap (R, Candidate));
   end loop;
   for I in Plane_Index loop
      -- Stable flip-source status must not shift the protected address by8.
      Reset;
      P (I).Before.Surface := P (I).Before.Surface or 8;
      P (I).After := P (I).Before;
      R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Complete and R.Count = 24);
      pragma Assert (R.Ranges (I).First = Unsigned_64 (I) * 65536);
      pragma Assert (not No_Scanout_Overlap
        (R, (True, Unsigned_64 (I) * 65536, 4096)));
      -- A changing flip source remains an unstable observation, even if
      -- both address fields agree. Incomplete evidence never admits memory.
      P (I).After.Surface := P (I).After.Surface xor 8;
      R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Unsupported_Plane and
        not No_Scanout_Overlap (R, Candidate));
      Reset; P (I).Collected := False; R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Incomplete and not No_Scanout_Overlap (R, Candidate));
      Reset; P (I).After.Surface := 0; R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Unsupported_Plane);
   end loop;
   for I in Cursor_Index loop
      for Mode in Unsigned_32 range 1 .. 63 loop
         if Mode not in 16#22# | 16#23# | 16#27# then
            Reset;
            C (I).Before.Control := Mode;
            C (I).After := C (I).Before;
            R := Collect (P, C, 8_388_608, Present_Pipes);
            pragma Assert (R.Status = Unsupported_Cursor);
            pragma Assert (not No_Scanout_Overlap (R, Candidate));
         end if;
      end loop;
      Reset; C (I).Collected := False; R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Incomplete);
      Reset; C (I).After.Base := 0; R := Collect (P, C, 8_388_608, Present_Pipes);
      pragma Assert (R.Status = Unsupported_Cursor);
   end loop;
   P := (others => (Collected => True, others => <>));
   C := (others => (Collected => True, others => <>));
   R := Collect (P, C, 8_388_608, Present_Pipes);
   pragma Assert (R.Status = Complete and R.Count = 0);
   pragma Assert (No_Scanout_Overlap (R, (True, 0, 4096)));
   pragma Assert (not No_Scanout_Overlap (R, (True, Unsigned_64'Last, 1)));
   pragma Assert (not No_Scanout_Overlap (R, (True, 0, 0)));
   pragma Assert (Collect (P, C, 0, Present_Pipes).Status = Incomplete);
   -- Independent endpoint-based oracle, including containment and adjacency.
   -- Production arithmetic uses differences rather than endpoint addition.
   R.Status := Complete; R.Count := 1;
   for A in Unsigned_64 range 0 .. 15 loop
      for B in Unsigned_64 range 0 .. 15 loop
         for N in Unsigned_64 range 1 .. 16 loop
            for M in Unsigned_64 range 1 .. 16 loop
               R.Ranges (1) := (True, B, M);
               pragma Assert (No_Scanout_Overlap (R, (True, A, N)) =
                 (A + N <= B or else B + M <= A));
            end loop;
         end loop;
      end loop;
   end loop;
   R.Ranges (1) := (True, Unsigned_64'Last, 1);
   pragma Assert (not No_Scanout_Overlap (R, (True, 0, 4096)));
   Ada.Text_IO.Put_Line ("scanout inventory PASS: all24 extents, each missing/changing observation, overlap boundaries");
end Scanout_Inventory_Tests;
