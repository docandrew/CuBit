with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Presentation; use Compositor_Presentation;
procedure Presentation_Tests is
   S, Other : State;
   Started : Boolean;
   Checks : Natural := 0;
   Good : constant Completion := (True, True, True, 7, 23, 7, True, True);
   procedure In_Flight_State is
   begin
      S := Open (23);
      Submit (S, 7, Started);
      pragma Assert (Started and Current (S) = In_Flight and not Writable (S));
   end In_Flight_State;
begin
   pragma Assert (Current (S) = Closed and not Writable (S));
   Complete (S, Good);
   pragma Assert (Current (S) = Quarantined);
   for Bits in Unsigned_32 range 0 .. 31 loop
      for K in ID range 6 .. 8 loop
         for Session_ID in ID range 22 .. 24 loop
            for Frame in ID range 6 .. 8 loop
               In_Flight_State;
               Complete (S, ((Bits and 1) /= 0, (Bits and 2) /= 0,
                 (Bits and 4) /= 0, K, Session_ID, Frame,
                 (Bits and 8) /= 0, (Bits and 16) /= 0));
               pragma Assert (Writable (S) =
                 (Bits = 31 and K = 7 and Session_ID = 23 and Frame = 7));
               -- A duplicate or later convincing packet cannot authorize reuse.
               Complete (S, Good);
               pragma Assert (Current (S) = Quarantined);
               Submit (S, 8, Started);
               pragma Assert (not Started and not Writable (S));
               Checks := Checks + 1;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Old completion arriving after a new submission must not retire new work.
   In_Flight_State;
   Complete (S, Good);
   Submit (S, 8, Started);
   pragma Assert (Started);
   Complete (S, Good);
   pragma Assert (Current (S) = Quarantined and Token (S) = 8);
   -- Independent outputs do not inherit one another's release fence.
   In_Flight_State;
   Other := Open (24);
   Submit (Other, 8, Started);
   Complete (Other, Good);
   Complete (S, Good);
   pragma Assert (Writable (S) and not Writable (Other));
   -- Failed submission is uncertain; even a matching completion stays closed.
   In_Flight_State;
   Quarantine (S);
   Complete (S, Good);
   pragma Assert (not Writable (S));
   -- No modular identifier wrap or replay, including at the final identifier.
   S := Open (ID'Last);
   Submit (S, ID'Last, Started);
   pragma Assert (Started);
   Complete (S, (True, True, True, ID'Last, ID'Last, ID'Last, True, True));
   pragma Assert (Writable (S));
   Submit (S, 0, Started);
   pragma Assert (not Started and Current (S) = Quarantined);
   S := Open (23);
   Submit (S, 0, Started);
   pragma Assert (not Started);
   In_Flight_State;
   Submit (S, 8, Started);
   pragma Assert (not Started and Token (S) = 7);
   Ada.Text_IO.Put_Line ("presentation: PASS" & Checks'Image & " completion faults and lifecycle traces");
end Presentation_Tests;
