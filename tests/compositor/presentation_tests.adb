with Ada.Text_IO;
with Compositor_Pool;
with Interfaces; use Interfaces;
with Compositor_Presentation; use Compositor_Presentation;
procedure Presentation_Tests is
   S, Other : State;
   Started : Boolean;
   Checks : Natural := 0;
   Good : constant Completion := (True, True, True, 7, 23, 7, True, True);
   procedure Submit (Value : in out State; Frame : ID; OK : out Boolean) is
   begin
      Prepare (Value, Frame, OK);
      if OK then Submitted (Value, True, 0); end if;
   end Submit;
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
   -- Uncertain submission is quarantined; a later packet cannot authorize reuse.
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
   -- A full mailbox cannot retire or mutate the retained frame. Repeated
   -- refusal leaves exactly one token and does not admit a completion.
   S := Open (23);
   Prepare (S, 7, Started);
   pragma Assert (Started and Current (S) = Prepared and not Writable (S));
   for Attempt in 1 .. 10_000 loop
      Submitted (S, False, 100);
      pragma Assert (Current (S) = Prepared and Token (S) = 7 and not Writable (S));
      pragma Assert (not Releases (S, Good));
      pragma Assert (not Can_Attempt (S, 100) and Can_Attempt (S, 101));
   end loop;
   Other := S;
   Submitted (S, True, 101);
   pragma Assert (Current (S) = In_Flight);
   Complete (S, Good);
   pragma Assert (Writable (S));
   Complete (Other, Good);
   pragma Assert (Current (Other) = Quarantined);
   -- Shutdown can cancel a never-published frame, but never accepted work.
   S := Open (23);
   Prepare (S, 7, Started);
   Submitted (S, False, 100);
   Cancel (S);
   pragma Assert (Writable (S) and Token (S) = 7);
   Prepare (S, 7, Started);
   pragma Assert (not Started and Current (S) = Quarantined);
   In_Flight_State;
   Cancel (S);
   pragma Assert (Current (S) = Quarantined and Token (S) = 7);
   -- Duplicate submission results cannot turn accepted work into retryable work.
   for Accepted in Boolean loop
      In_Flight_State;
      Submitted (S, Accepted, 101);
      pragma Assert (Current (S) = Quarantined);
   end loop;
   -- Time overflow or unavailable time can never authorize an immediate retry.
   S := Open (23);
   Prepare (S, 7, Started);
   Submitted (S, False, ID'Last - 1);
   pragma Assert (not Can_Attempt (S, ID'Last - 1) and not Can_Attempt (S, ID'Last));
   Submitted (S, False, ID'Last);
   pragma Assert (not Can_Attempt (S, 0));
   -- Exercise the actual pool with the policy: input-time drawing can own a
   -- different writer while the refused frame stays immutable. Cancellation
   -- releases only the never-published ticket; accepted work needs completion.
   declare
      package BP renames Compositor_Pool;
      use type BP.Ticket;
      Pool : BP.State := BP.Open (1);
      Writer, Held, Next_Writer : BP.Ticket;
   begin
      BP.Acquire (Pool, Writer);
      BP.Start_Render (Pool, Writer);
      BP.Finish_Render (Pool, Writer, BP.Completed);
      BP.Present (Pool, Held);
      S := Open (23);
      Prepare (S, 7, Started);
      BP.Acquire (Pool, Next_Writer);
      pragma Assert (Next_Writer /= Held and BP.Writable (Pool, Next_Writer));
      for Attempt in 1 .. 1_000 loop
         Submitted (S, False, ID (Attempt));
         pragma Assert (BP.Displayed (Pool) = Held and not BP.Writable (Pool, Held));
         pragma Assert (BP.Writable (Pool, Next_Writer) and not BP.Faulted (Pool));
      end loop;
      Cancel (S);
      pragma Assert (Writable (S));
      BP.Retire_Display (Pool, Held, True);
      pragma Assert (BP.Displayed (Pool) = BP.None and not BP.Faulted (Pool));
      pragma Assert (BP.Writable (Pool, Next_Writer));
   end;
   Ada.Text_IO.Put_Line ("presentation: PASS" & Checks'Image & " completion faults and lifecycle traces");
end Presentation_Tests;
