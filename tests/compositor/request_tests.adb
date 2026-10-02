with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Requests; use Compositor_Requests;
procedure Request_Tests is
   Sequence : ID := 0;
   Launch, Display, Old_Launch : ID;
   S : State;
   Accepted : Boolean;
begin
   -- Interleave routes while one launch owns its shared filename storage.
   for Cycle in 1 .. 3_000 loop
      Allocate (Sequence, Launch);
      Begin_Request (S, Launch, Accepted);
      pragma Assert (Accepted and Busy (S) and not Available (S));
      Allocate (Sequence, Display);
      pragma Assert (Display > Launch);
      Begin_Request (S, Display, Accepted);
      pragma Assert (not Accepted and Token (S) = Launch and Busy (S));
      Complete (S, Launch, True);
      pragma Assert (Available (S));
   end loop;
   Old_Launch := Launch;
   Allocate (Sequence, Launch);
   Begin_Request (S, Launch, Accepted);
   Complete (S, Old_Launch, True);
   pragma Assert (Faulted (S) and not Available (S));
   Complete (S, Launch, True);
   pragma Assert (Faulted (S));
   Allocate (Sequence, Display);
   Begin_Request (S, Display, Accepted);
   pragma Assert (not Accepted);
   declare Clean : State;
   begin
      Begin_Request (Clean, 1, Accepted);
      Complete (Clean, 1, False);
      pragma Assert (Faulted (Clean));
   end;
   declare Clean : State;
   begin
      Begin_Request (Clean, 0, Accepted);
      pragma Assert (not Accepted and Available (Clean));
      Begin_Request (Clean, ID'Last, Accepted);
      pragma Assert (not Accepted);
      Begin_Request (Clean, 1, Accepted);
      Quarantine (Clean);
      Complete (Clean, 1, True);
      pragma Assert (Faulted (Clean));
   end;
   Sequence := ID'Last - 2;
   Allocate (Sequence, Launch);
   pragma Assert (Launch = ID'Last - 1);
   Allocate (Sequence, Display);
   pragma Assert (Display = 0 and Sequence = ID'Last - 1);
   Sequence := ID'Last;
   Allocate (Sequence, Display);
   pragma Assert (Display = 0 and Sequence = ID'Last);
   Ada.Text_IO.Put_Line ("requests: PASS 3000 interleaved routes and uncertainty/exhaustion traces");
end Request_Tests;
