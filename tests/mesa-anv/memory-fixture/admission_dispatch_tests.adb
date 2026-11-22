with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Fixture;
with CuBit.Messages; use CuBit.Messages;
with Intel_Render_Admission;
with Intel_Render_Admission_Dispatch;
procedure Admission_Dispatch_Tests is
   package Core renames Intel_Render_Admission;
   use type Core.Phase;
   package D is new Intel_Render_Admission_Dispatch (2, 1000, 2000);
   Object : D.Dispatcher;
   First, Second, Rejected : D.Ticket;
   Used : Boolean;
   Now : Unsigned_64 := 10;
   Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   Driver_ID : constant Unsigned_64 := 77;
   Driver : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Fixture.Endpoint_Of (16#0000_0002_0000_0009#);
   procedure Reply (Token, Session : Unsigned_64; Slot : Unsigned_64 := 0) is
      Receipt : constant CompletionEntry :=
        (token => Token, from => Driver_ID, status => 0,
         msg => ((label => 16#0A21#, length => 4, protocol => 0, ipc => <>, reserved => 0), 0, [0, 1, Session, Slot]), valid => True);
   begin
      D.Complete (Object, Receipt, Now, Used);
   end Reply;
begin
   pragma Assert (not D.Runnable (Object));
   pragma Assert (D.Next_Deadline (Object) = Unsigned_64'Last);
   D.Start (Object, 0, Driver, Driver_ID, 0, 10, Rejected);
   pragma Assert (Rejected = 0);                        --  no identity
   D.Start (Object, 42, Driver, Driver_ID, 0, 10, First);
   D.Start (Object, 42, Driver, Driver_ID, 0, 100, Rejected);
   pragma Assert (First = 1 and Rejected = 0);         --  one per application
   D.Start (Object, 43, Driver, Driver_ID, 0, 100, Second);
   pragma Assert (Second = 2);
   pragma Assert (D.Runnable (Object) and D.Next_Deadline (Object) = 10);
   D.Step (Object, 1); pragma Assert (Last_Token = 1000);
   pragma Assert (D.Runnable (Object));
   D.Step (Object, 2); pragma Assert (Last_Token = 1003);
   pragma Assert (not D.Runnable (Object) and D.Next_Deadline (Object) = 10);
   D.Step (Object, 10);
   pragma Assert (D.State (Object, First) = Core.Reserve_Pending);
   pragma Assert (not D.Runnable (Object) and D.Next_Deadline (Object) = 100);
   Reply (999, 1); pragma Assert (not Used);
   Reply (1003, Base + 2);
   pragma Assert (Used and D.State (Object, Second) = Core.Activate_Ready);
   pragma Assert (D.Session (Object, Second) = Base + 2);
   Reply (1000, Base + 1);          --  past its deadline: abort, not activate
   pragma Assert (Used and D.State (Object, First) = Core.Abort_Ready);
   D.Step (Object, 11);
   pragma Assert (Last_Token = 1002 and Last_Submit.words (3) = 2);
   D.Step (Object, 11);
   pragma Assert (Last_Token = 1004 and Last_Submit.words (3) = 1);
   Now := 11;
   Reply (1002, Base + 1);
   pragma Assert (Used and D.State (Object, First) = Core.Failed);
   Now := 12;
   Reply (1004, Base + 2);
   pragma Assert (Used and D.State (Object, Second) = Core.Active);
   pragma Assert (not D.Runnable (Object));
   pragma Assert (D.Next_Deadline (Object) = Unsigned_64'Last);
   Reply (1004, Base + 2); pragma Assert (not Used);
   D.Step (Object, 200);
   pragma Assert (D.State (Object, Second) = Core.Active);
   D.Start (Object, 44, Driver, Driver_ID, 200, 300, Rejected);
   pragma Assert (Rejected = 0); -- Full: no reuse without confirmed retirement.
   D.Step (Object, 199); -- Clock rollback closes admission.
   pragma Assert (D.State (Object, Second) = Core.Abort_Pending);
   pragma Assert (Last_Token = 1005 and Last_Submit.words (3) = 2);
   --  A reply still naming a recipient slot is quarantined.
   declare
      Q : D.Dispatcher;
      ID : D.Ticket;
   begin
      D.Start (Q, 45, Driver, Driver_ID, 0, 100, ID);
      D.Step (Q, 1);
      D.Complete (Q, (token => 1000, from => Driver_ID, status => 0,
         msg => ((label => 16#0A21#, length => 4, protocol => 0, ipc => <>, reserved => 0), 0, [0, 1, Base + 3, 40]), valid => True), 2, Used);
      pragma Assert (Used and D.State (Q, ID) = Core.Quarantined);
   end;
   --  Token exhaustion fails closed.
   declare
      package Small is new Intel_Render_Admission_Dispatch (2, 3000, 3002);
      Limited_Object : Small.Dispatcher;
      ID, Rejected_ID : Small.Ticket;
      Receipt : CompletionEntry :=
        (token => 3000, from => Driver_ID, status => 0,
         msg => ((label => 16#0A21#, length => 4, protocol => 0, ipc => <>, reserved => 0), 0, [0, 1, 203, 0]), valid => True);
   begin
      Small.Start (Limited_Object, 46, Driver, Driver_ID, 0, 100, ID);
      Small.Step (Limited_Object, 1);
      Small.Start (Limited_Object, 47, Driver, Driver_ID, 1, 100, Rejected_ID);
      pragma Assert (Rejected_ID = 0);
      Small.Cancel (Limited_Object, ID);
      Small.Complete (Limited_Object, Receipt, 1, Used);
      pragma Assert (Used and Small.State (Limited_Object, ID) = Core.Abort_Ready);
      Small.Step (Limited_Object, 2);
      pragma Assert (Last_Token = 3002 and Last_Submit.words (3) = 2);
      Receipt.token := 3002;
      Small.Complete (Limited_Object, Receipt, 2, Used);
      pragma Assert (Used and Small.State (Limited_Object, ID) = Core.Failed);
   end;
   --  A deadline passed while pending is not replayed; a late success aborts.
   declare
      Waiting : D.Dispatcher;
      ID : D.Ticket;
      R : CompletionEntry :=
        (token => 1000, from => Driver_ID, status => 0,
         msg => ((label => 16#0A21#, length => 4, protocol => 0, ipc => <>, reserved => 0), 0, [0, 1, Base + 6, 0]), valid => True);
   begin
      D.Start (Waiting, 48, Driver, Driver_ID, 0, 5, ID);
      D.Step (Waiting, 1);
      D.Step (Waiting, 5);
      pragma Assert (D.State (Waiting, ID) = Core.Reserve_Pending);
      pragma Assert (D.Next_Deadline (Waiting) = Unsigned_64'Last);
      for Tick in 6 .. 20 loop
         D.Step (Waiting, Unsigned_64 (Tick));
         pragma Assert (not D.Runnable (Waiting) and Last_Token = 1000);
      end loop;
      D.Complete (Waiting, R, 21, Used);
      pragma Assert (Used and D.State (Waiting, ID) = Core.Abort_Ready);
      D.Step (Waiting, 21);
      R.token := 1002;
      D.Complete (Waiting, R, 22, Used);
      pragma Assert (Used and D.State (Waiting, ID) = Core.Failed);
   end;
   Put_Line ("Admission dispatcher PASS: fair steps, deadlines, late cleanup," &
     " duplicate identities, clock/token exhaustion (mock IPC)");
end Admission_Dispatch_Tests;
