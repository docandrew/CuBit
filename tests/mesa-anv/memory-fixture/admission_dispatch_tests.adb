with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
with Intel_Render_Admission;
with Intel_Render_Admission_Dispatch;
procedure Admission_Dispatch_Tests is
   package Core renames Intel_Render_Admission;
   use type Core.Phase;
   package D is new Intel_Render_Admission_Dispatch (2, 1000, 2000);
   Object : D.Dispatcher;
   First, Second, Rejected : D.Ticket;
   Target : Recipient;
   Used : Boolean;
   Now : Unsigned_64 := 10;
   Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   procedure Reply (Token, Session : Unsigned_64) is
      Receipt : constant CompletionEntry :=
        (token => Token, from => 77, status => 0,
         msg => ((16#0A21#, 4, 0, 0), [0, 1, Session, 0]), valid => True);
   begin
      D.Complete (Object, Receipt, Now, Used);
   end Reply;
begin
   pragma Assert (not D.Runnable (Object));
   pragma Assert (D.Next_Deadline (Object) = Unsigned_64'Last);
   Inspection := [1, 1, 0, 42, 0, 7];
   Target := Capture (7);
   Inspection := [1, 11, 0, 77, 0, 9];
   D.Start (Object, Target, 31, 30, 4, 0, 10, First);
   D.Start (Object, Target, 31, 30, 4, 0, 100, Rejected);
   pragma Assert (First = 1 and Rejected = 0);
   D.Start (Object, Target, 31, 30, 5, 0, 100, Second);
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
   pragma Assert (Used and D.State (Object, Second) = Core.Delegate_Ready);
   Reply (1000, Base + 1);
   pragma Assert (Used and D.State (Object, First) = Core.Abort_Ready);
   D.Step (Object, 11);
   pragma Assert (Last_Token = 1002 and Last_Submit.words (3) = 2);
   Inspection := [1, 9, 0, 42, 0, 7];
   D.Step (Object, 11);
   pragma Assert (D.State (Object, Second) = Core.Delegate_Ready);
   D.Step (Object, 11);
   pragma Assert (D.State (Object, Second) = Core.Activate_Ready);
   Now := 11;
   Reply (1002, Base + 1);
   pragma Assert (Used and D.State (Object, First) = Core.Failed);
   D.Step (Object, 12);
   Now := 12;
   pragma Assert (Last_Token = 1004 and Last_Submit.words (3) = 1);
   Reply (1004, Base + 2);
   pragma Assert (Used and D.State (Object, Second) = Core.Active);
   pragma Assert (not D.Runnable (Object));
   pragma Assert (D.Next_Deadline (Object) = Unsigned_64'Last);
   Reply (1004, Base + 2); pragma Assert (not Used);
   D.Step (Object, 200);
   pragma Assert (D.State (Object, Second) = Core.Active);
   D.Start (Object, Target, 31, 30, 6, 200, 300, Rejected);
   pragma Assert (Rejected = 0); -- No reuse without confirmed retirement.
   D.Step (Object, 199); -- Clock rollback closes admission.
   pragma Assert (D.State (Object, Second) = Core.Abort_Pending);
   pragma Assert (Last_Token = 1005 and Last_Submit.words (3) = 2);
   declare
      package Small is new Intel_Render_Admission_Dispatch (2, 3000, 3002);
      Limited_Object : Small.Dispatcher;
      ID : Small.Ticket;
      Rejected_ID : Small.Ticket;
      Receipt : CompletionEntry :=
        (token => 3000, from => 77, status => 0,
         msg => ((16#0A21#, 4, 0, 0), [0, 1, 203, 0]), valid => True);
   begin
      Inspection := [1, 11, 0, 77, 0, 9];
      Small.Start (Limited_Object, Target, 31, 30, 6, 0, 100, ID);
      Small.Step (Limited_Object, 1);
      Small.Start (Limited_Object, Target, 31, 30, 7, 1, 100, Rejected_ID);
      pragma Assert (Rejected_ID = 0);
      Small.Cancel (Limited_Object, ID);
      Small.Complete (Limited_Object, Receipt, 1, Used);
      pragma Assert (Used and
        Small.State (Limited_Object, ID) = Core.Abort_Ready);
      Small.Step (Limited_Object, 2);
      pragma Assert (Last_Token = 3002 and Last_Submit.words (3) = 2);
      Receipt.token := 3002;
      Small.Complete (Limited_Object, Receipt, 2, Used);
      pragma Assert (Used and Small.State (Limited_Object, ID) = Core.Failed);
   end;
   declare
      Late : D.Dispatcher;
      ID : D.Ticket;
      Receipt : CompletionEntry :=
        (token => 1000, from => 77, status => 0,
         msg => ((16#0A21#, 4, 0, 0), [0, 1, Base + 4, 0]), valid => True);
   begin
      D.Start (Late, Target, 31, 30, 6, 0, 6, ID);
      D.Step (Late, 1);
      D.Complete (Late, Receipt, 2, Used);
      Inspection := [1, 9, 0, 42, 0, 7];
      D.Step (Late, 3); -- recipient endpoint
      D.Step (Late, 4); -- render endpoint
      D.Step (Late, 5); -- activate
      Receipt.token := 1001;
      D.Complete (Late, Receipt, 6, Used); -- no timer step first
      pragma Assert (Used and D.State (Late, ID) = Core.Abort_Ready);
   end;
   declare
      Waiting : D.Dispatcher;
      ID : D.Ticket;
      R : CompletionEntry :=
        (token => 1000, from => 77, status => 0,
         msg => ((16#0A21#, 4, 0, 0), [0, 1, Base + 6, 0]), valid => True);
   begin
      Inspection := [1, 11, 0, 77, 0, 9];
      D.Start (Waiting, Target, 31, 30, 8, 0, 5, ID);
      D.Step (Waiting, 1);
      pragma Assert (not D.Runnable (Waiting) and D.Next_Deadline (Waiting) = 5);
      D.Step (Waiting, 5);
      pragma Assert (D.State (Waiting, ID) = Core.Reserve_Pending);
      pragma Assert (D.Next_Deadline (Waiting) = Unsigned_64'Last);
      for Tick in 6 .. 20 loop
         D.Step (Waiting, Unsigned_64 (Tick));
         pragma Assert (not D.Runnable (Waiting));
         pragma Assert (D.Next_Deadline (Waiting) = Unsigned_64'Last);
         pragma Assert (Last_Token = 1000); -- No timeout replay or busy work.
      end loop;
      D.Complete (Waiting, R, 21, Used);
      pragma Assert (Used and D.Runnable (Waiting));
      pragma Assert (D.State (Waiting, ID) = Core.Abort_Ready);
      D.Step (Waiting, 21);
      pragma Assert (not D.Runnable (Waiting));
      pragma Assert (D.Next_Deadline (Waiting) = Unsigned_64'Last);
      R.token := 1002;
      D.Complete (Waiting, R, 22, Used);
      pragma Assert (Used and D.State (Waiting, ID) = Core.Failed);
      pragma Assert (not D.Runnable (Waiting));
   end;
   Put_Line ("Admission dispatcher PASS: fair steps, deadlines, late cleanup," &
     " duplicate routing, retained slots, clock/token exhaustion (mock IPC)");
end Admission_Dispatch_Tests;
