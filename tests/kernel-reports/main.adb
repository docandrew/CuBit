--  Hosted tests for Kernel_Reports: reports are kept until taken, a PID
--  waits for its reports, a closed recipient's reports are dropped and
--  release the PID, faults coalesce with a count.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Kernel_Reports; use Kernel_Reports;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Parent     : constant Process := 5;
   Manager    : constant Process := 1;
   Supervisor : constant Process := 6;
   Parent_Gen  : constant Unsigned_64 := 3;
   Manager_Gen : constant Unsigned_64 := 9;
   Exit_Label  : constant Unsigned_32 := 16#0103#;
   Fault_Label : constant Unsigned_32 := 16#0104#;

   function Exit_Of (Subject : Process) return Report is
     (Label => Exit_Label, Length => 4,
      Words => [Unsigned_64 (Subject), 1, 0, 7], Further => 0);

   T : Table;
   Kept, Found, Free_Now : Boolean;
   Value : Report;
   Released : Process;
begin
   Open (T, Parent, Parent_Gen);
   Open (T, Manager, Manager_Gen);
   Open (T, Supervisor, 1);

   --  Both recipients must take the exit before the PID is released.
   Put (T, 10, Exit_To_Parent, (Parent, Parent_Gen), Exit_Of (10), Kept);
   Check (Kept, "exit kept for the parent");
   Put (T, 10, Exit_To_Manager, (Manager, Manager_Gen), Exit_Of (10), Kept);
   Check (Kept, "exit kept for the manager");
   Request_Free (T, 10, Free_Now);
   Check (not Free_Now, "PID waits while reports are unread");
   Take (T, Parent, Value, Found, Released);
   Check (Found and then Value = Exit_Of (10), "parent takes the exit");
   Check (Released = No_Process, "not released while the manager has not read");
   Take (T, Manager, Value, Found, Released);
   Check (Found and then Released = 10, "manager's take releases the PID");
   Take (T, Manager, Value, Found, Released);
   Check (not Found and then Released = No_Process, "nothing more to take");

   --  A closed recipient's reports are dropped, and that releases the PID.
   Put (T, 11, Exit_To_Parent, (Parent, Parent_Gen), Exit_Of (11), Kept);
   Request_Free (T, 11, Free_Now);
   Check (not Free_Now, "deferred again");
   Close (T, Parent, Released);
   Check (Released = 11, "closing the parent releases the child's PID");
   Close (T, Parent, Released);
   Check (Released = No_Process and then Closed (T, Parent), "parent fully closed");

   --  No report for a closed recipient or another incarnation.
   Put (T, 12, Exit_To_Parent, (Parent, Parent_Gen), Exit_Of (12), Kept);
   Check (not Kept, "a closed recipient takes no reports");
   Put (T, 12, Exit_To_Manager, (Manager, Manager_Gen + 1), Exit_Of (12), Kept);
   Check (not Kept, "another incarnation takes no reports");
   Request_Free (T, 12, Free_Now);
   Check (Free_Now, "nothing unread: freed at once");

   --  Faults while one is unread add to its count.
   for N in 1 .. 3 loop
      Put (T, 13, Fault_To_Supervisor, (Supervisor, 1),
           (Label => Fault_Label, Length => Report_Words,
            Words => [13, Unsigned_64 (N), 0, 0], Further => 0), Kept);
      Check (Kept, "fault kept");
   end loop;
   Take (T, Supervisor, Value, Found, Released);
   Check (Found and then Value.Words (1) = 1 and then
          Value.Further = 2,
          "first fault kept, two more counted");
   Take (T, Supervisor, Value, Found, Released);
   Check (not Found, "faults all taken");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("kernel-reports: PASS");
   else
      Ada.Text_IO.Put_Line ("kernel-reports: FAIL" & Failures'Image);
   end if;
end Main;
