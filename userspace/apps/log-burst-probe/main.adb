pragma Ada_2022;
with CuBit.Waits;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Records;
with CuBit.Logging;
with Startup;

--  Native regression for LOG-005 (CCL_CONSOLE_DEMO=log-burst): a service's
--  bring-up writes about a thousand records, as acpi.svc does; logstore
--  must keep them all (logs.retention's per-source ceiling is above that),
--  and (logs.recent "log-burst-probe") must show them, or say exactly how
--  many it leaves out. Then it writes past the ceiling: the oldest after
--  its boot window give way, and the result says exactly how many were not
--  retained. tests/log-burst/check_serial.py checks both results.
--
--  Phase one is written as fast as the publisher ring takes it (flushed
--  each Batch, as a busy service's ring is drained); phase two is paced
--  inside logstore's publication budget (Log_Budgets: 1000 a second), so
--  every record reaches logstore and only retention gives any up. The
--  records the ring shed, if any, are counted and printed for the check.
procedure Main is
   package L renames CuBit.Log_Records;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);

   --  Agreed with tests/headless/run.sh and tests/log-burst/check_serial.py.
   Phase_One_Lines : constant := 1_000;
   Phase_Two_Lines : constant := 4_000;
   Line_Prefix : constant String := "log-burst: line ";
   Batch : constant := 100;
   --  Phase two: Batch records every Batch_Pause_Ms, under the budget.
   Batch_Pause_Ms : constant := 150;
   --  Time for the console's (logs.recent ...) between the phases.
   Between_Phases_Ms : constant := 8_000;
   Flush_Wait_Ms : constant := 2_000;
   Exit_Failure : constant := 1;

   Ignore : Unsigned_64;
   Drained : Boolean;

   function Image (Value : Unsigned_64) return String is
      Raw : constant String := Unsigned_64'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;

   procedure Fail (Why : String) is
   begin
      debugPrint ("log-burst-probe: FAIL " & Why & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, Exit_Failure);
   end Fail;

   procedure Line (Number : Positive) is
      Made : constant L.Decoded := L.Make (Line_Prefix & Image (Unsigned_64 (Number)));
      Accepted : Boolean;
   begin
      if not Made.Success then
         Fail ("record not made");
      end if;
      --  A full ring sheds it and counts it (Dropped): reported below.
      CuBit.Logging.Emit (Writer, Made.Value, Accepted);
   end Line;

   procedure Flush is
   begin
      CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
      if not Drained then
         Fail ("logstore did not take its records within" & Natural'Image (Flush_Wait_Ms) & " ms");
      end if;
   end Flush;
begin
   for Number in 1 .. Phase_One_Lines loop
      Line (Number);
      if Number mod Batch = 0 then
         Flush;
      end if;
   end loop;
   Flush;
   debugPrint ("log-burst-probe: phase 1 done, " & Image (Phase_One_Lines) & " lines, " &
               Image (CuBit.Logging.Dropped (Writer)) & " shed by its ring" & ASCII.LF);
   Ignore := CuBit.Waits.Sleep_For (Between_Phases_Ms);
   for Number in Phase_One_Lines + 1 .. Phase_One_Lines + Phase_Two_Lines loop
      Line (Number);
      if Number mod Batch = 0 then
         Flush;
         Ignore := CuBit.Waits.Sleep_For (Batch_Pause_Ms);
      end if;
   end loop;
   Flush;
   debugPrint ("log-burst-probe: phase 2 done, " & Image (Phase_One_Lines + Phase_Two_Lines) & " lines, " &
               Image (CuBit.Logging.Dropped (Writer)) & " shed by its ring" & ASCII.LF);
   --  Its records outlive it (LOG-003); staying keeps the check simple.
   loop
      Ignore := CuBit.Waits.Sleep_For (Between_Phases_Ms);
   end loop;
end Main;
