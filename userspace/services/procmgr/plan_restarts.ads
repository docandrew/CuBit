with Interfaces; use Interfaces;

--  Supervision of startup-plan entries declared (restart on-failure)
--  (BUG-001: the desktop died and nothing started it again). One Run per
--  plan entry: the program is started again after a failure, after a delay
--  that doubles while it keeps failing and starts over once a run lasted.
--  Times are SYSCALL_GETTIME milliseconds. No I/O; procmgr launches.
package Plan_Restarts with SPARK_Mode is
   --  The first delay after a failure.
   First_Delay_Ms : constant := 1_000;
   --  The delay stops doubling here.
   Longest_Delay_Ms : constant := 30_000;
   --  A run at least this long is a fresh start: its failure waits only
   --  First_Delay_Ms again.
   Settled_Run_Ms : constant := 60_000;

   type Phase is (Idle, Running, Waiting);
   type Run is record
      State : Phase := Idle;
      Started_At : Unsigned_64 := 0;
      Delay_Ms : Unsigned_64 := 0;
      Due_At : Unsigned_64 := 0;
      Restarts : Natural := 0;
   end record;

   procedure Started (R : in out Run; Now : Unsigned_64)
     with Post => R.State = Running and R.Started_At = Now;

   --  The program ended. With Failed and Restartable, it is started again
   --  at R.Due_At (Scheduled); otherwise it stays ended.
   procedure Ended
     (R : in out Run; Now : Unsigned_64; Failed, Restartable : Boolean;
      Scheduled : out Boolean)
     with Pre => R.State = Running,
          Post => (Scheduled = (Failed and Restartable)) and
                  (if Scheduled then R.State = Waiting and
                     R.Delay_Ms in First_Delay_Ms .. Longest_Delay_Ms
                   else R.State = Idle);

   function Due (R : Run; Now : Unsigned_64) return Boolean is
     (R.State = Waiting and then Now >= R.Due_At);

   --  The restart could not be launched now: try again after the next delay.
   procedure Refused (R : in out Run; Now : Unsigned_64)
     with Pre => R.State = Waiting,
          Post => R.State = Waiting and R.Delay_Ms in First_Delay_Ms .. Longest_Delay_Ms;
end Plan_Restarts;
