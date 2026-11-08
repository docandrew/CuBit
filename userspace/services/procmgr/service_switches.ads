pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Service_Replacement;

--  One service switch in progress (CuBit.Service_Replacement): the explicit
--  machine procmgr drives from its service loop. No I/O: each step returns
--  the one Action procmgr must take now, and procmgr reports what happened
--  (an exit and whether it was clean, a start, the time and the service's
--  registration).
--
--    Stopping_Old  Stop sent; waiting for the old instance to end until
--                  the grace deadline
--    Ending_Old    ended by procmgr (Transparent only); waiting until
--                  End_Deadline_Ms
--    Starting      the program is being started (procmgr reports at once)
--    Checking      the program runs; the health window is open
--    Stopping_New  it failed; Stop sent (Graceful), waiting until the grace
--    Ending_New    it failed; ended by procmgr, waiting for its end
--    Restoring     the program the entry ran before is being started
--    Finished      Result (and Failure) is the answer
--
--  The entry's restart contract (CCL.Configurations.Restart_Contract)
--  decides how an instance is stopped:
--    Transparent  it may be ended: Stop, then ended after the grace (or at
--                 once with no grace);
--    Graceful     it is never ended to make way: Stop, and the old instance
--                 must exit cleanly (code 0: quiesced, its resources
--                 released) within the grace. One that does not exit in
--                 time keeps running and nothing starts (Stop_Failed); one
--                 that exits failing is not trusted to have released what
--                 it held, so the program before is started again in its
--                 place (Stop_Failed) rather than the new one. A new
--                 instance that fails its window is asked to Stop too, and
--                 ended only after the grace.
--  Two instances of an entry never run at once: the next starts only after
--  the kernel reported the previous one's exit.
--
--  Every waiting phase has a deadline, and a step at or past it always
--  leaves the phase for a later one (Rank grows): a switch ends within
--  CuBit.Service_Replacement.Answer_Bound_Ms of Begin_Switch, whatever the
--  instances do. Times are SYSCALL_GETTIME milliseconds.
package Service_Switches with SPARK_Mode is
   package SR renames CuBit.Service_Replacement;
   use type SR.Outcome, SR.Health_Failure;

   type Phase is
     (Idle, Stopping_Old, Ending_Old, Starting, Checking, Stopping_New, Ending_New, Restoring,
      Finished);

   --  What procmgr does now.
   type Action is
     (Nothing,
      Ask_Stop,        --  send the old instance Stop (a control event)
      End_Old,         --  end the old instance
      Start_Program,   --  start the requested program as the entry
      Ask_Stop_New,    --  send the new instance Stop
      End_New,         --  end the new instance
      Start_Previous,  --  start the program the entry ran before
      Answer);         --  answer the requester with Result and Failure

   --  Processes as opaque identity words; 0 is none.
   subtype Process_Word is Unsigned_64;
   No_Process : constant Process_Word := 0;

   --  The latest time a deadline is set from, so additions never wrap.
   Latest_Time : constant := Unsigned_64'Last - SR.Maximum_Answer_Bound_Ms;
   subtype Time is Unsigned_64 range 0 .. Latest_Time;

   type Switch is record
      State    : Phase := Idle;
      Graceful : Boolean := False;
      Old_Process, New_Process : Process_Word := No_Process;
      Deadline : Unsigned_64 := 0;
      Grace    : SR.Stop_Grace_Ms := 0;
      Health   : SR.Health_Window_Ms := SR.Minimum_Health_Window_Ms;
      --  The new instance provides a service role it must register.
      Must_Register : Boolean := False;
      --  Graceful: the old instance exited failing (not quiesced).
      Old_Unclean : Boolean := False;
      Result   : SR.Outcome := SR.Switched;
      Failure  : SR.Health_Failure := SR.Healthy;
   end record;

   function Waiting (P : Phase) return Boolean is
     (P in Stopping_Old | Ending_Old | Checking | Stopping_New | Ending_New);

   function Rank (P : Phase) return Natural is (Phase'Pos (P));

   function Active (S : Switch) return Boolean is (S.State /= Idle);

   --  Take an admitted switch. Old: the entry's running instance (none:
   --  it is not running, so the program starts at once). A Graceful entry
   --  needs a grace.
   procedure Begin_Switch
     (S : in out Switch; Old : Process_Word; Graceful : Boolean; Grace : SR.Stop_Grace_Ms;
      Health : SR.Health_Window_Ms; Now : Time; Next : out Action)
   with Pre  => S.State = Idle and then (if Graceful then Grace > 0),
        Post => (if Old = No_Process then S.State = Starting and Next = Start_Program
                 elsif Grace = 0 then S.State = Ending_Old and Next = End_Old
                 else S.State = Stopping_Old and Next = Ask_Stop)
                and S.Old_Process = Old and S.Health = Health and S.Graceful = Graceful
                and (if Waiting (S.State) then S.Deadline >= Now);

   --  A process ended (any process: those the switch does not watch are
   --  ignored). Clean: it exited with code 0.
   procedure Exited (S : in out Switch; Process : Process_Word; Clean : Boolean; Next : out Action)
   with Pre  => Process /= No_Process,
        Post => (if Next /= Nothing then Rank (S.State) > Rank (S.State'Old))
                and (if S.State'Old in Stopping_Old | Ending_Old and Process = S.Old_Process'Old
                       and (Clean or not S.Graceful'Old)
                     then S.State = Starting and Next = Start_Program)
                and (if S.State'Old in Stopping_Old | Ending_Old and Process = S.Old_Process'Old
                       and S.Graceful'Old and not Clean
                     then S.State = Restoring and Next = Start_Previous and S.Old_Unclean)
                and (if S.State'Old = Checking and Process = S.New_Process'Old
                     then S.State = Restoring and Next = Start_Previous
                          and S.Failure = SR.Exited_Early);

   --  Starting or Restoring: procmgr's start of the program reported.
   --  Process: the new instance (No_Process: not started). Must_Register:
   --  it provides a service role (judged at the end of the window).
   procedure Started
     (S : in out Switch; Process : Process_Word; Must_Register : Boolean; Now : Time;
      Next : out Action)
   with Pre  => S.State in Starting | Restoring,
        Post => Rank (S.State) > Rank (S.State'Old)
                and (if S.State'Old = Starting and Process /= No_Process
                     then S.State = Checking and Next = Nothing and S.Deadline >= Now)
                and (if S.State'Old = Starting and Process = No_Process
                     then S.State = Restoring and Next = Start_Previous
                          and S.Failure = SR.Start_Refused)
                and (if S.State'Old = Restoring
                     then S.State = Finished and Next = Answer
                          and S.Result = (if Process = No_Process then SR.Rollback_Failed
                                          elsif S.Old_Unclean then SR.Stop_Failed
                                          else SR.Rolled_Back));

   --  Time passed. Registered: the new instance has registered its role.
   procedure Tick (S : in out Switch; Now : Time; Registered : Boolean; Next : out Action)
   with Post => (if Waiting (S.State'Old) and then Now >= S.Deadline'Old
                 then Rank (S.State) > Rank (S.State'Old) and Next /= Nothing)
                and (if Next = Nothing then S = S'Old)
                and (if S.State'Old = Stopping_Old and then Now >= S.Deadline'Old and then S.Graceful
                     then S.State = Finished and Next = Answer and S.Result = SR.Stop_Failed)
                and (if S.State'Old = Checking and then Now >= S.Deadline'Old
                       and then (Registered or else not S.Must_Register'Old)
                     then S.State = Finished and Next = Answer and S.Result = SR.Switched);

   --  The answer was given (or the requester is gone): idle again.
   procedure Answered (S : in out Switch)
   with Pre => S.State = Finished, Post => S.State = Idle;

   --  When procmgr must look again (the deadline), for its receive.
   function Wake_At (S : Switch) return Unsigned_64 is
     (if Waiting (S.State) then S.Deadline else 0);
end Service_Switches;
