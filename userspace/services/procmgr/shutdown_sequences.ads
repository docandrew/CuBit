pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.System_Stops;

--  A root stop in progress (PWR-003 slice 1, CuBit.System_Stops): the
--  explicit machine procmgr drives from its service loop, as it drives a
--  service switch (Service_Switches). No I/O: each step returns the one
--  Action procmgr takes now, and procmgr reports what happened (an exit
--  and whether it was clean, the kernel's answer to Stop and to ending a
--  process, the filesystem's and devmgr's answers, the time).
--
--  The units are the processes procmgr stops itself, one at a time, each
--  within its own stop grace. The caller owns the array (procmgr allocates
--  it with exactly the units it has: no fixed capacity) and fills it once,
--  Pending or Kept, before Begin_Stop. The order is not the array's: the
--  next unit is always the least one still Pending (Precedes):
--
--    * by its declared stop stage (the plan entry's (stop-stage ...), the
--      Start_Child spec's Stage; a program another program launched is an
--      app): Apps, then Services, then, after the filesystem committed and
--      devmgr stopped its drivers, the Power_Tail;
--    * within a stage, the larger Key first: procmgr gives every launch
--      the next key, so a stage stops in reverse startup order and a
--      supervised child before its supervisor.
--
--  Each unit: Stop (a control event), then up to its grace for its exit;
--  past the grace it is ended (Forced) and given End_Deadline_Ms for its
--  exit to arrive (Force_Failed past that). Kept units (the requester, and
--  for a power-off the Power_Tail, which serves the firmware's last step)
--  are never stopped. After Apps and Services the filesystem commits
--  (Commit_Storage, answered by procmgr's own bounded call), devmgr stops
--  its own children (Ask_Drivers, then Poll_Drivers every Driver_Poll_Ms
--  until devmgr says finished or the drivers' deadline passes), then the
--  Power_Tail units still Pending. Then Answer.
--
--  Every waiting step has a deadline, and a step at or past it moves on,
--  so a stop always ends. Times are SYSCALL_GETTIME milliseconds.
package Shutdown_Sequences with SPARK_Mode is

   subtype Process_Word is Unsigned_64;
   No_Process : constant Process_Word := 0;

   --  CuBit.Child_Specs.Maximum_Grace_Ms: the longest stop grace anything
   --  declares.
   Maximum_Grace_Ms : constant := 600_000;
   subtype Grace_Ms is Unsigned_32 range 1 .. Maximum_Grace_Ms;
   --  How long an ended process has for its exit report to arrive.
   End_Deadline_Ms : constant := 2_000;
   --  How often devmgr is asked how its drivers' stop is going. Drivers
   --  that answer Stop end within a few milliseconds, and the stop waits
   --  for this poll to learn it: one call per 5 ms, a handful per stop.
   Driver_Poll_Ms : constant := 5;
   --  How late devmgr's report may be after its own end deadline (its
   --  loop wakes and answers) before procmgr goes on without it.
   Driver_Report_Margin_Ms : constant := 100;
   --  devmgr's own end deadline after the drivers' grace, plus that margin.
   Driver_Allowance_Ms : constant := End_Deadline_Ms + Driver_Report_Margin_Ms;

   --  The latest time a deadline is set from, so additions never wrap.
   Latest_Time : constant := Unsigned_64'Last - 2 * (Maximum_Grace_Ms + Driver_Allowance_Ms);
   subtype Time is Unsigned_64 range 0 .. Latest_Time;

   type Stop_Stage is (Apps, Services, Power_Tail);

   type Unit_State is
     (Pending,         --  not reached yet
      Kept,            --  never stopped (the requester, the firmware step)
      Stopping,        --  Stop sent; waiting for its exit until its grace
      Ending,          --  past its grace: ended, waiting for the exit
      Stopped_Clean,   --  exited with code 0 within its grace
      Exited_Failing,  --  exited failing (or was killed) within its grace
      Ended_Early,     --  ended on its own before its turn
      Forced,          --  ended after its grace
      Force_Failed);   --  could not be ended, or no exit by the end deadline
   subtype Decided is Unit_State range Stopped_Clean .. Force_Failed;
   subtype Open is Unit_State range Stopping .. Ending;

   type Unit is record
      Process : Process_Word := No_Process;
      Stage   : Stop_Stage := Apps;
      Key     : Unsigned_64 := 0;
      Grace   : Grace_Ms := Grace_Ms'First;
      State   : Unit_State := Pending;
      --  The kernel queued the Stop (else it is only waited for, then ended).
      Stop_Taken : Boolean := False;
      --  When Stop was sent and when the unit was decided.
      Stop_At, Decided_At : Unsigned_64 := 0;
   end record;

   type Unit_Array is array (Positive range <>) of Unit;

   --  Whether A stops before B.
   function Precedes (A, B : Unit) return Boolean is
     (A.Stage < B.Stage or else (A.Stage = B.Stage and then A.Key >= B.Key));

   --  The stages a phase stops: first Apps and Services; in the tail phase
   --  (after storage and devmgr's drivers), the Power_Tail.
   function In_Phase (U : Unit; Tail : Boolean) return Boolean is
     (if Tail then U.Stage = Power_Tail else U.Stage /= Power_Tail);

   --  The least Pending unit of the phase (Precedes), or 0.
   function Next_Pending (Units : Unit_Array; Length : Natural; Tail : Boolean) return Natural
   with Pre  => Units'First = 1 and then Length <= Units'Last,
        Post => Next_Pending'Result <= Length
                and then (if Next_Pending'Result = 0 then
                            (for all I in 1 .. Length =>
                               not (Units (I).State = Pending and then In_Phase (Units (I), Tail)))
                          else Units (Next_Pending'Result).State = Pending
                               and then In_Phase (Units (Next_Pending'Result), Tail)
                               and then (for all I in 1 .. Length =>
                                           (if Units (I).State = Pending
                                              and then In_Phase (Units (I), Tail)
                                            then Precedes (Units (Next_Pending'Result), Units (I)))));

   --  The unit of Process, or 0.
   function Find (Units : Unit_Array; Length : Natural; Process : Process_Word) return Natural
   with Pre  => Units'First = 1 and then Length <= Units'Last,
        Post => Find'Result <= Length
                and then (if Find'Result /= 0 then Units (Find'Result).Process = Process);

   function Count (Units : Unit_Array; Length : Natural; State : Unit_State) return Natural
   with Pre  => Units'First = 1 and then Length <= Units'Last,
        Post => Count'Result <= Length;

   ---------------------------------------------------------------------------
   --  The machine
   ---------------------------------------------------------------------------
   type Phase is
     (Idle,
      Stopping_Unit,     --  the current unit was sent Stop
      Ending_Unit,       --  the current unit was ended
      Committing,        --  the filesystem is committing
      Asking_Drivers,    --  devmgr is being asked to stop its drivers
      Stopping_Drivers,  --  devmgr stops them; polled
      Finished);
   subtype Middle is Phase range Committing .. Stopping_Drivers;

   type Action is
     (Nothing,
      Send_Stop,       --  send the current unit Stop
      End_Unit,        --  end the current unit
      Commit_Storage,  --  ask the filesystem to commit and detach
      Ask_Drivers,     --  ask devmgr to stop its drivers within Driver_Grace
      Poll_Drivers,    --  ask devmgr how far it is
      Answer);         --  answer the requester

   --  The filesystem's and devmgr's outcomes are the wire's.
   subtype Storage_Result is CuBit.System_Stops.Storage_Outcome;
   subtype Driver_Result is CuBit.System_Stops.Driver_Outcome;
   use type CuBit.System_Stops.Storage_Outcome, CuBit.System_Stops.Driver_Outcome;

   type Sequence is record
      State         : Phase := Idle;
      --  Stop_Apps_Operation: only Apps units, and no storage or driver
      --  stage; Finished once they are decided.
      Apps_Only     : Boolean := False;
      Current       : Natural := 0;
      Tail : Boolean := False;
      Deadline      : Unsigned_64 := 0;
      Next_Poll     : Unsigned_64 := 0;
      Driver_Grace  : Grace_Ms := Grace_Ms'First;
      Storage       : Storage_Result := CuBit.System_Stops.Not_Asked;
      Drivers       : Driver_Result := CuBit.System_Stops.Not_Asked;
   end record;

   --  The invariant procmgr's steps keep: only the current unit is open,
   --  in the state its phase says; every unit of a phase already passed is
   --  settled (Kept or Decided).
   function Settled (U : Unit) return Boolean is (U.State not in Pending | Open);

   function Consistent (S : Sequence; Units : Unit_Array; Length : Natural) return Boolean is
     (Units'First = 1 and then Length <= Units'Last
      and then (if S.State in Stopping_Unit | Ending_Unit then
                  S.Current in 1 .. Length
                  and then Units (S.Current).State = (if S.State = Stopping_Unit then Stopping else Ending)
                  and then In_Phase (Units (S.Current), S.Tail)
                  --  Stop order: the open unit precedes every unit of its
                  --  phase still Pending.
                  and then (for all I in 1 .. Length =>
                              (if Units (I).State = Pending and then In_Phase (Units (I), S.Tail)
                               then Precedes (Units (S.Current), Units (I)))))
      and then (for all I in 1 .. Length =>
                  (if Units (I).State in Open then S.State in Stopping_Unit | Ending_Unit
                                                   and then I = S.Current))
      --  Past the first phase, every Apps and Services unit is settled.
      and then (if S.State in Middle | Finished or else S.Tail
                then (for all I in 1 .. Length =>
                        (if Units (I).Stage /= Power_Tail then Settled (Units (I)))))
      and then (if S.State in Middle then not S.Tail)
      and then (if S.State = Finished then (for all I in 1 .. Length => Settled (Units (I))))
      and then (if S.Apps_Only then
                  not S.Tail
                  and then S.State not in Committing | Asking_Drivers | Stopping_Drivers
                  and then (for all I in 1 .. Length => Units (I).Stage = Apps)));

   --  No unit is open (a fresh array: Pending and Kept only).
   function Fresh (Units : Unit_Array; Length : Natural) return Boolean is
     (Units'First = 1 and then Length <= Units'Last
      and then (for all I in 1 .. Length => Units (I).State in Pending | Kept));

   --  Apps_Only: Units holds Apps units only (Stop_Apps_Operation).
   procedure Begin_Stop
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Driver_Grace : Grace_Ms; Apps_Only : Boolean; Now : Time; Next : out Action)
   with Pre  => S.State in Idle | Finished and then Fresh (Units, Length)
                and then (if Apps_Only then (for all I in 1 .. Length => Units (I).Stage = Apps)),
        Post => Consistent (S, Units, Length)
                and then S.Driver_Grace = Driver_Grace and then S.Apps_Only = Apps_Only
                and then (if Apps_Only then Next in Send_Stop | Answer
                          else Next in Send_Stop | Commit_Storage)
                and then (if Next = Send_Stop then
                            S.State = Stopping_Unit
                            and then S.Current = Next_Pending (Units'Old, Length, False)
                          elsif Next = Answer then S.State = Finished
                          else S.State = Committing);

   --  The kernel's answer to the Send_Stop just taken.
   procedure Stop_Sent (S : Sequence; Units : in out Unit_Array; Length : Natural; Taken : Boolean)
   with Pre  => Consistent (S, Units, Length) and then S.State = Stopping_Unit,
        Post => Consistent (S, Units, Length);

   --  A process ended (any process: those not among the units are ignored).
   --  Clean: it exited with code 0.
   procedure Exited
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Process : Process_Word; Clean : Boolean; Now : Time; Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then Process /= No_Process,
        Post => Consistent (S, Units, Length);

   --  The kernel's answer to the End_Unit just taken: Accepted False means
   --  the unit could not be ended (Force_Failed).
   procedure End_Sent
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Accepted : Boolean; Now : Time; Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then S.State = Ending_Unit,
        Post => Consistent (S, Units, Length);

   --  Time passed.
   procedure Tick
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Now : Time; Next : out Action)
   with Pre  => Consistent (S, Units, Length),
        Post => Consistent (S, Units, Length)
                --  A unit is ended only once its grace (from its Stop) ran out.
                and then (if Next = End_Unit then
                            S'Old.State = Stopping_Unit and then Now >= S'Old.Deadline
                            and then S.State = Ending_Unit and then S.Current = S'Old.Current);

   --  The filesystem's answer (Committed .. Storage_Timed_Out).
   procedure Storage_Done
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Result : Storage_Result; Now : Time; Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then S.State = Committing
                and then Result /= CuBit.System_Stops.Not_Asked,
        Post => Consistent (S, Units, Length) and then S.State = Asking_Drivers
                and then S.Storage = Result and then Next = Ask_Drivers;

   --  devmgr's receipt for Ask_Drivers. Refused: the Power_Tail next.
   procedure Drivers_Asked
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Accepted : Boolean; Now : Time;
      Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then S.State = Asking_Drivers,
        Post => Consistent (S, Units, Length)
                and then (if Accepted then S.State = Stopping_Drivers and then Next = Nothing
                          else S.Tail and then Next in Send_Stop | Answer
                               and then S.Drivers = CuBit.System_Stops.Drivers_Refused);

   --  devmgr's answer to Poll_Drivers. Done: none of its drivers runs; the
   --  Power_Tail next.
   procedure Drivers_Polled
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Done : Boolean; Now : Time;
      Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then S.State = Stopping_Drivers,
        Post => Consistent (S, Units, Length)
                and then (if Done then S.Tail and then Next in Send_Stop | Answer
                                       and then S.Drivers = CuBit.System_Stops.Drivers_Stopped);

   --  When procmgr must look again, for its receive (0: no deadline).
   function Wake_At (S : Sequence) return Unsigned_64 is
     (case S.State is
        when Stopping_Unit | Ending_Unit => S.Deadline,
        when Stopping_Drivers => Unsigned_64'Min (S.Deadline, S.Next_Poll),
        when others => 0);

   function Active (S : Sequence) return Boolean is (S.State /= Idle);

   --  Finished: every unit is Kept or decided.
   procedure Lemma_Finished_Settled (S : Sequence; Units : Unit_Array; Length : Natural)
   with Ghost,
        Pre  => Consistent (S, Units, Length) and then S.State = Finished,
        Post => (for all I in 1 .. Length => Units (I).State in Kept | Decided);
end Shutdown_Sequences;
