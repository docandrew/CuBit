------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  When a driver started before logstore asks devmgr for its logstore
--  publisher, and what it does with the answer (a pure state machine).
--
--  @description
--  Shared by drivers that capture bring-up lines until devmgr grants them a
--  publisher endpoint (xhci: request label 16#0228#). It is the state
--  machine intel-gpu introduced as Intel_GPU_Log_Grant (IdeaPad 46D4, no
--  intel-gpu line at all, 2026-10-24: a busy or refused answer, or a lost
--  reply, ended the grant for good). Here none of those is final:
--
--    * no request before logstore is registered; then one at a time;
--    * busy (devmgr still starting other drivers), refused (devmgr has no
--      logstore endpoint yet) and malformed answers are counted and retried
--      after Retry_Interval_Ms;
--    * after Availability_Deadline_Ms without a grant, the driver says so
--      once on serial (Notice) and keeps asking every Late_Retry_Interval_Ms:
--      a late logstore still gets the captured lines;
--    * once attached, one report line says how long it took and why.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Driver_Log_Grant with SPARK_Mode, Pure is

   subtype Milliseconds is Unsigned_64;

   --  Explicit limits (no hidden defaults): how long until the serial-only
   --  notice, and how often to ask before and after it.
   Availability_Deadline_Ms : constant Milliseconds := 60_000;
   Retry_Interval_Ms        : constant Milliseconds := 250;
   Late_Retry_Interval_Ms   : constant Milliseconds := 5_000;

   --  devmgr's answer labels to a supervisor request.
   Supervisor_Granted : constant Unsigned_64 := 16#F000#;
   Supervisor_Refused : constant Unsigned_64 := 16#F001#;
   Supervisor_Busy    : constant Unsigned_64 := 16#F002#;

   Event_Limit : constant := 1_000_000;
   type Event_Count is range 0 .. Event_Limit;

   type Phase is (Idle, Requested, Attached);

   type Reply_Kind is (Granted, Busy, Refused, Malformed);

   function Kind_Name (Kind : Reply_Kind) return String is
     (case Kind is
         when Granted   => "granted",
         when Busy      => "busy",
         when Refused   => "refused",
         when Malformed => "malformed");

   --  What devmgr's completion means. Completed: the kernel delivered an
   --  answer. Label/Length/Words_Zero: the reply's tag without IPC flags.
   --  Endpoint_Valid: the reply carried an endpoint this driver accepted.
   function Classify
     (Completed : Boolean; Label, Length : Unsigned_64;
      Words_Zero, Endpoint_Valid : Boolean) return Reply_Kind
   is (if not Completed then Refused
       elsif Label = Supervisor_Granted and then Length = 0 and then Words_Zero
       then (if Endpoint_Valid then Granted else Malformed)
       elsif Label = Supervisor_Busy then Busy
       elsif Label = Supervisor_Refused then Refused
       else Malformed)
   with Post =>
     (Classify'Result = Granted) =
       (Completed and then Label = Supervisor_Granted and then Length = 0
        and then Words_Zero and then Endpoint_Valid);

   type State is record
      Current           : Phase := Idle;
      Started           : Milliseconds := 0;
      Next_Request      : Milliseconds := 0;
      Requests          : Event_Count := 0;
      Submit_Failures   : Event_Count := 0;
      Busy_Replies      : Event_Count := 0;
      Refusals          : Event_Count := 0;
      Malformed_Replies : Event_Count := 0;
      Deadline_Passed   : Boolean := False;
      Notice_Due        : Boolean := False;
      Attached_At       : Milliseconds := 0;
      Report_Due        : Boolean := False;
   end record;

   function Initial (Now : Milliseconds) return State is
     ((Started => Now, Next_Request => Now, others => <>))
   with Post => Initial'Result.Current = Idle
     and then not Initial'Result.Deadline_Passed
     and then not Initial'Result.Notice_Due;

   function Elapsed (S : State; Now : Milliseconds) return Milliseconds is
     (if Now >= S.Started then Now - S.Started else 0);

   function Retry_Interval (S : State) return Milliseconds is
     (if S.Deadline_Passed then Late_Retry_Interval_Ms else Retry_Interval_Ms);

   --  Ask devmgr now? Logstore_Registered is read only when the rest holds
   --  (it is a system query).
   function Request_Due (S : State; Now : Milliseconds) return Boolean is
     (S.Current = Idle and then Now >= S.Next_Request);

   --  Logstore was not registered yet: look again after the retry interval.
   procedure Not_Registered (S : in out State; Now : Milliseconds)
   with Pre => S.Current = Idle,
     Post => S.Current = Idle and then S.Next_Request >= Now
       and then S.Deadline_Passed = S'Old.Deadline_Passed;

   --  The request was submitted (Accepted) or the submission failed.
   procedure Submitted (S : in out State; Now : Milliseconds; Accepted : Boolean)
   with Pre => Request_Due (S, Now),
     Post => (if Accepted then S.Current = Requested
              else S.Current = Idle and then S.Next_Request >= Now)
       and then S.Deadline_Passed = S'Old.Deadline_Passed;

   --  devmgr answered the outstanding request.
   procedure Answered (S : in out State; Now : Milliseconds; Kind : Reply_Kind)
   with Pre => S.Current = Requested,
     Post => (if Kind = Granted then S.Current = Attached and then S.Report_Due
              else S.Current = Idle and then S.Next_Request >= Now)
       and then S.Deadline_Passed = S'Old.Deadline_Passed;

   --  Passage of time: at the deadline without a grant, the notice is due,
   --  exactly once.
   procedure Observe_Time (S : in out State; Now : Milliseconds)
   with Post => S.Current = S'Old.Current
     and then (if S'Old.Deadline_Passed then S.Notice_Due = S'Old.Notice_Due
               and then S.Deadline_Passed)
     and then (if S.Notice_Due and then not S'Old.Notice_Due
               then not S'Old.Deadline_Passed and then S.Deadline_Passed);

   --  Consume the one-time notice / attachment report.
   procedure Take_Notice (S : in out State; Due : out Boolean)
   with Post => Due = S'Old.Notice_Due and then not S.Notice_Due
     and then S.Current = S'Old.Current
     and then S.Deadline_Passed = S'Old.Deadline_Passed;
   procedure Take_Report (S : in out State; Due : out Boolean)
   with Post => Due = S'Old.Report_Due and then not S.Report_Due
     and then S.Current = S'Old.Current;

   function Publishing (S : State) return Boolean is (S.Current = Attached);

   --  Should a stopping driver wait (boundedly) for its lines to go out?
   function Worth_Waiting (S : State) return Boolean is
     (S.Current = Attached or else not S.Deadline_Passed);

end Driver_Log_Grant;
