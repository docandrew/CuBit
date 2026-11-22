--  The host side of streams (docs/ccl-streams.md, phase 1): the session's
--  stream table, the timer source, and a session that binds, reads and
--  lets go of streams. The clock is the test's, so every tick is exact.
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Replay;
with CCL.Host_Values;
with CCL.Interfaces.Timer;
with CCL.Language;
with CCL.Objects;
with CCL.Sessions;
with CCL.Streams;
with CCL_Stream_Table;
with CCL.VM;

procedure Session_Tests is
   package S renames CCL.Streams;
   package T renames CCL_Stream_Table;
   use type S.Handle;
   use type S.View_Status;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Descriptor_Digest;
   use type CCL.Host_Values.Value_Kind;

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function View (Item : T.Table; Handle : S.Handle; Kind : S.View_Kind; Count : S.Window_Length := 1)
     return S.View_Reply
   is
      Reply : S.View_Reply;
   begin
      T.Read (Item, (Stream => Handle, View => Kind, Count => Count), Reply);
      return Reply;
   end View;
   function Cell (Reply : S.View_Reply; Index : Positive) return Integer_64 is
     (CCL.Objects.Integer_Of (Reply.Elements.Cells (Index)));

   procedure Table_Tests is
      Table : T.Table;
      Ticks, Other, Reopened : S.Handle;
      Changed : Boolean;
      Reply : S.View_Reply;
   begin
      T.Open_Timer (Table, 100, 1_000, Ticks);
      Check (Ticks /= S.No_Handle and then T.Open_Count (Table) = 1, "a timer opens a stream");
      Check (T.Next_Due (Table) = 1_100, "its first tick is one period away");
      Check (View (Table, Ticks, S.Latest_View).Status = S.Stream_Empty, "nothing before the first tick");
      Check (View (Table, Ticks, S.Window_View, 5).Status = S.View_Answered and then
             Cell (View (Table, Ticks, S.Window_View, 5), 1) = 0, "an empty window");
      T.Pump (Table, 1_099, Changed);
      Check (not Changed, "not due yet");
      T.Pump (Table, 1_350, Changed);
      Check (Changed, "three ticks due");
      Reply := View (Table, Ticks, S.Latest_View);
      Check (Reply.Status = S.View_Answered and then Cell (Reply, 1) = 1_300, "latest is the newest tick");
      Reply := View (Table, Ticks, S.Window_View, 2);
      Check (Cell (Reply, 1) = 2 and then Cell (Reply, 2) = 1_200 and then Cell (Reply, 3) = 1_300,
             "a window is the newest n, oldest first");
      Check (View (Table, Ticks, S.Arrived_View).Total = 3 and then
             View (Table, Ticks, S.Lost_View).Total = 0, "arrivals counted, none lost");

      --  A full ring loses its oldest. A live host pumps every frame.
      for Step in 1 .. 300 loop
         T.Pump (Table, Unsigned_64 (1_300 + Step * 100), Changed);
      end loop;
      Check (View (Table, Ticks, S.Arrived_View).Total = 303, "every tick arrived");
      Check (View (Table, Ticks, S.Lost_View).Total = 303 - Integer_64 (T.CAPACITY), "the ring keeps its capacity");
      Reply := View (Table, Ticks, S.Window_View, S.Maximum_Window);
      Check (Cell (Reply, 1) = S.Maximum_Window and then
             Cell (Reply, 2) = 1_300 + 300 * 100 - Integer_64 (S.Maximum_Window - 1) * 100 and then
             Cell (Reply, S.Maximum_Window + 1) = 1_300 + 300 * 100,
             "the widest window is the newest of the ring, oldest first");

      --  Suspended for an hour: skip ahead, do not replay.
      T.Pump (Table, 1_000_000_000, Changed);
      Reply := View (Table, Ticks, S.Latest_View);
      Check (Cell (Reply, 1) = 1_000_000_000, "a late timer catches up to now");
      Check (View (Table, Ticks, S.Arrived_View).Total = 303 + Integer_64 (T.CAPACITY), "and delivers at most a ring");

      --  Handles name a generation: a closed stream's handle stays dead.
      T.Open_Timer (Table, 10, 0, Other);
      T.Close (Table, Ticks);
      Check (View (Table, Ticks, S.Latest_View).Status = S.No_Such_Stream, "a closed stream reads nothing");
      T.Open_Timer (Table, 50, 0, Reopened);
      Check (Reopened /= Ticks and then View (Table, Ticks, S.Arrived_View).Status = S.No_Such_Stream,
             "reopening a slot gives a new handle; the old one stays closed");
      Check (View (Table, 0, S.Arrived_View).Status = S.No_Such_Stream and then
             View (Table, S.Maximum_Handle, S.Arrived_View).Status = S.No_Such_Stream,
             "no stream behind invented handles");

      declare
         function Keep_Other (Handle : S.Handle) return Boolean is (Handle = Other);
         procedure Retain is new T.Retain (Keep_Other);
      begin
         Retain (Table);
         Check (T.Open_Count (Table) = 1 and then
                View (Table, Other, S.Arrived_View).Status = S.View_Answered and then
                View (Table, Reopened, S.Arrived_View).Status = S.No_Such_Stream,
                "retain closes what nothing holds");
      end;

      T.Clear (Table);
      Check (T.Open_Count (Table) = 0 and then T.Next_Due (Table) = Unsigned_64'Last, "clear closes all");
      for I in 1 .. T.MAX_STREAMS loop
         T.Open_Timer (Table, 10, 0, Other);
      end loop;
      T.Open_Timer (Table, 10, 0, Other);
      Check (Other = S.No_Handle, "a full table opens nothing");
   end Table_Tests;

   --  Outlet streams (docs/ccl-launch-parameters.md): fed by the host from a
   --  launched program's connectors, text a line at a time; a one-shot ends.
   procedure Port_Tests is
      Table : T.Table;
      Lines, Status : S.Handle;
      Pushed, Changed : Boolean;
      Reply : S.View_Reply;
      --  A text cell: its offset and length in the image's text.
      function Text (Reply : S.View_Reply; Index : Positive) return String is
        (Reply.Elements.Text
           (Natural (Reply.Elements.Cells (Index).First) + 1 ..
            Natural (Reply.Elements.Cells (Index).First + Reply.Elements.Cells (Index).Second)));
   begin
      T.Open_Outlet (Table, T.Text_Elements, Lines);
      T.Open_Outlet (Table, T.Integer_Elements, Status);
      Check (Lines /= S.No_Handle and then Status /= S.No_Handle and then T.Open_Count (Table) = 2,
             "connectors open streams");
      Check (T.Next_Due (Table) = Unsigned_64'Last, "a connector is not timed");
      Check (View (Table, Lines, S.Latest_View).Status = S.Stream_Empty, "no line yet");
      T.Push_Text (Table, Lines, "hello.s: Assembler messages:", Pushed);
      T.Push_Text (Table, Lines, "hello.s:3: Error: no such instruction", Pushed);
      Check (Pushed, "lines arrive");
      T.Pump (Table, 1_000, Changed);
      Check (not Changed, "pumping time leaves connectors alone");
      Reply := View (Table, Lines, S.Latest_View);
      Check (Reply.Status = S.View_Answered and then Text (Reply, 1) = "hello.s:3: Error: no such instruction",
             "latest is the newest line");
      Reply := View (Table, Lines, S.Window_View, 5);
      Check (Cell (Reply, 1) = 2 and then Text (Reply, 2) = "hello.s: Assembler messages:"
             and then Text (Reply, 3) = "hello.s:3: Error: no such instruction",
             "a window of lines, oldest first");
      T.Push_Integer (Table, Lines, 1, Pushed);
      Check (not Pushed, "a text connector takes no Integer");
      T.Push_Text (Table, Status, "x", Pushed);
      Check (not Pushed, "an Integer connector takes no text");
      T.Push_Integer (Table, Status, 0, Pushed);
      T.End_Stream (Table, Status);
      Check (Pushed and then T.Ended (Table, Status)
             and then Cell (View (Table, Status, S.Latest_View), 1) = 0, "a one-shot value, then ended");
      T.Push_Integer (Table, Status, 1, Pushed);
      Check (not Pushed, "nothing after the end");
      for I in 1 .. T.TEXT_HISTORY + 10 loop
         T.Push_Text (Table, Lines, [1 .. 300 => 'x'], Pushed);
      end loop;
      Check (View (Table, Lines, S.Arrived_View).Total = Integer_64 (T.TEXT_HISTORY + 12)
             and then View (Table, Lines, S.Lost_View).Total = 12, "a full history counts its losses");
      Check (Text (View (Table, Lines, S.Latest_View), 1)'Length = T.MAX_LINE, "a long line is cut");
   end Port_Tests;

   --  Tasks: pending until completed once; the result is read by Wait_View.
   procedure Task_Tests is
      Table : T.Table;
      Job, Stream : S.Handle;
      Jobs : array (1 .. T.MAX_RESULTS + 1) of S.Handle;
      Result : CCL.Objects.Image;
      Completed, Pushed : Boolean;
   begin
      Result.Cells (1) := CCL.Objects.Integer_Cell (42);
      Result.Used_Cells := 1;
      T.Open_Task (Table, Job);
      Check (Job /= S.No_Handle and then not T.Task_Done (Table, Job), "a task opens pending");
      Check (View (Table, Job, S.Wait_View).Status = S.Stream_Empty, "a pending task has no result");
      T.Push_Integer (Table, Job, 1, Pushed);
      Check (not Pushed, "a task takes no stream elements");
      T.Complete_Task (Table, Job, Result, Completed);
      Check (Completed and then T.Task_Done (Table, Job), "a task completes");
      Check (View (Table, Job, S.Wait_View).Status = S.View_Answered and then
             Cell (View (Table, Job, S.Wait_View), 1) = 42, "wait reads its result");
      T.Complete_Task (Table, Job, Result, Completed);
      Check (not Completed, "a task completes once");
      T.Open_Outlet (Table, T.Integer_Elements, Stream);
      T.Complete_Task (Table, Stream, Result, Completed);
      Check (not Completed and then View (Table, Stream, S.Wait_View).Status = S.No_Such_Stream,
             "a stream is no task: waiting on it names nothing");
      Check (View (Table, Job, S.Latest_View).Status = S.No_Such_Stream and then
             View (Table, Job, S.Arrived_View).Status = S.No_Such_Stream,
             "a task is no stream: its views name nothing");
      T.Close (Table, Job);
      T.Close (Table, Stream);
      for I in Jobs'Range loop
         T.Open_Task (Table, Jobs (I));
         T.Complete_Task (Table, Jobs (I), Result, Completed);
         Check (Completed = (I <= T.MAX_RESULTS),
                (if I <= T.MAX_RESULTS then "a closed task's result slot is reused"
                 else "results beyond MAX_RESULTS wait"));
      end loop;
      T.Close (Table, Jobs (1));
      T.Complete_Task (Table, Jobs (Jobs'Last), Result, Completed);
      Check (Completed and then Cell (View (Table, Jobs (Jobs'Last), S.Wait_View), 1) = 42,
             "closing a task frees its result");
   end Task_Tests;

   --  A session with timer.every, reading through the same table.
   Clock_Now : Unsigned_64 := 5_000;
   Table : T.Table;
   TIMER_BINDING : constant := 7;
   --  job.start: a Task<Integer> the test completes itself, counting starts.
   JOB_BINDING : constant := 8;
   JOB_DIGEST : constant CCL.Catalog.Descriptor_Digest := [1, 2, 3, 4];
   Job_Starts : Natural := 0;
   Last_Job : S.Handle := S.No_Handle;
   type Host is null record;
   procedure Invoke
     (Context : in out Host; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
      Handle : S.Handle := S.No_Handle;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if Binding = TIMER_BINDING and then Argument.Kind = CCL.Host_Values.Integer_Value and then
        Argument.Integer in T.MIN_PERIOD_MS .. T.MAX_PERIOD_MS
      then
         T.Open_Timer (Table, T.Period_Ms (Argument.Integer), Clock_Now, Handle);
         Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Handle)),
                   Success => Handle /= S.No_Handle, Why => <>);
      elsif Binding = JOB_BINDING then
         T.Open_Task (Table, Handle);
         Job_Starts := Job_Starts + 1;
         Last_Job := Handle;
         Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Handle)),
                   Success => Handle /= S.No_Handle, Why => <>);
      end if;
   end Invoke;
   procedure Read (Context : in out Host; Request : S.View_Request; Reply : in out S.View_Reply) is
      pragma Unreferenced (Context);
   begin
      T.Read (Table, Request, Reply);
   end Read;
   procedure Submit is new CCL.Sessions.Submit_With_Values (Host, Invoke, Read_Stream => Read);
   package Replay is new CCL.Host_Replay (Host, Invoke, Read);
   procedure Submit_Logged is new CCL.Sessions.Submit_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);
   procedure Resume is new CCL.Sessions.Resume_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);

   --  Publish job.start, a host operation whose result is a Task<Integer>.
   procedure Publish_Job (Catalog : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error) is
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Define_Interface ("job", 1, 0, JOB_DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation
           ("start", 1,
            (Argument => CCL.Host_Values.Integer_Value, Result => CCL.Host_Values.Integer_Value,
             Result_Task => True, Authority => CCL.VM.Observe_Authority, others => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Catalog, Descriptor, Error);
      end if;
   end Publish_Job;

   --  Awaiting a pending task in the interpreter: the entry stops, and once
   --  the task completes it runs again with its host calls answered from
   --  the log, so the job starts once.
   procedure Wait_Tests is
      Session : CCL.Sessions.Session;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Error : CCL.Catalog.Catalog_Error;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found, Resumed, Completed : Boolean;
      Grant : CCL.Catalog.Grant_Result;
      Context : Replay.Context;
      Outcome : CCL.Language.Interpretation_Result;
      Result : CCL.Objects.Image;
      procedure Enter (Source : String) is
      begin
         Replay.Clear (Context);
         Submit_Logged (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      end Enter;
      procedure Again is
      begin
         Replay.Rewind (Context.Calls);
         Resume (Session, CCL.Sessions.Length (Session), CCL.Sessions.Default_Fuel, Grants,
                 Context, Outcome, Resumed);
      end Again;
      procedure Finish_Job (Value : Integer_64) is
      begin
         Result := (others => <>);
         --  As a host builds it: under its schema's key (the table keeps a
         --  local image, which the reader checks against the type it waits for).
         Result.Schema := [others => 16#C0DE#];
         Result.Cells (1) := CCL.Objects.Integer_Cell (Value);
         Result.Used_Cells := 1;
         T.Complete_Task (Table, Last_Job, Result, Completed);
      end Finish_Job;
      function Shown return String is (CCL.Sessions.Result_Image (Outcome));
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      Publish_Job (Catalog, Error);
      CCL.Catalog.Resolve (Catalog, "job.start", Resolved, Found);
      Check (Error = CCL.Catalog.Catalog_Valid and then Found and then Resolved.Import.Result_Task,
             "job.start is published as a task source");
      CCL.Catalog.Install (Grants, Resolved, JOB_BINDING, Grant);
      CCL.Sessions.Initialize (Session, Catalog);

      Enter ("(job.start 1)");
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Is_Task and then
             CCL.Sessions.Result_Type_Image (Outcome) = "Task<Integer>", "a task source returns a task: " & Shown);
      Enter ("(+ 1 (wait (job.start 1)))");
      Check (Outcome.Status = CCL.Language.Waiting_On_Task and then
             S."=" (Outcome.Waited_On, Last_Job) and then Replay.Length (Context.Calls) = 1,
             "waiting on a pending task stops the entry, its call logged");
      Again;
      Check (Resumed and then Outcome.Status = CCL.Language.Waiting_On_Task and then Job_Starts = 2,
             "still pending: it stops again, starting nothing new");
      Finish_Job (42);
      Again;
      Check (Resumed and then Outcome.Status = CCL.Language.Succeeded and then
             Outcome.Result_Value.Integer = 43 and then Job_Starts = 2,
             "once complete the entry finishes, the job started once: " & Shown);
      Again;
      Check (not Resumed, "a finished entry does not resume");

      Enter ("(define answer (wait (job.start 2)))");
      Check (Outcome.Status = CCL.Language.Waiting_On_Task, "a definition can wait");
      Finish_Job (7);
      Again;
      Enter ("(* answer 6)");
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 42,
             "resuming binds the name: " & Shown);
      Check (Job_Starts = 3, "each waiting entry started its job once");

      --  Looking at a task never waits: Running, then Done with its result.
      Enter ("(define job (job.start 3))");
      Check (Shown = "Task<Integer>: Running", "a pending task shows Running: " & Shown);
      Enter ("job");
      Check (Shown = "Task<Integer>: Running", "and still does when looked at again: " & Shown);
      Finish_Job (9);
      Enter ("job");
      Check (Shown = "Task<Integer>: Done 9", "a finished task shows Done and its result: " & Shown);
      Enter ("(+ (wait job) 1)");
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 10,
             "waiting on a finished task does not stop");
   end Wait_Tests;

   procedure Session_Tests_Run is
      Session : CCL.Sessions.Session;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Error : CCL.Catalog.Catalog_Error;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
      Grant : CCL.Catalog.Grant_Result;
      Context : Host;
      Outcome : CCL.Language.Interpretation_Result;
      Changed : Boolean;
      procedure Enter (Source : String) is
      begin
         Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      end Enter;
      function Shown return String is (CCL.Sessions.Result_Image (Outcome));
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      CCL.Interfaces.Timer.Publish (Catalog, Error);
      CCL.Interfaces.Timer.Resolve_Every (Catalog, Resolved, Found);
      Check (Error = CCL.Catalog.Catalog_Valid and then Found and then Resolved.Import.Result_Stream and then
             Resolved.Interface_Digest = CCL.Interfaces.Timer.DESCRIPTOR_DIGEST,
             "timer.every is published as a stream source");
      CCL.Catalog.Install (Grants, Resolved, TIMER_BINDING, Grant);
      Check (Grant = CCL.Catalog.Grant_Added, "and granted");
      CCL.Sessions.Initialize (Session, Catalog);

      Enter ("(define ticks (timer.every 100))");
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Stream, "a source returns a stream");
      Check (CCL.Sessions.Result_Type_Image (Outcome) = "Stream<Integer>", "its type shows as Stream<Integer>");
      Check (CCL.Sessions.Result_Value_Image (Outcome) (1 .. 8) = "#<stream", "it shows as a description");
      Check (CCL.Sessions.Holds_Stream (Session, Outcome.Stream), "the session holds it");
      Enter ("(latest ticks)");
      Check (Outcome.Status = CCL.Language.Stream_Empty, "nothing has arrived yet: " & Shown);
      Clock_Now := 5_250;
      T.Pump (Table, Clock_Now, Changed);
      Enter ("(latest ticks)");
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 5_200,
             "the binding reads the live stream: " & Shown);
      Enter ("(window 5 ticks)");
      Check (Shown = "List<Integer>: [5100, 5200]", "a window of what arrived: " & Shown);
      Enter ("(- (latest ticks) (sum (first 1 (window 2 ticks))))");
      Check (Outcome.Result_Value.Integer = 100, "views compose with ordinary code");
      Enter ("(+ ticks 1)");
      Check (Outcome.Status = CCL.Language.Type_Check_Failed, "the handle is not a number");
      Enter ("(timer.every 5)");
      Check (Outcome.Status = CCL.Language.Host_Call_Failed, "a period below 10 ms is refused");
      Enter ("(define ticks 3)");
      Check (not CCL.Sessions.Holds_Stream (Session, 1), "rebinding the name lets go of the stream");
   end Session_Tests_Run;
begin
   Table_Tests;
   Port_Tests;
   Task_Tests;
   Session_Tests_Run;
   Wait_Tests;
   if Failures = 0 then
      Put_Line ("ccl-streams sessions: all passed");
   else
      Put_Line ("ccl-streams sessions:" & Natural'Image (Failures) & " failed");
      Set_Exit_Status (Failure);
   end if;
end Session_Tests;
