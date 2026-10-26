with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with Intel_GPU_Diagnostics;
with Intel_GPU_Log_Grant;
with Intel_GPU_Supervisor;
with Intel_GPU_Wait_Heartbeat;
--  Regression for the IdeaPad 46D4 (2026-10-24): not one intel-gpu line
--  reached logstore. Before the fix (a) a busy or refused grant answer
--  stopped the publisher for good, (b) bring-up waits polled completions
--  directly and dropped the grant reply, (c) lines went out only from the
--  request loop. Hosted: scripted completions, clock and logstore.
procedure Log_Grant_Tests is
   package G renames Intel_GPU_Log_Grant;
   package D renames Intel_GPU_Diagnostics;
   package S renames Intel_GPU_Supervisor;
   package HB renames Intel_GPU_Wait_Heartbeat;
   package Logs renames CuBit.Log_Records;
   use type G.Phase;
   use type G.Event_Count;
   use type G.Reply_Kind;
   use type HB.Line_Kind;
   use type Logs.Severity;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   function Starts_With (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then
      Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);

   Grant_Token : constant Unsigned_64 := 16#4947_0001#;
   Request_Label : constant Unsigned_64 := 16#0229#;
   function Answer (Label : Unsigned_64; Status : Unsigned_64 := COMPLETION_OK)
     return CompletionEntry is
     ((token => Grant_Token, status => Status,
       msg => (tag => (label => Label, others => <>), words => [0, 0, 0, 0])));
   Other : constant CompletionEntry :=
     (token => 77, status => COMPLETION_OK,
      msg => (tag => (label => 16#F000#, others => <>), words => [1, 2, 3, 4]));
   Other_2 : constant CompletionEntry :=
     (token => 78, status => COMPLETION_OK,
      msg => (tag => (label => 16#F000#, others => <>), words => [5, 6, 7, 8]));
   R : aliased CompletionEntry;

   procedure Pure_State_Machine is
      St : G.State := G.Initial (1_000);
      Due : Boolean;
   begin
      Check (G.Request_Due (St, 1_000), "a request is due at start");
      G.Not_Registered (St, 1_000);
      Check (not G.Request_Due (St, 1_000 + G.Retry_Interval_Ms - 1), "no logstore: wait the interval");
      Check (G.Request_Due (St, 1_000 + G.Retry_Interval_Ms), "then look again");
      G.Submitted (St, 1_250, Accepted => False);
      Check (St.Current = G.Idle and St.Submit_Failures = 1, "failed submit: retry later");
      G.Submitted (St, 1_500, Accepted => True);
      Check (St.Current = G.Requested and not G.Request_Due (St, 99_999), "one request at a time");
      G.Answered (St, 1_600, G.Busy);
      Check (St.Current = G.Idle and St.Busy_Replies = 1 and
             St.Next_Request = 1_600 + G.Retry_Interval_Ms, "busy: retried, not final");
      Check (G.Classify (True, G.Supervisor_Granted, 0, True, True) = G.Granted, "grant");
      Check (G.Classify (True, G.Supervisor_Granted, 0, True, False) = G.Malformed,
             "grant without endpoint");
      Check (G.Classify (True, G.Supervisor_Granted, 1, True, True) = G.Malformed, "grant, wrong length");
      Check (G.Classify (True, G.Supervisor_Busy, 0, True, False) = G.Busy, "busy");
      Check (G.Classify (True, G.Supervisor_Refused, 0, True, False) = G.Refused, "refused");
      Check (G.Classify (False, G.Supervisor_Granted, 0, True, True) = G.Refused, "IPC failure");
      Check (G.Classify (True, 0, 0, True, False) = G.Malformed, "null reply");
      --  Deadline: the notice exactly once, retries slow down but go on.
      G.Observe_Time (St, 1_000 + G.Availability_Deadline_Ms - 1);
      Check (not St.Notice_Due, "no notice before the deadline");
      G.Observe_Time (St, 1_000 + G.Availability_Deadline_Ms);
      G.Take_Notice (St, Due);
      Check (Due and not G.Worth_Waiting (St), "notice at the deadline; exit need not wait");
      G.Observe_Time (St, 1_000 + 3 * G.Availability_Deadline_Ms);
      G.Take_Notice (St, Due);
      Check (not Due, "notice only once");
      Check (G.Retry_Interval (St) = G.Late_Retry_Interval_Ms, "slower after the deadline");
      --  A clock that steps back does not count as elapsed time.
      Check (G.Elapsed (G.Initial (500), 100) = 0, "backward clock");
   end Pure_State_Machine;

   procedure Heartbeat_Bound is
      M : HB.Monitor;
      Line : HB.Line_Kind;
      Now : HB.Microseconds := 10_000_000;
      Lines, Entries : Natural := 0;
      Last_Line_At : HB.Microseconds := 0;
   begin
      --  A 60 s hang polled every 10 us: one entry, then at most one line per
      --  period, at most Lines_Per_Wait in all.
      for Poll in 1 .. 6_000_000 loop
         HB.Pulse (M, Now, Line);
         if Line /= HB.None then
            if Line = HB.Entry_Line then
               Entries := Entries + 1;
               Check (HB.Elapsed (M, Now) >= HB.Slow_Threshold_Us, "entry after the threshold");
            else
               Check (Now - Last_Line_At >= HB.Period_Us, "progress spaced by the period");
            end if;
            Lines := Lines + 1;
            Last_Line_At := Now;
         end if;
         Now := Now + 10;
      end loop;
      Check (Entries = 1, "one entry line per wait");
      Check (Lines = HB.Lines_Per_Wait, "line bound per wait");
      --  A fast wait (under the threshold) says nothing.
      Now := Now + HB.Wait_Gap_Us;
      Lines := 0;
      for Poll in 1 .. 1_000 loop
         HB.Pulse (M, Now, Line);
         if Line /= HB.None then Lines := Lines + 1; end if;
         Now := Now + 10;
      end loop;
      Check (Lines = 0, "a 10 ms wait is silent");
      --  A pause longer than the gap starts a new wait (its own entry line).
      Now := Now + HB.Wait_Gap_Us;
      Lines := 0;
      for Poll in 1 .. 3_000 loop
         HB.Pulse (M, Now, Line);
         if Line = HB.Entry_Line then Lines := Lines + 1; end if;
         Now := Now + 10;
      end loop;
      Check (Lines = 1, "next slow wait gets its own entry line");
   end Heartbeat_Bound;

   procedure Glue_Story is
      Serial_Before : Natural;
   begin
      --  Bring-up begins before logstore exists: lines go to serial, no request.
      Now := 1_000;
      D.Capture ("intel-gpu: startup step=begin");
      Check (S.Submissions = 0 and Serial_Lines = 1, "serial at once, no request before logstore");
      Check (D.Pending, "captured, waiting for logstore");
      --  Logstore registers; the next line asks (label 16#0229#).
      Logstore := 42;
      Now := Now + G.Retry_Interval_Ms;
      D.Capture ("intel-gpu: stage identify ok device=8086:46D4");
      Check (S.Submissions = 1 and S.Last_Label = Request_Label, "asked once logstore is registered");
      --  A bring-up wait's completion and devmgr's busy answer arrive; the
      --  wait polls through Poll_Driver: it gets its own completion, and the
      --  busy answer is handled, not dropped and not final.
      Inject (Other); Inject (Answer (G.Supervisor_Busy));
      Check (D.Poll_Driver (R'Address) = 1 and R = Other, "caller's completion returned");
      Check (D.Poll_Driver (R'Address) = 0, "busy answer consumed");
      Now := Now + G.Retry_Interval_Ms;
      D.Tick;
      Check (S.Submissions = 2, "busy: asked again");
      --  Refused (devmgr has no logstore endpoint yet): asked again.
      Inject (Answer (G.Supervisor_Refused));
      D.Tick;   --  Tick itself takes the answer.
      Now := Now + G.Retry_Interval_Ms;
      D.Tick;
      Check (S.Submissions = 3, "refused: asked again");
      --  The reply is late; a hardware wait's Tick (heartbeat) polls while
      --  another completion is queued ahead of the grant: the completion is
      --  kept for the wait, in order.
      Inject (Other); Inject (Other_2);
      Now := Now + G.Retry_Interval_Ms;
      D.Tick;
      Check (Queued = 0, "Tick polled while the grant was outstanding");
      Check (D.Poll_Driver (R'Address) = 1 and R = Other, "kept completion first");
      Check (D.Poll_Driver (R'Address) = 1 and R = Other_2, "kept completions in order");
      Check (CuBit.Logging.Emissions = 0, "nothing published before the grant");
      --  The deadline passes without a grant: one serial notice.
      Now := 1_000 + G.Availability_Deadline_Ms;
      Serial_Before := Serial_Lines;
      D.Tick;
      Check (Serial_Lines = Serial_Before + 1 and then
             Starts_With (Serial_Text, "intel-gpu: logstore not available after"), "serial-only notice");
      Check (not D.Pending, "past the deadline unattached: exit does not wait");
      D.Tick;
      Check (Serial_Lines = Serial_Before + 1, "notice once");
      --  Logstore arrives late: the grant (with endpoint) attaches, the
      --  attachment report and every captured line go out.
      S.Carries_Endpoint := True;
      Inject (Answer (G.Supervisor_Granted));
      Check (D.Poll_Driver (R'Address) = 0, "grant consumed");
      Check (CuBit.Logging.Attachments = 1, "attached");
      D.Tick;
      Check (CuBit.Logging.Emissions = 1 + 2 + 1, "report + both lines + capture summary");
      --  Attached: a new stage line goes out immediately (no batching).
      D.Capture ("intel-gpu: stage guc waiting elapsed-ms=20", Logs.Warning);
      Check (CuBit.Logging.Emissions = 5 + 1, "line published as captured");
      Check (CuBit.Logging.Last_Value.Level = Logs.Debug, "followed by its summary (no loss)");
      Check (not D.Pending, "flushed: a stopping driver has nothing to wait for");
      Check (S.Submissions = 3, "no request after attachment");
   end Glue_Story;
begin
   Pure_State_Machine;
   Heartbeat_Bound;
   Glue_Story;
   Ada.Text_IO.Put_Line ("log_grant PASS:" & Natural'Image (Checks) &
     " checks (grant before ready, busy/refused retried, kept completions, late logstore notice once, flush per line, heartbeat bound)");
end Log_Grant_Tests;
