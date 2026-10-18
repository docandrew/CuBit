------------------------------------------------------------------------------
--  ipctest-client's channel tests (CAP-002 step 3): opens within and past
--  the server's limits, a ping-pong through doorbells (timed), a pipelined
--  batch, a wait that times out at its explicit deadline and is answered
--  later, and a close the server sees end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Protocol;
with CuBit.Deadlines;
with CuBit.Deadlines.Clock;
with CuBit.Messages; use CuBit.Messages;
with CuBit.System_Facts;
with System.Machine_Code;
with IPCtest_Channel;
with IPCtest_Channel_Clients;

procedure Channel_Tests (Server : CuBit.Handles.Endpoint; Ok : in out Boolean) is
   use ASCII;
   package IC renames IPCtest_Channel;
   package Shared renames IPCtest_Channel_Clients;
   package Clients renames Shared.Clients;
   use type Clients.Open_Outcome, Clients.Submit_Result, Clients.Wait_Outcome;

   Open_Ms     : constant := 2_000;
   Answer_Ms   : constant := 2_000;
   Silent_Ms   : constant := 50;
   Silent_Latest_Ms : constant := 250;
   Ping_Count  : constant := 20_000;
   Batch_Count : constant := 16;

   A : Clients.Client renames Shared.Pool (1);
   B : Clients.Client renames Shared.Pool (2);
   C : Clients.Client;
   Outcome : Clients.Open_Outcome;
   Refusal : CuBit.Channel_Protocol.Open_Refusal;

   procedure Fail (Reason : String) is
   begin
      debugPrint ("TEST: FAIL async-ipc channel-" & Reason & LF);
      Ok := False;
   end Fail;

   function Image (V : Unsigned_64) return String is
      S : constant String := V'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   --  One request on Ch and its answer, checked.
   function Round_Trip (Ch : in out Clients.Client; Value : Unsigned_64) return Boolean is
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Answer : IC.Result;
      Waited : Clients.Wait_Outcome;
   begin
      Clients.Submit (Ch, (Value => Value, Kind => IC.Echo), Number, Submitted);
      if Submitted /= Clients.Queued then
         return False;
      end if;
      Clients.Wait (Ch, Number, CuBit.Deadlines.Clock.After (Answer_Ms), Answer, Waited);
      return Waited = Clients.Answered and then Answer.Value = Value * IC.Multiplier
        and then Answer.Echoed = Value;
   end Round_Trip;

   function TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      System.Machine_Code.Asm ("rdtsc", Outputs => (Unsigned_32'Asm_Output ("=a", Low),
                                                    Unsigned_32'Asm_Output ("=d", High)),
                               Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end TSC;

   function Thread_Calls return Unsigned_64 is
      Value : CuBit.System_Facts.SP.Statistics;
      Read : Boolean;
   begin
      CuBit.System_Facts.Read_Statistics (Value, Read);
      return (if Read then Value.Thread_System_Calls else 0);
   end Thread_Calls;

   --  One request through Call (the ring and the wait in one system call).
   --  Phase_Deadline: computed once per phase (this test profile publishes
   --  no clock, so each computation would be a GETTIME).
   function Call_Trip (Ch : in out Clients.Client; Value : Unsigned_64;
                       Phase_Deadline : CuBit.Deadlines.Deadline) return Boolean is
      Answer : IC.Result;
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Outcome : Clients.Wait_Outcome;
   begin
      Clients.Submit (Ch, (Value => Value, Kind => IC.Echo), Number, Submitted);
      if Submitted /= Clients.Queued then
         return False;
      end if;
      Clients.Wait (Ch, Number, Phase_Deadline, Answer, Outcome);
      return Outcome = Clients.Answered and then Answer.Value = Value * IC.Multiplier;
   end Call_Trip;

   Msg : Message;
   Calls_Before, Client_Calls, Server_Before, Server_Calls : Unsigned_64;
   Phase_Ms : constant := 30_000;
   Calibration_Ms : constant := 100;
   Bench_Batch   : constant := 32;
   Bench_Batches : constant := 500;
   --  Requests the probes and the batch bench add to the server's count.
   Probe_Requests : constant := 3 * IC.Samples + Bench_Batch * Bench_Batches;
   Phase : CuBit.Deadlines.Deadline;

   function Server_Count return Unsigned_64 is
      M : Message := NULL_MESSAGE;
      T : MessageTag;
      pragma Unreferenced (T);
   begin
      M.tag := (label => IC.OP_CHANNEL_COUNT, length => 0, protocol => 0, ipc => <>, reserved => 0);
      T := capCall (Server, M, Deadline_After (Open_Ms));
      return M.words (0);
   end Server_Count;
   Reply_Tag : MessageTag;
   pragma Unreferenced (Reply_Tag);
   Started, Elapsed : Unsigned_64;
begin
   --  Start the server's session.
   Msg := NULL_MESSAGE;
   Msg.tag := (label => IC.OP_CHANNEL_SESSION, length => 0, protocol => 0, ipc => <>, reserved => 0);
   Reply_Tag := capCall (Server, Msg, Deadline_After (Open_Ms));
   if Msg.tag.label /= 16#F000# then
      Fail ("session");
      return;
   end if;

   --  Opens up to the per-client limit (past the server table's first
   --  block, so it grows), then one past it.
   for K in Shared.Pool'Range loop
      Clients.Open (Shared.Pool (K), Server, CuBit.Deadlines.Clock.After (Open_Ms), Outcome, Refusal);
      if Outcome /= Clients.Connected then Fail ("open" & K'Image); return; end if;
   end loop;
   Clients.Open (C, Server, CuBit.Deadlines.Clock.After (Open_Ms), Outcome, Refusal);
   if Outcome /= Clients.Limit_Reached then
      Fail ("per-client-limit");
      return;
   end if;
   debugPrint ("ipctest-client: channel limits PASS" & LF);

   --  Ping-pong through a channel (reported only: a strict request-then-
   --  wait belongs on a synchronous call).
   Server_Before := Server_Count;
   Calls_Before := Thread_Calls;
   Started := syscall (SYSCALL_GETTIME);
   for K in 1 .. Ping_Count loop
      if not Round_Trip (A, Unsigned_64 (K)) then
         Fail ("ping-pong");
         return;
      end if;
   end loop;
   Elapsed := syscall (SYSCALL_GETTIME) - Started;
   Client_Calls := Thread_Calls - Calls_Before;
   Server_Calls := Server_Count - Server_Before;
   debugPrint ("ipctest-client: channel ping-pong" & Natural'Image (Ping_Count)
               & " round trips in " & Image (Elapsed) & " ms, client calls " & Image (Client_Calls)
               & " server calls " & Image (Server_Calls) & " PASS" & LF);
   --  The same run's synchronous reference: capCall to the same server.
   Calls_Before := Thread_Calls;
   Started := syscall (SYSCALL_GETTIME);
   for K in 1 .. Ping_Count loop
      Msg := NULL_MESSAGE;
      Msg.tag := (label => IC.OP_CHANNEL_PING, length => 1, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := Unsigned_64 (K);
      Reply_Tag := capCall (Server, Msg, Wait_Forever);
      if Msg.tag.label /= 16#F000# or else Msg.words (0) /= Unsigned_64 (K) * IC.Multiplier then
         Fail ("sync-reference");
         return;
      end if;
   end loop;
   Elapsed := syscall (SYSCALL_GETTIME) - Started;
   Client_Calls := Thread_Calls - Calls_Before;
   debugPrint ("ipctest-client: sync reference" & Natural'Image (Ping_Count)
               & " calls in " & Image (Elapsed) & " ms, client calls " & Image (Client_Calls) & LF);

   --  Wake latency and batches (TSC ticks; Ticks_Per_Ms calibrated here).
   --  Doorbell waiter: publish to running for a thread asleep in its wait.
   --  Event loop: submit to running for the server asleep in its wait.
   --  Saved reply: the same probe as a held call answered later.
   --  Batch: Batch_Size requests per ring, Batches times.
   declare
      type Sample_Array is array (1 .. IC.Samples) of Unsigned_64;
      Doorbell_Samples, Reply_Samples, Ring_Samples : Sample_Array := [others => 0];
      Ticks_Per_Ms : Unsigned_64;
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Answer : IC.Result;
      Waited : Clients.Wait_Outcome;
      T0, M0 : Unsigned_64;
      Ignore : Unsigned_64;

      procedure Sort (S : in out Sample_Array) is
      begin
         for X in 2 .. S'Last loop
            declare
               V : constant Unsigned_64 := S (X);
               Y : Natural := X - 1;
            begin
               while Y >= 1 and then S (Y) > V loop
                  S (Y + 1) := S (Y);
                  Y := Y - 1;
               end loop;
               S (Y + 1) := V;
            end;
         end loop;
      end Sort;

      --  Nanoseconds for Ticks.
      function Ns (Ticks : Unsigned_64) return Unsigned_64 is
        (if Ticks_Per_Ms = 0 then 0 else Ticks * 1_000_000 / Ticks_Per_Ms);

      procedure Report (Name : String; Min, Median, P99, Calls_Per_Wake : Unsigned_64) is
      begin
         debugPrint ("BENCH: channel wake " & Name & " min_ns=" & Image (Ns (Min)) & " p50_ns="
                     & Image (Ns (Median)) & " p99_ns=" & Image (Ns (P99))
                     & " syscalls_per_wake_x100=" & Image (Calls_Per_Wake) & LF);
      end Report;

      Calls_Before, Server_Before, Wait_Calls, Sample : Unsigned_64;
   begin
      M0 := syscall (SYSCALL_GETTIME);
      T0 := TSC;
      Ignore := syscall (SYSCALL_SLEEP, Calibration_Ms);
      Ticks_Per_Ms := (TSC - T0) / Unsigned_64'Max (1, syscall (SYSCALL_GETTIME) - M0);

      --  Doorbell waiter (the client's thread): its wait is the one call.
      --  The waiting thread's calls are counted around each Wait alone
      --  (the counter read itself is one call, subtracted), so the figure
      --  is the consumer's cost per wake, not the submit's ring.
      Phase := CuBit.Deadlines.Clock.After (Phase_Ms);
      Wait_Calls := 0;
      for K in Doorbell_Samples'Range loop
         Clients.Submit (A, (Value => Unsigned_64 (K), Kind => IC.Delayed_Stamp), Number, Submitted);
         if Submitted /= Clients.Queued then Fail ("probe-submit"); return; end if;
         Calls_Before := Thread_Calls;
         Clients.Wait (A, Number, Phase, Answer, Waited);
         Sample := TSC;
         Wait_Calls := Wait_Calls + (Thread_Calls - Calls_Before - 1);
         if Waited /= Clients.Answered then Fail ("probe-wait"); return; end if;
         Doorbell_Samples (K) := Sample - Answer.Value;
      end loop;
      Sort (Doorbell_Samples);
      Report ("end-of-turn-to-waiter", Doorbell_Samples (1), Doorbell_Samples (IC.Samples / 2),
              Doorbell_Samples (IC.Samples * 99 / 100), Wait_Calls * 100 / IC.Samples);

      --  Ring to running: the server stamps, then rings at once and goes on.
      Wait_Calls := 0;
      for K in Ring_Samples'Range loop
         Clients.Submit (A, (Value => Unsigned_64 (K), Kind => IC.Ring_Stamp), Number, Submitted);
         if Submitted /= Clients.Queued then Fail ("ring-submit"); return; end if;
         Calls_Before := Thread_Calls;
         Clients.Wait (A, Number, Phase, Answer, Waited);
         Sample := TSC;
         Wait_Calls := Wait_Calls + (Thread_Calls - Calls_Before - 1);
         if Waited /= Clients.Answered then Fail ("ring-wait"); return; end if;
         Ring_Samples (K) := Sample - Answer.Value;
      end loop;
      Sort (Ring_Samples);
      Report ("ring-to-waiter", Ring_Samples (1), Ring_Samples (IC.Samples / 2),
              Ring_Samples (IC.Samples * 99 / 100), Wait_Calls * 100 / IC.Samples);

      --  Event loop (the server): the client stamps as it submits.
      Server_Before := Server_Count;
      for K in 1 .. IC.Samples loop
         Ignore := syscall (SYSCALL_SLEEP, IC.Hold_Ms);
         Clients.Submit (A, (Value => TSC, Kind => IC.Wake_Stamp), Number, Submitted);
         if Submitted /= Clients.Queued then Fail ("loop-submit"); return; end if;
         Clients.Wait (A, Number, Phase, Answer, Waited);
         if Waited /= Clients.Answered then Fail ("loop-wait"); return; end if;
      end loop;
      Server_Calls := Server_Count - Server_Before - 1;
      Msg := NULL_MESSAGE;
      Msg.tag := (label => IC.OP_CHANNEL_STATS, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Reply_Tag := capCall (Server, Msg, Deadline_After (Open_Ms));
      Report ("event-loop", Msg.words (0), Msg.words (1), Msg.words (2),
              Server_Calls * 100 / IC.Samples);

      --  The saved-reply path for the same probe.
      for K in Reply_Samples'Range loop
         Msg := NULL_MESSAGE;
         Msg.tag := (label => IC.OP_CHANNEL_DELAYED, length => 0, protocol => 0, ipc => <>, reserved => 0);
         Reply_Tag := capCall (Server, Msg, Deadline_After (Open_Ms));
         Reply_Samples (K) := TSC - Msg.words (0);
      end loop;
      Sort (Reply_Samples);
      Report ("saved-reply", Reply_Samples (1), Reply_Samples (IC.Samples / 2),
              Reply_Samples (IC.Samples * 99 / 100), 100);

      --  Batches: Batch_Size requests, at most one ring, then every answer.
      declare
         Numbers : array (1 .. Bench_Batch) of Clients.Tag;
         Started_Ms, Elapsed_Ms : Unsigned_64;
      begin
         Server_Before := Server_Count;
         Calls_Before := Thread_Calls;
         Started_Ms := syscall (SYSCALL_GETTIME);
         for R in 1 .. Bench_Batches loop
            for K in Numbers'Range loop
               Clients.Submit (A, (Value => Unsigned_64 (K), Kind => IC.Echo), Numbers (K), Submitted);
               if Submitted /= Clients.Queued then Fail ("bench-batch-submit"); return; end if;
            end loop;
            for K in Numbers'Range loop
               Clients.Wait (A, Numbers (K), Phase, Answer, Waited);
               if Waited /= Clients.Answered then Fail ("bench-batch-wait"); return; end if;
            end loop;
         end loop;
         Elapsed_Ms := syscall (SYSCALL_GETTIME) - Started_Ms;
         Client_Calls := Thread_Calls - Calls_Before - 2;
         Server_Calls := Server_Count - Server_Before - 1;
         debugPrint ("BENCH: channel batch size=" & Image (Bench_Batch) & " messages="
                     & Image (Bench_Batch * Bench_Batches) & " total_ms=" & Image (Elapsed_Ms)
                     & " client_syscalls_per_message_x100="
                     & Image (Client_Calls * 100 / (Bench_Batch * Bench_Batches))
                     & " server_syscalls_per_message_x100="
                     & Image (Server_Calls * 100 / (Bench_Batch * Bench_Batches)) & LF);
      end;
   end;

   --  Two threads on one channel: the worker and this thread Call on A at
   --  once; each answer must reach the thread that asked.
   declare
      Right : Natural := 0;
   begin
      if not Shared.Start_Worker then
         Fail ("thread-start");
         return;
      end if;
      Phase := CuBit.Deadlines.Clock.After (Phase_Ms);
      for K in 1 .. Shared.Worker_Calls loop
         if Call_Trip (A, Unsigned_64 (K), Phase) then
            Right := Right + 1;
         end if;
      end loop;
      Shared.Join_Worker;
      if Right /= Shared.Worker_Calls or else Natural (Shared.Worker_Right) /= Shared.Worker_Calls then
         Fail ("threads main " & Image (Unsigned_64 (Right)) & " worker "
               & Image (Unsigned_64 (Shared.Worker_Right)));
         return;
      end if;
      debugPrint ("ipctest-client: channel two threads" & Natural'Image (2 * Shared.Worker_Calls)
                  & " calls PASS" & LF);
   end;

   --  A pipelined batch on B: submit many, then reap them all.
   declare
      Numbers : array (1 .. Batch_Count) of Clients.Tag;
      Submitted : Clients.Submit_Result;
      Answer : IC.Result;
      Waited : Clients.Wait_Outcome;
   begin
      for K in Numbers'Range loop
         Clients.Submit (B, (Value => Unsigned_64 (100 + K), Kind => IC.Echo), Numbers (K), Submitted);
         if Submitted /= Clients.Queued then Fail ("batch-submit"); return; end if;
      end loop;
      for K in Numbers'Range loop
         Clients.Wait (B, Numbers (K), CuBit.Deadlines.Clock.After (Answer_Ms), Answer, Waited);
         if Waited /= Clients.Answered or else Answer.Value /= Unsigned_64 (100 + K) * IC.Multiplier then
            Fail ("batch-answer");
            return;
         end if;
      end loop;
      debugPrint ("ipctest-client: channel batch PASS" & LF);
   end;

   --  A held request: the wait ends at its explicit deadline, not before
   --  and not much after; the answer arrives once the server is released.
   declare
      Held, Releasing : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Answer : IC.Result;
      Waited : Clients.Wait_Outcome;
      T0, Took : Unsigned_64;
   begin
      Clients.Submit (A, (Value => 77, Kind => IC.Silent), Held, Submitted);
      if Submitted /= Clients.Queued then Fail ("silent-submit"); return; end if;
      T0 := syscall (SYSCALL_GETTIME);
      Clients.Wait (A, Held, CuBit.Deadlines.Clock.After (Silent_Ms), Answer, Waited);
      Took := syscall (SYSCALL_GETTIME) - T0;
      if Waited /= Clients.Timed_Out then Fail ("deadline-not-timed-out"); return; end if;
      if Took + 1 < Silent_Ms or else Took > Silent_Latest_Ms then
         Fail ("deadline-time " & Image (Took));
         return;
      end if;
      Clients.Submit (A, (Value => 78, Kind => IC.Release), Releasing, Submitted);
      if Submitted /= Clients.Queued then Fail ("release-submit"); return; end if;
      Clients.Wait (A, Held, CuBit.Deadlines.Clock.After (Answer_Ms), Answer, Waited);
      if Waited /= Clients.Answered or else Answer.Value /= 77 * IC.Multiplier then
         Fail ("held-answer");
         return;
      end if;
      Clients.Wait (A, Releasing, CuBit.Deadlines.Clock.After (Answer_Ms), Answer, Waited);
      if Waited /= Clients.Answered then Fail ("release-answer"); return; end if;
      debugPrint ("ipctest-client: channel deadline PASS" & LF);
   end;

   --  Close B; then the server's report: everything served, one limit
   --  refusal, B's end seen.
   Clients.Close (B);
   --  One more round trip through A: the server handles B's end (a kernel
   --  grant notice, already waiting) before it serves this request.
   declare
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Answer : IC.Result;
      Waited : Clients.Wait_Outcome;
   begin
      Clients.Submit (A, (Value => 1, Kind => IC.Echo), Number, Submitted);
      if Submitted /= Clients.Queued then
         Fail ("after-close-submit " & Clients.Submit_Result'Image (Submitted));
         return;
      end if;
      Clients.Wait (A, Number, CuBit.Deadlines.Clock.After (Answer_Ms), Answer, Waited);
      if Waited /= Clients.Answered or else Answer.Value /= IC.Multiplier then
         Fail ("after-close " & Clients.Wait_Outcome'Image (Waited) & " reason "
               & Clients.End_Reason'Image (Clients.Reason (A)));
         return;
      end if;
   end;
   --  Close the rest; the server sees every end before it answers the end
   --  of the session (its loop takes grant notices first).
   for K in Shared.Pool'Range loop
      if K /= 2 then
         Clients.Close (Shared.Pool (K));
      end if;
   end loop;
   Msg := NULL_MESSAGE;
   Msg.tag := (label => IC.OP_CHANNEL_END, length => 0, protocol => 0, ipc => <>, reserved => 0);
   Reply_Tag := capCall (Server, Msg, Deadline_After (Open_Ms));
   if Msg.tag.label /= 16#F000# then
      Fail ("session-end");
   elsif Msg.words (IC.Report_Limit) /= 1 then
      Fail ("server-limit-count " & Image (Msg.words (IC.Report_Limit)));
   elsif Msg.words (IC.Report_Served) /=
     Unsigned_64 (Ping_Count + 2 * Shared.Worker_Calls + Batch_Count + 3 + Probe_Requests)
   then
      Fail ("server-served " & Image (Msg.words (IC.Report_Served)));
   elsif Msg.words (IC.Report_Ended) /= IC.Per_Client then
      Fail ("server-ends " & Image (Msg.words (IC.Report_Ended)));
   else
      debugPrint ("ipctest-client: channel server calls " & Image (Msg.words (IC.Report_Server_Calls))
                  & " for" & Natural'Image (Ping_Count + 2 * Shared.Worker_Calls + Batch_Count + 3 + Probe_Requests)
                  & " requests" & LF);
      debugPrint ("ipctest-client: channel end PASS" & LF);
   end if;
   --  The kernel told this side each grant came back: read those notices so
   --  the tests after this one start with an empty event lane.
   declare
      Event : Message;
   begin
      while Poll_Event (Event) loop
         null;
      end loop;
   end;
end Channel_Tests;
