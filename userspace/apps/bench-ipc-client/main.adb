------------------------------------------------------------------------------
--  CuBit IPC benchmark client
--
--  Prints compact final summaries only. Serial output is intentionally kept
--  out of measured loops so it does not dominate the numbers.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Benchmark_Clock;
with CuBit.Timing_Histograms;

procedure main is
   use ASCII;
   package Clock renames CuBit.Benchmark_Clock;
   package Timing renames CuBit.Timing_Histograms;
   Rate : Unsigned_64;

   CAP_SLOT_BENCH : constant CapabilitySlot := 18;
   OP_BENCH_ECHO  : constant Unsigned_32 := 16#0910#;
   OP_BENCH_DIE   : constant Unsigned_32 := 16#0911#;
   OP_BENCH_HOLD  : constant Unsigned_32 := 16#0912#;
   REPLY_OK       : constant Unsigned_32 := 16#F000#;
   XOR_MAGIC      : constant Unsigned_64 := 16#C0B1_7000_BE11#;

   WARMUP_COUNT : constant Natural := 64;
   SYNC_COUNT   : constant Natural := 20_000;
   ASYNC_COUNT  : constant Natural := 8_192;
   TOKEN_BASE   : constant Unsigned_64 := 16#B100_0000#;

   ok : Boolean := True;

   procedure printDec (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;

      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;

      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   procedure fail (reason : String) is
   begin
      debugPrint ("BENCH: FAIL ipc ");
      debugPrint (reason);
      debugPrint (LF & "");
      ok := False;
   end fail;

   function nowMs return Unsigned_64 is
   begin
      return syscall (SYSCALL_GETTIME);
   end nowMs;

   function echoMsg (value : Unsigned_64) return Message is
      msg : Message := NULL_MESSAGE;
   begin
      msg.tag := (label  => OP_BENCH_ECHO,
                  length => 1,
                  flags  => 0,
                  reserved  => 0);
      msg.words (0) := value;
      return msg;
   end echoMsg;

   procedure verifyReply (replyMsg : Message; expected : Unsigned_64) is
   begin
      if replyMsg.tag.label /= REPLY_OK then
         fail ("reply-label");
      elsif replyMsg.words (0) /= expected then
         fail ("reply-value");
      elsif replyMsg.words (1) /= (expected xor XOR_MAGIC) then
         fail ("reply-check");
      end if;
   end verifyReply;

   procedure runSync is
      msg : Message;
      t0  : Unsigned_64;
      t1  : Unsigned_64;
      Started, Finished : Unsigned_64;
      Samples : Timing.Histogram;
   begin
      for i in 1 .. WARMUP_COUNT loop
         msg := echoMsg (Unsigned_64 (i));
         msg.tag := capCall (CAP_SLOT_BENCH, msg);
         verifyReply (msg, Unsigned_64 (i));
         exit when not ok;
      end loop;

      if not ok then
         return;
      end if;

      t0 := nowMs;
      for i in 1 .. SYNC_COUNT loop
         msg := echoMsg (Unsigned_64 (i));
         Started := Clock.Read_Counter;
         msg.tag := capCall (CAP_SLOT_BENCH, msg);
         Finished := Clock.Read_Counter;
         if Finished < Started then
            fail ("counter-went-backwards");
            exit;
         end if;
         Timing.Add (Samples, Finished - Started);
         verifyReply (msg, Unsigned_64 (i));
         exit when not ok;
      end loop;
      t1 := nowMs;

      if not ok then
         return;
      end if;

      debugPrint ("BENCH: ipc sync count=");
      printDec (Unsigned_64 (SYNC_COUNT));
      debugPrint (" total_ms=");
      printDec (t1 - t0);
      debugPrint (LF & "");
      Clock.Report ("ipc-sync-round-trip", Samples);
   end runSync;

   procedure runAsync is
      completions : CompletionRing;
      ret        : Unsigned_64;
      submitOk   : Boolean;
      submitted  : Natural := 0;
      completed  : Natural := 0;
      inFlight   : Natural := 0;
      t0         : Unsigned_64;
      t1         : Unsigned_64;
      seen       : array (1 .. ASYNC_COUNT) of Boolean := (others => False);
      Started : array (1 .. ASYNC_COUNT) of Unsigned_64 := (others => 0);
      Samples : Timing.Histogram;
      Finished : Unsigned_64;
   begin
      completions := (others => NULL_COMPLETION);
      for I in 1 .. WARMUP_COUNT loop
         submitOk := capSubmit
           (CAP_SLOT_BENCH, echoMsg (Unsigned_64 (I)), Unsigned_64 (I));
         if not submitOk then fail ("async-warmup-submit"); return; end if;
         ret := waitCompletion (completions'Address, 1, 1);
         if ret /= 1 or else completions (0).status /= COMPLETION_OK then
            fail ("async-warmup-completion"); return;
         end if;
         verifyReply (completions (0).msg, Unsigned_64 (I));
         if not ok then return; end if;
      end loop;
      t0 := nowMs;

      while completed < ASYNC_COUNT and then ok loop
         --  Keep the kernel completion queue busy without turning this into a
         --  queue-depth test. Current async capacity is small, so refill as
         --  completions arrive and measure sustained throughput.
         while submitted < ASYNC_COUNT and then inFlight < 16 loop
            Started (submitted + 1) := Clock.Read_Counter;
            submitOk := capSubmit
              (CAP_SLOT_BENCH,
               echoMsg (Unsigned_64 (submitted + 1)),
               TOKEN_BASE + Unsigned_64 (submitted + 1));

            exit when not submitOk;
            submitted := submitted + 1;
            inFlight := inFlight + 1;
         end loop;

         if inFlight = 0 and then submitted < ASYNC_COUNT then
            fail ("async-submit");
            exit;
         end if;

         ret := waitCompletion (completions'Address, 16, 1);

         if ret = 0 then
            fail ("async-wait");
            exit;
         end if;

         for slot in CompletionIndex loop
            exit when slot >= Natural (ret);

            if completions (slot).status /= COMPLETION_OK then
               fail ("async-status");
            elsif completions (slot).token <= TOKEN_BASE or else
                  completions (slot).token >
                     TOKEN_BASE + Unsigned_64 (ASYNC_COUNT)
            then
               fail ("async-token");
            else
               declare
                  idx : constant Natural :=
                     Natural (completions (slot).token - TOKEN_BASE);
               begin
                  if seen (idx) then
                     fail ("async-duplicate");
                  else
                     Finished := Clock.Read_Counter;
                     if Finished < Started (idx) then
                        fail ("counter-went-backwards");
                        exit;
                     end if;
                     Timing.Add (Samples, Finished - Started (idx));
                     verifyReply (completions (slot).msg,
                                  Unsigned_64 (idx));
                     seen (idx) := True;
                     completed := completed + 1;
                     if inFlight > 0 then
                        inFlight := inFlight - 1;
                     end if;
                  end if;
               end;
            end if;

            exit when not ok;
         end loop;
      end loop;
      t1 := nowMs;

      if not ok then
         return;
      end if;

      debugPrint ("BENCH: ipc async submitted=");
      printDec (Unsigned_64 (submitted));
      debugPrint (" completed=");
      printDec (Unsigned_64 (completed));
      debugPrint (" total_ms=");
      printDec (t1 - t0);
      debugPrint (LF & "");
      Clock.Report ("ipc-async-observed-completion", Samples);
   end runAsync;

   procedure runRetirement is
      msg : Message;
      entryResult : aliased CompletionEntry;
      ignored : Unsigned_64;
      tag : MessageTag;
      died : Natural := 0;
      seen : array (1 .. 64) of Boolean := (others => False);
      deadline : Unsigned_64;
   begin
      -- Leave completed results unread, filling 63 of the 64 reservations.
      for i in 1 .. 63 loop
         msg := echoMsg (Unsigned_64(i));
         deadline := nowMs + 2_000;
         while not capSubmit (CAP_SLOT_BENCH, msg, Unsigned_64(i)) loop
            if nowMs >= deadline then fail ("saturation-submit"); return; end if;
            ignored := syscall (SYSCALL_SLEEP, 1);
         end loop;
         ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      msg := NULL_MESSAGE;
      msg.tag.label := OP_BENCH_HOLD;
      if not capSubmit (CAP_SLOT_BENCH, msg, 64) then
         fail ("last-completion-reservation"); return;
      end if;
      msg := echoMsg (65);
      if capSubmit (CAP_SLOT_BENCH, msg, 65) then
         fail ("completion-overcommit"); return;
      end if;
      -- Exit with both a blocked synchronous caller and an outstanding async
      -- request. Both must complete; the death result occupies the last slot.
      msg := NULL_MESSAGE;
      msg.tag.label := OP_BENCH_DIE;
      tag := capCall (CAP_SLOT_BENCH, msg);
      if tag.label /= 0 then fail ("sync-target-exit"); return; end if;
      deadline := nowMs + 2_000;
      for i in 1 .. 64 loop
         loop
            exit when Poll_Completion (entryResult'Address) = 1;
            if nowMs >= deadline then fail ("lost-exit-completion"); return; end if;
            ignored := syscall (SYSCALL_SLEEP, 1);
         end loop;
         if entryResult.token not in 1 .. 64 then
            fail ("completion-token-range"); return;
         end if;
         if seen(Natural(entryResult.token)) then
            fail ("duplicate-completion"); return;
         end if;
         seen(Natural(entryResult.token)) := True;
         if entryResult.token = 64 then
            if entryResult.status /= COMPLETION_TARGET_DIED then
               fail ("missing-target-died"); return;
            end if;
            died := died + 1;
         elsif entryResult.status /= COMPLETION_OK then
            fail ("lost-completed-reply"); return;
         end if;
      end loop;
      if died /= 1 or else capSubmit (CAP_SLOT_BENCH, msg, 65) then
         fail ("retired-endpoint-admission"); return;
      end if;
      debugPrint ("IPC-RETIREMENT-CHECK: PASS" & LF);
   end runRetirement;

begin
   debugPrint ("bench-ipc-client: starting" & LF);
   Clock.Calibrate (Rate);
   if Rate = 0 then
      fail ("unstable-counter-calibration");
      return;
   end if;
   declare
      Overhead : Timing.Histogram;
      Before, After : Unsigned_64;
   begin
      for I in 1 .. 1024 loop
         Before := Clock.Read_Counter;
         After := Clock.Read_Counter;
         if After >= Before then
            Timing.Add (Overhead, After - Before);
         end if;
      end loop;
      Clock.Report ("counter-read-pair", Overhead);
   end;
   debugPrint ("BENCH: phase=untraced async_depth=16" & LF);
   runSync;
   if ok then runAsync; end if;
   debugPrint ("BENCH: phase=traced async_depth=16" & LF);
   declare
      ignored : Unsigned_64;
   begin
      ignored := syscall (SYSCALL_TRACE_RESET);
   end;

   runSync;
   if ok then
      runAsync;
   end if;

   if ok then
      debugPrint ("BENCH: PASS ipc" & LF);
   end if;

   declare
      ignored : Unsigned_64;
   begin
      ignored := syscall (SYSCALL_TRACE_SUMMARY);
   end;

   if ok then runRetirement; end if;

   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_EXIT);
      loop
         ret := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end;
end main;
