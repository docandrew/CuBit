------------------------------------------------------------------------------
--  CuBit IPC benchmark client
--
--  Prints compact final summaries only. Serial output is intentionally kept
--  out of measured loops so it does not dominate the numbers.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   CAP_SLOT_BENCH : constant CapabilitySlot := 18;
   OP_BENCH_ECHO  : constant Unsigned_32 := 16#0910#;
   OP_BENCH_DIE   : constant Unsigned_32 := 16#0911#;
   REPLY_OK       : constant Unsigned_32 := 16#F000#;
   XOR_MAGIC      : constant Unsigned_64 := 16#C0B1_7000_BE11#;

   WARMUP_COUNT : constant Natural := 64;
   SYNC_COUNT   : constant Natural := 2000;
   ASYNC_COUNT  : constant Natural := 512;
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
                  badge  => 0);
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
      totalUs : Unsigned_64;
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
         msg.tag := capCall (CAP_SLOT_BENCH, msg);
         verifyReply (msg, Unsigned_64 (i));
         exit when not ok;
      end loop;
      t1 := nowMs;

      if not ok then
         return;
      end if;

      totalUs := (t1 - t0) * 1000;
      debugPrint ("BENCH: ipc sync count=");
      printDec (Unsigned_64 (SYNC_COUNT));
      debugPrint (" total_ms=");
      printDec (t1 - t0);
      debugPrint (" avg_us=");
      printDec (totalUs / Unsigned_64 (SYNC_COUNT));
      debugPrint (LF & "");
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
      totalUs    : Unsigned_64;
      seen       : array (1 .. ASYNC_COUNT) of Boolean := (others => False);
   begin
      t0 := nowMs;

      while completed < ASYNC_COUNT and then ok loop
         --  Keep the kernel completion queue busy without turning this into a
         --  queue-depth test. Current async capacity is small, so refill as
         --  completions arrive and measure sustained throughput.
         while submitted < ASYNC_COUNT and then inFlight < 16 loop
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

      totalUs := (t1 - t0) * 1000;
      debugPrint ("BENCH: ipc async submitted=");
      printDec (Unsigned_64 (submitted));
      debugPrint (" completed=");
      printDec (Unsigned_64 (completed));
      debugPrint (" total_ms=");
      printDec (t1 - t0);
      debugPrint (" avg_us=");
      if completed > 0 then
         printDec (totalUs / Unsigned_64 (completed));
      else
         printDec (0);
      end if;
      debugPrint (LF & "");
   end runAsync;

begin
   debugPrint ("bench-ipc-client: starting" & LF);
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

   declare
      msg : Message := NULL_MESSAGE;
      ignore : MessageTag;
   begin
      msg.tag := (label  => OP_BENCH_DIE,
                  length => 0,
                  flags  => 0,
                  badge  => 0);
      ignore := capCall (CAP_SLOT_BENCH, msg);
   end;

   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_EXIT);
      loop
         ret := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end;
end main;
