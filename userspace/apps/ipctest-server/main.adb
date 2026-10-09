------------------------------------------------------------------------------
--  CuBit headless IPC regression server
--
--  Receives async requests, saves the kernel-minted reply capability, and
--  completes the request later via replyCap.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   OP_ASYNC_ECHO     : constant Unsigned_32 := 16#0901#;
   OP_REVERSE_ECHO   : constant Unsigned_32 := 16#0902#;
   OP_ONEWAY_PROBE   : constant Unsigned_32 := 16#0903#;
   OP_DOUBLE_REPLY   : constant Unsigned_32 := 16#0904#;
   OP_STATUS         : constant Unsigned_32 := 16#0905#;
   OP_PRESSURE_HOLD  : constant Unsigned_32 := 16#0906#;
   OP_PRESSURE_RELEASE : constant Unsigned_32 := 16#0907#;
   OP_DIE            : constant Unsigned_32 := 16#0908#;
   OP_OCCUPIED_HOLD  : constant Unsigned_32 := 16#0909#;
   OP_OCCUPIED_PROBE : constant Unsigned_32 := 16#090A#;
   OP_DEPARTING_HOLD : constant Unsigned_32 := 16#090B#;
   OP_DEPARTING_READY : constant Unsigned_32 := 16#090C#;
   OP_RETIRE_DEPARTED : constant Unsigned_32 := 16#090D#;
   OP_FAIR_BEGIN : constant Unsigned_32 := 16#090E#;
   OP_FAIR_POLL : constant Unsigned_32 := 16#090F#;
   OP_FAIR_QUEUED : constant Unsigned_32 := 16#0910#;
   OP_FAIR_END : constant Unsigned_32 := 16#0911#;
   --  Call deadlines (docs/ipc-fastpath.md): a call never answered, a late
   --  answer to it, a request held up behind a busy server, and the report.
   OP_DEADLINE_SILENT : constant Unsigned_32 := 16#0912#;
   OP_DEADLINE_LATE   : constant Unsigned_32 := 16#0913#;
   OP_DEADLINE_BUSY   : constant Unsigned_32 := 16#0914#;
   OP_DEADLINE_QUEUED : constant Unsigned_32 := 16#0915#;
   OP_DEADLINE_STATUS : constant Unsigned_32 := 16#0916#;
   SILENT_SLOT : constant CapabilitySlot := 13;
   --  Long enough for the caller's 50 ms deadline to pass while queued.
   BUSY_MS : constant Unsigned_64 := 300;
   LATE_MARK : constant Unsigned_64 := 16#1A7E_0000#;
   silentHeld : Boolean := False;
   lateRefused : Boolean := False;
   forgedRefused : Boolean := False;
   queuedSeen : Boolean := False;
   type Receive_Mode is (Blocking, Service_Poll, Mixed_Poll, Timed, Activity);
   mode : Receive_Mode := Blocking;
   fairSeen : Boolean := False;
   found : Boolean;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   REPLY_ERR     : constant Unsigned_32 := 16#F001#;

   REPLY_SLOT : constant CapabilitySlot := 10;
   REVERSE_COUNT : constant Natural := 3;
   PRESSURE_COUNT : constant Natural := 16;
   XOR_MAGIC  : constant Unsigned_64 := 16#C0B1_7000#;
   FPU_SENTINEL : constant Unsigned_64 := 16#51D0_CAFE_F00D_BAAD#;

   reverseSlots  : array (1 .. REVERSE_COUNT) of CapabilitySlot :=
      (20, 21, 22);
   reverseValues : array (1 .. REVERSE_COUNT) of Unsigned_64 :=
      (others => 0);
   reverseFrom   : array (1 .. REVERSE_COUNT) of Process_ID :=
      (others => No_Process);
   reversePending : Natural := 0;

   pressureSlots  : array (1 .. PRESSURE_COUNT) of CapabilitySlot :=
      (30, 31, 32, 33, 34, 35, 36, 37,
       38, 39, 40, 41, 42, 43, 44, 45);
   pressureValues : array (1 .. PRESSURE_COUNT) of Unsigned_64 :=
      (others => 0);
   pressureFrom   : array (1 .. PRESSURE_COUNT) of Process_ID :=
      (others => No_Process);
   pressurePending : Natural := 0;

   oneWaySaveRejected : Boolean := False;
   doubleUseRejected  : Boolean := False;
   occupiedValue      : Unsigned_64 := 0;
   DEPARTING_SLOT : constant CapabilitySlot := 12;
   departingCaller : Process_ID := No_Process;
   departingReady : Boolean := False;

   from : Process_ID;
   msg  : Message;
   ret  : Unsigned_64;

   function boolWord (value : Boolean) return Unsigned_64 is
   begin
      if value then
         return 1;
      else
         return 0;
      end if;
   end boolWord;

   procedure loadFPUProbe is
   begin
      --  XMM0 carries this context-isolation probe. The program is built
      --  with SSE, so set it just before the system call under test and
      --  read it just after.
      Asm ("movq %0, %%xmm0",
           Inputs   => Unsigned_64'Asm_Input ("r", FPU_SENTINEL),
           Volatile => True);
   end loadFPUProbe;

   function readFPUProbe return Unsigned_64 is
      value : Unsigned_64;
   begin
      Asm ("movq %%xmm0, %0",
           Outputs  => Unsigned_64'Asm_Output ("=r", value),
           Volatile => True);
      return value;
   end readFPUProbe;

   procedure sendReply
     (replyTo : Process_ID;
      label   : Unsigned_32;
      word0   : Unsigned_64 := 0;
      word1   : Unsigned_64 := 0;
      word2   : Unsigned_64 := 0)
   is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 3,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words (0) := word0;
      replyMsg.words (1) := word1;
      replyMsg.words (2) := word2;
      pragma Unreferenced (replyTo);
      ignore := replyCap (CapabilitySlot'Last, replyMsg);
   end sendReply;

   function makeEchoReply
     (value    : Unsigned_64;
      sender   : Process_ID) return Message
   is
      replyMsg : Message := NULL_MESSAGE;
   begin
      replyMsg.tag := (label  => REPLY_OK,
                       length => 3,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words (0) := value;
      replyMsg.words (1) := value xor XOR_MAGIC;
      replyMsg.words (2) := To_Word (sender);
      return replyMsg;
   end makeEchoReply;
begin
   debugPrint ("ipctest-server: starting" & LF);

   ret := registerDriver (DRIVER_IPCTEST);
   if ret = Unsigned_64'Last then
      debugPrint ("TEST: FAIL async-ipc server-register" & LF);
      loop
         ret := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end if;

   debugPrint ("ipctest-server: registered" & LF);

   loop
      loop
         --  Leave a distinctive value live in XMM0 across the receive: a
         --  newly started process must never inherit it, and every return
         --  to this process must restore it. Set before each receive, since
         --  this program's own record copies may use XMM0 (it is built with
         --  SSE).
         loadFPUProbe;
         found := True;
         case mode is
            when Blocking => receive (from, msg);
            when Service_Poll => Poll_Service_Request (from, msg, found);
            when Mixed_Poll => Poll_Any_Ipc (from, msg, found);
            when Timed =>
               receiveUntil (syscall (SYSCALL_GETTIME) + 1000, from, msg, found);
            when Activity =>
               declare
                  activity : constant Activity_Result :=
                    Wait_For_Activity_Until (Unsigned_64'Last);
               begin
                  if activity /= Work_Available then
                     debugPrint ("TEST: FAIL async-ipc activity-server-wake" & LF);
                  end if;
                  Poll_Any_Ipc (from, msg, found);
               end;
         end case;
         exit when found;
      end loop;

      --  Only the modes that are a bare system call: the runtime's poll
      --  helpers use XMM0 themselves.
      if mode in Blocking | Timed and then readFPUProbe /= FPU_SENTINEL then
         debugPrint ("TEST: FAIL async-ipc fpu-server-restore" & LF);
      end if;

      if msg.tag.label = OP_FAIR_BEGIN then
         mode := Receive_Mode'Val (msg.words (0));
         fairSeen := False;
         sendReply (from, REPLY_OK);
         -- Let the caller enqueue a one-way request AND block in a synchronous
         -- poll before the next receive. On CPU 0, direct reply handoff then
         -- keeps that poller hot, reproducing the closed-window starvation.
         ret := syscall (SYSCALL_SLEEP, 25);
      elsif msg.tag.label = OP_FAIR_QUEUED then
         fairSeen := True;
         if saveReplyCap (11) /= 0 then
            debugPrint ("TEST: FAIL async-ipc fairness-oneway-authority" & LF);
         end if;
      elsif msg.tag.label = OP_FAIR_POLL then
         sendReply (from, REPLY_OK, boolWord (fairSeen));
      elsif msg.tag.label = OP_FAIR_END then
         mode := Blocking;
         sendReply (from, REPLY_OK);
      elsif msg.tag.label = OP_ASYNC_ECHO then
         declare
            activity : constant Activity_Result :=
              Wait_For_Activity_Until (syscall (SYSCALL_GETTIME));
         begin
            -- Readiness must not clear the current request's reply authority.
            if activity = Unavailable then
               debugPrint ("TEST: FAIL async-ipc activity-reply-authority" & LF);
            end if;
         end;
         ret := saveReplyCap (REPLY_SLOT);
         if ret /= 1 then
            debugPrint ("TEST: FAIL async-ipc save-reply-cap" & LF);
         else
            ret := syscall (SYSCALL_SLEEP, 25);

            declare
               replyMsg : Message := makeEchoReply (msg.words (0), from);
            begin
               ret := replyCap (REPLY_SLOT, replyMsg);
               if ret /= 1 then
                  debugPrint ("TEST: FAIL async-ipc reply-cap" & LF);
               end if;
            end;
         end if;
      elsif msg.tag.label = OP_REVERSE_ECHO then
         if msg.words (1) /= 16#1122_3344_5566_7788# or else
            msg.words (2) /= 16#8877_6655_4433_2211# or else
            msg.words (3) /= 16#FEDC_BA98_7654_3210#
         then
            debugPrint ("TEST: FAIL async-ipc four-word payload" & LF);
         end if;
         if reversePending < REVERSE_COUNT then
            reversePending := reversePending + 1;
            reverseValues (reversePending) := msg.words (0);
            reverseFrom (reversePending) := from;
            ret := saveReplyCap (reverseSlots (reversePending));
            if ret /= 1 then
               debugPrint ("TEST: FAIL async-ipc reverse-save" & LF);
            end if;
         else
            debugPrint ("TEST: FAIL async-ipc reverse-overflow" & LF);
         end if;

         if reversePending = REVERSE_COUNT then
            for i in reverse 1 .. REVERSE_COUNT loop
               declare
                  replyMsg : Message :=
                     makeEchoReply (reverseValues (i), reverseFrom (i));
               begin
                  ret := replyCap (reverseSlots (i), replyMsg);
                  if ret /= 1 then
                     debugPrint ("TEST: FAIL async-ipc reverse-reply" & LF);
                  end if;
               end;
            end loop;
            reversePending := 0;
         end if;
      elsif msg.tag.label = OP_ONEWAY_PROBE then
         ret := saveReplyCap (REPLY_SLOT);
         if ret = 0 then
            oneWaySaveRejected := True;
         else
            debugPrint ("TEST: FAIL async-ipc oneway-reply-cap" & LF);
         end if;
      elsif msg.tag.label = OP_DOUBLE_REPLY then
         ret := saveReplyCap (REPLY_SLOT);
         if ret /= 1 then
            debugPrint ("TEST: FAIL async-ipc double-save" & LF);
         else
            declare
               replyMsg : Message := makeEchoReply (msg.words (0), from);
            begin
               ret := replyCap (REPLY_SLOT, replyMsg);
               if ret /= 1 then
                  debugPrint ("TEST: FAIL async-ipc double-first" & LF);
               end if;

               ret := replyCap (REPLY_SLOT, replyMsg);
               if ret = 0 then
                  doubleUseRejected := True;
               else
                  debugPrint ("TEST: FAIL async-ipc double-second" & LF);
               end if;
            end;
         end if;
      elsif msg.tag.label = OP_OCCUPIED_HOLD then
         occupiedValue := msg.words (0);
         ret := saveReplyCap (REPLY_SLOT);
         if ret /= 1 then
            debugPrint ("TEST: FAIL async-ipc occupied-hold-save" & LF);
         end if;
      elsif msg.tag.label = OP_OCCUPIED_PROBE then
         --  REPLY_SLOT still owns the preceding HOLD request. Saving this
         --  request over it must fail and must leave slot 63 untouched.
         ret := saveReplyCap (REPLY_SLOT);
         if ret /= 0 then
            debugPrint ("TEST: FAIL async-ipc occupied-overwrite" & LF);
         else
            declare
               currentReply : Message := makeEchoReply (msg.words (0), from);
               heldReply    : Message := makeEchoReply (occupiedValue, from);
            begin
               ret := replyCap (CapabilitySlot'Last, currentReply);
               if ret /= 1 then
                  debugPrint
                    ("TEST: FAIL async-ipc occupied-current-reply" & LF);
               end if;

               ret := replyCap (REPLY_SLOT, heldReply);
               if ret /= 1 then
                  debugPrint
                    ("TEST: FAIL async-ipc occupied-held-reply" & LF);
               end if;
            end;
         end if;
      elsif msg.tag.label = OP_STATUS then
         sendReply (from,
                    REPLY_OK,
                    boolWord (oneWaySaveRejected),
                    boolWord (doubleUseRejected),
                    Unsigned_64 (reversePending));
      elsif msg.tag.label = OP_PRESSURE_HOLD then
         if pressurePending < PRESSURE_COUNT then
            pressurePending := pressurePending + 1;
            pressureValues (pressurePending) := msg.words (0);
            pressureFrom (pressurePending) := from;
            ret := saveReplyCap (pressureSlots (pressurePending));
            if ret /= 1 then
               debugPrint ("TEST: FAIL async-ipc pressure-save" & LF);
            end if;
         else
            debugPrint ("TEST: FAIL async-ipc pressure-overflow" & LF);
         end if;
      elsif msg.tag.label = OP_PRESSURE_RELEASE then
         for i in 1 .. pressurePending loop
            declare
               replyMsg : Message :=
                  makeEchoReply (pressureValues (i), pressureFrom (i));
            begin
               ret := replyCap (pressureSlots (i), replyMsg);
               if ret /= 1 then
                  debugPrint ("TEST: FAIL async-ipc pressure-reply" & LF);
               end if;
            end;
         end loop;
         pressurePending := 0;
      elsif msg.tag.label = OP_DEPARTING_HOLD then
         ret := saveReplyCap (DEPARTING_SLOT);
         if ret /= 1 then
            debugPrint ("TEST: FAIL async-ipc departing save" & LF);
            sendReply (from, REPLY_ERR);
         else
            departingCaller := from;
         end if;
      elsif msg.tag.label = OP_DEPARTING_READY then
         departingReady := from = departingCaller;
         sendReply (from, (if departingReady then REPLY_OK else REPLY_ERR));
      elsif msg.tag.label = OP_RETIRE_DEPARTED then
         if not departingReady then
            sendReply (from, REPLY_ERR, 1);
         else
            -- Test-only scheduling allowance after the barrier acknowledgement.
            -- Sleep is not the oracle: delivery MUST fail, then the same slot
            -- MUST accept a fresh reply and complete the survivor's token.
            ret := syscall (SYSCALL_SLEEP, 50);
            ret := replyCap (DEPARTING_SLOT, makeEchoReply (0, departingCaller));
            if ret /= 0 then
               debugPrint ("TEST: FAIL async-ipc dead caller reply delivered" & LF);
            end if;
            ret := replyCap (DEPARTING_SLOT, makeEchoReply (0, departingCaller));
            if ret /= 0 then
               debugPrint ("TEST: FAIL async-ipc dead caller double reply" & LF);
            end if;
            ret := saveReplyCap (DEPARTING_SLOT);
            if ret /= 1 then
               debugPrint ("TEST: FAIL async-ipc dead caller reply slot leaked" & LF);
               sendReply (from, REPLY_ERR, 2);
            else
               ret := replyCap (DEPARTING_SLOT, makeEchoReply (msg.words (0), from));
               if ret /= 1 then
                  debugPrint ("TEST: FAIL async-ipc reused reply slot" & LF);
               else
                  debugPrint ("ipctest-server: dead caller reply retired and slot reused" & LF);
               end if;
            end if;
         end if;
      elsif msg.tag.label = OP_DEADLINE_SILENT then
         -- Hold the call and never answer it: the caller's deadline ends it.
         silentHeld := saveReplyCap (SILENT_SLOT) = 1;
      elsif msg.tag.label = OP_DEADLINE_LATE then
         -- The silent call's caller timed out and is now in this call. The
         -- late answer must be refused, not delivered to this call.
         if silentHeld then
            ret := replyCap (SILENT_SLOT, makeEchoReply (LATE_MARK, from));
            lateRefused := ret = 0;
         end if;
         -- A server cannot answer with a label only the kernel gives, and
         -- the refusal leaves its reply authority in place.
         declare
            forged : Message := NULL_MESSAGE;
         begin
            forged.tag := (label => REPLY_TIMEOUT, length => 0, flags => 0,
                           reserved => 0);
            forgedRefused := replyCap (CapabilitySlot'Last, forged) = 0;
         end;
         sendReply (from, REPLY_OK, msg.words (0), boolWord (lateRefused),
                    boolWord (forgedRefused));
      elsif msg.tag.label = OP_DEADLINE_BUSY then
         -- One way: keep the server from receiving while a call waits.
         ret := syscall (SYSCALL_SLEEP, BUSY_MS);
      elsif msg.tag.label = OP_DEADLINE_QUEUED then
         -- Its caller timed out while queued: this must never be seen.
         queuedSeen := True;
         sendReply (from, REPLY_OK);
      elsif msg.tag.label = OP_DEADLINE_STATUS then
         sendReply (from, REPLY_OK, boolWord (queuedSeen));
      elsif msg.tag.label = OP_DIE then
         -- Give the submitter time to enter its combined readiness wait.
         ret := syscall (SYSCALL_SLEEP, 25);
         ret := syscall (SYSCALL_EXIT);
         loop
            ret := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      else
         sendReply (from, REPLY_ERR, Unsigned_64 (msg.tag.label));
      end if;
   end loop;
end main;
