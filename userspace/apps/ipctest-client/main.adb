------------------------------------------------------------------------------
--  CuBit headless IPC regression client
--
--  Submits multiple async capability calls and verifies completions preserve
--  request identity, token identity, and reply payload identity.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   CAP_SLOT_IPCTEST : constant CapabilitySlot := 18;
   CAP_SLOT_EMPTY   : constant CapabilitySlot := 19;

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
   REPLY_OK          : constant Unsigned_32 := 16#F000#;

   REQUEST_COUNT : constant Natural := 3;
   PRESSURE_COUNT : constant Natural := 16;
   TOKEN_BASE    : constant Unsigned_64 := 16#A510_0000#;
   DOUBLE_TOKEN  : constant Unsigned_64 := 16#D0B1_E000#;
   PRESSURE_TOKEN_BASE : constant Unsigned_64 := 16#BEEF_0000#;
   RECOVERY_TOKEN : constant Unsigned_64 := 16#BEEF_F00D#;
   DEATH_TOKEN    : constant Unsigned_64 := 16#DEAD_D1ED#;
   OCCUPIED_HOLD_TOKEN  : constant Unsigned_64 := 16#0CC0_0001#;
   OCCUPIED_PROBE_TOKEN : constant Unsigned_64 := 16#0CC0_0002#;
   XOR_MAGIC     : constant Unsigned_64 := 16#C0B1_7000#;
   FPU_SENTINEL  : constant Unsigned_64 := 16#C11E_17F0_CAFE_5AFE#;

   seen       : array (1 .. REQUEST_COUNT) of Boolean := (others => False);
   pressureSeen : array (1 .. PRESSURE_COUNT) of Boolean :=
      (others => False);
   completed  : Natural := 0;
   pressureCompleted : Natural := 0;
   ok         : Boolean := True;
   submitOk   : Boolean;
   completion : CompletionEntry;
   ret        : Unsigned_64;
   firstReverseValue : Unsigned_64 := 0;
   occupiedCompleted : Natural := 0;
   occupiedHoldSeen  : Boolean := False;
   occupiedProbeSeen : Boolean := False;

   procedure fail (reason : String) is
   begin
      debugPrint ("TEST: FAIL async-ipc ");
      debugPrint (reason);
      debugPrint (LF & "");
      ok := False;
   end fail;

   procedure loadFPUProbe is
   begin
      --  The projects are compiled with -mno-sse/-mno-sse2, so XMM0 is
      --  reserved exclusively for this context-isolation regression probe.
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

begin
   --  This must be the first explicit FP/SIMD access made by the process.
   --  A zero result proves that the initialized process image was restored,
   --  rather than exposing XMM state left by the previously running server.
   if readFPUProbe /= 0 then
      fail ("fpu-initial-isolation");
   end if;

   debugPrint ("ipctest-client: starting" & LF);

   declare
      msg : Message := NULL_MESSAGE;
   begin
      msg.tag := (label  => OP_ASYNC_ECHO,
                  length => 1,
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := 99;
      submitOk := capSubmit (CAP_SLOT_EMPTY, msg, TOKEN_BASE + 99);
      if submitOk then
         fail ("cap-denial");
      end if;
   end;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_ONEWAY_PROBE,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := 77;
         submitOk := capSubmit (CAP_SLOT_IPCTEST, msg, NO_COMPLETION_TOKEN);
         if not submitOk then
            fail ("oneway-submit");
         end if;
      end;
   end if;

   if ok then
      ret := syscall (SYSCALL_SLEEP, 50);
      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);
      if ret /= 0 then
         fail ("oneway-completion");
      end if;
   end if;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_DOUBLE_REPLY,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := 42;
         submitOk := capSubmit (CAP_SLOT_IPCTEST, msg, DOUBLE_TOKEN);
         if not submitOk then
            fail ("double-submit");
         end if;
      end;
   end if;

   for attempt in 1 .. 300 loop
      exit when not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);

      if ret = 1 then
         if completion.status /= COMPLETION_OK then
            fail ("double-status");
         elsif completion.token /= DOUBLE_TOKEN then
            fail ("double-token");
         elsif completion.msg.tag.label /= REPLY_OK then
            fail ("double-label");
         elsif completion.msg.words (0) /= 42 then
            fail ("double-payload");
         end if;
         exit;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;

      if attempt = 300 then
         fail ("double-timeout");
      end if;
   end loop;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_OCCUPIED_HOLD,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := 501;
         submitOk := capSubmit
           (CAP_SLOT_IPCTEST, msg, OCCUPIED_HOLD_TOKEN);
         if not submitOk then
            fail ("occupied-hold-submit");
         end if;

         msg.tag.label := OP_OCCUPIED_PROBE;
         msg.words (0) := 502;
         submitOk := capSubmit
           (CAP_SLOT_IPCTEST, msg, OCCUPIED_PROBE_TOKEN);
         if not submitOk then
            fail ("occupied-probe-submit");
         end if;
      end;
   end if;

   for attempt in 1 .. 300 loop
      exit when occupiedCompleted = 2 or not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);
      if ret = 1 then
         if completion.status /= COMPLETION_OK or else
            completion.msg.tag.label /= REPLY_OK
         then
            fail ("occupied-completion-status");
         elsif completion.token = OCCUPIED_HOLD_TOKEN then
            if occupiedHoldSeen or else completion.msg.words (0) /= 501 then
               fail ("occupied-held-authority");
            else
               occupiedHoldSeen := True;
               occupiedCompleted := occupiedCompleted + 1;
            end if;
         elsif completion.token = OCCUPIED_PROBE_TOKEN then
            if occupiedProbeSeen or else completion.msg.words (0) /= 502 then
               fail ("occupied-current-authority");
            else
               occupiedProbeSeen := True;
               occupiedCompleted := occupiedCompleted + 1;
            end if;
         else
            fail ("occupied-token");
         end if;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;

      if attempt = 300 then
         fail ("occupied-timeout");
      end if;
   end loop;

   for i in 1 .. PRESSURE_COUNT loop
      if ok then
         declare
            msg : Message := NULL_MESSAGE;
         begin
            msg.tag := (label  => OP_PRESSURE_HOLD,
                        length => 1,
                        flags  => 0,
                        badge  => 0);
            msg.words (0) := Unsigned_64 (i);

            submitOk := capSubmit (
               CAP_SLOT_IPCTEST,
               msg,
               PRESSURE_TOKEN_BASE + Unsigned_64 (i));

            if not submitOk then
               fail ("pressure-submit");
            end if;
         end;
      end if;
   end loop;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_PRESSURE_HOLD,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := Unsigned_64 (PRESSURE_COUNT + 1);

         submitOk := capSubmit (
            CAP_SLOT_IPCTEST,
            msg,
            PRESSURE_TOKEN_BASE + Unsigned_64 (PRESSURE_COUNT + 1));

         if submitOk then
            fail ("pressure-overflow");
         end if;
      end;
   end if;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_PRESSURE_RELEASE,
                     length => 0,
                     flags  => 0,
                     badge  => 0);
         submitOk := capSubmit (
            CAP_SLOT_IPCTEST,
            msg,
            NO_COMPLETION_TOKEN);
         if not submitOk then
            fail ("pressure-release-submit");
         end if;
      end;
   end if;

   for attempt in 1 .. 500 loop
      exit when pressureCompleted = PRESSURE_COUNT or not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);

      if ret = 1 then
         if completion.status /= COMPLETION_OK then
            fail ("pressure-status");
         elsif completion.msg.tag.label /= REPLY_OK then
            fail ("pressure-label");
         elsif completion.requestId = 0 then
            fail ("pressure-request-id");
         elsif completion.token <= PRESSURE_TOKEN_BASE or else
               completion.token >
                  PRESSURE_TOKEN_BASE + Unsigned_64 (PRESSURE_COUNT)
         then
            fail ("pressure-token");
         else
            declare
               idx : constant Natural :=
                  Natural (completion.token - PRESSURE_TOKEN_BASE);
            begin
               if completion.msg.words (0) /= Unsigned_64 (idx) then
                  fail ("pressure-payload");
               elsif completion.msg.words (1) /=
                     (Unsigned_64 (idx) xor XOR_MAGIC)
               then
                  fail ("pressure-check");
               elsif pressureSeen (idx) then
                  fail ("pressure-duplicate");
               else
                  pressureSeen (idx) := True;
                  pressureCompleted := pressureCompleted + 1;
               end if;
            end;
         end if;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;
   end loop;

   if ok and then pressureCompleted /= PRESSURE_COUNT then
      fail ("pressure-timeout");
   end if;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_ASYNC_ECHO,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := 123;
         submitOk := capSubmit (CAP_SLOT_IPCTEST, msg, RECOVERY_TOKEN);
         if not submitOk then
            fail ("pressure-recovery-submit");
         end if;
      end;
   end if;

   for attempt in 1 .. 300 loop
      exit when not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);

      if ret = 1 then
         if completion.status /= COMPLETION_OK then
            fail ("pressure-recovery-status");
         elsif completion.token /= RECOVERY_TOKEN then
            fail ("pressure-recovery-token");
         elsif completion.msg.tag.label /= REPLY_OK then
            fail ("pressure-recovery-label");
         elsif completion.msg.words (0) /= 123 then
            fail ("pressure-recovery-payload");
         end if;
         exit;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;

      if attempt = 300 then
         fail ("pressure-recovery-timeout");
      end if;
   end loop;

   for i in 1 .. REQUEST_COUNT loop
      if ok then
         declare
            msg : Message := NULL_MESSAGE;
         begin
            msg.tag := (label  => OP_REVERSE_ECHO,
                        length => 1,
                        flags  => 0,
                        badge  => 0);
            msg.words (0) := Unsigned_64 (i);

            submitOk := capSubmit (
               CAP_SLOT_IPCTEST,
               msg,
               TOKEN_BASE + Unsigned_64 (i));

            if not submitOk then
               fail ("submit");
            end if;
         end;
      end if;
   end loop;

   for attempt in 1 .. 300 loop
      exit when completed = REQUEST_COUNT or not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);

      if ret = 1 then
         if completion.status /= COMPLETION_OK then
            fail ("reply-status");
         elsif completion.msg.tag.label /= REPLY_OK then
            fail ("reply-label");
         elsif completion.requestId = 0 then
            fail ("request-id");
         elsif completion.token <= TOKEN_BASE or else
               completion.token > TOKEN_BASE + Unsigned_64 (REQUEST_COUNT)
         then
            fail ("token-range");
         else
            declare
               idx : constant Natural :=
                  Natural (completion.token - TOKEN_BASE);
            begin
               if completion.msg.words (0) /= Unsigned_64 (idx) then
                  fail ("payload-id");
               elsif completion.msg.words (1) /=
                     (Unsigned_64 (idx) xor XOR_MAGIC)
               then
                  fail ("payload-check");
               elsif seen (idx) then
                  fail ("duplicate");
               else
                  seen (idx) := True;
                  completed := completed + 1;
                  if completed = 1 then
                     firstReverseValue := completion.msg.words (0);
                  end if;
               end if;
            end;
         end if;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;
   end loop;

   if ok and then completed /= REQUEST_COUNT then
      fail ("timeout");
   end if;

   if ok and then firstReverseValue /= Unsigned_64 (REQUEST_COUNT) then
      fail ("reverse-order");
   end if;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
         tag : MessageTag;
      begin
         msg.tag := (label  => OP_STATUS,
                     length => 0,
                     flags  => 0,
                     badge  => 0);
         loadFPUProbe;
         tag := capCall (CAP_SLOT_IPCTEST, msg);
         if readFPUProbe /= FPU_SENTINEL then
            fail ("fpu-direct-switch");
         elsif tag.label /= REPLY_OK then
            fail ("status-label");
         elsif msg.words (0) /= 1 then
            fail ("oneway-reply-cap");
         elsif msg.words (1) /= 1 then
            fail ("reply-cap-single-use");
         elsif msg.words (2) /= 0 then
            fail ("reverse-pending");
         end if;
      end;
   end if;

   if ok then
      declare
         msg : Message := NULL_MESSAGE;
      begin
         msg.tag := (label  => OP_DIE,
                     length => 0,
                     flags  => 0,
                     badge  => 0);
         submitOk := capSubmit (CAP_SLOT_IPCTEST, msg, DEATH_TOKEN);
         if not submitOk then
            fail ("death-submit");
         end if;
      end;
   end if;

   for attempt in 1 .. 300 loop
      exit when not ok;

      completion := NULL_COMPLETION;
      ret := Poll_Completion (completion'Address);

      if ret = 1 then
         if completion.token /= DEATH_TOKEN then
            fail ("death-token");
         elsif completion.status /= COMPLETION_TARGET_DIED then
            fail ("death-status");
         elsif completion.from = 0 then
            fail ("death-from");
         end if;
         exit;
      else
         ret := syscall (SYSCALL_SLEEP, 10);
      end if;

      if attempt = 300 then
         fail ("death-timeout");
      end if;
   end loop;

   if ok then
      debugPrint ("TEST: PASS async-ipc" & LF);
   end if;

   loop
      ret := syscall (SYSCALL_SLEEP, 1000);
   end loop;
end main;
