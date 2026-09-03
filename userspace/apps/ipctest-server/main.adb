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
   reverseFrom   : array (1 .. REVERSE_COUNT) of ProcessID :=
      (others => NO_PROCESS);
   reversePending : Natural := 0;

   pressureSlots  : array (1 .. PRESSURE_COUNT) of CapabilitySlot :=
      (30, 31, 32, 33, 34, 35, 36, 37,
       38, 39, 40, 41, 42, 43, 44, 45);
   pressureValues : array (1 .. PRESSURE_COUNT) of Unsigned_64 :=
      (others => 0);
   pressureFrom   : array (1 .. PRESSURE_COUNT) of ProcessID :=
      (others => NO_PROCESS);
   pressurePending : Natural := 0;

   oneWaySaveRejected : Boolean := False;
   doubleUseRejected  : Boolean := False;
   occupiedValue      : Unsigned_64 := 0;

   from : ProcessID;
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

   procedure sendReply
     (replyTo : ProcessID;
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
                       badge  => 0);
      replyMsg.words (0) := word0;
      replyMsg.words (1) := word1;
      replyMsg.words (2) := word2;
      pragma Unreferenced (replyTo);
      ignore := replyCap (CapabilitySlot'Last, replyMsg);
   end sendReply;

   function makeEchoReply
     (value    : Unsigned_64;
      sender   : ProcessID) return Message
   is
      replyMsg : Message := NULL_MESSAGE;
   begin
      replyMsg.tag := (label  => REPLY_OK,
                       length => 3,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words (0) := value;
      replyMsg.words (1) := value xor XOR_MAGIC;
      replyMsg.words (2) := sender;
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

   --  Leave a distinctive value live in XMM0. A newly started process must
   --  never inherit it, and every return to this process must restore it.
   loadFPUProbe;

   loop
      receive (from, msg);

      if readFPUProbe /= FPU_SENTINEL then
         debugPrint ("TEST: FAIL async-ipc fpu-server-restore" & LF);
      end if;

      if msg.tag.label = OP_ASYNC_ECHO then
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
      elsif msg.tag.label = OP_DIE then
         ret := syscall (SYSCALL_EXIT);
         loop
            ret := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      else
         sendReply (from, REPLY_ERR, Unsigned_64 (msg.tag.label));
      end if;
   end loop;
end main;
