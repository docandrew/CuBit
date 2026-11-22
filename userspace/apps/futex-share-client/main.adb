------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared futex checks (IPC-010, headless --test futex-share): the owner.
--
--  @description
--  Grants the server two pages and checks, across the two processes:
--  malformed and foreign shared words are refused; a ping-pong of 20,000
--  rounds through two shared words completes with no wake lost, with
--  wake-plus-wait (one kernel entry per hand-off) and with a separate wake
--  and wait (timed, with system calls per round); eight waiters of one
--  shared word are woken by the server in the order they waited (FIFO per
--  key); revoking a grant wakes its waiters on both sides as
--  Futex_Revoked and later waits and wakes on it are refused; a ping-pong
--  beside CPU-bound bystanders on every CPU takes at most one CPU from
--  them.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.Monotonic;
with CuBit.Process_Events;
with CuBit.System_Facts;
with CuBit.Waits;
with Wait_ABI;
with Futex_ABI;
with Futex_Share_Support; use Futex_Share_Support;
with Startup;

procedure Main is
   use ASCII;

   OP_SHARE     : constant Unsigned_32 := 16#0A10#;
   OP_PINGPONG  : constant Unsigned_32 := 16#0A11#;
   OP_FIFO_WAKE : constant Unsigned_32 := 16#0A12#;
   OP_WAIT_END  : constant Unsigned_32 := 16#0A13#;
   OP_PEER_WAKE : constant Unsigned_32 := 16#0A15#;
   OP_CHURN     : constant Unsigned_32 := 16#0A16#;
   --  Notified grants the server takes and gives back, each leaving this
   --  process a grant-return notice; then an idle activity loop of
   --  Idle_Window_Ms may turn at most Idle_Turn_Limit times.
   Churn_Rounds    : constant := 2_000;
   Idle_Window_Ms  : constant := 300;
   Idle_Turn_Limit : constant := 50;
   Peer_Wake_Bound_Ms : constant := 2_000;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   Mode_Hand_Off : constant Unsigned_64 := 0;
   Mode_Separate : constant Unsigned_64 := 1;

   Rounds      : constant := 20_000;
   Answer_Ms   : constant := 5_000;
   Run_Ms      : constant := 30_000;
   Settle_Ms   : constant := 20;
   Window_Ms   : constant := 500;
   Page        : constant := 4_096;
   Nanoseconds_Per_Microsecond : constant := 1_000;

   Server : constant CuBit.Handles.Endpoint := Startup.Ipc_Test;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("futex-share: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("futex-share: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (V : Unsigned_64) return String is
      S : constant String := V'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Deadline return Unsigned_64 is (Deadline_After (Answer_Ms));

   --  A shared futex name with any word, well formed or not.
   function Raw (Word : Unsigned_64) return W.Futex_Name is
      Name : W.Futex_Name;
   begin
      Name.Shared := True;
      Name.Word := Word;
      return Name;
   end Raw;


   function Call (Label : Unsigned_32; A, B, C : Unsigned_64 := 0) return Boolean is
      M : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      M.tag := (label => Label, length => 3, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := A;
      M.words (1) := B;
      M.words (2) := C;
      Tag := capCall (Server, M, Deadline);
      return Tag.label = REPLY_OK;
   end Call;

   function Thread_Calls return Unsigned_64 is
     (CuBit.System_Facts.Thread_System_Calls);

   --  Ping rounds against the server's pong. True when every round was
   --  answered (no wake lost, nothing timed out).
   --  Diagnostics of the last Ping: the fastest batch of Batch_Rounds, and
   --  how often the peer was found asleep (a wake woke a waiter) and how
   --  often this side's wait slept (woken) or found the answer (retry).
   Batch_Rounds : constant := 2_000;
   Min_Batch_Us : Unsigned_64 := Unsigned_64'Last;
   Wakes_Woke, Waits_Slept, Waits_Retried : Unsigned_64 := 0;

   function Ping (Count : Unsigned_64; Mode : Unsigned_64) return Boolean is
      Ping_W : constant Word_Access := At_Offset (First_Base, Ping_At);
      Pong_W : constant Word_Access := At_Offset (First_Base, Pong_At);
      Ping_Name : constant W.Futex_Name := W.Shared_Word (First, Ping_At);
      Pong_Name : constant W.Futex_Name := W.Shared_Word (First, Pong_At);
      Base : constant Unsigned_32 := Unsigned_32 (Ping_W.all);
      --  One deadline for the whole run: computing one per round could be a
      --  system call of its own (no published clock in some profiles).
      Run_Deadline : constant Unsigned_64 := Deadline_After (Run_Ms);
      Result : Unsigned_64;
      Value : Unsigned_32;
      Batch_Start : Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Min_Batch_Us := Unsigned_64'Last;
      Wakes_Woke := 0;
      Waits_Slept := 0;
      Waits_Retried := 0;
      --  The server starts from Base: a ping written before it looks is
      --  still news to it.
      if not Call (OP_PINGPONG, Count, Mode, Unsigned_64 (Base)) then
         return False;
      end if;
      for Round in 1 .. Count loop
         Value := Base + Unsigned_32 (Round mod 2 ** 31);
         Ping_W.all := Word (Value);
         if Mode = Mode_Hand_Off then
            Result := W.Hand_Off (Ping_Name, Pong_Name, Unsigned_64 (Value - 1), Run_Deadline);
         else
            if W.Futex_Wake (Ping_Name, 1) = 1 then
               Wakes_Woke := Wakes_Woke + 1;
            end if;
            Result := W.Futex_Wait (Pong_Name, Unsigned_64 (Value - 1), Run_Deadline);
         end if;
         if Result = Wait_ABI.Futex_Woken then
            Waits_Slept := Waits_Slept + 1;
         elsif Result = Wait_ABI.Futex_Retry then
            Waits_Retried := Waits_Retried + 1;
         end if;
         while Unsigned_32 (Pong_W.all) /= Value loop
            if Result = Wait_ABI.Futex_Timed_Out or else Result = Wait_ABI.Futex_Fault then
               return False;
            end if;
            Result := W.Futex_Wait (Pong_Name, Unsigned_64 (Value - 1), Run_Deadline);
         end loop;
         if Round mod Batch_Rounds = 0 then
            declare
               Now_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
            begin
               Min_Batch_Us := Unsigned_64'Min (Min_Batch_Us, Now_Us - Batch_Start);
               Batch_Start := Now_Us;
            end;
         end if;
      end loop;
      return True;
   end Ping;

   procedure Timed_Ping (Mode : Unsigned_64; Name : String; Round_Us : out Unsigned_64) is
      Calls_Before : constant Unsigned_64 := Thread_Calls;
      Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
      Answered : constant Boolean := Ping (Rounds, Mode);
      Elapsed : constant Unsigned_64 := CuBit.Monotonic.Microseconds - Started;
      Calls : constant Unsigned_64 := Thread_Calls - Calls_Before;
   begin
      Round_Us := Elapsed / Rounds;
      Check (Answered, "ping-pong of" & Rounds'Image & " rounds, " & Name & ", no wake lost");
      debugPrint ("FUTEX-SHARE: pingpong mode=" & Name & " rounds=" & Image (Rounds) &
                  " us=" & Image (Elapsed) &
                  " ns_per_round=" & Image (Elapsed * Nanoseconds_Per_Microsecond / Rounds) &
                  " client_calls_per_round_x100=" & Image (Calls * 100 / Rounds) &
                  " min_batch_ns_per_round=" &
                  Image ((if Min_Batch_Us = Unsigned_64'Last then 0
                          else Min_Batch_Us * Nanoseconds_Per_Microsecond / Batch_Rounds)) &
                  " wakes_woke=" & Image (Wakes_Woke) & " waits_slept=" & Image (Waits_Slept) &
                  " waits_retried=" & Image (Waits_Retried) & LF);
   end Timed_Ping;

   Heap : Unsigned_64;
   Aligned : Unsigned_64;
   Ok, Ok2 : Boolean;
   Result : Unsigned_64;
   Ignore : Unsigned_64;
   Hand_Off_Us, Separate_Us : Unsigned_64 := 0;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("futex-share: starting" & LF);

   --  Two pages this process owns, granted to the server.
   Heap := syscall (SYSCALL_SBRK, 5 * Page);
   Aligned := (Heap + Page - 1) and not (Page - 1);
   First_Base := To_Address (Integer_Address (Aligned));
   Second_Base := To_Address (Integer_Address (Aligned + Page));
   for Offset in 0 .. 2 * Page / 4 - 1 loop
      At_Offset (First_Base, Offset * 4).all := 0;
   end loop;
   CuBit.Memory_Grants.Create_Via_Capability (Server, First_Base, 1, True, First, Ok);
   CuBit.Memory_Grants.Create_Via_Capability (Server, Second_Base, 1, True, Second, Ok2);
   Check (Heap /= Unsigned_64'Last and then Ok and then Ok2, "two pages granted");
   Check (Call (OP_SHARE, GR.Encode (First), GR.Encode (Second)), "server acquired them");

   --  Malformed and unaligned words, words past the grant, and slot 0.
   Check (W.Futex_Wait (Raw (Futex_ABI.Shared_Word (First.slot, 2)), 0, W.Now)
            = Wait_ABI.Futex_Fault, "unaligned shared word refused");
   Check (W.Futex_Wait (Raw (Futex_ABI.Shared_Word (First.slot, Page)), 0, W.Now)
            = Wait_ABI.Futex_Fault, "shared word past the grant refused");
   Check (W.Futex_Wait (Raw (0), 0, W.Now) = Wait_ABI.Futex_Fault,
          "slot 0 refused");
   Check (W.Futex_Wake (Raw (0), 1) = Futex_ABI.Wake_Refused,
          "wake of slot 0 refused");
   Check (W.Futex_Wait (W.Shared_Word (First, Ping_At), 7, W.Now) = Wait_ABI.Futex_Retry,
          "shared word holding another value: retry");
   Check (W.Futex_Wait (W.Shared_Word (First, Ping_At), 0, W.Now) = Wait_ABI.Futex_Timed_Out,
          "shared word holding the value, deadline Now: timed out");
   Check (W.Futex_Wake (W.Shared_Word (First, Ping_At), 1) = 0,
          "owner's wake of a shared word with no waiter: none woken");
   Check (W.Hand_Off (Raw (0), W.Shared_Word (First, Ping_At), 0, W.Now)
            = Wait_ABI.Futex_Fault, "hand-off with a refused wake does not wait");

   --  Ping-pong across the two processes (a wake racing every sleep).
   Timed_Ping (Mode_Hand_Off, "handoff", Hand_Off_Us);
   Timed_Ping (Mode_Separate, "separate", Separate_Us);

   --  FIFO per key: eight waiters, one at a time, woken by the server.
   declare
      In_Order : Boolean := True;
   begin
      Fifo_Deadline := Deadline;
      for I in Fifo_Index loop
         Ignore := Spawn (Fifo_Waiter'Access, New_Stack, Unsigned_64 (I),
                          Words.To_Pointer (Fifo_Done (I)'Address));
         --  Let it reach its wait before the next one starts.
         W.Sleep_For (Settle_Ms);
      end loop;
      Check (Call (OP_FIFO_WAKE, Fifo_Waiters), "server woke the waiters one by one");
      for I in Fifo_Index loop
         Join (Words.To_Pointer (Fifo_Done (I)'Address));
      end loop;
      for I in Fifo_Index loop
         In_Order := In_Order and then Unsigned_64 (Order (I)) = Unsigned_64 (I);
      end loop;
      Check (Fifo_Failed = 0 and then In_Order, "eight waiters woken oldest first");
   end;

   --  Revoking a grant wakes its waiters on both sides.
   declare
      Revoked_Word : constant Word_Access := At_Offset (First_Base, Revoked_At);
   begin
      Doomed_Deadline := Deadline;
      Ignore := Spawn (Doomed_Waiter'Access, New_Stack, 0,
                       Words.To_Pointer (Doomed_Done'Address));
      Check (Call (OP_WAIT_END), "server waits on the doomed page");
      W.Sleep_For (Settle_Ms);
      CuBit.Memory_Grants.Revoke (Second, Ok);
      Check (Ok, "the doomed grant revoked");
      Join (Words.To_Pointer (Doomed_Done'Address));
      Check (Unsigned_64 (Doomed_Result) = Wait_ABI.Futex_Revoked,
             "owner's waiter woken as revoked");
      while Revoked_Word.all = 0 loop
         Result := W.Futex_Wait (W.Shared_Word (First, Revoked_At), 0, Deadline);
         exit when Result = Wait_ABI.Futex_Timed_Out;
      end loop;
      Check (Unsigned_64 (Revoked_Word.all) = Wait_ABI.Futex_Revoked + 1,
             "grantee's waiter woken as revoked");
      Check (W.Futex_Wait (W.Shared_Word (Second, Doomed_At), 0, W.Now) = Wait_ABI.Futex_Fault,
             "wait on an ended grant refused");
      Check (W.Futex_Wake (W.Shared_Word (Second, Doomed_At), 1) = Futex_ABI.Wake_Refused,
             "wake on an ended grant refused");
   end;

   --  Starvation: bystanders on every CPU keep their share while a
   --  ping-pong runs: the pair takes at most one CPU's worth.
   declare
      CPUs : constant Unsigned_64 := Unsigned_64'Min
        (CuBit.System_Facts.Processor_Count, Maximum_Bystanders);
      Alone, Beside : Unsigned_64 := 0;
      Round_Us : constant Unsigned_64 := Unsigned_64'Max (Hand_Off_Us, 1);
      Started : Unsigned_64;

      function Total return Unsigned_64 is
         Sum : Unsigned_64 := 0;
      begin
         for I in 1 .. Bystander_Index (CPUs) loop
            Sum := Sum + Progress (I);
         end loop;
         return Sum;
      end Total;

      procedure Run_Bystanders (Pinging : Boolean; Count : out Unsigned_64) is
         Before : Unsigned_64;
         Answered : Boolean := True;
      begin
         Running := 1;
         for I in 1 .. Bystander_Index (CPUs) loop
            Ignore := Spawn (Bystander'Access, New_Stack, Unsigned_64 (I),
                             Words.To_Pointer (Bystander_Done (I)'Address));
         end loop;
         W.Sleep_For (Settle_Ms);
         Before := Total;
         Started := CuBit.Monotonic.Microseconds;
         if Pinging then
            Answered := Ping (Window_Ms * 1_000 / Round_Us, Mode_Hand_Off);
         end if;
         loop
            exit when CuBit.Monotonic.Microseconds - Started >= Window_Ms * 1_000;
            W.Sleep_For (1);
         end loop;
         Count := (Total - Before) * 1_000_000 / (CuBit.Monotonic.Microseconds - Started);
         Running := 0;
         for I in 1 .. Bystander_Index (CPUs) loop
            Join (Words.To_Pointer (Bystander_Done (I)'Address));
            Bystander_Done (I) := 1;
         end loop;
         if Pinging then
            Check (Answered, "ping-pong beside bystanders answered");
         end if;
      end Run_Bystanders;
   begin
      Run_Bystanders (False, Alone);
      Run_Bystanders (True, Beside);
      debugPrint ("FUTEX-SHARE: starvation cpus=" & Image (CPUs) & " alone_per_s=" & Image (Alone) &
                  " beside_per_s=" & Image (Beside) &
                  " share_x1000=" & Image (Beside * 1_000 / Unsigned_64'Max (Alone, 1)) & LF);
      --  At most one CPU's worth taken from CPUs bystanders.
      Check (CPUs < 2 or else Beside * CPUs >= Alone * (CPUs - 1),
             "bystanders keep their share beside a ping-pong");
   end;

   --  A peer wake (IPC-010 option B): the server wakes this process's wake
   --  word through a grant made with the notify flag, while it sleeps in an
   --  ordinary activity wait; through a grant without the flag it may not.
   declare
      Notified_Base : constant System.Address := To_Address (Integer_Address (Aligned + 2 * Page));
      Notified : GR.Reference;
      M : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Started : Unsigned_64;
      Woke : Activity_Result;
   begin
      At_Offset (Notified_Base, 0).all := 0;
      CuBit.Memory_Grants.Create_Via_Capability
        (Server, Notified_Base, 1, False, Notified, Ok, notify => True);
      Check (Ok, "a notified grant made");
      M.tag := (label => OP_PEER_WAKE, length => 1, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := GR.Encode (Notified);
      Tag := capCall (Server, M, Deadline);
      Check (Tag.label = REPLY_OK and then M.words (0) = 1,
             "a peer wake through a grant without the notify flag refused");
      Started := CuBit.Monotonic.Microseconds;
      Woke := Wait_For_Activity_Until (Deadline_After (Answer_Ms));
      Check (Woke = Work_Available and then
             CuBit.Monotonic.Microseconds - Started < Peer_Wake_Bound_Ms * 1_000,
             "a peer wake ends an activity wait");
   end;

   --  A grant-return notice is counted on the readiness page when it is
   --  posted and its doorbell rings once grantLock is free; a thread that
   --  takes it in between must not leave the doorbell answering every
   --  activity wait with Work (devmgr spun 1.4 million turns a second).
   --  A second thread takes notices as they come while the server returns
   --  notified grants; then an idle activity loop must sleep.
   declare
      Churn_Base : constant System.Address := To_Address (Integer_Address (Aligned + 3 * Page));
      Churned : GR.Reference;
      M : Message;
      Tag : MessageTag;
      Made, All_Given_Back : Boolean := True;
      Turns : Unsigned_64 := 0;
      Idle_Until : Unsigned_64;
   begin
      Notice_Poller_Stop := 0;
      Ignore := Spawn (Notice_Poller'Access, New_Stack, 0,
                       Words.To_Pointer (Notice_Poller_Done'Address));
      for I in 1 .. Churn_Rounds loop
         CuBit.Memory_Grants.Create_Via_Capability
           (Server, Churn_Base, 1, False, Churned, Made, notify => True);
         M := NULL_MESSAGE;
         M.tag := (label => OP_CHURN, length => 1, protocol => 0, ipc => <>, reserved => 0);
         M.words (0) := GR.Encode (Churned);
         Tag := capCall (Server, M, Deadline);
         All_Given_Back := All_Given_Back and then Made and then Tag.label = REPLY_OK;
      end loop;
      Notice_Poller_Stop := 1;
      Join (Words.To_Pointer (Notice_Poller_Done'Address));
      Idle_Until := Deadline_After (Idle_Window_Ms);
      loop
         Turns := Turns + 1;
         CuBit.Process_Events.Serve_Defaults;
         exit when Wait_For_Activity_Until (Idle_Until) = Deadline_Reached
           or else Turns > Idle_Turn_Limit;
      end loop;
      debugPrint ("futex-share: idle turns after notices=" & Image (Turns) & LF);
      Check (All_Given_Back, "2000 notified grants taken and given back");
      Check (Turns <= Idle_Turn_Limit,
             "an idle activity loop sleeps after grant-return notices");
   end;

   if Failures = 0 then
      debugPrint ("futex-share: ALL PASS" & LF);
   else
      debugPrint ("futex-share: SOME FAIL" & LF);
   end if;
   loop
      W.Sleep_For (1_000);
   end loop;
end Main;
