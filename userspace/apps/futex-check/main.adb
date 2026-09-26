------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Thread and futex regression checks (docs/threads.md), meant for several
--  CPUs so that threads of this process really run at once.
--
--  THREAD_CREATE, THREAD_EXIT (with the exit word cleared and futex-woken,
--  which is how threads are joined), FUTEX_WAIT and FUTEX_WAKE. Checks a
--  futex mutex under contention, join, per-thread FS base, wait timeouts and
--  value mismatch, bad arguments, the per-process thread quota, reuse of
--  exited threads, and finally exits with threads still blocked and running
--  so the kernel must stop and reap them with the process.
--
--  Thread bodies avoid the secondary stack and printing: only the main
--  thread prints.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Futex_Check_Support; use Futex_Check_Support;

procedure Main is
   use ASCII;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("futex-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("futex-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   Stacks : array (1 .. 130) of Unsigned_64 := (others => 0);
   Result : Unsigned_64;
   Start  : Unsigned_64;
   Created : Natural;
begin
   debugPrint ("futex-check: starting" & LF);

   for I in Stacks'Range loop
      Stacks (I) := New_Stack;
   end loop;
   Check ((for all S of Stacks => S /= 0), "thread stacks allocated");

   --  Futex argument and value checks.
   Check (Futex_Wait (Mutex'Access, 5) = FUTEX_RETRY,
          "wait on a changed value returns at once");
   Check (syscall (SYSCALL_FUTEX_WAIT, Addr (Mutex'Access) + 1, 0, FOREVER) = FUTEX_FAULT,
          "unaligned futex word refused");
   Check (syscall (SYSCALL_FUTEX_WAIT, 16#FFFF_8000_0000_0000#, 0, FOREVER) = FUTEX_FAULT,
          "kernel-half futex word refused");
   Check (syscall (SYSCALL_FUTEX_WAIT, 16#0000_7000_0000_0000#, 0, FOREVER) = FUTEX_FAULT,
          "unmapped futex word refused");
   Check (Futex_Wake (Mutex'Access, 1) = 0, "wake with no waiters wakes none");
   Start := Now_Ms;
   Result := Futex_Wait (Mutex'Access, 0, Now_Ms + 30);
   Check (Result = FUTEX_TIMED_OUT and then Now_Ms - Start >= 30,
          "wait times out at its deadline");
   Check (Futex_Wait (Mutex'Access, 0, 0) = FUTEX_TIMED_OUT,
          "wait with a passed deadline times out at once");
   Check (syscall (SYSCALL_THREAD_CREATE, 0, Stacks (1), 0, 0, 0) = CREATE_FAILED,
          "thread with null entry refused");
   Check (syscall (SYSCALL_THREAD_CREATE,
                   Unsigned_64 (To_Integer (Quick'Address)),
                   16#FFFF_8000_0000_0000#, 0, 0, 0) = CREATE_FAILED,
          "thread with kernel-half stack refused");

   --  A sleeper is woken by another thread's store and wake.
   Exit_Words (1) := 1;
   Result := Spawn (Waker'Access, Stacks (1), 20, 0, Exit_Words (1)'Access);
   Check (Result /= CREATE_FAILED, "thread created");
   Result := Futex_Wait (Flag'Access, 0);
   Check (Result = FUTEX_WOKEN and then Flag = 1, "wait woken by another thread");
   Join (Exit_Words (1)'Access);
   Check (Exit_Words (1) = 0, "join through the exit word");

   --  Test 1.
   for I in 1 .. Workers loop
      Exit_Words (I) := 1;
      Result := Spawn (Worker'Access, Stacks (I), Unsigned_64 (I),
                       FS_Pattern + Unsigned_64 (I) * 16#1000#,
                       Exit_Words (I)'Access);
      if Result = CREATE_FAILED then
         Failures := Failures + 1;
         debugPrint ("futex-check: worker create FAIL" & LF);
      end if;
   end loop;
   for I in 1 .. Workers loop
      Join (Exit_Words (I)'Access);
   end loop;
   Check (Counter = Workers * Iterations, "mutex counter exact under contention");
   Check (Overlaps = 0, "mutual exclusion held");
   Check (FS_Wrong = 0, "each thread keeps its own fs base");
   Check (Mutex = 0, "mutex released");

   --  Test 2: create and exit far more threads than the quota, in waves,
   --  so exited threads are reaped and their IDs and stacks reused.
   Short_Lived := 0;
   for Wave in 1 .. 40 loop
      for I in 1 .. 8 loop
         Exit_Words (I) := 1;
         Result := Spawn (Quick'Access, Stacks (I), 1, 0, Exit_Words (I)'Access);
         if Result = CREATE_FAILED then
            Exit_Words (I) := 0;
            Failures := Failures + 1;
         end if;
      end loop;
      for I in 1 .. 8 loop
         Join (Exit_Words (I)'Access);
      end loop;
   end loop;
   Check (Short_Lived = 320, "320 short-lived threads ran and were joined");

   --  Test 3: the quota. Park threads until creation fails.
   Created := 0;
   Gate := 0;
   Parked := 0;
   for I in Stacks'Range loop
      Result := Spawn (Sleeper'Access, Stacks (I), 0, 0, null);
      exit when Result = CREATE_FAILED;
      Created := Created + 1;
   end loop;
   debugPrint ("futex-check: parked threads created before refusal:" &
               Created'Image & LF);
   Check (Created = 127, "thread quota is 128 including the main thread");
   Start := Now_Ms;
   while Natural (Parked) < Created and then Now_Ms - Start < 5_000 loop
      Ignore := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Check (Natural (Parked) = Created, "every parked thread ran");

   --  Release half of them (the wake count is honoured), then let the rest
   --  stay blocked across the process exit.
   Gate := 1;
   Result := Futex_Wake (Gate'Access, 60);
   Check (Result <= 60, "wake count bounds the waiters woken");

   --  Woken threads exit and are reaped, returning quota: two more threads
   --  can then start. They spin on other CPUs while the rest stay blocked.
   for S in 129 .. 130 loop
      Start := Now_Ms;
      loop
         Result := Spawn (Spinner'Access, Stacks (S), 0, 0, null);
         exit when Result /= CREATE_FAILED or else Now_Ms - Start > 5_000;
         Ignore := syscall (SYSCALL_SLEEP, 5);
      end loop;
      Check (Result /= CREATE_FAILED, "exited threads reaped and quota returned");
   end loop;

   if Failures = 0 then
      debugPrint ("TEST: PASS futex" & LF);
   else
      debugPrint ("TEST: FAIL futex" & LF);
   end if;
   --  The process ends with threads blocked and running.
   debugPrint ("futex-check: exiting with live threads" & LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
