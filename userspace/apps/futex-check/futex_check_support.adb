with System; use System;
with System.Machine_Code; use System.Machine_Code;

package body Futex_Check_Support is

   ---------------------------------------------------------------------------
   --  Atomics
   ---------------------------------------------------------------------------

   --  Compare-and-swap; returns the previous value.
   function CAS (W : access Word; Expected, Desired : Unsigned_32)
     return Unsigned_32
   is
      Previous : Unsigned_32;
   begin
      Asm ("lock cmpxchgl %2, %1",
           Outputs => (Unsigned_32'Asm_Output ("=a", Previous),
                       Word'Asm_Output ("+m", W.all)),
           Inputs  => (Unsigned_32'Asm_Input ("r", Desired),
                       Unsigned_32'Asm_Input ("a", Expected)),
           Clobber => "memory, cc", Volatile => True);
      return Previous;
   end CAS;

   function Exchange (W : access Word; Value : Unsigned_32) return Unsigned_32 is
      Previous : Unsigned_32 := Value;
   begin
      Asm ("xchgl %0, %1",
           Outputs => (Unsigned_32'Asm_Output ("+r", Previous),
                       Word'Asm_Output ("+m", W.all)),
           Clobber => "memory", Volatile => True);
      return Previous;
   end Exchange;

   procedure Add (W : access Word; Value : Unsigned_32) is
   begin
      Asm ("lock addl %1, %0",
           Outputs => Word'Asm_Output ("+m", W.all),
           Inputs  => Unsigned_32'Asm_Input ("r", Value),
           Clobber => "memory, cc", Volatile => True);
   end Add;


   ---------------------------------------------------------------------------
   --  A futex mutex (0 unlocked, 1 locked, 2 locked with waiters), the
   --  protocol Rust's std uses.
   ---------------------------------------------------------------------------
   procedure Lock (M : access Word) is
      C : Unsigned_32 := CAS (M, 0, 1);
   begin
      if C = 0 then
         return;
      end if;
      if C /= 2 then
         C := Exchange (M, 2);
      end if;
      while C /= 0 loop
         Ignore := Futex_Wait (M, 2);
         C := Exchange (M, 2);
      end loop;
   end Lock;

   procedure Unlock (M : access Word) is
   begin
      if Exchange (M, 0) = 2 then
         Ignore := Futex_Wake (M, 1);
      end if;
   end Unlock;

   ---------------------------------------------------------------------------
   --  Threads
   ---------------------------------------------------------------------------

   --  Stack memory from the heap. The kernel starts a thread with RSP as
   --  given; a SysV function expects RSP + 8 to be 16-byte aligned.
   function New_Stack return Unsigned_64 is
      Base : constant Unsigned_64 := syscall (SYSCALL_SBRK, Stack_Size);
   begin
      if Base = Unsigned_64'Last or else Base = 0 then
         return 0;
      end if;
      return ((Base + Stack_Size) and not 15) - 8;
   end New_Stack;


   function Spawn (Code : Entry_Point; Stack, Argument : Unsigned_64;
                   FS_Base : Unsigned_64; Exit_Word : access Word)
     return Unsigned_64
   is
   begin
      return syscall (SYSCALL_THREAD_CREATE,
                      Unsigned_64 (To_Integer (Code.all'Address)),
                      Stack, Argument, FS_Base,
                      (if Exit_Word = null then 0 else Addr (Exit_Word)));
   end Spawn;

   procedure Thread_Exit is
   begin
      Ignore := syscall (SYSCALL_THREAD_EXIT);
   end Thread_Exit;

   --  Join: the kernel clears the exit word, then wakes it, when the thread
   --  ends. The word starts nonzero.
   procedure Join (Exit_Word : access Word) is
      V : Unsigned_32;
   begin
      loop
         V := Unsigned_32 (Exit_Word.all);
         exit when V = 0;
         Ignore := Futex_Wait (Exit_Word, V);
      end loop;
   end Join;

   function Read_FS return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("rdfsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Value),
           Volatile => True);
      return Value;
   end Read_FS;


   ---------------------------------------------------------------------------
   --  Test 1: mutex contention across CPUs, join, FS base.
   ---------------------------------------------------------------------------

   procedure Worker (Argument : Unsigned_64) is
   begin
      for I in 1 .. Iterations loop
         Lock (Mutex'Access);
         if CAS (Inside'Access, 0, 1) /= 0 then
            Add (Overlaps'Access, 1);
         end if;
         Counter := Counter + 1;
         Inside := 0;
         Unlock (Mutex'Access);
         if I mod 1024 = 0 and then Read_FS /= FS_Pattern + Argument * 16#1000# then
            Add (FS_Wrong'Access, 1);
         end if;
      end loop;
      Thread_Exit;
   end Worker;

   ---------------------------------------------------------------------------
   --  Test 3: blocked threads (quota, then exit while blocked).
   ---------------------------------------------------------------------------

   procedure Sleeper (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      Add (Parked'Access, 1);
      while Gate = 0 loop
         Ignore := Futex_Wait (Gate'Access, 0);
      end loop;
      Thread_Exit;
   end Sleeper;

   procedure Waker (Argument : Unsigned_64) is
   begin
      Ignore := syscall (SYSCALL_SLEEP, Argument);
      Flag := 1;
      Ignore := Futex_Wake (Flag'Access, 1);
      Thread_Exit;
   end Waker;

   procedure Quick (Argument : Unsigned_64) is
   begin
      Add (Short_Lived'Access, Unsigned_32 (Argument and 1));
      Thread_Exit;
   end Quick;

   procedure Spinner (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      loop
         Add (Short_Lived'Access, 1);
      end loop;
   end Spinner;
end Futex_Check_Support;
