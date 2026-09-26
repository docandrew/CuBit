with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;

--  Atomics, the futex mutex, shared state and thread bodies for futex-check.
--  Library-level, so thread entry points need no trampolines.
package Futex_Check_Support is

   SYSCALL_THREAD_CREATE : constant Unsigned_64 := 90;
   SYSCALL_THREAD_EXIT   : constant Unsigned_64 := 91;
   SYSCALL_FUTEX_WAIT    : constant Unsigned_64 := 92;
   SYSCALL_FUTEX_WAKE    : constant Unsigned_64 := 93;

   FUTEX_WOKEN     : constant Unsigned_64 := 0;
   FUTEX_RETRY     : constant Unsigned_64 := 1;
   FUTEX_TIMED_OUT : constant Unsigned_64 := 2;
   FUTEX_FAULT     : constant Unsigned_64 := Unsigned_64'Last;
   FOREVER         : constant Unsigned_64 := Unsigned_64'Last;
   CREATE_FAILED   : constant Unsigned_64 := Unsigned_64'Last;

   Ignore : Unsigned_64;

   type Word is mod 2 ** 32 with Atomic, Size => 32;

   --  Compare-and-swap; returns the previous value.
   function CAS (W : access Word; Expected, Desired : Unsigned_32)
     return Unsigned_32;
   function Exchange (W : access Word; Value : Unsigned_32) return Unsigned_32;
   procedure Add (W : access Word; Value : Unsigned_32);

   function Addr (W : access Word) return Unsigned_64 is
     (Unsigned_64 (To_Integer (W.all'Address)));

   function Futex_Wait (W : access Word; Expected : Unsigned_32;
                        Deadline : Unsigned_64 := FOREVER) return Unsigned_64 is
     (syscall (SYSCALL_FUTEX_WAIT, Addr (W), Unsigned_64 (Expected), Deadline));

   function Futex_Wake (W : access Word; Count : Unsigned_64) return Unsigned_64 is
     (syscall (SYSCALL_FUTEX_WAKE, Addr (W), Count));

   --  A futex mutex (0 unlocked, 1 locked, 2 locked with waiters), the
   --  protocol Rust's std uses.
   procedure Lock (M : access Word);
   procedure Unlock (M : access Word);

   Stack_Size : constant := 16 * 1024;
   function New_Stack return Unsigned_64;

   type Entry_Point is access procedure (Argument : Unsigned_64)
     with Convention => C;

   function Spawn (Code : Entry_Point; Stack, Argument : Unsigned_64;
                   FS_Base : Unsigned_64; Exit_Word : access Word)
     return Unsigned_64;
   procedure Thread_Exit;
   procedure Join (Exit_Word : access Word);
   function Read_FS return Unsigned_64;
   function Now_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   --  Test 1: mutex contention across CPUs, join, FS base.
   Workers    : constant := 8;
   Iterations : constant := 20_000;

   Mutex     : aliased Word := 0;
   Counter   : Unsigned_64 := 0 with Volatile;
   Inside    : aliased Word := 0;
   Overlaps  : aliased Word := 0;
   FS_Wrong  : aliased Word := 0;
   Exit_Words : array (1 .. Workers) of aliased Word;

   FS_Pattern : constant Unsigned_64 := 16#0000_4F53_0000_0000#;
   procedure Worker (Argument : Unsigned_64) with Convention => C;

   --  Blocked threads (quota, then exit while blocked).
   Gate   : aliased Word := 0;
   Parked : aliased Word := 0;
   procedure Sleeper (Argument : Unsigned_64) with Convention => C;

   Flag : aliased Word := 0;
   procedure Waker (Argument : Unsigned_64) with Convention => C;

   Short_Lived : aliased Word := 0;
   procedure Quick (Argument : Unsigned_64) with Convention => C;
   procedure Spinner (Argument : Unsigned_64) with Convention => C;

end Futex_Check_Support;
