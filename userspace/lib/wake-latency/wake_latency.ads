------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Wake-latency measurement (IPC-012 gate): how long from a FUTEX_WAKE of a
--  word to the waiting thread running. A waker thread stamps the TSC, bumps
--  the word and wakes it; the waiter stamps the TSC when its wait returns.
--  Used by futex-check (the one-word Futex form, an A/B baseline for IPC
--  revision 2; wake-model's wait-set-check also times Word_Set forms).
--  Thread bodies avoid the secondary stack and printing.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wake_Latency is

   type Word is mod 2 ** 32 with Atomic, Size => 32;

   --  Wakes measured per run.
   Rounds : constant := 2_000;
   subtype Round_Index is Positive range 1 .. Rounds;

   --  The waiter's wait, set before the waiter starts: it waits for the
   --  word Go to leave Expected and returns when woken.
   type Wait_Procedure is access procedure (Go : access Word; Expected : Unsigned_32)
     with Convention => Ada;

   Go     : aliased Word := 0;
   Ack    : aliased Word := 0;
   Ready  : aliased Word := 0;
   Sent   : Unsigned_64 := 0 with Volatile;
   Cycles : array (Round_Index) of Unsigned_64 := [others => 0] with Volatile_Components;
   Waiter_Wait : Wait_Procedure := null;

   function TSC return Unsigned_64 with Inline;

   procedure Futex_Wake (X : access Word);
   procedure Futex_Wait (X : access Word; Expected : Unsigned_32);

   --  The waiting thread's body (Argument unused).
   procedure Waiter (Argument : Unsigned_64) with Convention => C;

   --  Run Rounds wakes against a waiter already started on Waiter (the
   --  caller waits for Ready first) and wait for each to be acknowledged.
   procedure Drive;

   type Summary is record
      P50_Us, P99_Us, Max_Us : Unsigned_64;
   end record;

   --  The measured cycles in microseconds (TSC rate from a sleep of
   --  Calibrate_Ms against the monotonic clock).
   Calibrate_Ms : constant := 50;
   function Summarise return Summary;

end Wake_Latency;
