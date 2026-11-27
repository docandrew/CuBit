------------------------------------------------------------------------------
--  Shared state and thread bodies for notify-check (library level, so the
--  thread entry points need no trampolines). Thread bodies use no secondary
--  stack: they share the main thread's thread area.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles.Notifications;
with CuBit.Timing_Histograms;

package Notify_Check_Support is

   package N renames CuBit.Handles.Notifications;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   type Flag is new Boolean with Atomic;

   --  A thread's start: Create_Thread with a fresh stack; Exit_Word (which
   --  starts nonzero) is cleared and woken when it ends.
   type Entry_Point is access procedure (Argument : Unsigned_64) with Convention => C;
   function Spawn (Code : Entry_Point; Argument : Unsigned_64; Exit_Word : access Word)
     return Boolean;
   procedure Join (Exit_Word : access Word);

   function Now_Us return Unsigned_64;

   ---------------------------------------------------------------------------
   --  The waiter thread: one Wait on Target, its result kept.
   ---------------------------------------------------------------------------
   Target        : N.Waitable;
   Target_Result : Unsigned_64 := 0 with Volatile;
   Waiting       : Flag := False;
   procedure Wait_Once (Argument : Unsigned_64) with Convention => C;

   --  The waker thread: one Wake of By after Delay_Us.
   By       : N.Waker;
   Delay_Us : Unsigned_64 := 0;
   procedure Wake_Later (Argument : Unsigned_64) with Convention => C;

   --  A private futex word: the sleeper waits while it holds 0.
   Gate        : aliased Word := 0;
   Gate_Result : Unsigned_64 := 0 with Volatile;
   procedure Sleep_On_Gate (Argument : Unsigned_64) with Convention => C;

   ---------------------------------------------------------------------------
   --  The wake-flood benchmark: a server thread answers a victim's ping on
   --  its own notification; hostile threads spin Wake on the server's.
   ---------------------------------------------------------------------------
   Victim_Badge  : constant Unsigned_64 := 1;
   Hostile_Badge : constant Unsigned_64 := 2;
   Stop_Badge    : constant Unsigned_64 := 4;
   Server_In     : N.Waitable;     --  the server waits here
   Server_Reply  : N.Waker;        --  the server answers the victim here
   Victim_In     : N.Waitable;     --  the victim waits here
   Victim_Ping   : N.Waker;        --  badge Victim_Badge on Server_In
   Hostile_Ping  : N.Waker;        --  badge Hostile_Badge on Server_In
   Stop_Ping     : N.Waker;        --  badge Stop_Badge on Server_In
   Flooding      : Flag := False;
   Hostile_Wakes : Unsigned_64 := 0 with Volatile;
   Server_Turns  : Unsigned_64 := 0 with Volatile;
   procedure Serve (Argument : Unsigned_64) with Convention => C;
   procedure Flood (Argument : Unsigned_64) with Convention => C;

   --  Victim round trips: Rounds pings, each timed (TSC ticks).
   procedure Ping_Pong (Rounds : Positive; Samples : out CuBit.Timing_Histograms.Histogram;
                        Failures : out Natural);

end Notify_Check_Support;
