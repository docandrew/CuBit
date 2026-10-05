------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Time arithmetic for the libc's system calls (docs/c-removal.md): C
--  timespecs and timevals to the kernel's millisecond and microsecond
--  clocks, and back.
--
--  @description
--  The kernel's clocks are Unsigned_64 counts since boot. A deadline past
--  their range saturates to Forever, the deadline that never comes, rather
--  than wrapping (the C this replaces multiplied seconds unchecked, signed
--  overflow included). Proved (tests/libc-ada): no overflow, and every
--  result rounds toward the later time, so a timeout never ends early.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;

package CuBit.Libc_Time with Pure, SPARK_Mode is

   Forever : constant Unsigned_64 := Unsigned_64'Last;

   Milliseconds_Per_Second : constant := 1_000;
   Microseconds_Per_Second : constant := 1_000_000;
   Nanoseconds_Per_Second  : constant := 1_000_000_000;
   Microseconds_Per_Millisecond : constant := 1_000;
   Nanoseconds_Per_Microsecond  : constant := 1_000;
   Nanoseconds_Per_Millisecond  : constant := 1_000_000;

   --  POSIX: a valid time has a non-negative second count and a fraction
   --  below one second (EINVAL otherwise).
   function Valid (T : Timespec) return Boolean is
     (T.Seconds >= 0 and then T.Nanoseconds in 0 .. Nanoseconds_Per_Second - 1);
   function Valid (T : Timeval) return Boolean is
     (T.Seconds >= 0
      and then T.Microseconds in 0 .. Microseconds_Per_Second - 1);

   function Saturating_Add (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B)
   with Post => Saturating_Add'Result >= A
                and then Saturating_Add'Result >= B;

   --  Whole units, rounded up; Forever if too large.
   function Milliseconds (T : Timespec) return Unsigned_64
   with Pre => Valid (T);
   function Microseconds (T : Timespec) return Unsigned_64
   with Pre => Valid (T);
   function Milliseconds (T : Timeval) return Unsigned_64
   with Pre => Valid (T);

   --  A deadline on the millisecond clock Duration from Now.
   function After (Now, Duration : Unsigned_64) return Unsigned_64 renames
     Saturating_Add;

   --  An absolute monotonic time (microseconds since boot) as a deadline on
   --  the millisecond clock, given both clocks now.
   function Monotonic_Deadline
     (Now_Milliseconds, Now_Microseconds, At_Microseconds : Unsigned_64)
      return Unsigned_64
   is (if At_Microseconds <= Now_Microseconds then Now_Milliseconds
       else Saturating_Add
              (Now_Milliseconds,
               (At_Microseconds - Now_Microseconds) / Microseconds_Per_Millisecond
               + (if (At_Microseconds - Now_Microseconds)
                       mod Microseconds_Per_Millisecond = 0 then 0 else 1)));

   --  An absolute wall-clock time as a deadline on a clock that reads Now
   --  while the wall clock reads Wall (same units).
   function Wall_Deadline (Now, Wall, At_Time : Unsigned_64) return Unsigned_64
   is (if At_Time <= Wall then Now else Saturating_Add (Now, At_Time - Wall));

   function From_Milliseconds (Count : Unsigned_64) return Timespec
   with Post => Valid (From_Milliseconds'Result);
   function From_Microseconds (Count : Unsigned_64) return Timespec
   with Post => Valid (From_Microseconds'Result);

end CuBit.Libc_Time;
