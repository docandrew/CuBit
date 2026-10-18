------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Explicit deadlines (CAP-002 step 3; the rule: no hidden default
--  timeouts, Forever spelled out). Every channel API takes a Deadline and
--  none has a default.
--
--  @description
--  A deadline is an absolute monotonic millisecond (the kernel's receive,
--  call and wait deadlines), or Forever. Its kernel word is the
--  millisecond, or Unsigned_64'Last for Forever, which is what the system
--  calls take. After (relative) needs the clock: CuBit.Deadlines.Clock.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Deadlines with Pure, SPARK_Mode is

   type Deadline is private;

   --  No deadline, spelled out by the caller.
   Forever : constant Deadline;

   --  The kernel's word for Forever.
   Forever_Word : constant Unsigned_64 := Unsigned_64'Last;
   subtype Monotonic_Ms is Unsigned_64 range 0 .. Forever_Word - 1;

   function At_Monotonic (Milliseconds : Monotonic_Ms) return Deadline;

   function Is_Forever (D : Deadline) return Boolean;

   --  The word a system call takes (Forever_Word for Forever).
   function Kernel_Word (D : Deadline) return Unsigned_64;

   --  The earlier of two deadlines (Forever is the latest).
   function Earliest (A, B : Deadline) return Deadline;

   --  Whether the deadline has passed at Now (never for Forever).
   function Passed (D : Deadline; Now : Monotonic_Ms) return Boolean;

private
   type Deadline is record
      Word : Unsigned_64 := Forever_Word;
   end record;

   Forever : constant Deadline := (Word => Forever_Word);

   function At_Monotonic (Milliseconds : Monotonic_Ms) return Deadline is
     ((Word => Milliseconds));
   function Is_Forever (D : Deadline) return Boolean is (D.Word = Forever_Word);
   function Kernel_Word (D : Deadline) return Unsigned_64 is (D.Word);
   function Earliest (A, B : Deadline) return Deadline is
     (if A.Word <= B.Word then A else B);
   function Passed (D : Deadline; Now : Monotonic_Ms) return Boolean is
     (D.Word /= Forever_Word and then Now >= D.Word);
end CuBit.Deadlines;
