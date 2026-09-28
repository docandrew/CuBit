------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Connection timers: one deadline per timer id in a binary min-heap with
--  back-pointers. Arm, re-arm and cancel take O(log n); the earliest
--  deadline is at the root (docs/netstack-redesign.md, "Performance").
--
--  Proved (tests/net-tcp):
--  - the root is due no later than any armed timer, so Next_Due returns
--    the earliest, and never one whose deadline has not passed;
--  - arming sets exactly that timer's deadline; cancelling disarms exactly
--    that timer; no other timer's state changes;
--  - Count is the number of armed timers.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

generic
   Max_Timers : Positive;
package Timer_Heap with SPARK_Mode is

   subtype Timer_Id is Positive range 1 .. Max_Timers;
   subtype Timer_Count is Natural range 0 .. Max_Timers;
   subtype Time is Unsigned_64;

   type Heap is private;

   function Valid (H : Heap) return Boolean with Ghost;
   function Armed (H : Heap; T : Timer_Id) return Boolean;
   function Deadline (H : Heap; T : Timer_Id) return Time with Pre => Armed (H, T);
   function Count (H : Heap) return Timer_Count;

   --  Every timer other than T is as it was.
   function Others_Unchanged (A, B : Heap; T : Timer_Id) return Boolean is
     (for all U in Timer_Id =>
        (if U /= T then
           Armed (A, U) = Armed (B, U) and then
           (if Armed (A, U) then Deadline (A, U) = Deadline (B, U))));

   procedure Initialize (H : out Heap) with
     Post => Valid (H) and then Count (H) = 0 and then
             (for all T in Timer_Id => not Armed (H, T));

   --  Set T to fire at At_Time (arming it, or moving its deadline).
   procedure Arm (H : in out Heap; T : Timer_Id; At_Time : Time) with
     Pre  => Valid (H),
     Post => Valid (H) and then Armed (H, T) and then Deadline (H, T) = At_Time and then
             Others_Unchanged (H, H'Old, T) and then
             Count (H) = Count (H'Old) + (if Armed (H'Old, T) then 0 else 1);

   procedure Cancel (H : in out Heap; T : Timer_Id) with
     Pre  => Valid (H),
     Post => Valid (H) and then not Armed (H, T) and then
             Others_Unchanged (H, H'Old, T) and then
             Count (H) = Count (H'Old) - (if Armed (H'Old, T) then 1 else 0);

   --  The earliest armed timer, if it is due by Now (it stays armed; the
   --  caller cancels or re-arms it).
   procedure Next_Due (H : Heap; Now : Time; T : out Timer_Id; Found : out Boolean) with
     Pre  => Valid (H),
     Post => (if Found then
                Armed (H, T) and then Deadline (H, T) <= Now and then
                (for all U in Timer_Id =>
                   (if Armed (H, U) then Deadline (H, T) <= Deadline (H, U)))
              else
                (for all U in Timer_Id => (if Armed (H, U) then Deadline (H, U) > Now)));

private
   subtype Index is Positive range 1 .. Max_Timers;
   type Slots is array (Index) of Timer_Id;
   --  A timer's place in the heap, or Not_Armed.
   Not_Armed : constant := 0;
   type Positions is array (Timer_Id) of Timer_Count;
   type Deadlines is array (Timer_Id) of Time;

   type Heap is record
      Slot : Slots;
      Pos  : Positions;
      Due  : Deadlines;
      Size : Timer_Count;
   end record;

   function Key (H : Heap; I : Index) return Time is (H.Due (H.Slot (I)));

   --  Armed timers among 1 .. N.
   function Armed_Count (P : Positions; N : Timer_Count) return Timer_Count is
     (if N = 0 then 0 else Armed_Count (P, N - 1) + (if P (N) /= Not_Armed then 1 else 0))
   with Ghost, Pre => N <= Max_Timers, Post => Armed_Count'Result <= N,
        Subprogram_Variant => (Decreases => N);

   --  Slot and Pos are inverse on 1 .. Size, and Size counts the armed.
   function Linked (H : Heap) return Boolean is
     ((for all I in 1 .. H.Size => H.Pos (H.Slot (I)) = I) and then
      (for all T in Timer_Id =>
         H.Pos (T) <= H.Size and then (if H.Pos (T) /= Not_Armed then H.Slot (H.Pos (T)) = T)) and then
      H.Size = Armed_Count (H.Pos, Max_Timers))
   with Ghost;

   --  Every node but the root is due no earlier than its parent.
   function Ordered (H : Heap) return Boolean is
     (for all I in 2 .. H.Size => Key (H, I / 2) <= Key (H, I))
   with Ghost;

   function Valid (H : Heap) return Boolean is (Linked (H) and then Ordered (H));

   function Armed (H : Heap; T : Timer_Id) return Boolean is (H.Pos (T) /= Not_Armed);
   function Deadline (H : Heap; T : Timer_Id) return Time is (H.Due (T));
   function Count (H : Heap) return Timer_Count is (H.Size);
end Timer_Heap;
