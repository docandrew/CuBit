-- Index arithmetic of a growable FIFO ring (Intel_GPU_Allocation_Waiters):
-- where the n-th waiter after the head lives, where the slot before the
-- head is, and how far the ring grows. Proved free of run-time errors so
-- the queue stays sound under -gnatp; the storage itself is not here.
package Intel_GPU_Waiter_Ring with SPARK_Mode, Pure is
   -- Largest ring this arithmetic admits: Head + Offset stays far inside
   -- Natural. Instances name their own ceiling at or below it.
   Size_Limit : constant := 2 ** 24;
   subtype Ring_Size is Positive range 1 .. Size_Limit;
   -- A physical slot of the ring, 0-based.
   subtype Ring_Slot is Natural range 0 .. Size_Limit - 1;
   -- How many waiters the ring holds.
   subtype Ring_Count is Natural range 0 .. Size_Limit;

   -- The slot of the waiter Offset places after Head.
   function Slot (Head : Ring_Slot; Offset : Ring_Count; Size : Ring_Size) return Ring_Slot is
     ((Head + Offset) mod Size)
     with Pre => Head < Size and then Offset < Size,
          Post => Slot'Result < Size;

   -- The slot just before Head (where a waiter put back at the head goes).
   function Before (Head : Ring_Slot; Size : Ring_Size) return Ring_Slot is
     (if Head = 0 then Size - 1 else Head - 1)
     with Pre => Head < Size,
          Post => Before'Result < Size;

   -- The next size when Size is full: double it, never past Ceiling.
   function Grown (Size, Ceiling : Ring_Size) return Ring_Size is
     (if Size > Ceiling / 2 then Ceiling else Size * 2)
     with Pre => Size < Ceiling,
          Post => Grown'Result > Size and then Grown'Result <= Ceiling;
end Intel_GPU_Waiter_Ring;
