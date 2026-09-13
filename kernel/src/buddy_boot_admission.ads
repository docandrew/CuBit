pragma Ada_2022;
with Buddy_Geometry;
with Interfaces;

-- The boot high-water mark is INCLUSIVE. Firmware-region validation and the
-- fact that all boot allocations lie at/below Highest remain caller duties.
package Buddy_Boot_Admission with SPARK_Mode, Pure is
   subtype Frame is Buddy_Geometry.Frame;
   use type Buddy_Geometry.Count;
   use type Interfaces.Unsigned_64;
   type Admission_Source is (Boot_Bitmap, Unallocated_Tail);

   subtype Word_Count is Buddy_Geometry.Count range 1 .. 2 ** 34;
   subtype Word_Index is Buddy_Geometry.Count range 0 .. Word_Count'Last - 1;
   subtype Bit_Index is Natural range 0 .. 63;
   function Last_Frame (Words : Word_Count) return Frame is (Words * 64 - 1)
     with Static, Inline_Always;
   function Word_Of (Item : Frame) return Word_Index is
     (Word_Index (Interfaces.Unsigned_64 (Item) / 64))
     with Inline_Always;
   function Bit_Of (Item : Frame) return Bit_Index is
     (Bit_Index (Interfaces.Unsigned_64 (Item) mod 64))
     with Inline_Always;

   procedure Prove_Word_Bound (Item : Frame; Words : Word_Count) with Ghost,
     Pre => Item <= Last_Frame (Words),
     Post => Word_Of (Item) < Words and then
       Word_Of (Item) * 64 + Buddy_Geometry.Count (Bit_Of (Item)) = Item;

   function Source_Of (Item, Highest : Frame) return Admission_Source is
     (if Item <= Highest then Boot_Bitmap else Unallocated_Tail)
     with Inline_Always;

   procedure Prove_Whole_Tail (First, Last, Highest : Frame) with Ghost,
     Pre => First <= Last and then Source_Of (First, Highest) = Unallocated_Tail,
     Post => (for all Item in First .. Last =>
       Source_Of (Item, Highest) = Unallocated_Tail);

   procedure Prove_Bitmap_Bound (Item, Highest, Bitmap_Last : Frame) with Ghost,
     Pre => Highest <= Bitmap_Last and then Source_Of (Item, Highest) = Boot_Bitmap,
     Post => Item <= Bitmap_Last;

   procedure Prove_Boundary (Highest : Frame) with Ghost,
     Post => Source_Of (Highest, Highest) = Boot_Bitmap;
end Buddy_Boot_Admission;
