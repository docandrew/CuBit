-- Aligned physical-frame spans, independent of addresses and payload links.
-- The buddy adapter supplies power-of-two spans from its allocation orders.
package Buddy_Geometry with SPARK_Mode, Pure is
   type Count is range 0 .. 2 ** 41;
   subtype Frame is Count range 0 .. 2 ** 40 - 1;
   subtype Frame_Count is Count range 1 .. 2 ** 39;
   function Fits (First : Frame; Length : Frame_Count; Last : Frame) return Boolean is
     (First <= Last and then Length - 1 <= Last - First and then
      First mod Length = 0);

   type Block is private;
   function Valid (Item : Block) return Boolean with Ghost;
   function First (Item : Block) return Frame;
   function Length (Item : Block) return Frame_Count;
   function Last (Item : Block) return Frame with Pre => Valid (Item);
   function Make (Start : Frame; Length : Frame_Count) return Block with
     Pre => Fits (Start, Length, Frame'Last),
     Post => Valid (Make'Result) and then First (Make'Result) = Start
       and then Buddy_Geometry.Length (Make'Result) = Length;

   -- Adjacent equal-sized ranges are not necessarily buddies. In particular,
   -- [1,1] and [2,2] must not merge: their union is not parent-aligned.
   function Can_Merge (Left, Right : Block) return Boolean with
     Pre => Valid (Left) and then Valid (Right);

   procedure Split (Parent : Block; Left, Right : out Block) with
     Pre => Valid (Parent) and then Length (Parent) mod 2 = 0,
     Post => Valid (Left) and then Valid (Right)
       and then Length (Left) * 2 = Length (Parent)
       and then Length (Right) = Length (Left)
       and then First (Left) = First (Parent)
       and then Last (Left) + 1 = First (Right)
       and then Last (Right) = Last (Parent)
       and then Can_Merge (Left, Right);
   function Merge (Left, Right : Block) return Block with
     Pre => Valid (Left) and then Valid (Right) and then Can_Merge (Left, Right),
     Post => Valid (Merge'Result)
       and then Length (Merge'Result) = Length (Left) * 2
       and then First (Merge'Result) = First (Left)
       and then Last (Merge'Result) = Last (Right);

   procedure Prove_Split_Merge (Parent : Block) with Ghost,
     Pre => Valid (Parent) and then Length (Parent) mod 2 = 0;
private
   type Block is record
      Start : Frame := 0;
      Span : Frame_Count := 1;
   end record;
   function First (Item : Block) return Frame is (Item.Start);
   function Length (Item : Block) return Frame_Count is (Item.Span);
   function Valid (Item : Block) return Boolean is
     (Fits (Item.Start, Item.Span, Frame'Last));
   function Last (Item : Block) return Frame is
     (Item.Start + Item.Span - 1);
   function Can_Merge (Left, Right : Block) return Boolean is
     (Length (Left) <= Frame_Count'Last / 2
      and then Length (Left) = Length (Right)
      and then Last (Left) + 1 = First (Right)
      and then First (Left) mod (Length (Left) * 2) = 0);
end Buddy_Geometry;
