package body Buddy_Geometry with SPARK_Mode is
   procedure Prove_Half_Alignment (Start : Frame; Half : Frame_Count) with
     Ghost, Pre => Start mod (2 * Half) = 0,
     Post => Start mod Half = 0 and then (Start + Half) mod Half = 0
   is
   begin
      pragma Assert (Start = (Start / (2 * Half)) * (2 * Half));
      pragma Assert (Start = ((Start / (2 * Half)) * 2) * Half);
   end Prove_Half_Alignment;

   function Make (Start : Frame; Length : Frame_Count) return Block is
   begin
      return (Start => Start, Span => Length);
   end Make;

   procedure Split (Parent : Block; Left, Right : out Block) is
      Half : constant Frame_Count := Parent.Span / 2;
   begin
      Prove_Half_Alignment (Parent.Start, Half);
      Left := (Start => Parent.Start, Span => Half);
      Right := (Start => Parent.Start + Half, Span => Half);
   end Split;

   function Merge (Left, Right : Block) return Block is
   begin
      return (Start => Left.Start, Span => Left.Span * 2);
   end Merge;

   procedure Prove_Split_Merge (Parent : Block) is
      Left, Right : Block;
   begin
      Split (Parent, Left, Right);
      pragma Assert (Merge (Left, Right) = Parent);
   end Prove_Split_Merge;
end Buddy_Geometry;
