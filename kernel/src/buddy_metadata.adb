package body Buddy_Metadata with SPARK_Mode is
   procedure Prove_Slot (Item, Highest : Frame; Kind : Table_Kind) is
   begin
      null;
   end Prove_Slot;
   procedure Prove_Separate (Left, Right : Frame; Kind : Table_Kind) is
   begin
      null;
   end Prove_Separate;
   procedure Prove_Block
     (First : Frame; Length : Buddy_Geometry.Frame_Count; Highest : Frame;
      Kind : Table_Kind) is
   begin
      null;
   end Prove_Block;
   procedure Prove_Page_Coverage
     (Highest : Frame; Kind : Table_Kind; Granule : Page_Size) is
   begin
      null;
   end Prove_Page_Coverage;
   procedure Prove_Address_Separation
     (Base : Interfaces.Unsigned_64; Left, Right, Highest : Frame;
      Kind : Table_Kind) is
   begin
      null;
   end Prove_Address_Separation;
end Buddy_Metadata;
