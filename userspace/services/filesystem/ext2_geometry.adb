------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Volume geometry (see the spec).
------------------------------------------------------------------------------
package body Ext2_Geometry with SPARK_Mode is

   function Supported (Fields : Superblock_Fields) return Boolean is
      Size, Inode_Bytes : Count_32;
   begin
      if Fields.Signature /= Signature or else
        Fields.Block_Shift > Maximum_Block_Shift or else
        Fields.Block_Count = 0 or else
        Fields.Block_Count <= Fields.First_Data_Block or else
        Fields.Inode_Count = 0 or else
        Fields.Blocks_Per_Group = 0 or else
        Fields.Inodes_Per_Group = 0
      then
         return False;
      end if;
      Size := Block_Bytes_Of (Fields.Block_Shift);
      Inode_Bytes := Inode_Bytes_Of (Fields);
      return Inode_Bytes >= Good_Old_Inode_Bytes and then
        Inode_Bytes <= Size and then
        Power_Of_Two (Inode_Bytes) and then
        Fields.Blocks_Per_Group <= Size * Bits_Per_Byte and then
        Fields.Inodes_Per_Group <= Size * Bits_Per_Byte and then
        Fields.Free_Blocks <= Fields.Block_Count - Fields.First_Data_Block and then
        Fields.Free_Inodes <= Fields.Inode_Count;
   end Supported;

   --  Q <= N / D implies Q * D <= N (ghost).
   procedure Lemma_Quotient_Product (Q, N, D : Count_32)
     with Ghost, Global => null,
          Pre  => D > 0 and then Q <= N / D,
          Post => Long_Long_Integer (Q) * Long_Long_Integer (D) <= Long_Long_Integer (N);
   procedure Lemma_Quotient_Product (Q, N, D : Count_32) is
   begin
      pragma Assert (Long_Long_Integer (Q) * Long_Long_Integer (D) <=
                     Long_Long_Integer (N / D) * Long_Long_Integer (D));
   end Lemma_Quotient_Product;

   function Group_First_Block (Shape : Geometry; Group : Group_Number) return Block_Number is
      Last : constant Count_32 := Shape.Block_Count - Shape.First_Data_Block - 1;
   begin
      Lemma_Quotient_Product (Count_32 (Group), Last, Shape.Blocks_Per_Group);
      return Block_Number (Shape.First_Data_Block + Count_32 (Group) * Shape.Blocks_Per_Group);
   end Group_First_Block;

end Ext2_Geometry;
