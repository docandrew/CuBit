package body Block_Paths with SPARK_Mode is
   --  Each geometry in its own branch: every divisor is a literal, so the
   --  slot arithmetic is linear.

   function Decode_Single
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Pre => Logical >= Ext2_Inodes.NUM_DIRECT_BLOCKS and then
                 Logical < First_Double (Sectors),
          Post => Matches (Logical, Sectors, Decode_Single'Result)
   is
      Within : constant Natural :=
        Natural (Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS);
   begin
      case Sectors is
         when 2 => pragma Assert (Within < 256);
         when 4 => pragma Assert (Within < 512);
         when 8 => pragma Assert (Within < 1024);
      end case;
      return (Kind => Single_Indirect, Single_Slot => Within);
   end Decode_Single;

   function Decode_Double
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Pre => Logical >= First_Double (Sectors) and then
                 Logical < First_Triple (Sectors),
          Post => Matches (Logical, Sectors, Decode_Double'Result)
   is
      Within : constant Natural := Natural (Logical - First_Double (Sectors));
   begin
      case Sectors is
         when 2 =>
            pragma Assert (Within < 256 * 256);
            return (Kind => Double_Indirect,
                    Root_Slot => Within / 256, Leaf_Slot => Within mod 256);
         when 4 =>
            pragma Assert (Within < 512 * 512);
            return (Kind => Double_Indirect,
                    Root_Slot => Within / 512, Leaf_Slot => Within mod 512);
         when 8 =>
            pragma Assert (Within < 1024 * 1024);
            return (Kind => Double_Indirect,
                    Root_Slot => Within / 1024, Leaf_Slot => Within mod 1024);
      end case;
   end Decode_Double;

   function Decode_Triple
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Pre => Logical >= First_Triple (Sectors) and then
                 Logical < Block_Limit (Sectors),
          Post => Matches (Logical, Sectors, Decode_Triple'Result)
   is
      Within : constant Natural := Natural (Logical - First_Triple (Sectors));
   begin
      case Sectors is
         when 2 =>
            pragma Assert (Within < 256 * 256 * 256);
            return (Kind => Triple_Indirect,
                    Top_Slot => Within / 256 / 256,
                    Middle_Slot => Within / 256 mod 256,
                    Bottom_Slot => Within mod 256);
         when 4 =>
            pragma Assert (Within < 512 * 512 * 512);
            return (Kind => Triple_Indirect,
                    Top_Slot => Within / 512 / 512,
                    Middle_Slot => Within / 512 mod 512,
                    Bottom_Slot => Within mod 512);
         when 8 =>
            pragma Assert (Within < 1024 * 1024 * 1024);
            return (Kind => Triple_Indirect,
                    Top_Slot => Within / 1024 / 1024,
                    Middle_Slot => Within / 1024 mod 1024,
                    Bottom_Slot => Within mod 1024);
      end case;
   end Decode_Triple;

   function Decode_Direct
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Pre => Logical < Ext2_Inodes.NUM_DIRECT_BLOCKS,
          Post => Matches (Logical, Sectors, Decode_Direct'Result)
   is
   begin
      return (Kind => Direct, Direct_Slot => Direct_Index (Logical));
   end Decode_Direct;

   function Decode_Beyond
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
     with Pre => Logical >= Block_Limit (Sectors),
          Post => Matches (Logical, Sectors, Decode_Beyond'Result)
   is
   begin
      return (Kind => Unsupported);
   end Decode_Beyond;

   function Decode
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
   is
      --  Each case proves Matches itself; here it is only passed on.
      pragma Annotate
        (GNATprove, Hide_Info, "Expression_Function_Body", Matches);
   begin
      if Logical < Ext2_Inodes.NUM_DIRECT_BLOCKS then
         return Decode_Direct (Logical, Sectors);
      elsif Logical < First_Double (Sectors) then
         return Decode_Single (Logical, Sectors);
      elsif Logical < First_Triple (Sectors) then
         return Decode_Double (Logical, Sectors);
      elsif Logical < Block_Limit (Sectors) then
         return Decode_Triple (Logical, Sectors);
      end if;
      return Decode_Beyond (Logical, Sectors);
   end Decode;
end Block_Paths;
