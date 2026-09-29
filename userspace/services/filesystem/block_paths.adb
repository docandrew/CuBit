package body Block_Paths with SPARK_Mode is
   --  Splitting on the three block geometries makes every divisor a constant,
   --  so the slot bounds need only linear arithmetic.
   procedure Lemma_Double_Slots
     (Sectors : Sector_Accounting.Block_Sectors; Within : Unsigned_64)
     with Ghost,
          Pre => Within < Middle_Span (Sectors),
          Post => Within / Pointer_Count (Sectors) < Pointer_Count (Sectors)
   is
   begin
      case Sectors is
         when 2 => null;
         when 4 => null;
         when 8 => null;
      end case;
   end Lemma_Double_Slots;

   procedure Lemma_Triple_Slots
     (Sectors : Sector_Accounting.Block_Sectors; Within : Unsigned_64)
     with Ghost,
          Pre => Within < Middle_Span (Sectors) * Pointer_Count (Sectors),
          Post => Within / Middle_Span (Sectors) < Pointer_Count (Sectors)
   is
   begin
      case Sectors is
         when 2 => null;
         when 4 => null;
         when 8 => null;
      end case;
   end Lemma_Triple_Slots;

   --  Slot conversions, split per geometry so that each divisor and bound
   --  is a constant.
   function Below
     (Value : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Pointer_Index
     with Pre => Value < Pointer_Count (Sectors),
          Post => Unsigned_64 (Below'Result) = Value
                  and then Below'Result < Slot_Count (Sectors)
   is
   begin
      case Sectors is
         when 2 => return Pointer_Index (Value);
         when 4 => return Pointer_Index (Value);
         when 8 => return Pointer_Index (Value);
      end case;
   end Below;

   function Remainder
     (Value : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Pointer_Index
     with Post => Unsigned_64 (Remainder'Result) = Value mod Pointer_Count (Sectors)
                  and then Remainder'Result < Slot_Count (Sectors)
   is
      Slot : Unsigned_64;
   begin
      case Sectors is
         when 2 =>
            pragma Assert (Pointer_Count (Sectors) = 256);
            Slot := Value mod 256;
            pragma Assert (Slot = Value mod Pointer_Count (Sectors));
            return Below (Slot, Sectors);
         when 4 =>
            pragma Assert (Pointer_Count (Sectors) = 512);
            Slot := Value mod 512;
            pragma Assert (Slot = Value mod Pointer_Count (Sectors));
            return Below (Slot, Sectors);
         when 8 =>
            pragma Assert (Pointer_Count (Sectors) = 1024);
            Slot := Value mod 1024;
            pragma Assert (Slot = Value mod Pointer_Count (Sectors));
            return Below (Slot, Sectors);
      end case;
   end Remainder;

   function Decode
     (Logical : Unsigned_64; Sectors : Sector_Accounting.Block_Sectors)
      return Block_Path
   is
      Pointers : constant Unsigned_64 := Pointer_Count (Sectors);
      Result : Block_Path := (Kind => Unsupported);
      Outer, Middle, Inner : Pointer_Index;
   begin
      --  Each branch establishes its own case of the contract, keeping every
      --  verification condition small enough for level-1 provers.
      if Logical < Ext2_Inodes.NUM_DIRECT_BLOCKS then
         Result := (Kind => Direct, Direct_Slot => Direct_Index (Logical));
         pragma Assert (Matches (Logical, Sectors, Result));
         return Result;
      elsif Logical < First_Double (Sectors) then
         pragma Assert (Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS < Pointers);
         Inner := Below (Logical - Ext2_Inodes.NUM_DIRECT_BLOCKS, Sectors);
         Result := (Kind => Single_Indirect, Single_Slot => Inner);
         pragma Assert (Matches (Logical, Sectors, Result));
         return Result;
      elsif Logical < First_Triple (Sectors) then
         declare
            Within_Double : constant Unsigned_64 := Logical - First_Double (Sectors);
         begin
            Lemma_Double_Slots (Sectors, Within_Double);
            Outer := Below (Within_Double / Pointers, Sectors);
            Inner := Remainder (Within_Double, Sectors);
            Result := (Kind => Double_Indirect, Root_Slot => Outer, Leaf_Slot => Inner);
            pragma Assert (Matches (Logical, Sectors, Result));
            return Result;
         end;
      elsif Logical < Block_Limit (Sectors) then
         declare
            Within_Triple : constant Unsigned_64 := Logical - First_Triple (Sectors);
         begin
            Lemma_Triple_Slots (Sectors, Within_Triple);
            Outer := Below (Within_Triple / Middle_Span (Sectors), Sectors);
            Middle := Remainder (Within_Triple / Pointers, Sectors);
            Inner := Remainder (Within_Triple, Sectors);
            Result := (Kind => Triple_Indirect, Top_Slot => Outer,
                       Middle_Slot => Middle, Bottom_Slot => Inner);
            pragma Assert (Matches (Logical, Sectors, Result));
            return Result;
         end;
      end if;
      pragma Assert (Result.Kind = Unsupported);
      pragma Assert (Matches (Logical, Sectors, Result));
      return Result;
   end Decode;
end Block_Paths;
