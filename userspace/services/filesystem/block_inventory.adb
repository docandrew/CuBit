package body Block_Inventory with SPARK_Mode is
   procedure Sort_And_Check (Blocks : in out Block_Array; Unique : out Boolean) is
      procedure Sift (Start, Last : Positive)
        with Pre => Blocks'First = 1 and Last <= Blocks'Last and Start <= Last
      is
         Root : Positive := Start;
         Child : Positive;
         Saved : Unsigned_32;
      begin
         while Root <= Last / 2 loop
            Child := Root * 2;
            if Child < Last and then Blocks (Child) < Blocks (Child + 1) then
               Child := Child + 1;
            end if;
            exit when Blocks (Root) >= Blocks (Child);
            Saved := Blocks (Root);
            Blocks (Root) := Blocks (Child);
            Blocks (Child) := Saved;
            Root := Child;
         end loop;
      end Sift;
      Saved : Unsigned_32;
   begin
      for Root in reverse 1 .. Blocks'Length / 2 loop
         Sift (Root, Blocks'Last);
      end loop;
      for Last in reverse 2 .. Blocks'Last loop
         Saved := Blocks (1);
         Blocks (1) := Blocks (Last);
         Blocks (Last) := Saved;
         Sift (1, Last - 1);
      end loop;
      Unique := (for all I in 2 .. Blocks'Last => Blocks (I - 1) < Blocks (I));
   end Sort_And_Check;
end Block_Inventory;
