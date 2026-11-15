pragma Ada_2022;

package body Charts_Quantiles with SPARK_Mode is

   procedure Swap (Items : in out Sample_Array; A, B : Natural)
     with Pre => A in Items'Range and then B in Items'Range;
   procedure Swap (Items : in out Sample_Array; A, B : Natural) is
      Held : constant Unsigned_64 := Items (A);
   begin
      Items (A) := Items (B);
      Items (B) := Held;
   end Swap;

   function Median_Of (A, B, C : Unsigned_64) return Unsigned_64 is
     (if A <= B then (if B <= C then B elsif A <= C then C else A)
      else (if A <= C then A elsif B <= C then C else B));

   --  Three-way partitioning (values equal to the pivot gather in the
   --  middle), so runs of equal samples (a late count of zero) stay linear.
   procedure Select_Kth (Items : in out Sample_Array; K : Positive; Result : out Unsigned_64) is
      Target : constant Natural := Items'First + (K - 1);
      Lo : Natural := Items'First;
      Hi : Natural := Items'Last;
   begin
      while Lo < Hi loop
         pragma Loop_Invariant (Lo in Items'Range and then Hi in Items'Range and then Target in Lo .. Hi);
         declare
            Pivot : constant Unsigned_64 := Median_Of (Items (Lo), Items (Lo + (Hi - Lo) / 2), Items (Hi));
            Lt : Natural := Lo;
            I : Natural := Lo;
            Gt : Natural := Hi;
            Done : Boolean := False;
         begin
            while not Done loop
               pragma Loop_Invariant (Lo <= Lt and then Lt <= I and then I <= Hi + 1 and then Gt <= Hi
                                      and then Gt + 1 >= I and then Lt <= Gt + 1);
               if I > Gt then
                  Done := True;
               elsif Items (I) < Pivot then
                  Swap (Items, Lt, I);
                  Lt := Lt + 1;
                  I := I + 1;
               elsif Items (I) > Pivot then
                  Swap (Items, I, Gt);
                  if Gt = Lo then
                     --  Nothing at or below Gt is left: every value is above
                     --  the pivot, which cannot be (the pivot is one of them).
                     Done := True;
                  else
                     Gt := Gt - 1;
                  end if;
               else
                  I := I + 1;
               end if;
            end loop;
            if Target < Lt then
               Hi := Lt - 1;
            elsif Target > Gt then
               Lo := Gt + 1;
            else
               Lo := Target;
               Hi := Target;
            end if;
         end;
      end loop;
      Result := Items (Target);
   end Select_Kth;
end Charts_Quantiles;
