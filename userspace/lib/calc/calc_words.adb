with Interfaces; use Interfaces;
with Calc_Limbs; use type Calc_Limbs.Ordering;

package body Calc_Words with SPARK_Mode is
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");

   --  Exact steps that run only while State is Done.
   procedure Step
     (Operation : Binary_Operation; A, B : Number; Budget : Size_Budget;
      Result : in out Number; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then (if State = Done then Within (A, Budget) and then Within (B, Budget)),
          Post => (if State = Done then Within (Result, Budget) and then State'Old = Done else Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         Apply (Operation, A, B, Budget, Result, State);
      end if;
   end Step;

   procedure Two_To (Exponent : Natural; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         Power_Of_Two (Exponent, Budget, Result, State);
      end if;
   end Two_To;

   procedure Pattern
     (X : Number; Word : Word_Size; Budget : Size_Budget; Result : in out Number; State : out Status)
   is
      Modulus : Number;
   begin
      State := (if Is_Integer (X) then Done else Not_An_Integer);
      Two_To (Width (Word), Budget, Modulus, State);
      Step (Modulo, X, Modulus, Budget, Result, State);
      Free (Modulus);
   end Pattern;

   procedure Wrap
     (X : Number; Word : Word_Size; Signed : Boolean; Budget : Size_Budget;
      Result : in out Number; Wrapped : out Boolean; State : out Status)
   is
      Bits, Half, Modulus : Number;
      Order : Calc_Limbs.Ordering := Calc_Limbs.Less;
   begin
      Wrapped := False;
      if not Is_Integer (X) then
         State := Not_An_Integer;
         return;
      elsif Word = Unbounded then
         Copy (X, Result);
         State := Done;
         return;
      end if;
      Pattern (X, Word, Budget, Bits, State);
      Two_To (Width (Word) - 1, Budget, Half, State);
      if State = Done and then Size (Bits) <= Size_Budget'Last and then Size (Half) <= Size_Budget'Last then
         Compare (Bits, Half, Order);
      end if;
      if State = Done and then Signed and then Order /= Calc_Limbs.Less then
         Two_To (Width (Word), Budget, Modulus, State);
         Step (Subtract, Bits, Modulus, Budget, Result, State);
      elsif State = Done then
         Copy (Bits, Result);
      end if;
      if State = Done then
         Wrapped := not Same (X, Result);
      end if;
      Free (Bits);
      Free (Half);
      Free (Modulus);
   end Wrap;

   procedure Rotate
     (X, Count : Number; Left : Boolean; Word : Word_Size; Signed : Boolean; Budget : Size_Budget;
      Result : in out Number; State : out Status)
   is
      Bits, Width_Number, Steps, Back, Rotated_Out, Kept, High, Low, Combined, Lower : Number;
      Wrapped : Boolean;
   begin
      State := (if Is_Integer (X) and then Is_Integer (Count) then Done else Not_An_Integer);
      Pattern (X, Word, Budget, Bits, State);
      if State = Done then
         From_Integer (Integer_64 (Width (Word)), Width_Number);
         if Size (Width_Number) > Budget then
            State := Too_Large;  --  never: a width fits a limb
         end if;
      end if;
      if State = Done and then not (Within (Bits, Budget) and then Within (Width_Number, Budget)) then
         State := Too_Large;  --  never: Pattern and the check above
      end if;
      --  Left by n: (u << n) | (u >> (w - n)); right by n is left by w - n.
      Step (Modulo, Count, Width_Number, Budget, Steps, State);
      if State = Done and then not Left then
         Step (Subtract, Width_Number, Steps, Budget, Back, State);
         Free (Steps);
         Step (Modulo, Back, Width_Number, Budget, Steps, State);
         Free (Back);
      end if;
      Step (Calc_Numbers.Shift_Left, Bits, Steps, Budget, High, State);
      Step (Subtract, Width_Number, Steps, Budget, Back, State);
      Step (Calc_Numbers.Shift_Right, Bits, Back, Budget, Low, State);
      Step (Bit_Or, High, Low, Budget, Combined, State);
      if State = Done then
         Pattern (Combined, Word, Budget, Lower, State);
      end if;
      if State = Done then
         Wrap (Lower, Word, Signed, Budget, Result, Wrapped, State);
      end if;
      Free (Bits);
      Free (Width_Number);
      Free (Steps);
      Free (Back);
      Free (Rotated_Out);
      Free (Kept);
      Free (High);
      Free (Low);
      Free (Combined);
      Free (Lower);
   end Rotate;
end Calc_Words;
