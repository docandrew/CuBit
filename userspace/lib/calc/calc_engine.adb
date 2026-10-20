package body Calc_Engine with SPARK_Mode is
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Release"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Move"" but not used", Reason => "moves end ownership");

   type Number_Array is array (Positive range <>) of Number;

   procedure Evaluate_Terms
     (Text : String; Terms : Term_Array; Previous : Number; Has_Previous : Boolean; Bounds : Limits;
      Result : in out Number; Failure : out Failure_Kind; Position : out Column)
   is
      --  The operands so far, in postfix order: Values (1 .. Top).
      Values : Number_Array (1 .. Terms'Length);
      Top : Natural := 0;
      Made : Number;
      State : Status;

      --  Free every value still held.
      procedure Release
        with Global => (In_Out => (Values, Top), Proof_In => Terms),
             Pre => Top <= Values'Last
                    and then (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K)))),
             Post => (for all K in Values'Range => Is_Reclaimed (Values (K)))
      is
      begin
         while Top > 0 loop
            pragma Loop_Invariant (Top <= Values'Last);
            pragma Loop_Invariant (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K))));
            pragma Loop_Variant (Decreases => Top);
            Free (Values (Top));
            Top := Top - 1;
         end loop;
      end Release;
   begin
      Failure := No_Failure;
      Position := 1;
      for T of Terms loop
         pragma Loop_Invariant (Top <= Values'Last and then Failure = No_Failure and then Is_Reclaimed (Made));
         pragma Loop_Invariant (for all K in 1 .. Top => Within (Values (K), Bounds.Budget));
         pragma Loop_Invariant (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K))));
         Position := T.Position;
         State := Done;
         case T.Kind is
            when Literal =>
               From_Decimal (Text (T.Whole_First .. T.Whole_Last), Text (T.Fraction_First .. T.Fraction_Last),
                             T.Exponent, Bounds.Budget, Made, State);
            when Previous_Result =>
               if not Has_Previous then
                  Failure := No_Previous_Result;
               elsif not Within (Previous, Bounds.Budget) then
                  State := Too_Large;
               else
                  Copy (Previous, Made);
               end if;
            when Named_Constant =>
               Make_Constant (T.Value, Bounds.Precision, Bounds.Budget, Made, State);
            when Unary =>
               if Top = 0 then
                  Failure := Expected_Operand;  --  never: Parse orders the terms
               else
                  Apply (T.Unary_Operation, Values (Top), Bounds.Budget, Bounds.Precision, Made, State);
                  if State = Done then
                     Free (Values (Top));
                     Top := Top - 1;
                  end if;
               end if;
            when Binary =>
               if Top < 2 then
                  Failure := Expected_Operand;  --  never: Parse orders the terms
               else
                  Apply (T.Binary_Operation, Values (Top - 1), Values (Top), Bounds.Budget, Made, State);
                  if State = Done then
                     Free (Values (Top));
                     Free (Values (Top - 1));
                     Top := Top - 2;
                  end if;
               end if;
         end case;
         if Failure = No_Failure and then State /= Done then
            Failure := Failure_Of (State);
         end if;
         if Failure /= No_Failure then
            Free (Made);
            Release;
            return;
         end if;
         if Top = Values'Last then
            --  never: each term pushes one value after taking its operands
            Free (Made);
            Failure := Expected_Operand;
            Release;
            return;
         end if;
         Top := Top + 1;
         Move (Made, Values (Top));
      end loop;
      if Top /= 1 then
         Failure := (if Top = 0 then Empty_Expression else Expected_Operator);  --  never after Parse
         Release;
         return;
      end if;
      Move (Values (1), Result);
   end Evaluate_Terms;

   procedure Evaluate
     (Text : String; Previous : Number; Has_Previous : Boolean; Bounds : Limits;
      Result : in out Number; Failure : out Failure_Kind; Position : out Column)
   is
      Terms : Term_Array (1 .. Text'Length);
      Count : Natural;
      Error : Syntax_Error;
   begin
      Parse (Text, Terms, Count, Error, Position);
      if Error /= No_Error then
         Failure := Failure_Of (Error);
         return;
      end if;
      Evaluate_Terms (Text, Terms (1 .. Count), Previous, Has_Previous, Bounds, Result, Failure, Position);
   end Evaluate;
end Calc_Engine;
