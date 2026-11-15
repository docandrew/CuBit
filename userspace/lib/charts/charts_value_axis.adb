pragma Ada_2022;

package body Charts_Value_Axis with SPARK_Mode is
   Decimal_Base : constant := 10;

   function Nice_Ceiling (V : Value) return Value is
      Power : Value := 1;
   begin
      if V <= 1 then
         return 1;
      end if;
      loop
         pragma Loop_Invariant (Power >= 1 and then Power < V and then Power <= Value_Ceiling / Decimal_Base);
         if 2 * Power >= V then
            return 2 * Power;
         elsif 5 * Power >= V then
            return 5 * Power;
         elsif Decimal_Base * Power >= V then
            return Decimal_Base * Power;
         elsif Power > Value_Ceiling / (Decimal_Base * Decimal_Base) then
            return Value_Ceiling;
         end if;
         Power := Power * Decimal_Base;
      end loop;
   end Nice_Ceiling;

   function Tick_Step (High : Value; Ticks : Tick_Count) return Value is
     (Nice_Ceiling ((High + Unsigned_64 (Ticks) - 1) / Unsigned_64 (Ticks)));

   function Axis_Top (High : Value; Ticks : Tick_Count) return Value is
      Step : constant Value := Tick_Step (High, Ticks);
      Steps : constant Unsigned_64 := (High + Step - 1) / Step;
   begin
      if Steps = 0 then
         return Step;
      elsif Steps > Value_Ceiling / Step then
         return Value_Ceiling;
      else
         return Steps * Step;
      end if;
   end Axis_Top;

   function Row_Of (V : Unsigned_64; Top : Value; Rows : Row_Count) return Natural is
      Shown : constant Value := (if V > Top then Top else V);
      --  Shown * (Rows - 1) <= 2^48 * 2^13: no overflow.
      Above_Zero : constant Unsigned_64 := Shown * Unsigned_64 (Rows - 1) / Top;
   begin
      return Rows - 1 - Natural (Above_Zero);
   end Row_Of;
end Charts_Value_Axis;
