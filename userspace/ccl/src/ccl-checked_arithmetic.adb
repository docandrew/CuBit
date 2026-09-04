package body CCL.Checked_Arithmetic with
   SPARK_Mode => On
is
   use type Interfaces.Integer_64;

   type Wide_Integer is range -(2 ** 127) .. 2 ** 127 - 1;
   for Wide_Integer'Size use 128;
   subtype Narrow_Wide_Integer is Wide_Integer range
     Wide_Integer (Interfaces.Integer_64'First) ..
     Wide_Integer (Interfaces.Integer_64'Last);

   procedure Add
     (Left, Right : Interfaces.Integer_64;
      Sum         : out Interfaces.Integer_64;
      Overflow    : out Boolean)
   is
      Wide_Sum : Wide_Integer;
   begin
      Wide_Sum := Wide_Integer (Left) + Wide_Integer (Right);
      if Wide_Sum in
        Wide_Integer (Interfaces.Integer_64'First) ..
        Wide_Integer (Interfaces.Integer_64'Last)
      then
         declare
            Narrow_Sum : constant Narrow_Wide_Integer :=
              Narrow_Wide_Integer (Wide_Sum);
         begin
            Sum := Interfaces.Integer_64 (Narrow_Sum);
            Overflow := False;
         end;
      else
         Sum := 0;
         Overflow := True;
      end if;
   end Add;

   procedure Multiply
     (Left, Right : Interfaces.Integer_64;
      Product     : out Interfaces.Integer_64;
      Overflow    : out Boolean)
   is
      Wide_Product : Wide_Integer;
   begin
      Wide_Product := Wide_Integer (Left) * Wide_Integer (Right);
      if Wide_Product in Narrow_Wide_Integer then
         Product := Interfaces.Integer_64
           (Narrow_Wide_Integer (Wide_Product));
         Overflow := False;
      else
         Product := 0;
         Overflow := True;
      end if;
   end Multiply;

   procedure Divide
     (Left, Right : Interfaces.Integer_64;
      Quotient    : out Interfaces.Integer_64;
      Error       : out Arithmetic_Error) is
   begin
      Quotient := 0;
      if Right = 0 then
         Error := Division_By_Zero;
      elsif Left = Interfaces.Integer_64'First and then Right = -1 then
         Error := Arithmetic_Overflow;
      else
         Quotient := Left / Right;
         Error := Arithmetic_Ok;
      end if;
   end Divide;

   procedure Modulo
     (Left, Right : Interfaces.Integer_64;
      Remainder   : out Interfaces.Integer_64;
      Error       : out Arithmetic_Error) is
   begin
      Remainder := 0;
      if Right = 0 then
         Error := Division_By_Zero;
      elsif Right = -1 then
         --  The mathematical result is zero.  Handling it directly avoids
         --  evaluating the unrepresentable First / -1 quotient.
         Error := Arithmetic_Ok;
      else
         Remainder := Left mod Right;
         Error := Arithmetic_Ok;
      end if;
   end Modulo;
end CCL.Checked_Arithmetic;
