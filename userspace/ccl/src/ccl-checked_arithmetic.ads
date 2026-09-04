with Interfaces;

package CCL.Checked_Arithmetic with
   SPARK_Mode => On
is
   type Arithmetic_Error is
     (Arithmetic_Ok, Arithmetic_Overflow, Division_By_Zero);

   procedure Add
     (Left, Right : Interfaces.Integer_64;
      Sum         : out Interfaces.Integer_64;
      Overflow    : out Boolean);

   procedure Multiply
     (Left, Right : Interfaces.Integer_64;
      Product     : out Interfaces.Integer_64;
      Overflow    : out Boolean);

   procedure Divide
     (Left, Right : Interfaces.Integer_64;
      Quotient    : out Interfaces.Integer_64;
      Error       : out Arithmetic_Error);

   procedure Modulo
     (Left, Right : Interfaces.Integer_64;
      Remainder   : out Interfaces.Integer_64;
      Error       : out Arithmetic_Error);
end CCL.Checked_Arithmetic;
