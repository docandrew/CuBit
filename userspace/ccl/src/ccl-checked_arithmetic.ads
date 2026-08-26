with Interfaces;

package CCL.Checked_Arithmetic with
   SPARK_Mode => On
is
   procedure Add
     (Left, Right : Interfaces.Integer_64;
      Sum         : out Interfaces.Integer_64;
      Overflow    : out Boolean);
end CCL.Checked_Arithmetic;
