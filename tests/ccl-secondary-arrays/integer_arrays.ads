pragma SPARK_Mode;
with Interfaces;
with CCL.Secondary_Arrays;
with Integer_Array_Types; use Integer_Array_Types;
--  A small instance for hosted tests and proof.
package Integer_Arrays is new CCL.Secondary_Arrays
  (Element_Type => Interfaces.Integer_64, Null_Element => 0,
   Element_Array => Integer_Array, Capacity => 16, Max_Values => 4);
