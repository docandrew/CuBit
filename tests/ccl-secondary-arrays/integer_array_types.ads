with Interfaces;
package Integer_Array_Types with SPARK_Mode is
   type Integer_Array is array (Positive range <>) of Interfaces.Integer_64;
end Integer_Array_Types;
