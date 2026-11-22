with Interfaces;
with System.Secondary_Stack;

-- Audited standalone GNAT runtime boundary. Every caller is serialized by
-- the Rust adapter on its owner thread. This is not a SPARK proof boundary.
package Servo_Secondary_Stack with SPARK_Mode => Off is
   function Get return System.Secondary_Stack.SS_Stack_Ptr
     with Export, Convention => C,
       External_Name => "__wrap___gnat_get_secondary_stack";
   function Check return Interfaces.Unsigned_32
     with Export, Convention => C,
       External_Name => "cubit_servo_stack_check";
end Servo_Secondary_Stack;
