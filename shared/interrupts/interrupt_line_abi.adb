pragma Ada_2022;

package body Interrupt_Line_ABI with
   SPARK_Mode => On
is

   --  The codes are the positions 0 .. N (the representation clause), so a
   --  code word is its outcome's position.
   function To_Outcome (Word : Unsigned_64) return Route_Outcome is
     (Route_Outcome'Val (Natural (Word)));

end Interrupt_Line_ABI;
