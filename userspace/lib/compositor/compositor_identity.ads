-- Long-lived internal identities. Zero is never issued; exhaustion refuses
-- new work instead of wrapping into an identity a delayed callback could hold.
package Compositor_Identity with SPARK_Mode, Pure is
   type Value is range 0 .. 2 ** 63 - 1 with Size => 64;
   subtype Positive_Value is Value range 1 .. Value'Last;
   function Next (Current : Value; Last : Positive_Value := Positive_Value'Last)
     return Value is (if Current < Last then Current + 1 else 0)
     with Pre => Current <= Last,
       Post => Next'Result <= Last and then
         (if Current < Last then Next'Result = Current + 1 and Next'Result > Current
          else Next'Result = 0);
end Compositor_Identity;
