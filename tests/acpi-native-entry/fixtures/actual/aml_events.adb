package body AML_Events with SPARK_Mode is
   procedure Unavailable_Timeout (Milliseconds : Timeout; Outcome : out Timeout_Outcome) is
      pragma Unreferenced (Milliseconds);
   begin Outcome := Unavailable; end Unavailable_Timeout;
   function Normalize_Timeout (Value : AML_Decode.Integer_Value) return Timeout is
     (Timeout (Value mod Timeout_Modulus));
end AML_Events;
