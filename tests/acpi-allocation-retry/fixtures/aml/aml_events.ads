with AML_Decode;
package AML_Events with Pure, SPARK_Mode is
   use type AML_Decode.Integer_Value;
   type Action is (Signal_Event, Reset_Event, Wait_Event);
   Timeout_Modulus : constant := 2 ** 16;
   subtype Timeout is Natural range 0 .. Timeout_Modulus - 1;
   Poll_Only : constant Timeout := 0;
   Wait_Forever : constant Timeout := Timeout'Last;
   type Timeout_Outcome is (Elapsed, Unavailable, Failed);
   -- Quiescent local backend only: no concurrent signal delivery is possible.
   -- Elapsed certifies an actual finite deadline, never a consumed event unit.
   -- The default rejects blocking and never invents a forever timeout.
   procedure Unavailable_Timeout (Milliseconds : Timeout; Outcome : out Timeout_Outcome)
     with Global => null, Post => Outcome = Unavailable;
   function Normalize_Timeout (Value : AML_Decode.Integer_Value) return Timeout
     with Global => null,
       Post => AML_Decode.Integer_Value (Normalize_Timeout'Result) = Value mod Timeout_Modulus;
end AML_Events;
