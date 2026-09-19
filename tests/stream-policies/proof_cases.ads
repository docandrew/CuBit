with CuBit.Protocols.Stream_Policies;
package Proof_Cases with SPARK_Mode is
   --  Universal properties, not assertions about one fixed demo profile.
   procedure Check (Left, Right : CuBit.Protocols.Stream_Policies.Policy)
     with Ghost;
end Proof_Cases;
