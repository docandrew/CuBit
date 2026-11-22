with Interfaces;
with Intel_GPU_Observation;
--  Instantiate the generic so GNATprove checks actual capture code. This
--  reader is a pure model, not a claim about MMIO or device power lifetime.
package Observation_Proof with SPARK_Mode is
   function Model_Read (Address : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_32 is
        (Interfaces.Unsigned_32 (Interfaces.Shift_Right (Address, 32)));
   procedure Capture is new Intel_GPU_Observation.Capture (Model_Read);
end Observation_Proof;
