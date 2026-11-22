pragma Ada_2022;
with Intel_GPIO_IRQ;
with Intel_ADLN_GPIO;
-- Trusted imported device boundary, arbitrary typed values/statuses.
-- Matches existing GPIO proofs; this is not a proof of MMIO or hardware.
package GPIO_Pad_Proof_Boundary with SPARK_Mode,
  Abstract_State => (Device with External => Async_Writers) is
   procedure Read (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Read_ID;
      Value : out Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status)
     with Import, Global => (In_Out => Device), Always_Terminates;
end GPIO_Pad_Proof_Boundary;
