with Detection_Proof_IO;
with GMBUS_Proof;
with Intel_GPU_Port_Scan;
with Intel_GPU_GMBUS_Outcome;
package Detection_Proof with SPARK_Mode is
   package Scan is new Intel_GPU_Port_Scan
     (Detection_Proof_IO.Read_MMIO, Detection_Proof_IO.Is_Lit, Detection_Proof_IO.Read_DDC_Base);
   function Outcome_Of is new Intel_GPU_GMBUS_Outcome (GMBUS_Proof.Instance);
end Detection_Proof;
