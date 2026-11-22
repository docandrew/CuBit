with GMBUS_Proof_IO;
with Intel_GPU_DP_AUX;
package AUX_Proof with SPARK_Mode is
   package Instance is new Intel_GPU_DP_AUX (GMBUS_Proof_IO.Read, GMBUS_Proof_IO.Write, GMBUS_Proof_IO.Now_Us);
end AUX_Proof;
