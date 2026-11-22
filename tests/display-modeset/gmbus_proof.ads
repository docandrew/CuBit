with GMBUS_Proof_IO;
with Intel_GPU_GMBUS;
package GMBUS_Proof with SPARK_Mode is
   package Instance is new Intel_GPU_GMBUS (GMBUS_Proof_IO.Read, GMBUS_Proof_IO.Write, GMBUS_Proof_IO.Now_Us);
end GMBUS_Proof;
