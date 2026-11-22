pragma Ada_2022;
with GPIO_Pad_Diagnostics;
with GPIO_Pad_Proof_Boundary;
package GPIO_Pad_Proof_Instance with SPARK_Mode is
   package Sampler is new GPIO_Pad_Diagnostics (GPIO_Pad_Proof_Boundary.Read);
end GPIO_Pad_Proof_Instance;
