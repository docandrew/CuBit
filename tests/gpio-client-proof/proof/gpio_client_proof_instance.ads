pragma Ada_2022;
with GPIO_Client_Controls;
with GPIO_Client_Proof_Boundary;
package GPIO_Client_Proof_Instance with SPARK_Mode is
   package Owner is new GPIO_Client_Controls (GPIO_Client_Proof_Boundary.Submit);
end GPIO_Client_Proof_Instance;
