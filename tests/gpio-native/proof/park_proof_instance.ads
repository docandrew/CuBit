with GPIO_Community_Park;
with GPIO_Proof_Boundary;
package Park_Proof_Instance with SPARK_Mode is
   package Owner is new GPIO_Community_Park
     (GPIO_Proof_Boundary.Resource_Ready, GPIO_Proof_Boundary.Admit_Pin,
      GPIO_Proof_Boundary.Clock, GPIO_Proof_Boundary.Read_Plan, GPIO_Proof_Boundary.Write_Plan);
end Park_Proof_Instance;
