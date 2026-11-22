with Intel_GPIO_IRQ_Owner;
with GPIO_Proof_Boundary;
package IRQ_Proof_Instance with SPARK_Mode is
   package Owner is new Intel_GPIO_IRQ_Owner
     (GPIO_Proof_Boundary.Clock, GPIO_Proof_Boundary.Read, GPIO_Proof_Boundary.Write);
end IRQ_Proof_Instance;
