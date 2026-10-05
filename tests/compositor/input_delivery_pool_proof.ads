with Input_Delivery_Proof;
with Compositor_Input_Delivery_Pool;
package Input_Delivery_Pool_Proof with SPARK_Mode is
   package Pool is new Compositor_Input_Delivery_Pool
     (3, Input_Delivery_Proof.Delivery);
end Input_Delivery_Pool_Proof;
