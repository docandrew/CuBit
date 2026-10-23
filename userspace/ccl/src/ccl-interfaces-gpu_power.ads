--  GPU power policy as CCL types: `gpu.power.waitboost` is one
--  GPU_Waitboost, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back by Desktop, which hands it to the GPU
--  driver (OP_GPU_BOOST policy; docs/gpu-async-submission.md "SLPC
--  waitboost"). Absent: waitboost stays on.
package CCL.Interfaces.GPU_Power with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type GPU_Waitboost (record (enabled Boolean true)))";
   ROOT_TYPE_NAME : constant String := "GPU_Waitboost";
end CCL.Interfaces.GPU_Power;
