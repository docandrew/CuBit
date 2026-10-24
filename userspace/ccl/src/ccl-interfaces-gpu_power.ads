--  GPU power policy as CCL types: `gpu.power.waitboost` is one
--  GPU_Waitboost, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back by Desktop, which hands it to the GPU
--  driver (OP_GPU_BOOST policy; docs/gpu-async-submission.md "SLPC
--  waitboost"). Absent: waitboost stays on.
--
--  `desktop.render.gpu` is one GPU_Rendering: whether Desktop may render on
--  the GPU (HW-002, docs/device-matching.md). It is a settings grant: a
--  startup plan names it with (render setting "desktop.render.gpu") and
--  procmgr approves the render session from it. Absent: enabled, so
--  Desktop renders on the GPU wherever a supported GPU is present (without
--  one, devmgr refuses the session and Desktop renders in software).
package CCL.Interfaces.GPU_Power with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type GPU_Waitboost (record (enabled Boolean true)))";
   ROOT_TYPE_NAME : constant String := "GPU_Waitboost";
   RENDERING_TYPE_SOURCE : constant String :=
     "(type GPU_Rendering (record (enabled Boolean true)))";
   RENDERING_ROOT_TYPE_NAME : constant String := "GPU_Rendering";
end CCL.Interfaces.GPU_Power;
