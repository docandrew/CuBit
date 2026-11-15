--  GPU power policy as CCL types: `gpu.power.waitboost` is one
--  GPU_Waitboost, checked when the configuration is compiled
--  (CCL.Typed_Settings) and read back, with the decoder generated from
--  these declarations (CCL.Setting_Codecs), by Desktop, which hands it to
--  the GPU driver (OP_GPU_BOOST policy; docs/gpu-async-submission.md "SLPC
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

   --  `desktop.composition` is one Desktop_Composition: which processor
   --  composes Desktop's UI (LAT-002): CPU (damage only, the default, and
   --  what an absent setting means) or GPU (the Vulkan scene, which needs
   --  the admitted render session desktop.render.gpu grants). It is an
   --  operator's A/B switch, read when Desktop starts; it never admits or
   --  refuses the GPU session itself, which stays for 3D work either way.
   COMPOSITION_TYPE_SOURCE : constant String :=
     "(type Composition_Path (enum CPU GPU)) " &
     "(type Desktop_Composition (record (path Composition_Path Composition_Path.CPU)))";
   COMPOSITION_ROOT_TYPE_NAME : constant String := "Desktop_Composition";
end CCL.Interfaces.GPU_Power;
