with Interfaces.C;
package Desktop_GPU_Scene_Real_Bridge is
   function Open return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_open";
   function Start (Version : Interfaces.C.int) return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_start";
   function Import_Image return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_import";
   function Render return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_render";
   function Poll_Upload return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_poll_upload";
   function Poll_Frame return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_poll_frame";
   function Close return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_gpu_scene_real_close";
end Desktop_GPU_Scene_Real_Bridge;
