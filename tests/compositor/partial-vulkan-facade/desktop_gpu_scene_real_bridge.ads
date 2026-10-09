with Interfaces.C; with Interfaces;
package Desktop_GPU_Scene_Real_Bridge is
 function Open return Interfaces.C.int with Export, Convention=>C, External_Name=>"desktop_gpu_scene_real_open";
 function Frame (Step : Interfaces.C.int) return Interfaces.C.int with Export, Convention=>C, External_Name=>"facade_frame";
 function Pump return Interfaces.C.int with Export, Convention=>C, External_Name=>"facade_pump";
 function Pixel (Index : Interfaces.C.int) return Interfaces.Unsigned_32 with Export, Convention=>C, External_Name=>"facade_pixel";
 function Close return Interfaces.C.int with Export, Convention=>C, External_Name=>"desktop_gpu_scene_real_close";
 function Wrong_Writer return Interfaces.C.int with Export, Convention => C, External_Name => "facade_wrong_writer";
end Desktop_GPU_Scene_Real_Bridge;
