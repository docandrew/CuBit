with Interfaces.C;
with System;
with Native_Scene_Bridge;
procedure Scene_Host is
   use type Interfaces.C.int;
   function Run (Argc : Interfaces.C.int; Argv : System.Address) return Interfaces.C.int
     with Import, Convention => C, External_Name => "run_mesa_scene_host";
begin
   if Run (1, System.Null_Address) /= 0 then
      raise Program_Error with "Mesa producer/native scene integration failed";
   end if;
end Scene_Host;
