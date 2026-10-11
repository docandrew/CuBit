with Intel_GPU_Pipe_Pages;
with Interfaces;
--  Grant and map one pipe's register page (Intel_GPU_Pipe_Pages) from the
--  broker, like Intel_GPU_Display_Mapping. One attempt per page; partial
--  grants and lost replies are not retried. Success is not a power reference.
package Intel_GPU_Pipe_Mapping is
   function Ready (Item : Intel_GPU_Pipe_Pages.Page_Index) return Boolean;
   function Prepare
     (Owner : Boolean; Register_Base : Interfaces.Unsigned_64;
      Item : Intel_GPU_Pipe_Pages.Page_Index) return String;
end Intel_GPU_Pipe_Mapping;
