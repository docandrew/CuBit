with Intel_GPU_Modeset_Pages;
with Interfaces;
--  Grant and map one modeset page (Intel_GPU_Modeset_Pages) from the broker,
--  like Intel_GPU_Pipe_Mapping. One attempt per page; partial grants and
--  lost replies are not retried. Success is not a power reference.
package Intel_GPU_Modeset_Mapping is
   function Ready (Item : Intel_GPU_Modeset_Pages.Page) return Boolean;
   function Prepare
     (Owner : Boolean; Register_Base : Interfaces.Unsigned_64;
      Item : Intel_GPU_Modeset_Pages.Page) return String;
end Intel_GPU_Modeset_Mapping;
