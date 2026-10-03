with Interfaces.C;
package body Vulkan_Backdrop_FFI with SPARK_Mode => Off is
   function Record_Native
     (Borrowed : System.Address; Description : access constant Compositor_Backdrop.Draw;
      W, H : Compositor_Backdrop.Word) return Interfaces.C.unsigned
     with Import, Convention => C, External_Name => "cubit_vulkan_record_backdrop";
   procedure Record_Draw
     (Borrowed : System.Address; Description : Compositor_Backdrop.Draw;
      W, H : Compositor_Backdrop.G.Physical_Extent; Accepted : out Boolean) is
      D : aliased constant Compositor_Backdrop.Draw := Description;
      use type Interfaces.C.unsigned;
   begin
      Accepted := Record_Native (Borrowed, D'Access,
        Compositor_Backdrop.Word (W), Compositor_Backdrop.Word (H)) = 0;
   end Record_Draw;
end Vulkan_Backdrop_FFI;
