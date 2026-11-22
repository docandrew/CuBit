with Intel_GPU_Render_Sessions.Layout;
package body Intel_GPU_Render_Control.Layout with SPARK_Mode => Off is
   function Bytes_Through
     (Object : Controller; Count : Intel_GPU_Render_Sessions.Slot_Count)
      return Interfaces.Unsigned_64 is
     (Intel_GPU_Render_Sessions.Layout.Bytes_Through
        (Object.Sessions, Object'Address, Count));
end Intel_GPU_Render_Control.Layout;
