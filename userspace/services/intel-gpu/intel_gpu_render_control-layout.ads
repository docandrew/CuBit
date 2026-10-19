with System;
with Intel_GPU_Render_Sessions;
-- The controller's storage prefix (Intel_GPU_Render_Sessions.Layout).
package Intel_GPU_Render_Control.Layout with SPARK_Mode => Off is
   -- Bytes from the controller's start that hold its own fields and its
   -- first Count session slots.
   function Bytes_Through
     (Object : Controller; Count : Intel_GPU_Render_Sessions.Slot_Count)
      return Interfaces.Unsigned_64;
end Intel_GPU_Render_Control.Layout;
