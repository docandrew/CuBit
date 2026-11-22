with System;
with Desktop_Composition;
--  Trusted C ABI conversion. Global null models exclusively borrowed command
--  state reachable through Borrowed; it does not assert physical GPU purity.
package Vulkan_Copy_FFI with SPARK_Mode is
   procedure Record_Plan
     (Borrowed : System.Address; P : Desktop_Composition.Blit_Plan;
      Accepted : out Boolean)
     with Global => null,
       Pre => P.Width > 0 and then P.Height > 0 and then
         P.Target_X <= Natural'Last - P.Width and then
         P.Target_Y <= Natural'Last - P.Height and then
         P.Source_X <= Natural'Last - P.Width and then
         P.Source_Y <= Natural'Last - P.Height;
end Vulkan_Copy_FFI;
