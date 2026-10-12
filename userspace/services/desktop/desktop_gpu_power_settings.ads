------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  `gpu.power.waitboost`, a typed CCL GPU_Waitboost (CCL.Interfaces.GPU_Power),
--  e.g. (GPU_Waitboost enabled => false). Desktop reads it once and hands it
--  to the GPU driver (Desktop_GPU_Wake.Set_Waitboost). Absent or invalid:
--  waitboost stays on.
------------------------------------------------------------------------------
with CCL.Interfaces.GPU_Power;
with CCL.Typed_Settings;

package Desktop_GPU_Power_Settings is
   Config_Key : constant String := CCL.Interfaces.GPU_Power.WAITBOOST_KEY;
   Maximum_Source : constant := CCL.Typed_Settings.MAXIMUM_CANONICAL;
   Default_Waitboost : constant Boolean := True;
   procedure Decode (Source : String; Waitboost : out Boolean; Success : out Boolean);
end Desktop_GPU_Power_Settings;
