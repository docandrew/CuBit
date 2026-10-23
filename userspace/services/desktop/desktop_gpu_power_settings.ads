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

package Desktop_GPU_Power_Settings is
   Default_Waitboost : constant Boolean := True;
   procedure Decode (Source : String; Waitboost : out Boolean; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is Boolean;
end Desktop_GPU_Power_Settings;
