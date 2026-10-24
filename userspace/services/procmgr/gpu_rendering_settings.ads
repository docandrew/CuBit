------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  `desktop.render.gpu`, a typed CCL GPU_Rendering (CCL.Interfaces.GPU_Power),
--  e.g. (GPU_Rendering enabled => false): whether Desktop may render on the
--  GPU (HW-002). procmgr reads it when a startup plan delegates the render
--  approval to it, (render setting "desktop.render.gpu"). Absent: enabled.
------------------------------------------------------------------------------

package GPU_Rendering_Settings is
   Default_Enabled : constant Boolean := True;
   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is Boolean;
end GPU_Rendering_Settings;
