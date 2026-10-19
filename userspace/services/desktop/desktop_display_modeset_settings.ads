------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  `desktop.display.modeset`, a typed CCL Display_Modeset
--  (CCL.Interfaces.Desktop_Display), e.g. (Display_Modeset enabled => true).
--  When enabled, Desktop asks display.svc for each Intel output's best
--  admitted mode (DISP-006). Absent or invalid: off (the firmware mode).
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Display;
with CCL.Typed_Settings;

package Desktop_Display_Modeset_Settings is
   Config_Key : constant String := CCL.Interfaces.Desktop_Display.MODESET_KEY;
   Maximum_Source : constant := CCL.Typed_Settings.MAXIMUM_CANONICAL;
   Default_Enabled : constant Boolean := False;
   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean);
end Desktop_Display_Modeset_Settings;
