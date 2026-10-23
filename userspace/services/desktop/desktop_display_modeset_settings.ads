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

package Desktop_Display_Modeset_Settings is
   Default_Enabled : constant Boolean := False;
   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean);
   --  The setting catalog's codec (CAP-003).
   subtype Value is Boolean;
end Desktop_Display_Modeset_Settings;
