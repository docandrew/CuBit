------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The desktop's key-repeat setting, read from Config.
--
--  `desktop.input.key-repeat` holds a typed CCL Key_Repeat
--  (CCL.Interfaces.Desktop_Input), checked when the configuration is
--  compiled and stored as its canonical source, e.g.
--
--    (Key_Repeat enabled => true delay_ms => 500 rate_hz => 30)
--
--  Decode reads it back through the same typed path (CCL.Typed_Settings);
--  anything but the canonical, schema-valid form is refused and the desktop
--  keeps Key_Repeat.DEFAULT_SETTINGS. (Tested on the host: tests/key-repeat.)
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Input;
with CCL.Typed_Settings;
with Key_Repeat;

package Desktop_Key_Repeat_Settings is
   Config_Key : constant String := CCL.Interfaces.Desktop_Input.KEY_REPEAT_KEY;
   --  The longest stored value Decode accepts.
   Maximum_Source : constant := CCL.Typed_Settings.MAXIMUM_CANONICAL;
   procedure Decode (Source : String; Item : out Key_Repeat.Settings; Success : out Boolean);
end Desktop_Key_Repeat_Settings;
