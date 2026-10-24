------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.Display_Modeset;

package body Desktop_Display_Modeset_Settings is
   package Codec renames CCL.Setting_Codecs.Display_Modeset;

   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean) is
      Setting : Codec.Display_Modeset_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Enabled := (if Success then Setting.enabled else Default_Enabled);
   end Decode;
end Desktop_Display_Modeset_Settings;
