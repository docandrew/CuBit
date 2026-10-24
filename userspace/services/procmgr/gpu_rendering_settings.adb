------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.GPU_Rendering;

package body GPU_Rendering_Settings is
   package Codec renames CCL.Setting_Codecs.GPU_Rendering;

   procedure Decode (Source : String; Enabled : out Boolean; Success : out Boolean) is
      Setting : Codec.GPU_Rendering_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Enabled := (if Success then Setting.enabled else Default_Enabled);
   end Decode;
end GPU_Rendering_Settings;
