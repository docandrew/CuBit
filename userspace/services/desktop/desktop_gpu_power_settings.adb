------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.GPU_Waitboost;

package body Desktop_GPU_Power_Settings is
   package Codec renames CCL.Setting_Codecs.GPU_Waitboost;

   procedure Decode (Source : String; Waitboost : out Boolean; Success : out Boolean) is
      Setting : Codec.GPU_Waitboost_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Waitboost := (if Success then Setting.enabled else Default_Waitboost);
   end Decode;
end Desktop_GPU_Power_Settings;
