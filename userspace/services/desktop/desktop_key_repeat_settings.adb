------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.Key_Repeat;

package body Desktop_Key_Repeat_Settings is
   package Codec renames CCL.Setting_Codecs.Key_Repeat;

   --  The schema's range types are Key_Repeat's bounds, so every decoded
   --  value converts.
   pragma Compile_Time_Error
     (Long_Long_Integer (Codec.Repeat_Delay_Value'First) /= Long_Long_Integer (Key_Repeat.Delay_Ms'First)
      or else Long_Long_Integer (Codec.Repeat_Delay_Value'Last) /= Long_Long_Integer (Key_Repeat.Delay_Ms'Last)
      or else Long_Long_Integer (Codec.Repeat_Rate_Value'First) /= Long_Long_Integer (Key_Repeat.Rate_Hz'First)
      or else Long_Long_Integer (Codec.Repeat_Rate_Value'Last) /= Long_Long_Integer (Key_Repeat.Rate_Hz'Last),
      "the CCL key-repeat bounds must be Key_Repeat's");

   procedure Decode (Source : String; Item : out Key_Repeat.Settings; Success : out Boolean) is
      Setting : Codec.Key_Repeat_Value;
   begin
      Item := Key_Repeat.DEFAULT_SETTINGS;
      Codec.Decode (Source, Setting, Success);
      if Success then
         Item := (Enabled       => Setting.enabled,
                  Initial_Delay => Key_Repeat.Delay_Ms (Setting.delay_ms),
                  Rate          => Key_Repeat.Rate_Hz (Setting.rate_hz));
      end if;
   end Decode;
end Desktop_Key_Repeat_Settings;
