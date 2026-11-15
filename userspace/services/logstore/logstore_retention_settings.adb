------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.Log_Retention;

package body Logstore_Retention_Settings is
   package Codec renames CCL.Setting_Codecs.Log_Retention;

   --  The schema's range types are Log_Fanout's ceilings, so every decoded
   --  value converts.
   pragma Compile_Time_Error
     (Long_Long_Integer (Codec.Retained_Per_Source_Value'First) /= Long_Long_Integer (Log_Fanout.Least_Per_Source)
      or else Long_Long_Integer (Codec.Retained_Per_Source_Value'Last) /= Long_Long_Integer (Log_Fanout.Greatest_Per_Source)
      or else Long_Long_Integer (Codec.Retained_Boot_Value'First) /= Long_Long_Integer (Log_Fanout.Least_Boot)
      or else Long_Long_Integer (Codec.Retained_Boot_Value'Last) /= Long_Long_Integer (Log_Fanout.Greatest_Boot)
      or else Long_Long_Integer (Codec.Retained_Total_Value'First) /= Long_Long_Integer (Log_Fanout.Least_Total)
      or else Long_Long_Integer (Codec.Retained_Total_Value'Last) /= Long_Long_Integer (Log_Fanout.Greatest_Total),
      "the CCL retention bounds must be Log_Fanout's");

   procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      Setting : Codec.Log_Retention_Value;
   begin
      Item := Log_Fanout.Until_Configured;
      Codec.Decode (Source, Setting, Success);
      if Success then
         Item := (Per_Source => Log_Fanout.Per_Source_Ceiling (Setting.per_source),
                  Boot       => Log_Fanout.Boot_Ceiling (Setting.boot),
                  Total      => Log_Fanout.Total_Ceiling (Setting.total));
         Success := Log_Fanout.Consistent (Item);
         if not Success then
            Item := Log_Fanout.Until_Configured;
         end if;
      end if;
   end Decode;
end Logstore_Retention_Settings;
