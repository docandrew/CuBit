--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit.
pragma Ada_2022;
with CCL.Setting_Codecs.Calc_Options;
with CCL.Setting_Codecs.Launch_Entry;
with CCL.Setting_Codecs.Key_Repeat;
with CCL.Setting_Codecs.Display_Scale;
with CCL.Setting_Codecs.Display_Mode;
with CCL.Setting_Codecs.Display_Modeset;
with CCL.Setting_Codecs.GPU_Waitboost;
with CCL.Setting_Codecs.Net_Interface;
with CCL.Setting_Codecs.GPU_Rendering;

package body CCL.Setting_Codecs.Checks with SPARK_Mode is
   package Kinds renames CCL.Setting_Kinds;
   --  Only the verdict matters here, not the decoded value.
   pragma Warnings (GNATprove, Off, """Item"" is set by ""Decode"" but not used after the call");

   function Canonical (Kind : CCL.Setting_Kinds.Setting_Kind; Source : String) return Boolean is
      OK : Boolean := False;
   begin
      case Kind is
         when Kinds.Calc_Options_Setting =>
            declare
               Item : Calc_Options.Calc_Options_Value;
            begin
               Calc_Options.Decode (Source, Item, OK);
            end;
         when Kinds.Launch_Entry_Setting =>
            declare
               Item : Launch_Entry.Launch_Entry_Value;
            begin
               Launch_Entry.Decode (Source, Item, OK);
            end;
         when Kinds.Key_Repeat_Setting =>
            declare
               Item : Key_Repeat.Key_Repeat_Value;
            begin
               Key_Repeat.Decode (Source, Item, OK);
            end;
         when Kinds.Display_Scale_Setting =>
            declare
               Item : Display_Scale.Display_Scale_Value;
            begin
               Display_Scale.Decode (Source, Item, OK);
            end;
         when Kinds.Display_Mode_Setting =>
            declare
               Item : Display_Mode.Display_Mode_Value;
            begin
               Display_Mode.Decode (Source, Item, OK);
            end;
         when Kinds.Display_Modeset_Setting =>
            declare
               Item : Display_Modeset.Display_Modeset_Value;
            begin
               Display_Modeset.Decode (Source, Item, OK);
            end;
         when Kinds.GPU_Waitboost_Setting =>
            declare
               Item : GPU_Waitboost.GPU_Waitboost_Value;
            begin
               GPU_Waitboost.Decode (Source, Item, OK);
            end;
         when Kinds.Net_Interface_Setting =>
            declare
               Item : Net_Interface.Net_Interface_Value;
            begin
               Net_Interface.Decode (Source, Item, OK);
            end;
         when Kinds.GPU_Rendering_Setting =>
            declare
               Item : GPU_Rendering.GPU_Rendering_Value;
            begin
               GPU_Rendering.Decode (Source, Item, OK);
            end;
         when Kinds.Untyped =>
            null;
      end case;
      return OK;
   end Canonical;
end CCL.Setting_Codecs.Checks;

