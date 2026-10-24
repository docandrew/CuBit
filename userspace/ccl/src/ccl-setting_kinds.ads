--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-kinds (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog.
pragma Ada_2022;
with CCL.Interfaces.Calc_Types;
with CCL.Interfaces.Desktop_Launch;
with CCL.Interfaces.Desktop_Input;
with CCL.Interfaces.Desktop_Display;
with CCL.Interfaces.GPU_Power;
with CCL.Interfaces.Net_Interface;

--  The settings whose values are CCL values of a declared type, by key
--  (a family's members included), with each type's declarations and root.
package CCL.Setting_Kinds with SPARK_Mode, Pure is
   type Setting_Kind is
     (Untyped,
      Calc_Options_Setting,
      Launch_Entry_Setting,
      Key_Repeat_Setting,
      Display_Scale_Setting,
      Display_Mode_Setting,
      Display_Modeset_Setting,
      GPU_Waitboost_Setting,
      Net_Interface_Setting,
      GPU_Rendering_Setting);

   function Starts (Key, Prefix : String) return Boolean is
     (Key'Length > Prefix'Length
      and then Key (Key'First .. Key'First + Prefix'Length - 1) = Prefix);

   function Kind_Of (Key : String) return Setting_Kind is
     (if Key = "calculator.options" then Calc_Options_Setting
      elsif Starts (Key, "desktop.launch.") then Launch_Entry_Setting
      elsif Key = "desktop.input.key-repeat" then Key_Repeat_Setting
      elsif Key = "desktop.display.scale" then Display_Scale_Setting
      elsif Starts (Key, "desktop.display.mode.") then Display_Mode_Setting
      elsif Key = "desktop.display.modeset" then Display_Modeset_Setting
      elsif Key = "gpu.power.waitboost" then GPU_Waitboost_Setting
      elsif Starts (Key, "net.interface.") then Net_Interface_Setting
      elsif Key = "desktop.render.gpu" then GPU_Rendering_Setting
      else Untyped);

   --  The type's declarations, and its root type's name.
   function Schema (Kind : Setting_Kind) return String is
     (case Kind is
         when Calc_Options_Setting => CCL.Interfaces.Calc_Types.TYPE_SOURCE,
         when Launch_Entry_Setting => CCL.Interfaces.Desktop_Launch.TYPE_SOURCE,
         when Key_Repeat_Setting => CCL.Interfaces.Desktop_Input.TYPE_SOURCE,
         when Display_Scale_Setting => CCL.Interfaces.Desktop_Display.TYPE_SOURCE,
         when Display_Mode_Setting => CCL.Interfaces.Desktop_Display.MODE_TYPE_SOURCE,
         when Display_Modeset_Setting => CCL.Interfaces.Desktop_Display.MODESET_TYPE_SOURCE,
         when GPU_Waitboost_Setting => CCL.Interfaces.GPU_Power.TYPE_SOURCE,
         when Net_Interface_Setting => CCL.Interfaces.Net_Interface.TYPE_SOURCE,
         when GPU_Rendering_Setting => CCL.Interfaces.GPU_Power.RENDERING_TYPE_SOURCE,
         when Untyped => "");
   function Root_Name (Kind : Setting_Kind) return String is
     (case Kind is
         when Calc_Options_Setting => "Calc_Options",
         when Launch_Entry_Setting => "Launch_Entry",
         when Key_Repeat_Setting => "Key_Repeat",
         when Display_Scale_Setting => "Display_Scale",
         when Display_Mode_Setting => "Display_Mode",
         when Display_Modeset_Setting => "Display_Modeset",
         when GPU_Waitboost_Setting => "GPU_Waitboost",
         when Net_Interface_Setting => "Net_Interface",
         when GPU_Rendering_Setting => "GPU_Rendering",
         when Untyped => "");
end CCL.Setting_Kinds;
