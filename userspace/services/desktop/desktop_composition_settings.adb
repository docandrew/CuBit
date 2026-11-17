------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.Desktop_Composition;

package body Desktop_Composition_Settings is
   package Codec renames CCL.Setting_Codecs.Desktop_Composition;
   use type Codec.Composition_Path_Value;

   procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      Setting : Codec.Desktop_Composition_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Item := (if not Success then (others => <>)
               else (case Setting.path is
                       when Codec.CPU => (Compositor_Backend_Selection.CPU_Composition, Direct),
                       when Codec.CPU_Staged => (Compositor_Backend_Selection.CPU_Composition, Staged),
                       when Codec.CPU_Copy => (Compositor_Backend_Selection.CPU_Composition, Copied),
                       when Codec.GPU => (Compositor_Backend_Selection.GPU_Composition, Direct)));
   end Decode;
end Desktop_Composition_Settings;
