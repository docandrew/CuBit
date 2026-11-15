------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Setting_Codecs.Desktop_Composition;

package body Desktop_Composition_Settings is
   package Codec renames CCL.Setting_Codecs.Desktop_Composition;
   use type Codec.Composition_Path_Value;

   procedure Decode (Source : String; Path : out Composition; Success : out Boolean) is
      Setting : Codec.Desktop_Composition_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Path := (if not Success then Default_Composition
               elsif Setting.path = Codec.GPU then Compositor_Backend_Selection.GPU_Composition
               else Compositor_Backend_Selection.CPU_Composition);
   end Decode;
end Desktop_Composition_Settings;
