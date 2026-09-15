--  Stable, semantic palette fields. No geometry, callbacks, paths or authority.
package CuBit.UI.Theme_Data with SPARK_Mode is
   type Field is
     (Desktop, Panel, Face, Edge, Shadow, Text, Muted, Accent, Good, Danger,
      Input_Field, Selection, Selection_Text, Highlight, Dark_Shadow,
      Active_Title_Top, Active_Title_Bottom, Inactive_Title_Top, Inactive_Title_Bottom);
   subtype RGB_Color is Color range 0 .. 16#FFFFFF#;
   type Palette_Colors is array (Field) of RGB_Color;
   function Name (Item : Field) return String;
   function Colors (Value : Theme) return Palette_Colors;
   function To_Theme (Value : Palette_Colors) return Theme;
   subtype Chunk_Index is Natural range 0 .. 3;
   type Color_Chunk is array (Positive range 1 .. 3) of Unsigned_64;
   function Chunk (Value : Palette_Colors; Index : Chunk_Index) return Color_Chunk;
   procedure Merge (Value : in out Palette_Colors; Index : Chunk_Index;
                    Data : Color_Chunk; Valid : out Boolean)
     with Post => (if not Valid then Value = Value'Old);
end CuBit.UI.Theme_Data;
