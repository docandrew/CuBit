package body CuBit.UI.Theme_Data with SPARK_Mode is
   function Chunk (Value : Palette_Colors; Index : Chunk_Index) return Color_Chunk is
      Result : Color_Chunk := [others => 0];
   begin
      for Offset in 0 .. 5 loop
         if Index * 6 + Offset <= Field'Pos (Field'Last) then
            Result (Offset / 2 + 1) := Result (Offset / 2 + 1) or
              Shift_Left (Unsigned_64 (Value (Field'Val (Index * 6 + Offset))),
                (if Offset mod 2 = 0 then 0 else 32));
         end if;
      end loop;
      return Result;
   end Chunk;
   procedure Merge (Value : in out Palette_Colors; Index : Chunk_Index;
                    Data : Color_Chunk; Valid : out Boolean) is
      Candidate : Palette_Colors := Value;
      Color : Unsigned_64;
   begin
      Valid := False;
      for Offset in 0 .. 5 loop
         Color := Shift_Right (Data (Offset / 2 + 1),
           (if Offset mod 2 = 0 then 0 else 32)) and 16#FFFF_FFFF#;
         if Color > 16#FFFFFF# then return; end if;
         if Index * 6 + Offset <= Field'Pos (Field'Last) then
            Candidate (Field'Val (Index * 6 + Offset)) := RGB_Color (Color);
         elsif Color /= 0 then return;
         end if;
      end loop;
      Value := Candidate;
      Valid := True;
   end Merge;
   function Name (Item : Field) return String is
     (case Item is
      when Desktop => "desktop", when Panel => "panel", when Face => "face",
      when Edge => "edge", when Shadow => "shadow", when Text => "text",
      when Muted => "muted", when Accent => "accent", when Good => "good",
      when Danger => "danger", when Input_Field => "field",
      when Selection => "selection", when Selection_Text => "selection-text",
      when Highlight => "highlight", when Dark_Shadow => "dark-shadow",
      when Active_Title_Top => "active-title-top",
      when Active_Title_Bottom => "active-title-bottom",
      when Inactive_Title_Top => "inactive-title-top",
      when Inactive_Title_Bottom => "inactive-title-bottom");
   function Colors (Value : Theme) return Palette_Colors is
     ([Value.desktop, Value.panel, Value.face, Value.edge, Value.shadow,
       Value.text, Value.muted, Value.accent, Value.good, Value.danger,
       Value.field, Value.selection, Value.selectionText, Value.highlight,
       Value.darkShadow, Value.activeTitleTop, Value.activeTitleBottom,
       Value.inactiveTitleTop, Value.inactiveTitleBottom]);
   function To_Theme (Value : Palette_Colors) return Theme is
     ((Value (Desktop), Value (Panel), Value (Face), Value (Edge), Value (Shadow),
       Value (Text), Value (Muted), Value (Accent), Value (Good), Value (Danger),
       Value (Input_Field), Value (Selection), Value (Selection_Text),
       Value (Highlight), Value (Dark_Shadow), Value (Active_Title_Top),
       Value (Active_Title_Bottom), Value (Inactive_Title_Top), Value (Inactive_Title_Bottom)));
end CuBit.UI.Theme_Data;
