with Compositor_Software_Text;
with Compositor_Software_Text_Target;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => Text) is
   package Glyphs renames Compositor_Software_Text;
   Text : Glyphs.State;
   function Selected return Boolean is (True);
   function Software_Text return Boolean is
   begin
      pragma Assert (Glyphs.Valid (Text));
      return Glyphs.Quiescent (Text);
   end Software_Text;
   procedure Begin_Output
     (Target : System.Address; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start) is
      pragma Unreferenced (Target, Screen, Secondary);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := (if Glyphs.Quiescent (Text) then Started else Start_Unsafe);
   end Begin_Output;
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
   begin pragma Assert (Glyphs.Valid (Text)); Drawn := False; Must_Restart := False; end Draw_Fill;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) is
      pragma Unreferenced (Secondary);
      OK : Boolean;
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Repaint := False; Must_Restart := False;
      if Length = 0 then Drawn := True; return; end if;
      if not Glyphs.Enabled (Text) or else not Compositor_Software_Text_Target.Supported
        (Target, Target_Bytes, Screen) then return; end if;
      for I in 1 .. Length loop
         Compositor_Software_Text_Target.Paint
           (Text, Target, Target_Bytes, (0, Items (I).Code, Screen.Scale),
            Screen, (Items (I).Cell.Left, Items (I).Cell.Top),
            Compositor_Text.Clip (Screen, Items (I).Cell, Damage), Tint, OK);
         if not OK then
            -- Earlier glyphs may have blended into the target. Replay before
            -- publication rather than blending those glyphs a second time.
            Glyphs.Disable (Text);
            Repaint := I > 1;
            return;
         end if;
         pragma Loop_Invariant (Glyphs.Valid (Text));
      end loop;
      Drawn := True;
   end Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Must_Restart := False;
   end Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False) is
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False;
      Must_Restart := False;
   end Draw_Output;
   procedure Complete_Output
     (Target : System.Address; Secondary, Poll : Boolean; Result : out Render_Completion) is
      pragma Unreferenced (Target, Secondary, Poll);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := (if Glyphs.Quiescent (Text) then Complete else Unsafe);
   end Complete_Output;
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) is
      pragma Unreferenced (Pixels);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := Source_Retired;
   end Forget_Source;
   procedure Forget_Targets (Result : out Target_Release) is
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := Targets_Retired;
   end Forget_Targets;
end Desktop_Compositor;
