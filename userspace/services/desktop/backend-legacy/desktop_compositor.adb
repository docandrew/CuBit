package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => null) is
   function Selected return Boolean is (False);
   function Software_Text return Boolean is (True);
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) is
   begin
      Drawn := Length = 0; Repaint := False; Must_Restart := False;
   end Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
   begin
      Drawn := False;
      Must_Restart := False;
   end Draw_Output;
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean) is
      pragma Unreferenced (Pixels);
   begin
      Safe := True;
   end Forget_Source;
   procedure Forget_Targets (Safe : out Boolean) is
   begin
      Safe := True;
   end Forget_Targets;
end Desktop_Compositor;
