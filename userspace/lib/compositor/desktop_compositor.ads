with Compositor_Text;
with System;
with Compositor_Formats;
with Desktop_Composition;
with CuBit.Display_Geometry;
package Desktop_Compositor with SPARK_Mode, Abstract_State => Engine is
   function Selected return Boolean with Global => null;
   function Software_Text return Boolean with Global => (Input => Engine);
   -- Completes or cancels the complete batch before return. Repaint means a
   -- known-quiescent failure may have modified the target: replay its scene
   -- before publication. Must_Restart prohibits ordinary reuse/retirement.
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean)
     with Global => (In_Out => Engine);
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (In_Out => Engine);
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
     with Global => (In_Out => Engine);
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean)
     with Global => (In_Out => Engine);
   procedure Forget_Targets (Safe : out Boolean)
     with Global => (In_Out => Engine);
end Desktop_Compositor;
