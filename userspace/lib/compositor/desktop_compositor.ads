with System;
with Compositor_Formats;
with Desktop_Composition;
package Desktop_Compositor with SPARK_Mode, Abstract_State => Engine is
   function Selected return Boolean with Global => null;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Global => (In_Out => Engine);
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean)
     with Global => (In_Out => Engine);
end Desktop_Compositor;
