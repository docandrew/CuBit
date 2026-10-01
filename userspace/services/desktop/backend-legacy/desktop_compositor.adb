package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => null) is
   function Selected return Boolean is (False);
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Client;
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean) is
      pragma Unreferenced (Pixels);
   begin
      Safe := True;
   end Forget_Source;
end Desktop_Compositor;
