package body Compositor_Client_Output with SPARK_Mode is
   function Plan
     (Screen : G.Output; Target_W, Target_H, Source_W, Source_H, X, Y : Natural;
      Blit : Desktop_Composition.Blit_Plan) return Result is
   begin
      if not Eligible (Screen, Target_W, Target_H, Source_W, Source_H, X, Y, Blit) then
         return (Valid => False);
      end if;
      return (True,
        (G.Logical_Coordinate (X), G.Logical_Coordinate (Y),
         G.Logical_Coordinate (X + Source_W), G.Logical_Coordinate (Y + Source_H)),
        (G.Pixel_Edge (Blit.Target_X), G.Pixel_Edge (Blit.Target_Y),
         G.Pixel_Edge (Blit.Target_X + Blit.Width), G.Pixel_Edge (Blit.Target_Y + Blit.Height)));
   end Plan;
end Compositor_Client_Output;
