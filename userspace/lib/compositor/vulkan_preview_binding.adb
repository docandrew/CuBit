with Vulkan_Preview_FFI;
package body Vulkan_Preview_Binding with SPARK_Mode is
   procedure Draw_Output
     (Borrowed : System.Address; Screen : P.G.Output;
      Bounds : P.G.Logical_Rectangle; Damage : P.G.Physical_Rectangle;
      Source_W, Source_H : P.S.Extent; Mode : P.S.Placement;
      Result : out Outcome)
   is
      Plan : constant P.Result := P.Plan (Screen, Bounds, Damage, Source_W, Source_H, Mode);
      Accepted : Boolean;
   begin
      if not Plan.Visible then Result := Empty; return; end if;
      Vulkan_Preview_FFI.Record_Draw (Borrowed, Plan, Screen.Width, Screen.Height, Accepted);
      Result := (if Accepted then Recorded else Rejected);
   end Draw_Output;
end Vulkan_Preview_Binding;
