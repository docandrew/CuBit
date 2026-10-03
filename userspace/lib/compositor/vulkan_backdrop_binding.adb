with Vulkan_Backdrop_FFI;
package body Vulkan_Backdrop_Binding with SPARK_Mode is
   procedure Draw_Output
     (Borrowed : System.Address; W, H, Source_W, Source_H : B.G.Physical_Extent;
      Mode : B.S.Placement; Damage : B.G.Physical_Rectangle; Result : out Outcome) is
      P : constant B.Result := B.Plan (W, H, Source_W, Source_H, Mode, Damage);
      Accepted : Boolean;
   begin
      if not P.Visible then Result := Empty; return; end if;
      Vulkan_Backdrop_FFI.Record_Draw (Borrowed, P.Value, W, H, Accepted);
      Result := (if Accepted then Recorded else Rejected);
   end Draw_Output;
end Vulkan_Backdrop_Binding;
