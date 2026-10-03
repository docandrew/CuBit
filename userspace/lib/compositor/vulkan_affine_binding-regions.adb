with Compositor_Transform;
with Vulkan_Region_FFI;
package body Vulkan_Affine_Binding.Regions with SPARK_Mode is
   procedure Draw_Output (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Result : out Outcome) is
      Full : constant A.Result := A.Plan (Screen, Surface, Over, Straight_Alpha);
      P : constant A.Result := (if Full.Visible then
        A.Clip (Full.Value, Screen.Width, Screen.Height, Damage) else (Visible => False));
      Accepted : Boolean;
   begin
      if not Compositor_Source_Region.Valid (Region) or else (Straight_Alpha and not Over) then
         Result := Rejected;
      elsif not P.Visible then
         Result := Empty;
      else
         Vulkan_Region_FFI.Record_Draw (Borrowed, P.Value,
           Compositor_Transform.Build (P.Value, Screen.Width, Screen.Height),
           Screen.Width, Screen.Height, Region, Accepted);
         Result := (if Accepted then Recorded else Rejected);
      end if;
   end Draw_Output;
end Vulkan_Affine_Binding.Regions;
