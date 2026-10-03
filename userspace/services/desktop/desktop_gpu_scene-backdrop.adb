with Compositor_Image_Sampling;
with Desktop_Backdrop_Style;
package body Desktop_GPU_Scene.Backdrop with SPARK_Mode is
   procedure Capture
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Accepted : out Boolean)
   is
      use CuBit.Appearance;
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Color : constant V.A.Word := Desktop_Backdrop_Style.Color (Style);
      Has_Image : constant Boolean := Desktop_Backdrop_Style.Has_Image (Style.Backdrop);
      Mode : constant Compositor_Image_Sampling.Placement := Desktop_Backdrop_Style.Placement (Style.Position);
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      if Has_Image then
         Pin_Image (S, Source, Accepted);
         if not Accepted then return; end if;
      end if;
      -- Paint the entire physical output under Fit/Center letterboxing. This
      -- deliberately ignores logical DPI and desktop origin, like Paint.
      V.Append_Physical_Fill (S.Scene, (0, 0, Screen.Width, Screen.Height), Color, Accepted);
      if Accepted and Has_Image then
         V.Append_Backdrop (S.Scene, Source, V.A.G.Physical_Extent (Desktop_Backdrop_Style.Width (Style.Backdrop)),
           V.A.G.Physical_Extent (Desktop_Backdrop_Style.Height (Style.Backdrop)), Mode, Accepted);
      end if;
      -- Failure rejects the whole capture; never present just its background.
      if not Accepted then S.Invalid := True; end if;
   end Capture;
end Desktop_GPU_Scene.Backdrop;
