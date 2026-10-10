with Compositor_Image_Sampling;
with Desktop_Backdrop_Style;
with Compositor_Preview_Geometry;
package body Desktop_GPU_Scene.Backdrop with SPARK_Mode is
   procedure Capture_Preview
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Bounds : V.A.G.Logical_Rectangle;
      Accepted : out Boolean)
   is
      package Sampling renames Compositor_Image_Sampling;
      W : constant Sampling.Wide := Sampling.Wide (Bounds.Right) - Sampling.Wide (Bounds.Left);
      H : constant Sampling.Wide := Sampling.Wide (Bounds.Bottom) - Sampling.Wide (Bounds.Top);
      Has_Image : constant Boolean := Desktop_Backdrop_Style.Has_Image (Style.Backdrop);
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Plan : Compositor_Preview_Geometry.Result;
      Area : V.A.G.Physical_Rectangle := (0, 0, 0, 0);
      use type V.A.Word;
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      if W not in 1 .. Sampling.Wide (Sampling.Extent'Last) or
         H not in 1 .. Sampling.Wide (Sampling.Extent'Last)
      then S.Invalid := True; return; end if;
      if Has_Image then
         Pin_Image (S, Source, Accepted);
         if not Accepted then return; end if;
      end if;
      Plan := Compositor_Preview_Geometry.Plan (Screen, Bounds,
        (0, 0, Screen.Width, Screen.Height), 1, 1, Sampling.Fill);
      if Plan.Visible then
         Area := (V.A.G.Pixel_Edge (Plan.Transform.Clip_X),
           V.A.G.Pixel_Edge (Plan.Transform.Clip_Y),
           V.A.G.Pixel_Edge (Plan.Transform.Clip_X + Plan.Transform.Clip_W),
           V.A.G.Pixel_Edge (Plan.Transform.Clip_Y + Plan.Transform.Clip_H));
      end if;
      V.Append_Physical_Fill (S.Scene, Area, Desktop_Backdrop_Style.Color (Style), Accepted);
      if Accepted and Has_Image then
         V.Append_Preview (S.Scene, Source, Bounds,
           (Desktop_Backdrop_Style.Width (Style.Backdrop),
            Desktop_Backdrop_Style.Height (Style.Backdrop),
            Desktop_Backdrop_Style.Placement (Style.Position)), Accepted);
      end if;
      if not Accepted then S.Invalid := True; end if;
   end Capture_Preview;
   procedure Capture
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean)
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
      Set_Clip (S, Damage, Accepted);
      if not Accepted then return; end if;
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
