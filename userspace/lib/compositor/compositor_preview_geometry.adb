package body Compositor_Preview_Geometry with SPARK_Mode is
   use type S.Wide, G.Pixel_Edge;
   -- First output pixel whose centre reaches a logical edge. Signed division
   -- in Ada truncates toward zero; explicitly implement mathematical ceiling.
   function Centre_Edge
     (Edge : G.Logical_Coordinate; Origin : G.Output_Origin;
      Scale : G.UI_Scale; Limit : G.Physical_Extent) return G.Pixel_Edge
   is
      N : constant S.Wide := 2 * S.Wide (Scale.Numerator) *
        (S.Wide (Edge) - S.Wide (Origin)) - S.Wide (Scale.Denominator);
      D : constant S.Wide := 2 * S.Wide (Scale.Denominator);
      Q : S.Wide := N / D;
   begin
      if N > 0 and then N rem D /= 0 then Q := Q + 1; end if;
      return G.Pixel_Edge (S.Wide'Max (0, S.Wide'Min (S.Wide (Limit), Q)));
   end Centre_Edge;
   function Plan
     (Screen : G.Output; Bounds : G.Logical_Rectangle;
      Damage : G.Physical_Rectangle; Source_W, Source_H : S.Extent;
      Mode : S.Placement) return Result
   is
      W : constant S.Wide := S.Wide (Bounds.Right) - S.Wide (Bounds.Left);
      H : constant S.Wide := S.Wide (Bounds.Bottom) - S.Wide (Bounds.Top);
      Mapping, Clipped : A.Result;
      UW : constant G.Physical_Extent :=
        (if Screen.Rotation in G.Unrotated | G.Clockwise_180 then Screen.Width else Screen.Height);
      UH : constant G.Physical_Extent :=
        (if Screen.Rotation in G.Unrotated | G.Clockwise_180 then Screen.Height else Screen.Width);
      L, T, R, B : G.Pixel_Edge;
      Area : G.Physical_Rectangle;
   begin
      -- Match Paint_Output's supported logical range before narrowing.
      if W not in 1 .. S.Wide (S.Extent'Last) or else
         H not in 1 .. S.Wide (S.Extent'Last)
      then return (Visible => False); end if;
      Mapping := A.Plan (Screen, Bounds);
      if not Mapping.Visible then return (Visible => False); end if;
      L := Centre_Edge (Bounds.Left, Screen.X, Screen.Scale, UW);
      R := Centre_Edge (Bounds.Right, Screen.X, Screen.Scale, UW);
      T := Centre_Edge (Bounds.Top, Screen.Y, Screen.Scale, UH);
      B := Centre_Edge (Bounds.Bottom, Screen.Y, Screen.Scale, UH);
      case Screen.Rotation is
         when G.Unrotated => Area := (L, T, R, B);
         when G.Clockwise_90 => Area := (Screen.Width - B, L, Screen.Width - T, R);
         when G.Clockwise_180 => Area := (Screen.Width - R, Screen.Height - B, Screen.Width - L, Screen.Height - T);
         when G.Clockwise_270 => Area := (T, Screen.Height - R, B, Screen.Height - L);
      end case;
      Area := (G.Pixel_Edge'Max (Area.Left, Damage.Left), G.Pixel_Edge'Max (Area.Top, Damage.Top),
        G.Pixel_Edge'Min (Area.Right, Damage.Right), G.Pixel_Edge'Min (Area.Bottom, Damage.Bottom));
      Clipped := A.Clip (Mapping.Value, Screen.Width, Screen.Height, Area);
      if not Clipped.Visible then return (Visible => False); end if;
      return (Visible => True, Transform => Clipped.Value,
        Placement => S.Prepare (S.Extent (W), S.Extent (H), Source_W, Source_H, Mode));
   end Plan;
end Compositor_Preview_Geometry;
