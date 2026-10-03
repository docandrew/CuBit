package body Compositor_Row_Copy with SPARK_Mode is
   function Plan
     (Screen : G.Output; Surface : G.Logical_Rectangle;
      Source_Width, Source_Height : G.Physical_Extent;
      Damage : G.Physical_Rectangle) return Region
   is
      Left : constant Wide := Wide (Surface.Left) - Wide (Screen.X);
      Top : constant Wide := Wide (Surface.Top) - Wide (Screen.Y);
      Right : constant Wide := Wide (Surface.Right) - Wide (Screen.X);
      Bottom : constant Wide := Wide (Surface.Bottom) - Wide (Screen.Y);
      L, T, R, B : Wide;
   begin
      if Screen.Rotation /= G.Unrotated or else
        Screen.Scale.Numerator /= Screen.Scale.Denominator or else
        Right - Left /= Wide (Source_Width) or else
        Bottom - Top /= Wide (Source_Height)
      then return (others => 0); end if;
      L := Wide'Max (Wide (Damage.Left), Wide'Max (0, Left));
      T := Wide'Max (Wide (Damage.Top), Wide'Max (0, Top));
      R := Wide'Min (Wide (Damage.Right), Wide'Min (Wide (Screen.Width), Right));
      B := Wide'Min (Wide (Damage.Bottom), Wide'Min (Wide (Screen.Height), Bottom));
      if L >= R or T >= B then return (others => 0); end if;
      return (Natural (L - Left), Natural (T - Top), Natural (L), Natural (T),
              Natural (R - L), Natural (B - T));
   end Plan;
end Compositor_Row_Copy;
