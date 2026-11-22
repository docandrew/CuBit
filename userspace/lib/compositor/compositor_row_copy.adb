package body Compositor_Row_Copy with SPARK_Mode is
   function Readback_Plan
     (Width, Height, First_Row : G.Pixel_Edge;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural)
      return Readback_Batch
   is
      Row : constant Wide := Wide (Width) * 4;
      Rows : Wide;
   begin
      if Width = 0 or else Height = 0 or else First_Row >= Height or else
         Row > Wide (Target_Pitch) or else Row > Wide (Byte_Budget) or else
         Wide (Height) * Row > Wide (Source_Bytes) or else
         Wide (Height - 1) * Wide (Target_Pitch) + Row > Wide (Target_Bytes)
      then return (others => 0); end if;
      Rows := Wide'Min (Wide (Height - First_Row), Wide (Byte_Budget) / Row);
      return (Natural (Wide (First_Row) * Row),
              Natural (Wide (First_Row) * Wide (Target_Pitch)),
              Natural (Row), Natural (Rows));
   end Readback_Plan;
   function Readback_Region_Plan
     (Width, Height, First_Row : G.Pixel_Edge;
      Repair : G.Physical_Rectangle;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural)
      return Readback_Batch
   is
      Stride : constant Wide := Wide (Width) * 4;
      Row : constant Wide := (Wide (Repair.Right) - Wide (Repair.Left)) * 4;
      Remaining : constant Wide := Wide (Repair.Bottom) - Wide (Repair.Top) - Wide (First_Row);
      Y : constant Wide := Wide (Repair.Top) + Wide (First_Row);
      X : constant Wide := Wide (Repair.Left) * 4;
      Rows : Wide;
   begin
      if Width = 0 or else Height = 0 or else
         Repair.Left >= Repair.Right or else Repair.Top >= Repair.Bottom or else
         Repair.Right > Width or else Repair.Bottom > Height or else Remaining <= 0 or else
         Stride > Wide (Target_Pitch) or else Row > Wide (Byte_Budget) or else
         Wide (Height) * Stride > Wide (Source_Bytes) or else
         Wide (Height - 1) * Wide (Target_Pitch) + Stride > Wide (Target_Bytes)
      then return (others => 0); end if;
      Rows := Wide'Min (Remaining, Wide (Byte_Budget) / Row);
      return (Natural (Y * Stride + X), Natural (Y * Wide (Target_Pitch) + X),
              Natural (Row), Natural (Rows));
   end Readback_Region_Plan;
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
