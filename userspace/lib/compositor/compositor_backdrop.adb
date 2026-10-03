package body Compositor_Backdrop with SPARK_Mode is
   subtype Coordinate is Natural range 0 .. 65_535;
   subtype Size is Positive range 1 .. 65_535;
   -- Keep ABI conversion proof separate from aspect-ratio multiplication.
   function Encode
     (P : S.Layout; W, H, SW, SH : G.Physical_Extent;
      L, T : Coordinate; CW, CH : Size) return Draw
     with Pre => L < Natural (W) and T < Natural (H) and
       CW <= Natural (W) - L and CH <= Natural (H) - T,
       Post => Valid (Encode'Result, W, H) and then
         Encode'Result.Source_W = Word (SW) and then
         Encode'Result.Source_H = Word (SH) and then
         Encode'Result.Width = Wide (S.Draw_Width (P)) and then
         Encode'Result.Height = Wide (S.Draw_Height (P)) and then
         Encode'Result.Left = Signed (S.Left (P)) and then
         Encode'Result.Top = Signed (S.Top (P))
   is
      D : constant Draw := (Signed (S.Left (P)), Signed (S.Top (P)),
        Wide (S.Draw_Width (P)), Wide (S.Draw_Height (P)),
        Pixel_Word (L), Pixel_Word (T), Pixel_Word (CW), Pixel_Word (CH), Word (SW), Word (SH));
   begin
      pragma Assert (Natural (D.Clip_X) = L);
      pragma Assert (Natural (D.Clip_Y) = T);
      pragma Assert (Natural (D.Clip_W) = CW);
      pragma Assert (Natural (D.Clip_H) = CH);
      pragma Assert (D.Clip_W /= 0 and D.Clip_H /= 0);
      pragma Assert (Natural (D.Clip_X) + Natural (D.Clip_W) <= Natural (W));
      pragma Assert (Natural (D.Clip_Y) + Natural (D.Clip_H) <= Natural (H));
      return D;
   end Encode;

   function Plan
     (W, H, Source_W, Source_H : G.Physical_Extent;
      Mode : S.Placement; Damage : G.Physical_Rectangle) return Result
   is
      L : constant Natural := Natural'Min (Natural (Damage.Left), Natural (W));
      T : constant Natural := Natural'Min (Natural (Damage.Top), Natural (H));
      R : constant Natural := Natural'Min (Natural (Damage.Right), Natural (W));
      B : constant Natural := Natural'Min (Natural (Damage.Bottom), Natural (H));
      P : S.Layout;
   begin
      if L >= R or T >= B then return (Visible => False); end if;
      P := S.Prepare (S.Extent (W), S.Extent (H), S.Extent (Source_W), S.Extent (Source_H), Mode);
      return (True, Encode (P, W, H, Source_W, Source_H, L, T, R - L, B - T));
   end Plan;
end Compositor_Backdrop;
