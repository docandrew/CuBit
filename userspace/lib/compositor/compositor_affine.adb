package body Compositor_Affine with SPARK_Mode is
   use type G.Pixel_Edge, G.Logical_Coordinate;
   function Encode (Value : Signed) return Word is (Word (Value))
     with Pre => Value in 0 .. 2 ** 32 - 1,
       Post => Signed (Encode'Result) = Value;
   function Edge (Logical : G.Logical_Coordinate; Origin : G.Output_Origin;
                  Scale : G.UI_Scale; Limit : G.Physical_Extent) return G.Pixel_Edge
     with Post => Edge'Result <= Limit
   is
      N : constant Signed := 2 * Signed (Scale.Numerator) *
        (Signed (Logical) - Signed (Origin)) - Signed (Scale.Denominator);
      D : constant Signed := 2 * Signed (Scale.Denominator);
   begin
      if N <= 0 then return 0; end if;
      return G.Pixel_Edge (Signed'Min (Signed (Limit), (N + D - 1) / D));
   end Edge;
   function Plan (Screen : G.Output; Surface : G.Logical_Rectangle;
                  Over : Boolean := False) return Result is
      Rotated : constant Boolean := Screen.Rotation in G.Clockwise_90 | G.Clockwise_270;
      W : constant G.Physical_Extent := (if Rotated then Screen.Height else Screen.Width);
      H : constant G.Physical_Extent := (if Rotated then Screen.Width else Screen.Height);
      L : constant G.Pixel_Edge := Edge (Surface.Left, Screen.X, Screen.Scale, W);
      T : constant G.Pixel_Edge := Edge (Surface.Top, Screen.Y, Screen.Scale, H);
      R : constant G.Pixel_Edge := Edge (Surface.Right, Screen.X, Screen.Scale, W);
      B : constant G.Pixel_Edge := Edge (Surface.Bottom, Screen.Y, Screen.Scale, H);
      X0, Y0, X1, Y1 : G.Pixel_Edge;
   begin
      if Surface.Left >= Surface.Right or else Surface.Top >= Surface.Bottom or else
        L >= R or else T >= B then return (Visible => False); end if;
      case Screen.Rotation is
         when G.Unrotated => X0 := L; Y0 := T; X1 := R; Y1 := B;
         when G.Clockwise_90 =>
            X0 := Screen.Width - B; Y0 := L; X1 := Screen.Width - T; Y1 := R;
         when G.Clockwise_180 =>
            X0 := Screen.Width - R; Y0 := Screen.Height - B;
            X1 := Screen.Width - L; Y1 := Screen.Height - T;
         when G.Clockwise_270 =>
            X0 := T; Y0 := Screen.Height - R; X1 := B; Y1 := Screen.Height - L;
      end case;
      pragma Assert (X0 < X1 and Y0 < Y1);
      pragma Assert (X1 <= Screen.Width and Y1 <= Screen.Height);
      declare D : constant Draw := (Signed (Screen.X) - Signed (Surface.Left),
        Signed (Screen.Y) - Signed (Surface.Top),
        Encode (Signed (Surface.Right) - Signed (Surface.Left)),
        Encode (Signed (Surface.Bottom) - Signed (Surface.Top)),
        Encode (Signed (Screen.Scale.Numerator)), Encode (Signed (Screen.Scale.Denominator)),
        Encode (Signed (G.Orientation'Pos (Screen.Rotation))),
        Encode (Signed (X0)), Encode (Signed (Y0)),
        Encode (Signed (X1 - X0)), Encode (Signed (Y1 - Y0)),
        (if Over then 1 else 0));
      begin
         pragma Assert (D.Origin_X in -(2 ** 31) .. 2 ** 31 and
                        D.Origin_Y in -(2 ** 31) .. 2 ** 31);
         pragma Assert (D.Logical_W in 1 .. 2 ** 31 and D.Logical_H in 1 .. 2 ** 31);
         pragma Assert (D.Numerator in 1 .. 16 and D.Denominator in 1 .. 16);
         pragma Assert (D.Rotation <= 3 and D.Over <= 1);
         pragma Assert (Signed (D.Clip_X) < Signed (Screen.Width) and Signed (D.Clip_Y) < Signed (Screen.Height));
         pragma Assert (Signed (D.Clip_W) in 1 .. Signed (Screen.Width) - Signed (D.Clip_X));
         pragma Assert (Signed (D.Clip_H) in 1 .. Signed (Screen.Height) - Signed (D.Clip_Y));
         return (True, D);
      end;
   end Plan;
   function Clip (D : Draw; Width, Height : G.Physical_Extent;
                  Area : G.Physical_Rectangle) return Result is
      -- Dimensions participate in the pre/postconditions, not the clip math.
      pragma Warnings (Off, Width);
      pragma Warnings (Off, Height);
      X0 : constant Signed := Signed'Max (Signed (D.Clip_X), Signed (Area.Left));
      Y0 : constant Signed := Signed'Max (Signed (D.Clip_Y), Signed (Area.Top));
      X1 : constant Signed := Signed'Min (Signed (D.Clip_X) + Signed (D.Clip_W), Signed (Area.Right));
      Y1 : constant Signed := Signed'Min (Signed (D.Clip_Y) + Signed (D.Clip_H), Signed (Area.Bottom));
   begin
      if X0 >= X1 or Y0 >= Y1 then return (Visible => False); end if;
      return (True, (D with delta Clip_X => Encode (X0), Clip_Y => Encode (Y0),
                     Clip_W => Encode (X1 - X0), Clip_H => Encode (Y1 - Y0)));
   end Clip;
end Compositor_Affine;
