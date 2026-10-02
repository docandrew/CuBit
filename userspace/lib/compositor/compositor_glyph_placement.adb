package body Compositor_Glyph_Placement with SPARK_Mode is
   use type G.Orientation;
   function Encode (Value : A.Signed) return A.Word is (A.Word (Value))
     with Pre => Value in 0 .. 65_535,
       Post => A.Signed (Encode'Result) = Value;
   function Snap (Logical : G.Logical_Coordinate; Origin : G.Output_Origin;
                  Scale : G.UI_Scale) return Position is
      N : constant A.Signed := (A.Signed (Logical) - A.Signed (Origin)) * A.Signed (Scale.Numerator);
      D : constant A.Signed := A.Signed (Scale.Denominator);
      Q : constant A.Signed := N / D;
      R : constant A.Signed := N rem D;
   begin
      if 2 * R >= D then return Q + 1;
      elsif 2 * R < -D then return Q - 1;
      else return Q; end if;
   end Snap;
   function Plan (Screen : G.Output; Origin : G.Logical_Point) return A.Result is
      Raster : constant L.Layout := L.Plan (Screen.Scale);
      Rotated : constant Boolean := Screen.Rotation in G.Clockwise_90 | G.Clockwise_270;
      W : constant A.Signed := A.Signed (if Rotated then Screen.Height else Screen.Width);
      H : constant A.Signed := A.Signed (if Rotated then Screen.Width else Screen.Height);
      X : constant Position := Snap (Origin.X, Screen.X, Screen.Scale);
      Y : constant Position := Snap (Origin.Y, Screen.Y, Screen.Scale);
      Left : constant A.Signed := A.Signed'Max (0, X);
      Top : constant A.Signed := A.Signed'Max (0, Y);
      Right : constant A.Signed := A.Signed'Min (W, X + A.Signed (Raster.Width));
      Bottom : constant A.Signed := A.Signed'Min (H, Y + A.Signed (Raster.Height));
      X0, Y0, X1, Y1 : A.Signed;
   begin
      if Left >= Right or Top >= Bottom then return (Visible => False); end if;
      case Screen.Rotation is
         when G.Unrotated => X0 := Left; Y0 := Top; X1 := Right; Y1 := Bottom;
         when G.Clockwise_90 =>
            X0 := A.Signed (Screen.Width) - Bottom; Y0 := Left;
            X1 := A.Signed (Screen.Width) - Top; Y1 := Right;
         when G.Clockwise_180 =>
            X0 := A.Signed (Screen.Width) - Right; Y0 := A.Signed (Screen.Height) - Bottom;
            X1 := A.Signed (Screen.Width) - Left; Y1 := A.Signed (Screen.Height) - Top;
         when G.Clockwise_270 =>
            X0 := Top; Y0 := A.Signed (Screen.Height) - Right;
            X1 := Bottom; Y1 := A.Signed (Screen.Height) - Left;
      end case;
      pragma Assert (0 <= X0 and X0 < X1 and X1 <= A.Signed (Screen.Width));
      pragma Assert (0 <= Y0 and Y0 < Y1 and Y1 <= A.Signed (Screen.Height));
      pragma Assert (-X in -(2 ** 31) .. 2 ** 31 and -Y in -(2 ** 31) .. 2 ** 31);
      return (True, (-X, -Y, A.Word (Raster.Width), A.Word (Raster.Height), 1, 1,
        A.Word (G.Orientation'Pos (Screen.Rotation)), Encode (X0), Encode (Y0),
        Encode (X1 - X0), Encode (Y1 - Y0), 1));
   end Plan;
end Compositor_Glyph_Placement;
