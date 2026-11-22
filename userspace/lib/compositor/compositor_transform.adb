package body Compositor_Transform with SPARK_Mode is
   function Build (D : A.Draw; Width, Height : G.Physical_Extent)
     return Coefficients is
      N : constant Signed := Signed (D.Numerator);
      S : constant Signed := Signed (D.Denominator);
      U : constant Signed := D.Origin_X * N;
      V : constant Signed := D.Origin_Y * N;
      UD : constant Divisor := N * Signed (D.Logical_W);
      VD : constant Divisor := N * Signed (D.Logical_H);
   begin
      case D.Rotation is
         when 0 => return (U, S, 0, V, 0, S, UD, VD);
         when 1 => return (U, 0, S, V + Signed (Width) * S, -S, 0, UD, VD);
         when 2 => return (U + Signed (Width) * S, -S, 0,
                           V + Signed (Height) * S, 0, -S, UD, VD);
         when others => return (U + Signed (Height) * S, 0, -S, V, S, 0, UD, VD);
      end case;
   end Build;
   function Vertices (D : A.Draw; Width, Height : G.Physical_Extent) return Quad is
      C : constant Coefficients := Build (D, Width, Height);
      W : constant Signed := Signed (Width);
      H : constant Signed := Signed (Height);
   begin
      return (((C.U0, C.V0),
               (C.U0 + C.UX * W, C.V0 + C.VX * W),
               (C.U0 + C.UX * W + C.UY * H, C.V0 + C.VX * W + C.VY * H),
               (C.U0 + C.UY * H, C.V0 + C.VY * H)),
              C.UD, C.VD, A.Word (Width), A.Word (Height));
   end Vertices;
end Compositor_Transform;
