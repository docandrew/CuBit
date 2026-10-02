package body Compositor_Image_Sampling with SPARK_Mode is
   function From_Centre (Centre : Natural; Size : Extent) return Position is
   begin
      return Position (Wide'Max (0, Wide'Min (Wide (Centre) - 128, Wide (Size - 1) * 256)));
   end From_Centre;

   function Prepare (Width, Height, Image_Width, Image_Height : Extent;
                     Mode : Placement) return Layout
   is
      P : Layout;
   begin
      P.SW := Image_Width; P.SH := Image_Height;
      if Mode = Center then
         P.W := Wide (Image_Width); P.H := Wide (Image_Height);
      elsif (Wide (Width) * Wide (Image_Height) >=
             Wide (Height) * Wide (Image_Width)) = (Mode = Fill)
      then
         P.W := Wide (Width);
         P.H := (Wide (Width) * Wide (Image_Height) + Wide (Image_Width) - 1) /
           Wide (Image_Width);
      else
         P.H := Wide (Height);
         P.W := (Wide (Height) * Wide (Image_Width) + Wide (Image_Height) - 1) /
           Wide (Image_Height);
      end if;
      P.X := (Wide (Width) - P.W) / 2;
      P.Y := (Wide (Height) - P.H) / 2;
      return P;
   end Prepare;

   type Axis_Sample (Valid : Boolean := False) is record
      case Valid is
         when True => First, Last : Index; Weight : Fraction;
         when False => null;
      end case;
   end record;

   function Axis (Point : Position; Origin : Offset; Size : Draw_Extent;
                  Pixels : Extent) return Axis_Sample
     with Post => (if Axis'Result.Valid then
       Axis'Result.First < Pixels and Axis'Result.Last < Pixels)
   is
      Local : constant Wide := Point - Origin * 256;
      Coordinate : Wide;
   begin
      if Local < 0 or else Local >= Size * 256 then return (Valid => False); end if;
      if Size = 1 then return (True, 0, Natural'Min (1, Pixels - 1), 0); end if;
      Coordinate := Wide'Min (Local, (Size - 1) * 256) * Wide (Pixels - 1) / (Size - 1);
      return (True, Index (Coordinate / 256),
        Index (Wide'Min (Coordinate / 256 + 1, Wide (Pixels - 1))),
        Fraction (Coordinate mod 256));
   end Axis;

   function At_Point (P : Layout; X, Y : Position) return Sample is
      AX : constant Axis_Sample := Axis (X, P.X, P.W, P.SW);
      AY : constant Axis_Sample := Axis (Y, P.Y, P.H, P.SH);
   begin
      if not AX.Valid or else not AY.Valid then return (Valid => False); end if;
      return (True, AX.First, AX.Last, AY.First, AY.Last, AX.Weight, AY.Weight);
   end At_Point;
end Compositor_Image_Sampling;
