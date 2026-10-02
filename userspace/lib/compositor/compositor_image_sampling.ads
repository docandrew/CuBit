package Compositor_Image_Sampling with SPARK_Mode, Pure is
   subtype Wide is Long_Long_Integer;
   subtype Extent is Positive range 1 .. 65_535;
   subtype Index is Natural range 0 .. 65_534;
   subtype Fraction is Natural range 0 .. 255;
   Maximum_Draw : constant Wide := 65_535 * 65_535;
   subtype Draw_Extent is Wide range 1 .. Maximum_Draw;
   subtype Offset is Wide range -Maximum_Draw .. Maximum_Draw;
   -- Logical coordinates in 1/256-pixel units, independent of output density.
   subtype Position is Wide range -65_535 * 256 .. 65_535 * 256;
   type Placement is (Fill, Fit, Center);
   -- Fine_Map returns pixel-centre coordinates in a Size*256 source grid.
   -- Shift to sample centres and clamp the half-pixel footprint at each edge.
   function From_Centre (Centre : Natural; Size : Extent) return Position
     with Pre => Centre < Size * 256,
       Post => From_Centre'Result >= 0 and then
         From_Centre'Result <= Wide (Size - 1) * 256 and then
         From_Centre'Result = Wide'Max
           (0, Wide'Min (Wide (Centre) - 128, Wide (Size - 1) * 256));
   type Layout is private;
   function Source_Width (P : Layout) return Extent;
   function Source_Height (P : Layout) return Extent;
   function Draw_Width (P : Layout) return Draw_Extent;
   function Draw_Height (P : Layout) return Draw_Extent;
   function Left (P : Layout) return Offset;
   function Top (P : Layout) return Offset;
   function Prepare (Width, Height, Image_Width, Image_Height : Extent;
                     Mode : Placement) return Layout
     with Post => Source_Width (Prepare'Result) = Image_Width and then
       Source_Height (Prepare'Result) = Image_Height and then
       Left (Prepare'Result) = (Wide (Width) - Draw_Width (Prepare'Result)) / 2 and then
       Top (Prepare'Result) = (Wide (Height) - Draw_Height (Prepare'Result)) / 2 and then
       (if Mode = Center then
          Draw_Width (Prepare'Result) = Wide (Image_Width) and
          Draw_Height (Prepare'Result) = Wide (Image_Height)
        elsif Mode = Fill then
          Draw_Width (Prepare'Result) >= Wide (Width) and
          Draw_Height (Prepare'Result) >= Wide (Height)
        else
          Draw_Width (Prepare'Result) <= Wide (Width) and
          Draw_Height (Prepare'Result) <= Wide (Height));
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True =>
            X0, X1, Y0, Y1 : Index;
            FX, FY : Fraction;
         when False => null;
      end case;
   end record;
   -- Outside the placed image means letterbox/background. At its trailing
   -- fractional edge, clamp the filter to the last source pixel.
   function At_Point (P : Layout; X, Y : Position) return Sample
     with Post => (if At_Point'Result.Valid then
       At_Point'Result.X0 < Source_Width (P) and
       At_Point'Result.X1 < Source_Width (P) and
       At_Point'Result.Y0 < Source_Height (P) and
       At_Point'Result.Y1 < Source_Height (P));
private
   type Layout is record
      W, H : Draw_Extent := 1;
      X, Y : Offset := 0;
      SW, SH : Extent := 1;
   end record;
   function Source_Width (P : Layout) return Extent is (P.SW);
   function Source_Height (P : Layout) return Extent is (P.SH);
   function Draw_Width (P : Layout) return Draw_Extent is (P.W);
   function Draw_Height (P : Layout) return Draw_Extent is (P.H);
   function Left (P : Layout) return Offset is (P.X);
   function Top (P : Layout) return Offset is (P.Y);
end Compositor_Image_Sampling;
