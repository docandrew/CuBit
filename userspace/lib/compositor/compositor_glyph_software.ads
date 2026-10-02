with Interfaces;
with Compositor_Glyph_Placement;
package Compositor_Glyph_Software with SPARK_Mode, Pure is
   package P renames Compositor_Glyph_Placement;
   package G renames P.G;
   package L renames P.L;
   use type G.Pixel_Edge;
   subtype Byte is Interfaces.Unsigned_8;
   subtype Word is Interfaces.Unsigned_32;
   use type Word;
   type Bytes is array (Natural range <>) of Byte;
   type Pixels is array (Natural range <>) of Word;
   type Placement is private;
   function Prepare (Screen : G.Output; Origin : G.Logical_Point) return Placement;
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => X : Natural range 0 .. 511; Y : Natural range 0 .. 271;
         when False => null;
      end case;
   end record;
   function Raster (Where : Placement) return L.Layout;
   function At_Pixel (Where : Placement; Pixel : G.Physical_Point) return Sample
     with Post => (if At_Pixel'Result.Valid then
       At_Pixel'Result.X < Raster (Where).Width and At_Pixel'Result.Y < Raster (Where).Height);
   -- Straight ARGB tint, A8 coverage and premultiplied ARGB destination.
   -- One final integer rounding, including destination alpha.
   function Over (Coverage : Byte; Tint, Background : Word) return Word;
   function Bounds (Screen : G.Output; Origin : G.Logical_Point;
                    Damage : G.Physical_Rectangle) return G.Physical_Rectangle
     with Post => Bounds'Result.Left <= Bounds'Result.Right and then
       Bounds'Result.Top <= Bounds'Result.Bottom and then
       Bounds'Result.Right <= Screen.Width and then Bounds'Result.Bottom <= Screen.Height and then
       (if Bounds'Result.Left < Bounds'Result.Right and Bounds'Result.Top < Bounds'Result.Bottom then
          Bounds'Result.Left >= Damage.Left and Bounds'Result.Top >= Damage.Top and
          Bounds'Result.Right <= Damage.Right and Bounds'Result.Bottom <= Damage.Bottom);
   function Inside (Index, Pitch : Natural; Area : G.Physical_Rectangle) return Boolean
     with Pre => Pitch > 0;
   function Fits_Target (Screen : G.Output; Target : Pixels; Pitch : Positive) return Boolean is
     (Target'First = 0 and then Pitch >= Natural (Screen.Width) and then
      Pitch <= Natural'Last / Natural (Screen.Height) and then
      Target'Last >= Pitch * Natural (Screen.Height) - 1);
   procedure Paint (Screen : G.Output; Origin : G.Logical_Point;
                    Damage : G.Physical_Rectangle; Mask : Bytes;
                    Target : in out Pixels; Pitch : Positive; Tint : Word)
     with Pre => Mask'First = 0 and then Mask'Last >= L.Plan (Screen.Scale).Bytes - 1 and then
       Fits_Target (Screen, Target, Pitch),
       Post => (for all I in Target'Range =>
         (if not Inside (I, Pitch, Bounds (Screen, Origin, Damage)) then Target (I) = Target'Old (I)));
private
   type Placement is record
      Width, Height : G.Physical_Extent;
      Rotation : G.Orientation;
      Left, Top : P.Position;
      Raster : L.Layout;
   end record;
   function Raster (Where : Placement) return L.Layout is (Where.Raster);
   function Prepare (Screen : G.Output; Origin : G.Logical_Point) return Placement is
     (Screen.Width, Screen.Height, Screen.Rotation,
      P.Snap (Origin.X, Screen.X, Screen.Scale),
      P.Snap (Origin.Y, Screen.Y, Screen.Scale), L.Plan (Screen.Scale));
end Compositor_Glyph_Software;
