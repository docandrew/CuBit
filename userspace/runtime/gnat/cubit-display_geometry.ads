pragma Ada_2022;
--  Shared output-local geometry; no device discovery, allocation or authority.
package CuBit.Display_Geometry with SPARK_Mode, Pure is
   type Logical_Coordinate is range -2 ** 30 .. 2 ** 30;
   subtype Output_Origin is Logical_Coordinate range -2 ** 24 .. 2 ** 24;
   type Pixel_Edge is range 0 .. 65_535;
   subtype Physical_Extent is Pixel_Edge range 1 .. Pixel_Edge'Last;
   subtype Pixel_Index is Pixel_Edge range 0 .. Pixel_Edge'Last - 1;
   type Scale_Component is range 1 .. 16;
   type UI_Scale is record
      Numerator, Denominator : Scale_Component := 1;
   end record;
   type Orientation is
     (Unrotated, Clockwise_90, Clockwise_180, Clockwise_270);
   type Output is record
      Width, Height : Physical_Extent;
      Rotation : Orientation := Unrotated;
      Scale : UI_Scale;
      X, Y : Output_Origin := 0;
   end record;
   type Logical_Point is record
      X, Y : Logical_Coordinate;
   end record;
   type Physical_Point is record
      X, Y : Pixel_Index;
   end record;
   --  Half-open edges. Inverted or zero-area input damage maps to empty.
   type Logical_Rectangle is record
      Left, Top, Right, Bottom : Logical_Coordinate;
   end record;
   type Physical_Rectangle is record
      Left, Top, Right, Bottom : Pixel_Edge;
   end record;
   Empty : constant Physical_Rectangle := (others => 0);
   function Bounds (Screen : Output) return Logical_Rectangle;
   function Contains (Screen : Output; Point : Logical_Point) return Boolean;
   --  Clip in desktop space, scale edges outward, rotate into native storage.
   --  Rotation is clockwise from logical content into the pixel buffer.
   function Damage (Screen : Output; Area : Logical_Rectangle)
     return Physical_Rectangle
     with Post =>
       Damage'Result.Left <= Damage'Result.Right and then
       Damage'Result.Top <= Damage'Result.Bottom and then
       Damage'Result.Right <= Screen.Width and then
       Damage'Result.Bottom <= Screen.Height;
   type Point_Mapping (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Logical_Point;
         when False => null;
      end case;
   end record;
   --  Inverse-map a native pixel's center, quantized to its logical unit.
   --  A partial final logical unit is allowed; Damage clips it to real pixels.
   function To_Desktop (Screen : Output; Point : Physical_Point)
     return Point_Mapping
     with Post =>
       (if To_Desktop'Result.Valid then
          Contains (Screen, To_Desktop'Result.Value));
end CuBit.Display_Geometry;
