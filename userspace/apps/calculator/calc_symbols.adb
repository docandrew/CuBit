package body Calc_Symbols with SPARK_Mode is

   --  Shapes are drawn on a design square 0 .. DESIGN (y down), scaled to
   --  the box: segments with round ends (their joins round too) and dots.
   DESIGN : constant := 100;
   subtype Design_Coordinate is Natural range 0 .. DESIGN;
   type Segment is record
      X0, Y0, X1, Y1 : Design_Coordinate;
   end record;
   type Segment_List is array (Positive range <>) of Segment;
   type Dot is record
      X, Y : Design_Coordinate;
   end record;
   type Dot_List is array (Positive range <>) of Dot;
   --  A dot's radius per stroke width: its diameter is a little under one
   --  and a half strokes, as a division sign's dots are in text faces.
   DOT_NUMERATOR : constant := 7;
   DOT_DENOMINATOR : constant := 10;

   PLUS : constant Segment_List := [(14, 50, 86, 50), (50, 14, 50, 86)];
   MINUS : constant Segment_List := [1 => (14, 50, 86, 50)];
   --  The diagonals' reach matches the plus's arms optically.
   TIMES : constant Segment_List := [(22, 22, 78, 78), (22, 78, 78, 22)];
   DIVIDE : constant Segment_List := [1 => (14, 50, 86, 50)];
   DIVIDE_DOTS : constant Dot_List := [(50, 22), (50, 78)];
   EQUALS : constant Segment_List := [(16, 37, 84, 37), (16, 63, 84, 63)];
   --  The small rising tick, the fall, the long rise, the bar over the
   --  radicand.
   ROOT : constant Segment_List :=
     [(12, 56, 22, 51), (22, 51, 38, 86), (38, 86, 58, 14), (58, 14, 88, 14)];
   --  The bar, the left leg leaning in, the right leg ending in a foot.
   PI : constant Segment_List :=
     [(12, 24, 88, 24), (36, 24, 30, 82), (66, 24, 66, 70), (66, 70, 69, 78), (69, 78, 76, 82),
      (76, 82, 86, 81)];
   --  A key pointing left, a cross in it.
   ERASE : constant Segment_List :=
     [(12, 50, 32, 22), (32, 22, 88, 22), (88, 22, 88, 78), (88, 78, 32, 78), (32, 78, 12, 50),
      (50, 38, 70, 62), (50, 62, 70, 38)];
   NO_DOTS : constant Dot_List (1 .. 0) := [others => (0, 0)];

   --  Positions in 1/64 physical pixel within the box.
   LARGEST : constant := MAXIMUM_EDGE * SUBPIXEL;
   subtype Wide is Long_Long_Integer range -4 * LARGEST * LARGEST .. 4 * LARGEST * LARGEST;
   subtype Position is Wide range 0 .. LARGEST;
   subtype Square is Wide range 0 .. 2 * LARGEST * LARGEST;

   --  A design coordinate in the box, on the stroke's pixel grid: a whole
   --  pixel stroke whose spine lies on a pixel centre (odd widths) or edge
   --  (even widths) covers whole pixels, so straight strokes stay sharp.
   function Scaled (Value : Design_Coordinate; Size : Edge; Offset : Position) return Position is
     (Position'Min (LARGEST, ((Wide (Value) * Wide (Size) * SUBPIXEL / DESIGN - Offset + SUBPIXEL / 2) / SUBPIXEL)
                             * SUBPIXEL + Offset))
     with Pre => Offset <= SUBPIXEL / 2;

   --  The integer square root (floor).
   function Root_Of (Value : Square) return Position
     with Post => Root_Of'Result * Root_Of'Result <= Value
   is
      Low : Position := 0;
      High : Position := LARGEST;
      Middle : Position;
   begin
      --  Invariant: Low * Low <= Value; the root is below High + 1.
      while Low < High loop
         pragma Loop_Invariant (Low * Low <= Value and then Low < High);
         pragma Loop_Variant (Decreases => High - Low);
         Middle := Low + (High - Low + 1) / 2;
         if Middle * Middle <= Value then
            Low := Middle;
         else
            High := Middle - 1;
         end if;
      end loop;
      return Low;
   end Root_Of;

   --  Coverage of a pixel whose centre is Distance from a shape's spine,
   --  the shape Reach wide on each side: a one-pixel ramp across the edge.
   function Ramp (Distance, Reach : Position) return Coverage is
     (if Distance >= Reach + SUBPIXEL / 2 then 0
      elsif Distance + SUBPIXEL / 2 <= Reach then OPAQUE
      else Coverage ((Reach + SUBPIXEL / 2 - Distance) * OPAQUE / SUBPIXEL));

   --  The distance from (X, Y) to the segment A-B.
   function Distance_To (X, Y, AX, AY, BX, BY : Position) return Position is
      DX : constant Wide := BX - AX;
      DY : constant Wide := BY - AY;
      PX : constant Wide := X - AX;
      PY : constant Wide := Y - AY;
      Length_Squared : constant Square := DX * DX + DY * DY;
      Along : constant Wide := PX * DX + PY * DY;
   begin
      if Length_Squared = 0 or else Along <= 0 then
         return Root_Of (PX * PX + PY * PY);
      elsif Along >= Length_Squared then
         return Root_Of ((X - BX) * (X - BX) + (Y - BY) * (Y - BY));
      end if;
      declare
         Length : constant Position := Position'Max (1, Root_Of (Length_Squared));
         Across : constant Wide := abs (PX * DY - PY * DX);
      begin
         return Position'Min (LARGEST, Across / Length);
      end;
   end Distance_To;

   procedure Rasterize (Item : Symbol; Stroke : Stroke_Width; Into : out Coverage_Map) is
      Size : constant Edge := Into'Length (1);
      Reach : constant Position := Wide (Stroke) / 2;
      Dot_Reach : constant Position := Wide (Stroke) * DOT_NUMERATOR / DOT_DENOMINATOR;

      function Strokes return Segment_List is
        (case Item is
            when Plus_Symbol => PLUS, when Minus_Symbol => MINUS, when Times_Symbol => TIMES,
            when Divide_Symbol => DIVIDE, when Equals_Symbol => EQUALS, when Root_Symbol => ROOT,
            when Pi_Symbol => PI, when Erase_Symbol => ERASE);
      function Dots return Dot_List is (if Item = Divide_Symbol then DIVIDE_DOTS else NO_DOTS);

      Offset : constant Position :=
        (if Stroke mod SUBPIXEL = 0 and then Stroke / SUBPIXEL mod 2 = 1 then SUBPIXEL / 2 else 0);
      Lines : constant Segment_List := Strokes;
      Points : constant Dot_List := Dots;
   begin
      Into := [others => [others => 0]];
      for Row in 0 .. Size - 1 loop
         for Column in 0 .. Size - 1 loop
            declare
               X : constant Position := Wide (Column) * SUBPIXEL + SUBPIXEL / 2;
               Y : constant Position := Wide (Row) * SUBPIXEL + SUBPIXEL / 2;
               Best : Coverage := 0;
            begin
               for L of Lines loop
                  Best := Coverage'Max
                    (Best, Ramp (Distance_To (X, Y, Scaled (L.X0, Size, Offset), Scaled (L.Y0, Size, Offset),
                                              Scaled (L.X1, Size, Offset), Scaled (L.Y1, Size, Offset)), Reach));
               end loop;
               for P of Points loop
                  Best := Coverage'Max
                    (Best, Ramp (Distance_To (X, Y, Scaled (P.X, Size, Offset), Scaled (P.Y, Size, Offset),
                                              Scaled (P.X, Size, Offset), Scaled (P.Y, Size, Offset)), Dot_Reach));
               end loop;
               Into (Row, Column) := Best;
            end;
         end loop;
      end loop;
   end Rasterize;
end Calc_Symbols;
