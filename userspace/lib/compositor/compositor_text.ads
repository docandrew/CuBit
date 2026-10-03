with Compositor_Affine;
package Compositor_Text with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   package G renames A.G;
   use type G.Pixel_Edge, G.Logical_Coordinate;
   subtype Count is Natural range 0 .. 32;
   subtype Index is Positive range 1 .. 32;
   subtype Attempt is Positive range 1 .. 2;
   type Scene_Decision is (Complete, Replay, Restart);
   function Finish (Pass : Attempt; Damaged : Boolean) return Scene_Decision is
     (if not Damaged then Complete elsif Pass = 1 then Replay else Restart)
     with Post => (Finish'Result = Complete) = not Damaged and
       (Finish'Result = Replay) = (Damaged and Pass = 1) and
       (Finish'Result = Restart) = (Damaged and Pass = 2);
   type Glyph is record
      Code : Natural range 32 .. 126 := 63;
      Cell : G.Logical_Rectangle := (others => 0);
   end record;
   type Glyphs is array (Index) of Glyph;
   --  Adjacent positive cells sharing one vertical interval have no gaps.
   --  Compare stored edges only: even extreme coordinates need no subtraction.
   function Can_Join (Items : Glyphs; Length : Count) return Boolean is
     (Length > 0 and then
      (for all I in 1 .. Length =>
         Items (I).Cell.Left < Items (I).Cell.Right and then
         Items (I).Cell.Top < Items (I).Cell.Bottom and then
         Items (I).Cell.Top = Items (1).Cell.Top and then
         Items (I).Cell.Bottom = Items (1).Cell.Bottom and then
         Items (I).Cell.Left >= Items (1).Cell.Left and then
         Items (I).Cell.Right <= Items (Length).Cell.Right and then
         (if I > 1 then Items (I).Cell.Left = Items (I - 1).Cell.Right)));
   function Background (Items : Glyphs; Length : Count)
     return G.Logical_Rectangle is
     (Items (1).Cell.Left, Items (1).Cell.Top,
      Items (Length).Cell.Right, Items (1).Cell.Bottom)
     with Pre => Can_Join (Items, Length),
       Post => Background'Result.Left < Background'Result.Right and then
         Background'Result.Top < Background'Result.Bottom and then
         (for all I in 1 .. Length =>
            Items (I).Cell.Left >= Background'Result.Left and then
            Items (I).Cell.Right <= Background'Result.Right and then
            Items (I).Cell.Top = Background'Result.Top and then
            Items (I).Cell.Bottom = Background'Result.Bottom);
   function Clip (Screen : G.Output; Cell : G.Logical_Rectangle;
                  Damage : G.Physical_Rectangle) return G.Physical_Rectangle
     with Post => Clip'Result.Right <= Screen.Width and Clip'Result.Bottom <= Screen.Height;
end Compositor_Text;
