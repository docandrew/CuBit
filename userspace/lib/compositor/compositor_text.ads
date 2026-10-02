with Compositor_Affine;
package Compositor_Text with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   package G renames A.G;
   use type G.Pixel_Edge;
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
   function Clip (Screen : G.Output; Cell : G.Logical_Rectangle;
                  Damage : G.Physical_Rectangle) return G.Physical_Rectangle
     with Post => Clip'Result.Right <= Screen.Width and Clip'Result.Bottom <= Screen.Height;
end Compositor_Text;
