with Compositor_Text;
with Compositor_Shadow;
-- Translate Desktop drawing requests into one retained frame. Damage is in
-- physical output pixels; surfaces/cells are in desktop logical coordinates.
-- Each call supplies its complete clip and does not inherit a prior draw clip.
-- No per-draw CPU fallback is safe once this capture has begun. Failure is
-- resolved by Finish/Discard on the complete scene, including cold uploads.
package Desktop_GPU_Scene.Drawing with SPARK_Mode is
   subtype Face_ID is Natural range 0 .. 1;
   pragma Unevaluated_Use_Of_Old (Allow);
   procedure Shadow (S : in out State; Window : V.A.G.Logical_Rectangle;
      Damage : V.A.G.Physical_Rectangle; Color : V.A.Word;
      Accepted : out Boolean; Depth : Compositor_Shadow.Depth := 3)
     with Global => null, Pre => Valid (S), Post => Valid (S) and
       Layer_Count (S) <= Layer_Count (S)'Old + 3 and
       Reader_Count (S) = Reader_Count (S)'Old and
       Image_Reader_Count (S) = Image_Reader_Count (S)'Old;
   procedure Fill (S : in out State; Area : V.A.G.Physical_Rectangle;
      Color : V.A.Word; Accepted : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- Desktop-space rectangles use the same outward edge rounding, rotation
   -- and output origin as native software fills. Damage is already physical;
   -- it must not be scaled a second time. Empty intersections emit no layers.
   procedure Logical_Fill (S : in out State;
      Area : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Color : V.A.Word; Accepted : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   procedure Image (S : in out State; Source : Vulkan_Submission.Source_Ticket;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean; Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
   procedure Image_Region (S : in out State; Source : Vulkan_Submission.Source_Ticket;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Region : V.R.Rectangle; Accepted : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and Layer_Count (S) <= Layer_Count (S)'Old + 2;
   procedure Text (S : in out State; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : V.A.G.Physical_Rectangle;
      Tint : V.A.Word; Accepted : out Boolean; Face : Face_ID := 0)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid;
end Desktop_GPU_Scene.Drawing;
