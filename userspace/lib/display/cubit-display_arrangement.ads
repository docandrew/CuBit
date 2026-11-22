pragma Ada_2022;
with CuBit.Display_Layouts;

-- Settings policy, not modesetting: move one viewport, snap to a shared edge,
-- validate using the common layout core, then normalize the scene origin.
package CuBit.Display_Arrangement with SPARK_Mode, Pure is
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   use type L.Layout;
   type Scale_Preset is (Scale_100, Scale_125, Scale_150, Scale_175, Scale_200);
   function Factor (Preset : Scale_Preset) return G.UI_Scale is
     (case Preset is when Scale_100 => (1, 1), when Scale_125 => (5, 4),
      when Scale_150 => (3, 2), when Scale_175 => (7, 4), when Scale_200 => (2, 1));
   -- Initial native Settings/application work-area floor, in logical units.
   Minimum_Width : constant := 800;
   Minimum_Height : constant := 480;
   procedure Rescale
     (Current : L.Layout; Moving : L.Viewport_Index; Preset : Scale_Preset;
      Proposed : out L.Layout; Accepted : out Boolean)
     with Post => (if not Accepted then Proposed = Current);
   procedure Move
     (Current : L.Layout; Moving : L.Viewport_Index;
      X, Y : G.Output_Origin; Proposed : out L.Layout; Accepted : out Boolean)
     with Post => (if not Accepted then Proposed = Current);
end CuBit.Display_Arrangement;
