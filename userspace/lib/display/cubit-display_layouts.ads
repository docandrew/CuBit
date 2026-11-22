pragma Ada_2022;
with Interfaces;
with CuBit.Display_Geometry;

--  Pure admission of one seat's extended-desktop viewports. IDs are names,
--  never capabilities. Mirrors must be collapsed to one viewport beforehand.
package CuBit.Display_Layouts with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   --  Initial model budget, not a hardware limit.
   Max_Viewports : constant := 16;
   subtype Viewport_Count is Natural range 0 .. Max_Viewports;
   subtype Viewport_Index is Viewport_Count range 1 .. Max_Viewports;
   type Named_Display_ID is new Interfaces.Unsigned_64
     range 1 .. Interfaces.Unsigned_64'Last;
   type Placement is record
      Display : Named_Display_ID := 1;
      Geometry : G.Output := (Width => 1, Height => 1, others => <>);
   end record;
   type Placement_Array is array (Viewport_Index) of Placement;
   type Layout is record
      Count : Viewport_Count := 0;
      Items : Placement_Array := [others => <>];
   end record;
   type Pointer_Position is record
      Screen : Viewport_Index;
      Point : G.Logical_Point;
   end record;
   --  Choose the nearest visible logical pixel by Manhattan distance; ties
   --  retain layout order. A pointer cannot disappear into the unlit rectangle
   --  below a shorter monitor. Scale/orientation use the same geometry model.
   function Confine (Ready : Layout; Point : G.Logical_Point) return Pointer_Position
     with Pre => Ready.Count > 0,
       Post => Confine'Result.Screen <= Ready.Count and then
         G.Contains (Ready.Items (Confine'Result.Screen).Geometry, Confine'Result.Point);

   --  Desktop-owned policy: the display service must not select this role.
   --  Not an output authority or a hardware scanout number.
   --  Ready contains only enabled, usable viewports. Keep the saved preference
   --  separately: temporary selection must never rewrite configuration.
   type Primary_Selection (Available : Boolean := False) is record
      case Available is
         when True =>
            Index : Viewport_Index;
            Display : Named_Display_ID;
         when False => null;
      end case;
   end record;
   type Primary_Update is
     (Preserve_Usable_Primary, Apply_Primary_Preference);
   function Valid_Primary (Ready : Layout; Choice : Primary_Selection)
      return Boolean with Ghost;
   function Select_Primary
     (Ready : Layout; Preferred : Named_Display_ID;
      Previous : Primary_Selection := (Available => False);
      Policy : Primary_Update := Preserve_Usable_Primary)
      return Primary_Selection
     with Post => Valid_Primary (Ready, Select_Primary'Result);

   type Empty_Layout_Policy is (Require_Interactive_Output, Permit_Headless);
   type Admission_Status is
     (Accepted, Empty_Not_Allowed, Repeated_Display,
      Overlapping_Displays, Disconnected);
   type Connection is record
      Parent : Viewport_Index := 1;
      Depth : Viewport_Count := 0; -- Zero means not reached from viewport one.
   end record;
   type Connection_Tree is array (Viewport_Index) of Connection;
   type Validation_Result is record
      Status : Admission_Status := Disconnected;
      First, Second : Viewport_Count := 0;
      Tree : Connection_Tree := [others => <>];
   end record;

   function Overlap (Left, Right : G.Logical_Rectangle) return Boolean;
   function Adjacent (Left, Right : G.Logical_Rectangle) return Boolean;
   function Pairwise_Valid
     (Candidate : Layout; Through : Viewport_Count) return Boolean
     with Ghost, Pre => Through <= Candidate.Count;
   --  A constructive connectivity witness: every reached non-root has a
   --  touching parent with strictly smaller positive depth. Finite descent
   --  reaches the sole root; cycles and unrelated islands cannot qualify.
   function Valid_Tree (Candidate : Layout; Tree : Connection_Tree)
     return Boolean with Ghost;
   function Complete_Tree (Candidate : Layout; Tree : Connection_Tree)
     return Boolean with Ghost;

   function Validate
     (Candidate : Layout;
      Policy : Empty_Layout_Policy := Require_Interactive_Output)
      return Validation_Result
     with Post =>
       (if Validate'Result.Status = Accepted then
          Pairwise_Valid (Candidate, Candidate.Count) and then
          (if Candidate.Count = 0 then Policy = Permit_Headless
           else Complete_Tree (Candidate, Validate'Result.Tree)));
end CuBit.Display_Layouts;
