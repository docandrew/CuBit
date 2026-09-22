pragma Ada_2022;
with CuBit.Display_Layouts;

--  Pure planning, not a window manager or a source of display authority.
package CuBit.Window_Placement with SPARK_Mode, Pure is
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   use type G.Logical_Coordinate;

   --  Includes the largest logical output supported by Display_Geometry.
   subtype Extent is G.Logical_Coordinate range 1 .. 2 ** 20;
   subtype Position is G.Logical_Coordinate range -2 ** 25 .. 2 ** 25;
   type Work_Area is record
      Display : L.Named_Display_ID := 1;
      X, Y : Position := 0;
      Width, Height : Extent := 1;
   end record;
   type Work_Area_Array is array (L.Viewport_Index) of Work_Area;
   type Ready_Areas is record
      Count : L.Viewport_Count := 0;
      Items : Work_Area_Array := [others => <>];
   end record;
   --  Local to the named display's usable work area, including decorations.
   type Local_Bounds is record
      X, Y : Position := 0;
      Width, Height : Extent := 1;
   end record;
   type Preference is record
      Home : L.Named_Display_ID := 1;
      Bounds : Local_Bounds;
   end record;
   type Display_Choice (Known : Boolean := False) is record
      case Known is
         when True => Display : L.Named_Display_ID;
         when False => null;
      end case;
   end record;
   type Fallback_Policy is
     (Await_Preferred_Output, Allow_Temporary_Fallback);
   type Interaction_State is (Idle, Pointer_Captured, Modal_Interaction);
   type Placement_Reason is
     (At_Home, Temporary_Fallback, Waiting_For_Home, No_Ready_Output,
      No_Suitable_Work_Area, Ambiguous_Display, Deferred_Interaction);
   type Destination (Available : Boolean := False) is record
      case Available is
         when True =>
            Index : L.Viewport_Index;
            Display : L.Named_Display_ID;
            Bounds : G.Logical_Rectangle;
         when False => null;
      end case;
   end record;
   type Plan_Result is record
      Reason : Placement_Reason := No_Ready_Output;
      Target : Destination;
   end record;

   function Suitable (Area : Work_Area; Window : Local_Bounds)
      return Boolean is
     (Window.Width <= Area.Width and then Window.Height <= Area.Height);
   function Contained (Area : Work_Area; Bounds : G.Logical_Rectangle)
      return Boolean is
     (Bounds.Left >= Area.X and then Bounds.Top >= Area.Y and then
      Bounds.Left < Bounds.Right and then Bounds.Top < Bounds.Bottom and then
      Bounds.Right <= Area.X + Area.Width and then
      Bounds.Bottom <= Area.Y + Area.Height);
   function Fit (Area : Work_Area; Window : Local_Bounds)
      return G.Logical_Rectangle
     with Pre => Suitable (Area, Window),
     Post => Contained (Area, Fit'Result) and then
       Fit'Result.Right - Fit'Result.Left = Window.Width and then
       Fit'Result.Bottom - Fit'Result.Top = Window.Height;

   function Safe_Plan
     (Desired : Preference; Areas : Ready_Areas; Result : Plan_Result)
      return Boolean with Ghost;

   --  Desired is never rewritten. Explicit user moves replace that preference
   --  in the owner; replan from the new intent, never replay an old proposal.
   --  Previous affects fallback selection, not permission to restore home.
   function Plan
     (Desired : Preference; Areas : Ready_Areas;
      Previous : Display_Choice := (Known => False);
      Policy : Fallback_Policy := Await_Preferred_Output;
      Interaction : Interaction_State := Idle) return Plan_Result
     with Post => Safe_Plan (Desired, Areas, Plan'Result) and then
       (if Interaction /= Idle then
          Plan'Result.Reason = Deferred_Interaction);

   --  Milliseconds from one monotonic clock. Timers own no device resources.
   type Instant is range 0 .. 2 ** 63 - 1;
   type Recovery_Episode is private;
   function Active (Episode : Recovery_Episode) return Boolean;
   function Deadline (Episode : Recovery_Episode) return Instant;
   procedure Begin_Recovery
     (Episode : in out Recovery_Episode; Now, Grace : Instant)
     with Post => Active (Episode) and then
       (if Active (Episode'Old) then Episode = Episode'Old
        else Deadline (Episode) >= Now);
   function Expired (Episode : Recovery_Episode; Now : Instant)
      return Boolean;
   --  Call on completed recovery, not on each transient probe or input event.
   procedure Complete_Recovery (Episode : out Recovery_Episode)
     with Post => not Active (Episode);
private
   type Recovery_Episode is record
      Started : Boolean := False;
      Due : Instant := 0;
   end record;
end CuBit.Window_Placement;
