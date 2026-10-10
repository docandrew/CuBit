pragma Ada_2022;
with Interfaces;

--  Pure display plane planner shared by display.svc, its clients and tests.
--
--  A plane request asks for content to be scanned out by a hardware plane
--  instead of being composited: a pointer cursor now; later a video surface
--  (overlay) or a fullscreen client (primary bypass). Several requests exist
--  at once, e.g. two mice, a remote-session pointer and an agent pointer.
--  Each has a typed identity, kind, format, source size, destination size,
--  anchor (a cursor's hotspot), position, visibility and priority.
--
--  Positions are anchor positions in one shared "desktop space"; every output
--  declares its origin in that space, so a request may straddle outputs. The
--  desktop owns the arrangement and which input drives which cursor. Display
--  owns this assignment of requests to each output's hardware planes.
--
--  Each output describes its planes: which request kinds and formats each
--  takes, size limits, scaler, and fixed stacking (Z). A driver lets a plane
--  take a kind only where its stacking keeps that kind's layer order (cursor
--  above overlay above primary), so the planner never reorders layers.
--
--  Plan is pure (Global => null), so equal inputs give equal plans. A visible
--  request is Hardware only when it holds a compatible plane on every output
--  it touches; every other visible request is Composited by the client. Plans
--  offer planes by priority (descending), then identity (ascending), each
--  request taking the lowest-numbered free compatible plane per output.
--  Proved at level 2: covered, exclusive, indexed, within the output's planes,
--  complete and compatible, prioritized (see the contracts below).
package CuBit.Display_Planes with Pure, SPARK_Mode is
   --  Declared capacities, not hidden defaults.
   Request_Capacity : constant := 8;
   type Request_Count is range 0 .. Request_Capacity;
   subtype Request_Id is Request_Count range 1 .. Request_Capacity;
   No_Request : constant Request_Count := 0;

   Output_Capacity : constant := 4;
   type Output_Id is range 0 .. Output_Capacity - 1;

   --  Hardware planes per output that a driver may offer to the planner.
   --  ADL-P/ADL-N pipes have one cursor plane and five universal planes.
   Plane_Capacity : constant := 8;
   type Plane_Count is range 0 .. Plane_Capacity;
   subtype Plane_Number is Plane_Count range 1 .. Plane_Capacity;
   No_Plane : constant Plane_Count := 0;

   type Plane_Kind is (Cursor, Overlay, Primary);
   type Kind_Set is array (Plane_Kind) of Boolean;

   --  ARGB is premultiplied ARGB8888 (DRM default blending). NV12 and P010
   --  are listed so that drivers can describe video-capable planes; only
   --  cursor requests (ARGB) are produced today.
   type Pixel_Format is (ARGB8888, XRGB8888, NV12, P010);
   type Format_Set is array (Pixel_Format) of Boolean;

   --  Stacking position of a plane on its output; higher is on top.
   type Plane_Z is range 0 .. Plane_Capacity;

   --  Higher value wins a plane first.
   type Request_Priority is range 0 .. 15;
   Primary_Pointer   : constant Request_Priority := 15;
   Secondary_Pointer : constant Request_Priority := 10;
   Remote_Pointer    : constant Request_Priority := 6;
   Agent_Pointer     : constant Request_Priority := 4;

   Surface_Extent_Limit : constant := 16_384;
   type Surface_Extent is range 1 .. Surface_Extent_Limit;
   type Anchor_Coordinate is range 0 .. Surface_Extent_Limit - 1;
   --  Largest cursor image any backend takes (Intel 256x256 ARGB mode).
   Cursor_Extent_Limit : constant := 256;
   subtype Cursor_Extent is Surface_Extent range 1 .. Cursor_Extent_Limit;
   subtype Hotspot_Coordinate is
     Anchor_Coordinate range 0 .. Cursor_Extent_Limit - 1;

   --  Matches CuBit.Display_Geometry's signed origin bound.
   Space_Limit : constant := 2 ** 24;
   type Space_Coordinate is range -Space_Limit .. Space_Limit;
   type Output_Extent is range 1 .. 65_535;

   type Request_State is record
      Live     : Boolean := False;
      Visible  : Boolean := False;
      Kind     : Plane_Kind := Cursor;
      Format   : Pixel_Format := ARGB8888;
      Priority : Request_Priority := 0;
      X, Y     : Space_Coordinate := 0;
      --  Source pixels, and the size shown on the output.
      Width, Height : Surface_Extent := 1;
      Shown_Width, Shown_Height : Surface_Extent := 1;
      --  Anchor inside the shown rectangle: a cursor's hotspot.
      Anchor_X, Anchor_Y : Anchor_Coordinate := 0;
      --  The pointer driving this cursor reports absolute positions, so
      --  the host's pointer and the guest's coincide (see Host_Pointer).
      Absolute : Boolean := False;
   end record;

   function Scaled (R : Request_State) return Boolean is
     (R.Shown_Width /= R.Width or else R.Shown_Height /= R.Height);

   function Valid_Anchor (R : Request_State) return Boolean is
     (Integer (R.Anchor_X) < Integer (R.Shown_Width) and then
      Integer (R.Anchor_Y) < Integer (R.Shown_Height));

   function Valid_Hotspot
     (Width, Height : Cursor_Extent; Hot_X, Hot_Y : Hotspot_Coordinate)
      return Boolean is
     (Integer (Hot_X) < Integer (Width) and then
      Integer (Hot_Y) < Integer (Height));

   --  A cursor request: ARGB, unscaled, hotspot as anchor.
   function Cursor_Request
     (Live, Visible : Boolean; Priority : Request_Priority;
      X, Y : Space_Coordinate; Width, Height : Cursor_Extent;
      Hot_X, Hot_Y : Hotspot_Coordinate; Absolute : Boolean)
      return Request_State is
     ((Live => Live, Visible => Visible, Kind => Cursor, Format => ARGB8888,
       Priority => Priority, X => X, Y => Y, Width => Width, Height => Height,
       Shown_Width => Width, Shown_Height => Height,
       Anchor_X => Hot_X, Anchor_Y => Hot_Y, Absolute => Absolute));

   type Plane_Descriptor is record
      Kinds      : Kind_Set := [others => False];
      Formats    : Format_Set := [others => False];
      Max_Width  : Surface_Extent := 1;
      Max_Height : Surface_Extent := 1;
      Scaler     : Boolean := False;
      Z          : Plane_Z := 0;
      --  The image is drawn by the host frontend as its pointer image (a
      --  virtual GPU's cursor queue), not by scanout. QEMU's GTK and SDL
      --  frontends show it only while the guest pointer is absolute: with
      --  relative input they hide the host pointer during a grab and expect
      --  the guest to draw its own. Only absolute requests may use it.
      Host_Pointer : Boolean := False;
   end record;
   type Plane_Table is array (Plane_Number) of Plane_Descriptor;

   type Output_State is record
      Present : Boolean := False;
      X, Y    : Space_Coordinate := 0;
      Width, Height : Output_Extent := 1;
      Count   : Plane_Count := 0;
      Planes  : Plane_Table;
   end record;

   function Compatible (R : Request_State; P : Plane_Descriptor)
      return Boolean is
     (P.Kinds (R.Kind) and then P.Formats (R.Format) and then
      R.Width <= P.Max_Width and then R.Height <= P.Max_Height and then
      (if Scaled (R) then P.Scaler) and then
      (if P.Host_Pointer then R.Absolute));

   function Shown (R : Request_State) return Boolean is
     (R.Live and then R.Visible);

   --  Shown rectangle top-left in desktop space.
   function Left (R : Request_State) return Integer is
     (Integer (R.X) - Integer (R.Anchor_X));
   function Top (R : Request_State) return Integer is
     (Integer (R.Y) - Integer (R.Anchor_Y));

   function Touches (R : Request_State; O : Output_State) return Boolean is
     (O.Present and then Shown (R) and then
      Left (R) < Integer (O.X) + Integer (O.Width) and then
      Left (R) + Integer (R.Shown_Width) > Integer (O.X) and then
      Top (R) < Integer (O.Y) + Integer (O.Height) and then
      Top (R) + Integer (R.Shown_Height) > Integer (O.Y));

   --  Output-local top-left. A partly visible rectangle may start left of
   --  or above the output; it never starts at or beyond the far edge.
   type Local_Coordinate is
     range -(Surface_Extent_Limit - 1) .. Integer (Output_Extent'Last) - 1;
   type Placement (Visible : Boolean := False) is record
      case Visible is
         when True  => X, Y : Local_Coordinate;
         when False => null;
      end case;
   end record;

   function Place (R : Request_State; O : Output_State) return Placement
     with Global => null,
          Post => Place'Result.Visible = Touches (R, O) and then
            (if Place'Result.Visible then
               Integer (Place'Result.X) = Left (R) - Integer (O.X) and then
               Integer (Place'Result.Y) = Top (R) - Integer (O.Y) and then
               Integer (Place'Result.X) + Integer (R.Shown_Width) > 0 and then
               Integer (Place'Result.Y) + Integer (R.Shown_Height) > 0 and then
               Integer (Place'Result.X) < Integer (O.Width) and then
               Integer (Place'Result.Y) < Integer (O.Height));

   --  Names one proposed plan. Frames rendered for a plan carry its epoch
   --  so display can commit plane changes with that frame.
   type Plan_Epoch is new Interfaces.Unsigned_64;
   No_Epoch : constant Plan_Epoch := 0;

   type Backing_Kind is (Hidden, Hardware, Composited);
   type Request_Table is array (Request_Id) of Request_State;
   type Output_Table is array (Output_Id) of Output_State;
   type Backing_Map is array (Request_Id) of Backing_Kind;
   type Plane_Map is array (Request_Id, Output_Id) of Plane_Count;
   type Holder_Map is array (Output_Id, Plane_Number) of Request_Count;
   --  Planes and Holders are two views of one relation: which plane of
   --  which output carries which request (see Indexed).
   type Plan is record
      Backing : Backing_Map := [others => Hidden];
      Planes  : Plane_Map := [others => [others => No_Plane]];
      Holders : Holder_Map := [others => [others => No_Request]];
   end record;

   --  Every live visible request is Hardware or Composited; nothing else is.
   function Covered (Requests : Request_Table; A : Plan) return Boolean is
     (for all R in Request_Id =>
        (A.Backing (R) = Hidden) = not Shown (Requests (R)));

   --  No plane on any output is given to two requests.
   function Exclusive (A : Plan) return Boolean is
     (for all O in Output_Id =>
        (for all R1 in Request_Id =>
           (for all R2 in Request_Id =>
              (if R1 /= R2 and then A.Planes (R1, O) /= No_Plane then
                 A.Planes (R1, O) /= A.Planes (R2, O)))));

   function Indexed (A : Plan) return Boolean is
     ((for all O in Output_Id =>
         (for all P in Plane_Number =>
            (if A.Holders (O, P) /= No_Request then
               A.Planes (A.Holders (O, P), O) = P and then
               A.Backing (A.Holders (O, P)) = Hardware))) and then
      (for all R in Request_Id =>
         (for all O in Output_Id =>
            (if A.Planes (R, O) /= No_Plane then
               A.Holders (O, A.Planes (R, O)) = R))));

   function Within_Outputs (Outputs : Output_Table; A : Plan)
      return Boolean is
     ((for all R in Request_Id =>
         (for all O in Output_Id => A.Planes (R, O) <= Outputs (O).Count))
      and then
      (for all O in Output_Id =>
         (for all P in Plane_Number =>
            (if P > Outputs (O).Count then A.Holders (O, P) = No_Request))));

   --  A Hardware request holds a compatible plane exactly on the outputs it
   --  touches; Hidden and Composited requests hold no plane.
   function Complete
     (Requests : Request_Table; Outputs : Output_Table; A : Plan)
      return Boolean is
     (for all R in Request_Id =>
        (if A.Backing (R) = Hardware then
           (for some O in Output_Id => Touches (Requests (R), Outputs (O)))
           and then
           (for all O in Output_Id =>
              (A.Planes (R, O) /= No_Plane) =
                Touches (Requests (R), Outputs (O))
              and then
              (if A.Planes (R, O) /= No_Plane then
                 Compatible (Requests (R),
                   Outputs (O).Planes (A.Planes (R, O)))))
         else
           (for all O in Output_Id => A.Planes (R, O) = No_Plane)));

   function Valid_Plan
     (Requests : Request_Table; Outputs : Output_Table; A : Plan)
      return Boolean is
     (Covered (Requests, A) and then Exclusive (A) and then Indexed (A) and then
      Within_Outputs (Outputs, A) and then Complete (Requests, Outputs, A));

   --  Offer order: priority descending, then identity ascending.
   function Precedes (Requests : Request_Table; First, Second : Request_Id)
      return Boolean is
     (Requests (First).Priority > Requests (Second).Priority or else
      (Requests (First).Priority = Requests (Second).Priority and then
       First < Second));

   --  Every plane of output O that could carry R is held by a request
   --  offered before R (vacuously true when no plane could carry it).
   function Taken_Before
     (Requests : Request_Table; Outputs : Output_Table; A : Plan;
      R : Request_Id; O : Output_Id) return Boolean is
     (for all P in Plane_Number =>
        (if P <= Outputs (O).Count and then
            Compatible (Requests (R), Outputs (O).Planes (P))
         then
           A.Holders (O, P) /= No_Request and then
           Precedes (Requests, A.Holders (O, P), R)));

   --  Priority: a visible request is composited only if it touches no
   --  output, or some output it touches has no compatible plane left that
   --  was not taken by a request of higher precedence.
   function Prioritized
     (Requests : Request_Table; Outputs : Output_Table; A : Plan)
      return Boolean is
     (for all R in Request_Id =>
        (if A.Backing (R) = Composited then
           (for all O in Output_Id => not Touches (Requests (R), Outputs (O)))
           or else
           (for some O in Output_Id =>
              Touches (Requests (R), Outputs (O)) and then
              Taken_Before (Requests, Outputs, A, R, O))));

   function Plan_Planes (Requests : Request_Table; Outputs : Output_Table)
      return Plan
     with Global => null,
          Post => Valid_Plan (Requests, Outputs, Plan_Planes'Result) and then
                  Prioritized (Requests, Outputs, Plan_Planes'Result);

   --  Moving a request between a plane and the client's composite must
   --  happen in the same visible frame, or the screen briefly shows it twice
   --  or not at all. Such transitions wait for a presented frame rendered
   --  for the new plan. Every other change (show, hide, renumbering on an
   --  output) is applied at once.
   function Swaps_Backing (Old_Kind, New_Kind : Backing_Kind) return Boolean is
     ((Old_Kind = Hardware and then New_Kind = Composited) or else
      (Old_Kind = Composited and then New_Kind = Hardware));

   function Needs_Frame (Old, Proposed : Plan) return Boolean is
     (for some R in Request_Id =>
        Swaps_Backing (Old.Backing (R), Proposed.Backing (R)));

   --  Output O must present a frame rendered for Proposed before it commits.
   function Frame_Affects
     (Old, Proposed : Plan; Requests : Request_Table;
      Outputs : Output_Table; O : Output_Id) return Boolean is
     (for some R in Request_Id =>
        Swaps_Backing (Old.Backing (R), Proposed.Backing (R)) and then
        (Old.Planes (R, O) /= No_Plane or else
         Proposed.Planes (R, O) /= No_Plane or else
         Touches (Requests (R), Outputs (O))));
end CuBit.Display_Planes;
