------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Graph view: nodes with rows and ports, edges between them, a stable
--  incremental layout, pan and zoom (the model-independent part)
------------------------------------------------------------------------------
--  The toolkit's node-and-edge canvas (docs/debugging.md 9; the stream
--  graph inspector and the capability graph share it): square boxes, raw
--  data, no glow. CuBit.UI.Graphs.Views draws any caller model through
--  accessor functions; this package holds what does not depend on it:
--  geometry, the layout, zoom steps and the editing vocabulary.
--
--  A node is a header (title and detail) over rows. A row may carry a
--  port: an out-port on its right edge (a pointer field, an outlet), an
--  in-port on its left (an inlet). An edge runs from a row's out-port to
--  another node, at its header (To_Row 0) or at one of its in-port rows.
--
--  Layout: a node is keyed by the caller's stable identity (for the
--  Debugger: address and type). A node once placed keeps its place across
--  updates; new nodes go beside their parent in free space, or below it
--  past the wrap width; a node that disappears leaves a gap and returns to
--  its place. Pinned nodes stay where the caller (or the user) put them.
--  Only Relayout moves placed, unpinned nodes.
--
--  Editing (planned, not built: the API is shaped for it): a view in
--  Editing mode lets the user drag from an out-port to an in-port to
--  propose a connection, delete edges, add and move nodes. Every proposal
--  is put to the caller's Connection_Check (a type check, for CCL streams
--  and CuBuilder) before it is drawn; a refused drop is shown refused and
--  nothing changes. Moving a node pins it (Pin), and pins persist in the
--  caller's model. Selection is a node or an edge; keyboard operations act
--  on it. Viewing mode (the Debugger) is read-only.
with CuBit.UI_Metrics;

package CuBit.UI.Graphs is

   ---------------------------------------------------------------------------
   --  Model vocabulary.
   subtype Node_Key is Unsigned_64;
   NO_KEY : constant Node_Key := 0;

   --  The header colour of a node.
   type Node_Style is (Record_Style, Array_Style, Pointer_Style, Enum_Style, Plain_Style);

   --  What a row's port shows: none; an out-port not yet followed (click
   --  to expand); one with its edge drawn; a null link (ground mark); a
   --  dangling one (red); an in-port.
   type Port_State is (No_Port, Open_Port, Linked_Port, Null_Port, Dangling_Port, In_Port);
   subtype Out_Port is Port_State range Open_Port .. Dangling_Port;

   --  A caller-defined port type, for the editing check.
   subtype Port_Type is Unsigned_32;
   ANY_PORT : constant Port_Type := 0;

   MAX_NODES : constant := 512;
   MAX_EDGES : constant := 1_024;
   MAX_ROWS  : constant := 64;
   subtype Node_Count is Natural range 0 .. MAX_NODES;
   subtype Node_Index is Positive range 1 .. MAX_NODES;
   subtype Edge_Count is Natural range 0 .. MAX_EDGES;
   subtype Edge_Index is Positive range 1 .. MAX_EDGES;
   subtype Row_Count is Natural range 0 .. MAX_ROWS;
   subtype Row_Index is Positive range 1 .. MAX_ROWS;
   --  0: the node's header (its own in-port).
   subtype Row_Or_Header is Natural range 0 .. MAX_ROWS;

   type Edge is record
      From     : Node_Index := 1;
      From_Row : Row_Index := 1;
      To       : Node_Index := 1;
      To_Row   : Row_Or_Header := 0;
      Back     : Boolean := False;   --  closes a cycle: drawn round, labelled
   end record;

   --  Editing (planned): a proposed connection, put to the caller.
   type Connection is record
      From     : Node_Index := 1;
      From_Row : Row_Index := 1;
      To       : Node_Index := 1;
      To_Row   : Row_Or_Header := 0;
   end record;
   type Mode is (Viewing, Editing);

   ---------------------------------------------------------------------------
   --  Geometry, in world units (logical pixels at zoom 100%).
   CHAR_WIDTH    : constant := CuBit.UI_Metrics.Mono_Advance;
   HEADER_HEIGHT : constant := CuBit.UI_Metrics.Line_Height + 7;
   ROW_HEIGHT    : constant := CuBit.UI_Metrics.Line_Height + 1;
   NODE_PAD      : constant := 9;
   PORT_WIDTH    : constant := 12;
   MIN_NODE_W    : constant := 150;
   MAX_NODE_W    : constant := 380;
   GAP_X         : constant := 64;
   GAP_Y         : constant := 18;

   WORLD_LIMIT : constant := 4_000_000;
   subtype World is Integer range -WORLD_LIMIT .. WORLD_LIMIT;
   subtype Extent is Natural range 0 .. 100_000;
   type Box is record
      X, Y : World := 0;
      W, H : Extent := 0;
   end record;
   function Overlaps (A, B : Box; Margin : Natural) return Boolean;

   --  A node's size from its text: the widest of header and rows (in
   --  characters) and its row count (rows past the ceiling: one more).
   function Node_Size (Widest_Chars : Natural; Rows : Row_Count; More_Rows : Boolean) return Box;
   --  Inside a node: row R's centre (R = 0: the header's).
   function Row_Center (R : Row_Or_Header) return Natural is
     (if R = 0 then HEADER_HEIGHT / 2 else HEADER_HEIGHT + (R - 1) * ROW_HEIGHT + ROW_HEIGHT / 2);

   ---------------------------------------------------------------------------
   --  The layout's input: each node's key, size and pin, and the edges.
   type Key_Array is array (Node_Index range <>) of Node_Key;
   type Box_Array is array (Node_Index range <>) of Box;
   type Pin_Array is array (Node_Index range <>) of Boolean;
   type Edge_Array is array (Edge_Index range <>) of Edge;

   MAX_PLACES : constant := 2 * MAX_NODES;
   subtype Place_Count is Natural range 0 .. MAX_PLACES;
   subtype Place_Index is Positive range 1 .. MAX_PLACES;
   type Place is record
      Key     : Node_Key := NO_KEY;
      Where   : Box;
      Present : Boolean := False;
      Pinned  : Boolean := False;
   end record;
   type Place_Array is array (Place_Index) of Place;
   type Node_Places is array (Node_Index) of Natural;
   type Layout is record
      Places : Place_Array;
      Total  : Place_Count := 0;
      Of_Node : Node_Places := [others => 0];
      Placed_New : Natural := 0;
   end record;

   --  Bring L up to date: known keys keep their place (resized to Sizes),
   --  new ones are placed (Sizes' X and Y are ignored, except for pinned
   --  nodes, which are put exactly there). Wrap_Width 0: never wrap.
   procedure Update
     (L : in out Layout; Keys : Key_Array; Sizes : Box_Array; Pinned : Pin_Array;
      Edges : Edge_Array; Wrap_Width : Natural := 0)
   with Pre => Keys'First = 1 and then Sizes'First = 1 and then Pinned'First = 1
     and then Sizes'Last = Keys'Last and then Pinned'Last = Keys'Last;
   --  Forget unpinned places and lay out afresh (on request only).
   procedure Relayout
     (L : in out Layout; Keys : Key_Array; Sizes : Box_Array; Pinned : Pin_Array;
      Edges : Edge_Array; Wrap_Width : Natural := 0)
   with Pre => Keys'First = 1 and then Sizes'First = 1 and then Pinned'First = 1
     and then Sizes'Last = Keys'Last and then Pinned'Last = Keys'Last;
   function Box_Of (L : Layout; N : Node_Index) return Box;
   function Bounds (L : Layout) return Box;
   --  Pin a node where it is now, or at X, Y (editing: a moved node).
   procedure Pin (L : in out Layout; N : Node_Index; X, Y : World);

   ---------------------------------------------------------------------------
   --  Zoom: the canvas density times N / D, so text stays crisp at each step.
   type Zoom_Step is range 1 .. 9;
   UNIT_ZOOM : constant Zoom_Step := 5;
   ZOOM_N : constant array (Zoom_Step) of Positive := [1, 5, 3, 7, 1, 5, 3, 7, 2];
   ZOOM_D : constant array (Zoom_Step) of Positive := [2, 8, 4, 8, 1, 4, 2, 4, 1];
   --  At or below this step nodes show bars for their rows, no text.
   SUMMARY_ZOOM : constant Zoom_Step := 2;
   function Zoom_Percent (Step : Zoom_Step) return Natural is (ZOOM_N (Step) * 100 / ZOOM_D (Step));
   --  The canvas C at Step's density, clipped to Area (and C's own clip),
   --  and Area in its coordinates. OK False: the density cannot be drawn.
   procedure Zoomed (C : Canvas; Area : Rect; Step : Zoom_Step; Result : out Canvas; Zone : out Rect;
                     OK : out Boolean);
end CuBit.UI.Graphs;
