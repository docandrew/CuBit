------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small control map for hit testing and damage lookup
------------------------------------------------------------------------------
with CuBit.UI.Keys;
package CuBit.UI.Controls is
   subtype Control_ID is Natural;
   NO_CONTROL : constant Control_ID := 0;

   --  Room for a busy window (several file panes with their columns,
   --  toolbar, drawer and menus) without an application budgeting IDs.
   MAX_CONTROLS : constant Natural := 256;
   subtype Control_Index is Natural range 1 .. MAX_CONTROLS;
   subtype Control_Count is Natural range 0 .. MAX_CONTROLS;

   --  Input behavior is retained independently of paint. Render_Driven keeps
   --  existing immediate controls compatible while they are migrated.
   type Control_Behavior is
     (Render_Driven,
      Retained_Button,
      Retained_Vertical_Scrollbar,
      Retained_Horizontal_Drag);
   type Pointer_Action is
     (Pointer_Press, Pointer_Move, Pointer_Release, Pointer_Cancel);

   type Control_Entry is record
      id           : Control_ID := NO_CONTROL;
      bounds       : CuBit.UI.Rect := (others => 0);
      --  Hover and pressed-state changes are always local to the control.
      visualDamage : CuBit.UI.Rect := (others => 0);
      --  Actions may change a containing view (selection, scroll, layout).
      actionDamage : CuBit.UI.Rect := (others => 0);
      cursor       : CuBit.UI.Pointer_Cursor_Style :=
        CuBit.UI.Pointer_Default;
      continuousAction : Boolean := False;
      behavior     : Control_Behavior := Render_Driven;
      value        : Natural := 0;
      minimumValue : Natural := 0;
      maximumValue : Natural := 0;
      pageSize     : Positive := 1;
      scrollbarPart : CuBit.UI.Scrollbar_Part := CuBit.UI.Scrollbar_None;
      grabOffset   : Natural := 0;
      coordinateOrigin : Natural := 0;
      dragOffset   : Long_Long_Integer := 0;
      active       : Boolean := False;
      activated    : Boolean := False;
      pendingValue : Boolean := False;
      enabled      : Boolean := False;
      --  The control's shortcut, as data (like aria-keyshortcuts) for
      --  screen readers and the modifier reveal: No_Chord for none.
      shortcut     : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
      --  A scroll container's wheel (UI-035): the area the wheel scrolls
      --  this scrollbar from, the value one notch moves (0: no wheel), and
      --  notches routed to an immediate-mode scrollbar not yet taken.
      wheelArea    : CuBit.UI.Rect := (others => 0);
      wheelStep    : Natural := 0;
      wheelPending : Integer := 0;
   end record;

   type Control_Entries is array (Control_Index) of Control_Entry;

   type Control_Map is record
      entries : Control_Entries := (others => (others => <>));
      entryCount : Control_Count := 0;
      --  The previous committed registry preserves per-control interaction
      --  state while the next declarative frame rebuilds geometry.
      retainedEntries : Control_Entries := (others => (others => <>));
      retainedCount : Control_Count := 0;
      valid : Boolean := True;
   end record;

   procedure Clear (m : in out Control_Map);

   --  False means registration exhausted its bounded storage or two live
   --  controls claimed the same ID. Hit testing and damage lookup then fail
   --  inertly for the complete map rather than returning ambiguous results.
   function Is_Valid (m : Control_Map) return Boolean;

   procedure Add
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       --  Region whose pixels may change when the control is operated.
       --  Ordinary pointer motion still invalidates only bounds.
       actionDamage : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;
       continuousAction : Boolean := False);

   --  An embedded renderer's area (see CuBit.UI.Surfaces). It owns pointer
   --  hits and the cursor shape there, but hover and press never repaint
   --  it: the renderer reports its own damage.
   procedure Add_Surface
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default);

   procedure Add_Vertical_Scrollbar
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       value : Natural;
       minValue, maxValue : Natural;
       pageSize : Positive := 1);

   procedure Add_Horizontal_Drag
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       value : Natural;
       minValue, maxValue : Natural;
       coordinateOrigin : Natural);

   procedure Add_Button
      (m : in out Control_Map;
       id : Control_ID;
       bounds : CuBit.UI.Rect;
       actionDamage : CuBit.UI.Rect;
       cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default);

   --  Dispatches retained behavior before painting. handled is true only for
   --  a migrated retained control. changed reports a semantic value change,
   --  allowing callers to avoid repainting expensive content at rest.
   procedure Dispatch_Pointer
      (m : in out Control_Map;
       id : Control_ID;
       action : Pointer_Action;
       x, y : Natural;
       changed : out Boolean;
       handled : out Boolean);

   procedure Take_Value
      (m : in out Control_Map;
       id : Control_ID;
       value : out Natural;
       available : out Boolean);

   function Is_Active
      (m : Control_Map; id : Control_ID) return Boolean;

   function Take_Activated
      (m : in out Control_Map; id : Control_ID) return Boolean;

   function Active_Scrollbar_Part
      (m : Control_Map; id : Control_ID) return CuBit.UI.Scrollbar_Part;

   function Hit
      (m : Control_Map; x, y : Natural) return Control_ID;

   --  The exact visible interaction rectangle registered for a control.
   --  Widgets use this for state evaluation so clipping and hit testing can
   --  never disagree.
   function Bounds
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Visual_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Action_Damage
      (m : Control_Map; id : Control_ID) return CuBit.UI.Rect;

   function Cursor
      (m : Control_Map; id : Control_ID)
      return CuBit.UI.Pointer_Cursor_Style;

   --  Scroll wheels (UI-035): the scroll container under the pointer
   --  takes the wheel, in the toolkit, so no app routes it. A vertical
   --  scrollbar names the area it scrolls (its list, tree, table or text
   --  view) and how far one notch moves it; Route_Wheel, called by
   --  CuBit.UI.App for every wheel event, moves the innermost such
   --  scrollbar under the pointer: a retained one's value directly (Take_Value
   --  returns it next frame), an immediate one's through Take_Wheel.
   --  Handled: a container took the notches (even at its end, so an outer
   --  one does not scroll instead); Damage: what to repaint.
   procedure Set_Wheel_Area
      (m : in out Control_Map; id : Control_ID; area : CuBit.UI.Rect; step : Positive);
   procedure Route_Wheel
      (m : in out Control_Map; x, y : Natural; notches : Integer;
       handled : out Boolean; damage : out CuBit.UI.Rect);
   --  Notches routed to an immediate-mode scrollbar in the last frame
   --  (positive: toward the top), taken once.
   function Take_Wheel (m : in out Control_Map; id : Control_ID) return Integer;

   --  A registered control's shortcut (accessibility data). Set after
   --  adding the control in the same frame; unknown IDs are ignored.
   procedure Set_Shortcut
      (m : in out Control_Map; id : Control_ID; shortcut : CuBit.UI.Keys.Chord);
   function Shortcut
      (m : Control_Map; id : Control_ID) return CuBit.UI.Keys.Chord;

   function Has_Continuous_Action
      (m : Control_Map; id : Control_ID) return Boolean;

   procedure Mark_Visual_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID);

   procedure Mark_Action_Dirty
      (dirty : in out CuBit.UI.Rect; m : Control_Map; id : Control_ID);
end CuBit.UI.Controls;
