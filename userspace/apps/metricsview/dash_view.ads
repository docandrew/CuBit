------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's window (MET-002's app): a dashboard's panels on a
--  12-column grid, drawn on the CPU from each track's column summaries,
--  and the interaction (hover, pan, zoom, range select, drill-down).
--
--  @description
--  No IPC happens on the input path: Handle changes the view and resamples
--  or shifts the retained columns, and the next frame shows it; Refresh
--  (once per frame, from the host's frame clock) asks the feed for what is
--  new and reports the damage, which is the changed columns' slivers when
--  only data arrived and the plots when the time axis moved. Render paints
--  any rectangle from retained state, so the toolkit's repair works.
--  Platform-free: the native host (main.adb) and the hosted tests drive
--  the same view.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.UI;
with Dash_Feeds;
with Dash_Model;

package Dash_View is
   subtype Rect is CuBit.UI.Rect;

   type Feed_Access is access all Dash_Feeds.Feed'Class;

   --  The program's one dashboard window (its state is the body's).
   type View is limited private;

   procedure Initialize (V : in out View; Source : Feed_Access; D : Dash_Model.Dashboard);
   --  The window's client area at its density (the retained frame's size).
   procedure Set_Bounds (V : in out View; Bounds : Rect; Numerator, Denominator : Positive := 1);
   function Bounds (V : View) return Rect;

   --  The data thread's step: what the store pushed, the series looked up,
   --  history fetched (Fetch_Budget_Us of feed time at most per step), the
   --  stat windows. All the I/O and decoding is here, with the shared lock
   --  let go around every call (Dash_Sync).
   Fetch_Budget_Us : constant := 2_000;
   procedure Data_Step (V : in out View);

   --  The UI thread's frame: viewports follow the clock and the input, and
   --  Damage grows by what changed. No I/O.
   procedure Refresh (V : in out View; Damage : in out Rect);

   type Key is
     (No_Key, Left, Right, Up, Down, Home, End_Key, Escape, Zoom_In, Zoom_Out, Live, Copy,
      Range_1, Range_2, Range_3, Range_4, Range_5, Range_6,
      --  The editor (keyboard first): E toggles it; Tab selects the next
      --  panel; arrows move it, Shift+arrows size it; Delete removes it,
      --  Insert copies it; Ctrl+Z undoes, Ctrl+Y redoes; C shows its CCL.
      Edit, Next_Panel, Remove, Duplicate, Undo, Redo, Show_Source,
      --  ? or F1: the keys.
      Show_Keys);
   type Event_Kind is (Pointer_Move, Pointer_Press, Pointer_Release, Wheel, Key_Press, Pointer_Leave);
   type Event is record
      Kind : Event_Kind := Pointer_Move;
      X, Y : Natural := 0;
      Shift : Boolean := False;
      Steps : Integer := 0;
      Pressed : Key := No_Key;
   end record;
   procedure Handle (V : in out View; E : Event; Damage : in out Rect);

   --  Damage of the window's canvas C, from the retained frame (only what
   --  changed since the last Render is drawn; the rest is copied).
   procedure Render (V : in out View; C : CuBit.UI.Canvas; Damage : Rect);

   --  R must be drawn again (a colour scheme change: the whole window).
   procedure Invalidate (V : in out View; R : Rect; Damage : in out Rect);

   --  The editor: whether it is on, the dashboard as it stands (CCL
   --  source), and how many edits so far (a change: save it).
   function Editing (V : View) return Boolean;
   function Dashboard_Source (V : View) return String;
   function Edits (V : View) return Natural;
   function Source_Showing (V : View) return Boolean;

   --  Saving (the data thread's: Dash_Runner, never under the lock's I/O):
   --  an edited dashboard is due Autosave_Delay_Ms after its last edit, or
   --  at once when editing ends. Take_Save (under the lock) says whether one
   --  is due and which edit it holds; the caller copies Dashboard_Source and
   --  Save_Name in the same hold, writes the file without the lock, then
   --  reports with End_Save (under the lock again).
   Autosave_Delay_Ms : constant := 1_000;
   procedure Take_Save (V : in out View; Due : out Boolean; Generation : out Natural);
   procedure End_Save (V : in out View; Generation : Natural; Saved : Boolean; Reason : String := "");
   function Save_Name (V : View) return String;
   type Save_State is (Unedited, Edited, Saving, Saved, Not_Saved);
   function Saving_State (V : View) return Save_State;

   --  The drill-down's CCL expression for the selected slice's logs
   --  ((logs.between (LogWindow "service" from to)), boot milliseconds),
   --  or "" with no slice selected.
   function Drill_Down_Expression (V : View) return String;

   --  For the host's markers and the tests.
   function Is_Live (V : View) return Boolean;
   function Span_Us (V : View) return Unsigned_64;
   function Resolved_Tracks (V : View) return Natural;
   function Track_Total (V : View) return Natural;
   --  Columns with samples, over every track.
   function Columns_With_Data (V : View) return Natural;
   --  The newest summary of the first track of the panel titled Title:
   --  samples in view, p99 of the newest column with data.
   procedure Probe
     (V : View; Title : String; Samples, Newest_P99, From_Us, To_Us : out Unsigned_64; Found : out Boolean);
   function Title (V : View) return String;
   --  Samples the store pushed so far, and whether it pushes.
   function Pushed_Samples (V : View) return Unsigned_64;
   function Push_Active (V : View) return Boolean;
   --  How long the UI thread waited for the shared lock since the last take.
   procedure Take_Lock_Wait (V : View; Waited_Us : out Unsigned_64);
   --  Panel P's card and its plot (tests aim the pointer with these).
   function Panel_Count (V : View) return Natural;
   function Panel_Bounds (V : View; P : Positive) return Rect;
   function Plot_Bounds (V : View; P : Positive) return Rect;
private
   --  One window per process: the view's state lives in the body.
   type View is limited null record;
end Dash_View;
