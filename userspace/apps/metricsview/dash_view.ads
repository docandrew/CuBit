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

   --  One frame's fetch: Fetch_Budget_Us of feed time at most for loading
   --  history (the live edge is always fetched). Damage grows by what to
   --  repaint.
   Fetch_Budget_Us : constant := 2_000;
   procedure Refresh (V : in out View; Damage : in out Rect);

   type Key is
     (No_Key, Left, Right, Home, End_Key, Escape, Zoom_In, Zoom_Out, Live, Copy,
      Range_1, Range_2, Range_3, Range_4, Range_5, Range_6);
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
   --  Panel P's card and its plot (tests aim the pointer with these).
   function Panel_Count (V : View) return Natural;
   function Panel_Bounds (V : View; P : Positive) return Rect;
   function Plot_Bounds (V : View; P : Positive) return Rect;
private
   --  One window per process: the view's state lives in the body.
   type View is limited null record;
end Dash_View;
