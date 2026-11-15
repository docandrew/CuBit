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
with CuBit.File_Selection;
with CuBit.UI;
with Dash_Choices;
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
      Show_Keys,
      --  The menu bar (F10; Alt+letter opens a menu), the panel's source
      --  picker (Enter while editing), a new panel (A), the dashboards
      --  (Ctrl+N new, Ctrl+O open, Ctrl+Shift+S save as), and the keys
      --  the menus, the picker and the dialogs move with.
      Menu_Bar, Panel_Source, Add_Panel, New_Dashboard, Open_Dashboard, Save_As,
      Enter, Space, Backspace, Page_Up, Page_Down, Select_All,
      --  A key known only by its letter (Letter): menu mnemonics and the
      --  picker's type-ahead.
      Letter_Key);
   --  Text_Input: a typed character (Character_Value), for the dialogs'
   --  name field. Modifier_Change: Alt went down or up (Alt: held now),
   --  for the menu bar's keyboard cues.
   type Event_Kind is
     (Pointer_Move, Pointer_Press, Pointer_Release, Wheel, Key_Press, Pointer_Leave, Text_Input, Modifier_Change);
   type Event is record
      Kind : Event_Kind := Pointer_Move;
      X, Y : Natural := 0;
      Shift : Boolean := False;
      Control, Alt : Boolean := False;
      Steps : Integer := 0;
      Pressed : Key := No_Key;
      --  The key's letter or digit, lower case (' ': none).
      Letter : Character := ' ';
      Character_Value : Character := ' ';
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
   --  What is saved: Save_Source and Save_Name, in the same hold as
   --  Take_Save (the dashboard shown, or one just left with edits not yet
   --  written: another one opened over it).
   procedure Take_Save (V : in out View; Due : out Boolean; Generation : out Natural);
   procedure End_Save (V : in out View; Generation : Natural; Saved : Boolean; Reason : String := "");
   function Save_Name (V : View) return String;
   function Save_Source (V : View) return String;
   --  The dashboard's own file in the dashboards folder (its leaf name).
   function Current_File (V : View) return String;
   type Save_State is (Unedited, Edited, Saving, Saved, Not_Saved);
   function Saving_State (V : View) return Save_State;

   --  The dashboards folder (the data thread's, as saving is): the window
   --  asks for a listing, a file opened, or the dashboard saved as a new
   --  file; Take_Job (under the lock) hands one out, the data thread does
   --  the I/O without the lock and answers (under it again). Leaf names
   --  only, in Dash_Model.Dashboards_Place.
   type Job_Kind is (No_Job, List_Folder, Open_File, Save_New_File);
   type Job is record
      Kind : Job_Kind := No_Job;
      Leaf : CuBit.File_Selection.File_Name;
   end record;
   procedure Take_Job (V : in out View; J : out Job);
   --  The folder's dashboards (.ccl files) and when each last changed,
   --  as many as the file dialog lists (its ceiling). Ready False: the
   --  folder could not be read (no grant, no disk).
   type Folder_Entry is record
      Leaf : CuBit.File_Selection.File_Name;
      Modified : Unsigned_64 := 0;
   end record;
   type Folder_Entries is array (1 .. CuBit.File_Selection.Maximum_Files) of Folder_Entry;
   type Folder_Listing is record
      Entries : Folder_Entries;
      Count : CuBit.File_Selection.File_Count := 0;
      Ready : Boolean := False;
   end record;
   procedure End_List (V : in out View; Listing : Folder_Listing);
   --  Open_File's answer: the file's bytes (Read), or why not. The window
   --  checks them against the Dashboard type at its next refresh (the
   --  checker wants more stack than the data thread has), shows the
   --  checker's message when they do not check, and keeps its dashboard.
   procedure End_Open (V : in out View; Leaf : String; Source : String; Read : Boolean; Reason : String);
   --  Save_New_File's: the source written as Leaf (Save_Source, taken in
   --  the same hold as the job), or why not.
   procedure End_Save_New (V : in out View; Leaf : String; Saved : Boolean; Reason : String);

   --  Copy as CCL (the menu's): the host copies Dashboard_Source to the
   --  clipboard when this says so (once).
   function Take_Copy_Request (V : in out View) return Boolean;

   --  What the window shows, for the tests and the host's markers: the
   --  open menu (1 is File; 0: none), the picker and its panel, the file
   --  dialog, the status bar's notice, the picker's choices.
   function Open_Menu (V : View) return Natural;
   function Menu_Bar_Bounds (V : View) return Rect;
   function Menu_Popup_Bounds (V : View) return Rect;
   function Picker_Panel (V : View) return Natural;
   function Picker_Bounds (V : View) return Rect;
   function Source_Button_Bounds (V : View; P : Positive) return Rect;
   function Dialog_Open (V : View) return Boolean;
   function Notice (V : View) return String;
   type Picker_Field is (Series_Field, View_Field);
   function Picker_Choices (V : View; Field : Picker_Field) return Natural;
   function Picker_Choice (V : View; Field : Picker_Field; Index : Positive) return String;
   function Picker_Selection (V : View; Field : Picker_Field) return Natural;
   function Panel_View (V : View; P : Positive; Choice : out Dash_Choices.View_Choice) return Boolean;
   function Selected (V : View) return Natural;
   --  Panel P's CCL expression ("" past the last panel).
   function Panel_Source (V : View; P : Positive) return String;

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

   --  The last Render's copy out of the retained frame: its time, the
   --  pixels of the toolkit's one damage rectangle it copied, and the
   --  pixels of the rectangles that changed (what a multi-rectangle damage
   --  list would copy and present; overlaps counted once per mark, so an
   --  upper bound).
   procedure Take_Copy_Stats (V : View; Paint_Us, Copy_Us, Union_Pixels, Exact_Pixels : out Unsigned_64);

   --  Draw the whole window into the retained frame now and say how long
   --  it took (a probe: the same work hosted and on CuBit compares the two
   --  machines; the frame shown next is a copy, as always).
   procedure Probe_Full_Redraw (V : in out View; Took_Us : out Unsigned_64);
   --  Panel P's card and its plot (tests aim the pointer with these).
   function Panel_Count (V : View) return Natural;
   function Panel_Bounds (V : View; P : Positive) return Rect;
   function Plot_Bounds (V : View; P : Positive) return Rect;
private
   --  One window per process: the view's state lives in the body.
   type View is limited null record;
end Dash_View;
