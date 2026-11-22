------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the debugger's panes, drawn from a snapshot
------------------------------------------------------------------------------
--  One view for the native app and the hosted tests: it draws only from a
--  Debugger_Model.Snapshot and its own View_State, and turns keys and
--  pointer input into Requests for whoever holds the engine session. It
--  never reads the target itself, so it holds no debug authority.
--
--  Damage-only: every input names the panes it changed (Dirty), and Render
--  paints only the panes that meet its damage.
with Interfaces; use Interfaces;
with CuBit.UI;
with Debugger_Model; use Debugger_Model;
with Debugger_Graph;
with CuBit.UI.Code_Views;
with CuBit.UI.Tree_Tables;

package Debugger_View is
   type Pane is (Threads_Pane, Stack_Pane, Source_Pane, Locals_Pane, Graph_Pane, Memory_Pane);
   type Region is (Toolbar_Region, Timeline_Region, Status_Region);
   type Pane_Rects is array (Pane) of CuBit.UI.Rect;
   type Region_Rects is array (Region) of CuBit.UI.Rect;

   --  What the holder of the session is asked to do.
   type Command is
     (No_Command, Continue, Refresh, Load_Children, Select_Thread, Select_Frame,
      Show_In_Graph, Follow_Pointer, Show_Memory, Toggle_Live, Quit,
      --  Attach to process... (A): list what runs; attach to the one picked.
      Pick_Process, Attach_Process);
   type Request is record
      Cmd     : Command := No_Command;
      Local   : Local_Index := 1;
      Node    : Node_Index := 1;
      Field   : Field_Index := 1;
      Thread  : Thread_Index := 1;
      Frame   : Frame_Index := 1;
      Address : Target_Address := NULL_ADDRESS;
      Process : Process_Index := 1;
   end record;

   PICK_PREFIX_CAPACITY : constant := 24;

   type Frame_Stats is record
      Last_Us, Worst_Us, Total_Us : Unsigned_64 := 0;
      Count : Unsigned_64 := 0;
      Last_Full : Boolean := False;
   end record;


   type View_State is record
      Width, Height  : Natural := 0;
      Panes          : Pane_Rects := [others => (others => 0)];
      Regions        : Region_Rects := [others => (others => 0)];
      Focus          : Pane := Source_Pane;
      Code           : CuBit.UI.Code_Views.View;   --  the source pane
      Locals         : CuBit.UI.Tree_Tables.View;  --  the locals tree table
      Thread_Row     : Thread_Index := 1;
      Frame_Row      : Frame_Index := 1;
      Graph_Fitted   : Boolean := False;
      --  The data-structure graph (the toolkit's graph view).
      Graph          : Debugger_Graph.Views.View;
      --  F: the focused pane fills the panes' area (F again: back).
      Maximized      : Boolean := False;
      Mem_Top        : Natural := 0;
      --  Type-ahead: the last character typed (the same again: next match).
      Last_Typed     : Character := ASCII.NUL;     --  rows above the first shown
      Stats          : Frame_Stats;
      --  Attach to process...: the list shows in the Threads pane; letters
      --  typed pick by name's start, Enter attaches, Esc goes back.
      Picking        : Boolean := False;
      Pick_Row       : Process_Index := 1;
      Pick_Prefix    : String (1 .. PICK_PREFIX_CAPACITY) := [others => ' '];
      Pick_Length    : Natural range 0 .. PICK_PREFIX_CAPACITY := 0;
      --  The A that opened the list arrives as text too: not a name's start.
      Pick_Skip_Text : Boolean := False;
   end record;

   --  Open the list (after Debugger_Source.List_Processes).
   procedure Start_Picking (V : in out View_State);

   --  Lay the panes out for a client area of Width x Height.
   procedure Arrange (V : in out View_State; Width, Height : Natural);
   --  After a new snapshot: visible locals, graph layout, source follow.
   procedure Absorb (V : in out View_State; S : Snapshot);

   procedure Render
     (C : CuBit.UI.Canvas; Damage : CuBit.UI.Rect; Colors : CuBit.UI.Theme;
      S : Snapshot; V : in out View_State; Now : Microseconds);
   function Pane_Rect (V : View_State; P : Pane) return CuBit.UI.Rect is (V.Panes (P));
   function Region_Rect (V : View_State; R : Region) return CuBit.UI.Rect is (V.Regions (R));
   function Everything (V : View_State) return CuBit.UI.Rect is (0, 0, V.Width, V.Height);

   --  Keyboard (PS/2 set-1 make codes, as CuBit.UI.Input delivers them).
   procedure Key
     (V : in out View_State; S : in out Snapshot; Code, Modifiers : Unsigned_64;
      Dirty : in out CuBit.UI.Rect; Req : out Request);
   --  Type-ahead (a typed character, INPUT_TEXT): in Threads, Call stack
   --  and Locals, the next row whose name starts with Ch. The command
   --  letters (G, M, L, F) are keys, never type-ahead.
   procedure Text_Key
     (V : in out View_State; S : Snapshot; Ch : Character; Dirty : in out CuBit.UI.Rect; Req : out Request);
   procedure Pointer_Down (V : in out View_State; S : Snapshot; X, Y : Natural; Dirty : in out CuBit.UI.Rect);
   procedure Pointer_Move (V : in out View_State; X, Y : Natural; Dirty : in out CuBit.UI.Rect);
   procedure Pointer_Up
     (V : in out View_State; S : in out Snapshot; X, Y : Natural; Dirty : in out CuBit.UI.Rect;
      Req : out Request);
   procedure Wheel
     (V : in out View_State; S : Snapshot; X, Y : Natural; Steps : Integer; Dirty : in out CuBit.UI.Rect);

   --  The panes whose changed marks are showing at Now and will go off
   --  later (Until: when the soonest goes; 0 if none).
   procedure Marks_Pending (S : Snapshot; Now : Microseconds; Area : out CuBit.UI.Rect;
                            Until_Us : out Microseconds; V : View_State);

   procedure Record_Frame (V : in out View_State; Took_Us : Unsigned_64; Full : Boolean);

   --  Text the view shows, for tests and the headless check: the current
   --  line's text, and the "name = value" of each top-level local.
   function Current_Line_Text (S : Snapshot) return String;

   --  Keys (set-1 make codes).
   KEY_TAB   : constant Unsigned_64 := 16#0F#;
   KEY_ENTER : constant Unsigned_64 := 16#1C#;
   KEY_SPACE : constant Unsigned_64 := 16#39#;
   KEY_UP    : constant Unsigned_64 := 16#48#;
   KEY_DOWN  : constant Unsigned_64 := 16#50#;
   KEY_LEFT  : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT : constant Unsigned_64 := 16#4D#;
   KEY_PGUP  : constant Unsigned_64 := 16#49#;
   KEY_PGDN  : constant Unsigned_64 := 16#51#;
   KEY_HOME  : constant Unsigned_64 := 16#47#;
   KEY_END   : constant Unsigned_64 := 16#4F#;
   KEY_F5    : constant Unsigned_64 := 16#3F#;
   KEY_F10   : constant Unsigned_64 := 16#44#;
   KEY_F11   : constant Unsigned_64 := 16#57#;
   KEY_MINUS : constant Unsigned_64 := 16#0C#;
   KEY_EQUAL : constant Unsigned_64 := 16#0D#;
   KEY_0     : constant Unsigned_64 := 16#0B#;
   KEY_R     : constant Unsigned_64 := 16#13#;
   KEY_L     : constant Unsigned_64 := 16#26#;
   KEY_F     : constant Unsigned_64 := 16#21#;
   KEY_M     : constant Unsigned_64 := 16#32#;
   KEY_G     : constant Unsigned_64 := 16#22#;
   KEY_ESC   : constant Unsigned_64 := 16#01#;
   KEY_A     : constant Unsigned_64 := 16#1E#;
   MOD_SHIFT : constant Unsigned_64 := 1;
   MOD_CTRL  : constant Unsigned_64 := 2;
end Debugger_View;
