------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Hosted tests of the Debugger's views (DBG-001), against the TEST DOUBLE
--  (double/debugger_source.adb). Linux-hosted: no CuBit process is read.
--  Writes build/debugger-<w>x<h>.ppm for looking at the layout; those are
--  test-double pictures and are never published as screenshots.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with Debugger_Model; use Debugger_Model;
with Debugger_View; use Debugger_View;
with CuBit.UI.Graphs;
with Debugger_Source;
with Debugger_Graph;
with Debugger_State; use Debugger_State;

procedure Debugger_Gui_Tests is
   package GL renames CuBit.UI.Graphs;
   use type Ada.Real_Time.Time;
   subtype Time is Ada.Real_Time.Time;
   function Clock return Time renames Ada.Real_Time.Clock;
   function To_Duration (D : Ada.Real_Time.Time_Span) return Duration renames Ada.Real_Time.To_Duration;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Put_Line ("PASS " & What);
      else
         Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   Start : constant Time := Clock;
   function Now_Us return Microseconds is
     (Microseconds (To_Duration (Clock - Start) * 1_000_000.0) + 1);

   type Pixel_Buffer is array (Natural range <>) of Color;
   type Pixel_Access is access Pixel_Buffer;

   procedure Write_PPM (Name : String; Pixels : Pixel_Buffer; W, H : Natural) is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
      type Bytes_Access is access Stream_Element_Array;
      RGB : constant Bytes_Access := new Stream_Element_Array (1 .. Stream_Element_Offset (W * H * 3));
      At_Byte : Stream_Element_Offset := 1;
      function Img (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (S'First + 1 .. S'Last);
      end Img;
   begin
      for P of Pixels loop
         RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
         RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
         RGB (At_Byte + 2) := Stream_Element (P and 255);
         At_Byte := At_Byte + 3;
      end loop;
      IO.Create (F, IO.Out_File, Name);
      String'Write (IO.Stream (F), "P6" & ASCII.LF & Img (W) & " " & Img (H) & ASCII.LF & "255" & ASCII.LF);
      IO.Write (F, RGB.all);
      IO.Close (F);
   end Write_PPM;

   function Make_Canvas (Pixels : Pixel_Access; W, H : Natural; Density : Positive) return Canvas is
     ((addr => Pixels.all'Address, width => W / Density, height => H / Density, pitch => W * 4,
       densityNumerator => Density, densityDenominator => 1, others => <>));

   --  Render the whole window Rounds times; the best and worst in microseconds.
   procedure Time_Render (C : Canvas; Damage : Rect; Rounds : Positive; Best, Worst : out Unsigned_64) is
      T0 : Time;
      Took : Unsigned_64;
   begin
      Best := Unsigned_64'Last;
      Worst := 0;
      for I in 1 .. Rounds loop
         T0 := Clock;
         Render (C, Damage, CuBit.UI.CuBit_Alloy, Current, View, Now_Us);
         Took := Unsigned_64 (To_Duration (Clock - T0) * 1_000_000.0);
         Best := Unsigned_64'Min (Best, Took);
         Worst := Unsigned_64'Max (Worst, Took);
      end loop;
   end Time_Render;

   procedure Show_Time (Label : String; Best, Worst : Unsigned_64) is
   begin
      Put_Line ("TIME: " & Label & " best" & Unsigned_64'Image (Best) & " us, worst" & Unsigned_64'Image (Worst)
                & " us");
   end Show_Time;

   Rounds : constant Positive :=
     (if Ada.Command_Line.Argument_Count > 0 then Positive'Value (Ada.Command_Line.Argument (1)) else 20);
   Result : Debugger_Source.Attach_Result;
   Req : Request;
   Dirty : Rect;
   Best, Worst : Unsigned_64;
   use type Debugger_Source.Attach_Result;
begin
   --  Attach and lay out at 1080p.
   Debugger_Source.Attach (Current, Result);
   Check (Result = Debugger_Source.Attached, "test double attaches");
   Check (Current.Target_Name = "test double", "the test double names itself");
   Arrange (View, 1920, 1040);
   Absorb (View, Current);
   Check (Current_Line_Text (Current) = "      Checkpoint (""advance"");", "current line text");

   --  Locals: expand Q, collapse it.
   View.Focus := Locals_Pane;
   View.Locals.Selected := 1;
   Debugger_View.Key (View, Current, KEY_RIGHT, 0, Dirty, Req);
   Check (Req.Cmd = Load_Children and then Req.Local = 1, "Right on an unloaded record asks for its children");
   Debugger_Source.Load_Children (Current, Req.Local);
   Absorb (View, Current);
   Check (View.Locals.Visible_Total = Current.Local_Total and then Current.Locals (2).Depth = 1, "children shown under Q");
   Debugger_View.Key (View, Current, KEY_LEFT, 0, Dirty, Req);
   Check (View.Locals.Visible_Total = Current.Local_Total - 2, "Left collapses Q");
   Debugger_View.Key (View, Current, KEY_RIGHT, 0, Dirty, Req);
   Check (View.Locals.Visible_Total = Current.Local_Total, "Right opens Q again without a read");

   --  Type-ahead: "c" jumps to Count; "g" is the graph command, not a jump.
   View.Focus := Locals_Pane;
   Debugger_View.Text_Key (View, Current, 'c', Dirty, Req);
   Check (Current.Locals (View.Locals.Selected).Name = "Count", "typing c selects Count");
   Debugger_View.Text_Key (View, Current, 'g', Dirty, Req);
   Check (Current.Locals (View.Locals.Selected).Name = "Count" and then Req.Cmd = No_Command,
          "command letters are not type-ahead");
   View.Focus := Stack_Pane;
   View.Frame_Row := 1;
   Debugger_View.Text_Key (View, Current, 'r', Dirty, Req);
   Check (Req.Cmd = Select_Frame and then Req.Frame = 2, "typing r in the stack selects Run");
   View.Focus := Locals_Pane;
   View.Locals.Selected := 1;
   Debugger_View.Text_Key (View, Current, 'q', Dirty, Req);
   Check (View.Locals.Selected = 1, "a new letter matching the current row stays on it");

   --  Tab cycles focus.
   View.Focus := Memory_Pane;
   Dirty := (others => 0);
   Debugger_View.Key (View, Current, KEY_TAB, 0, Dirty, Req);
   Check (View.Focus = Threads_Pane and then not Is_Empty (Dirty), "Tab wraps to Threads and damages both panes");
   Debugger_View.Key (View, Current, KEY_TAB, MOD_SHIFT, Dirty, Req);
   Check (View.Focus = Memory_Pane, "Shift+Tab goes back");

   --  F fills the window with the focused pane, and back.
   View.Focus := Graph_Pane;
   Debugger_View.Key (View, Current, KEY_F, 0, Dirty, Req);
   Check (Pane_Rect (View, Graph_Pane).w = 1920 and then Pane_Rect (View, Source_Pane).w = 0,
          "F maximizes the graph pane");
   Debugger_View.Key (View, Current, KEY_F, 0, Dirty, Req);
   Check (Pane_Rect (View, Source_Pane).w > 0, "F again restores the panes");
   View.Focus := Memory_Pane;

   --  F10 is greyed: no request.
   Debugger_View.Key (View, Current, KEY_F10, 0, Dirty, Req);
   Check (Req.Cmd = No_Command, "F10 does nothing while stepping is not built");
   Debugger_View.Key (View, Current, KEY_F5, 0, Dirty, Req);
   Check (Req.Cmd = Continue, "F5 continues");

   --  Graph: stable layout across a read, new node placed clear of the rest.
   declare
      Before_Boxes : array (1 .. Current.Node_Total) of GL.Box;
      Stable : Boolean := True;
      Clear : Boolean := True;
   begin
      for N in Before_Boxes'Range loop
         Before_Boxes (N) := GL.Box_Of (View.Graph.Places, N);
      end loop;
      Debugger_State.Before := Current;
      Debugger_Source.Continue (Debugger_State.Before, Current);
      Mark_Changes (Debugger_State.Before, Current, Now_Us);
      Absorb (View, Current);
      for N in Before_Boxes'Range loop
         Stable := Stable and then GL.Box_Of (View.Graph.Places, N).X = Before_Boxes (N).X
           and then GL.Box_Of (View.Graph.Places, N).Y = Before_Boxes (N).Y;
      end loop;
      Check (Stable, "graph nodes keep their places across a read");
      Check (Recently (Current.Nodes (1).Fields (3).Changed_At, Now_Us), "a changed field is marked");
      Check (Recently (Current.Locals (3).Changed_At, Now_Us) or else Current.Locals (3).Name /= "Count",
             "a changed local is marked");
      Debugger_Source.Follow (Current, 3, 4);
      Absorb (View, Current);
      Check (Current.Node_Total = 4, "following a pointer adds its node");
      for A in 1 .. Current.Node_Total loop
         for B in A + 1 .. Current.Node_Total loop
            Clear := Clear and then not GL.Overlaps (GL.Box_Of (View.Graph.Places, A), GL.Box_Of (View.Graph.Places, B), 0);
         end loop;
      end loop;
      Check (Clear, "no two nodes overlap");
      Check ((for some E in 1 .. Current.Edge_Total => Current.Edges (E).Back), "the cycle is a back edge");
   end;

   --  Pictures and frame times, 1080p (density 1) and 4K (density 2).
   for Config in 1 .. 3 loop
      declare
         Density : constant Positive := (if Config = 3 then 2 else 1);
         W : constant Natural := (if Config = 1 then 1920 else 3840);
         H : constant Natural := (if Config = 1 then 1040 else 2080);
         Pixels : constant Pixel_Access := new Pixel_Buffer (0 .. W * H - 1);
         C : constant Canvas := Make_Canvas (Pixels, W, H, Density);
         Tag : constant String := (case Config is when 1 => "1080p", when 2 => "4k", when others => "4k-density2");
      begin
         Pixels.all := [others => 0];
         Arrange (View, W / Density, H / Density);
         Absorb (View, Current);
         Time_Render (C, Everything (View), Rounds, Best, Worst);
         Show_Time (Tag & " full frame", Best, Worst);
         Time_Render (C, Pane_Rect (View, Graph_Pane), Rounds, Best, Worst);
         Show_Time (Tag & " graph pane", Best, Worst);
         Time_Render (C, Pane_Rect (View, Locals_Pane), Rounds, Best, Worst);
         Show_Time (Tag & " locals pane", Best, Worst);
         Time_Render (C, Pane_Rect (View, Source_Pane), Rounds, Best, Worst);
         Show_Time (Tag & " source pane", Best, Worst);
         Render (C, Everything (View), CuBit.UI.CuBit_Alloy, Current, View, Now_Us);
         Write_PPM ("build/debugger-" & Tag & ".ppm", Pixels.all, W, H);
         Render (C, Everything (View), CuBit.UI.CuBit_Alloy_Dark, Current, View, Now_Us);
         Write_PPM ("build/debugger-" & Tag & "-dark.ppm", Pixels.all, W, H);
         Render (C, Everything (View), CuBit.UI.CuBit_Alloy, Current, View, Now_Us);
         --  The current line's band is drawn.
         declare
            Src : constant Rect := Pane_Rect (View, Source_Pane);
            Seen : Boolean := False;
         begin
            for Y in Src.y * Density .. (Src.y + Src.h) * Density - 1 loop
               Seen := Seen or else Pixels (Y * W + (Src.x + Src.w / 2) * Density) = 16#233A2E#;
            end loop;
            Check (Seen, Tag & ": the current line is highlighted");
         end;
      end;
   end loop;

   --  The graph budget (docs/debugging.md 9): 500 visible nodes.
   declare
      W : constant := 1920;
      H : constant := 1040;
      Pixels : constant Pixel_Access := new Pixel_Buffer (0 .. W * H - 1);
      C : constant Canvas := Make_Canvas (Pixels, W, H, 1);
      Index : Natural;
      COLUMNS : constant := 25;
   begin
      Current.Node_Total := 0;
      Current.Edge_Total := 0;
      for K in 1 .. 500 loop
         declare
            N : Graph_Node;
         begin
            N.Id := Node_Id (K);
            N.Title := To_Text ("Job");
            N.Field_Total := 3;
            N.Fields (1) := (Name => To_Short ("Id"), Value => To_Short (Natural'Image (K)), others => <>);
            N.Fields (2) := (Name => To_Short ("Level"), Value => To_Short ("Normal"), others => <>);
            N.Fields (3) := (Name => To_Short ("Next"), Value => To_Short ("0x70001040"), Is_Pointer => True,
                             Pointee => 1, Target => Node_Id (K + 1), others => <>);
            Add_Node (Current, N, Index);
         end;
      end loop;
      for K in 1 .. 499 loop
         if K mod COLUMNS /= 0 then
            Add_Edge (Current, (From => K, From_Field => 3, To => K + 1, Back => False));
         end if;
      end loop;
      Arrange (View, W, H);
      Debugger_Graph.Views.Relayout (View.Graph, Current, Pane_Rect (View, Graph_Pane));
      --  Smallest zoom, so all of it is in view.
      View.Graph.Zoom := GL.Zoom_Step'First;
      View.Graph.Pan_X := 0;
      View.Graph.Pan_Y := 0;
      Time_Render (C, Pane_Rect (View, Graph_Pane), Rounds, Best, Worst);
      Show_Time ("500-node graph pane (summary zoom)", Best, Worst);
      View.Graph.Zoom := 3;
      Time_Render (C, Pane_Rect (View, Graph_Pane), Rounds, Best, Worst);
      Show_Time ("500-node graph pane (75% zoom, text)", Best, Worst);
      Write_PPM ("build/debugger-500.ppm", Pixels.all, W, H);
   end;

   if Failures = 0 then
      Put_Line ("debugger-gui: all checks passed");
   else
      Put_Line ("debugger-gui:" & Natural'Image (Failures) & " FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Debugger_Gui_Tests;
