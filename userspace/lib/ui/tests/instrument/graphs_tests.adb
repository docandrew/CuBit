------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CuBit.UI.Graphs over a small stream-wiring model (a source, a filter and
--  a sink, with in-ports): layout, stability, pins, wrap, fit, clicks and
--  drawing. Writes build/graphs.ppm.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Graphs; use CuBit.UI.Graphs;
with CuBit.UI.Graphs.Views;
with CuBit.UI.Instrument;

procedure Graphs_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS " else "FAIL ") & What);
      if not Condition then Failures := Failures + 1; end if;
   end Check;

   --  The model: stream stages. Row 1 is the input (in-port), row 2 the
   --  output (out-port) of each stage.
   MAX : constant := 8;
   type Stage is record
      Name : String (1 .. 8) := [others => ' '];
      Rate : Natural := 0;
      Pinned : Boolean := False;
      Changed : Boolean := False;
   end record;
   type Stages is array (1 .. MAX) of Stage;
   type Links is array (1 .. MAX) of Edge;
   type Wiring is record
      S : Stages;
      Count : Natural := 0;
      L : Links;
      Link_Count : Natural := 0;
   end record;

   function Node_Total (M : Wiring) return Node_Count is (M.Count);
   function Key_Of (M : Wiring; N : Node_Index) return Node_Key is (Node_Key (N) * 1_000 + Node_Key (M.Count * 0));
   function Title (M : Wiring; N : Node_Index) return String is (M.S (N).Name);
   function Detail (M : Wiring; N : Node_Index) return String is
     (Natural'Image (M.S (N).Rate) & "/s");
   function Style (M : Wiring; N : Node_Index) return Node_Style is (Plain_Style);
   function Row_Total (M : Wiring; N : Node_Index) return Row_Count is (2);
   function More_Rows (M : Wiring; N : Node_Index) return Natural is (0);
   function Row_Name (M : Wiring; N : Node_Index; R : Row_Index) return String is
     (if R = 1 then "in" else "out");
   function Row_Value (M : Wiring; N : Node_Index; R : Row_Index) return String is
     (if R = 1 then "Sample" else "Sample");
   function Port (M : Wiring; N : Node_Index; R : Row_Index) return Port_State is
     (if R = 1 then In_Port
      elsif (for some E in 1 .. M.Link_Count => M.L (E).From = N) then Linked_Port else Open_Port);
   function Row_Marked (M : Wiring; N : Node_Index; R : Row_Index) return Boolean is
     (R = 2 and then M.S (N).Changed);
   function Node_Marked (M : Wiring; N : Node_Index) return Boolean is (False);
   function Is_Pinned (M : Wiring; N : Node_Index) return Boolean is (M.S (N).Pinned);
   function Edge_Total (M : Wiring) return Edge_Count is (M.Link_Count);
   function Edge_At (M : Wiring; E : Edge_Index) return Edge is (M.L (E));
   function Check_Link (M : Wiring; Proposal : Connection) return Boolean is (Proposal.To_Row = 1);

   package Views is new CuBit.UI.Graphs.Views
     (Wiring, Node_Total, Key_Of, Title, Detail, Style, Row_Total, More_Rows, Row_Name, Row_Value,
      Port, Row_Marked, Node_Marked, Is_Pinned, Edge_Total, Edge_At, Check_Link);

   M : Wiring;
   V : Views.View;
   W : constant := 960;
   H : constant := 540;
   Area : constant Rect := (0, 0, W, H);
   type Pixels is array (0 .. W * H - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels'(others => 0);
   C : constant Canvas := (addr => Buffer.all'Address, width => W, height => H, pitch => W * 4, others => <>);

   procedure Add (Name : String; Rate : Natural) is
   begin
      M.Count := M.Count + 1;
      M.S (M.Count).Name (1 .. Name'Length) := Name;
      M.S (M.Count).Rate := Rate;
   end Add;
   procedure Link (From, To : Positive) is
   begin
      M.Link_Count := M.Link_Count + 1;
      M.L (M.Link_Count) := (From => From, From_Row => 2, To => To, To_Row => 1, Back => False);
   end Link;

   procedure Write_PPM (Name : String) is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
      type Bytes_Access is access Stream_Element_Array;
      RGB : constant Bytes_Access := new Stream_Element_Array (1 .. W * H * 3);
      At_Byte : Stream_Element_Offset := 1;
   begin
      for P of Buffer.all loop
         RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
         RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
         RGB (At_Byte + 2) := Stream_Element (P and 255);
         At_Byte := At_Byte + 3;
      end loop;
      IO.Create (F, IO.Out_File, Name);
      String'Write (IO.Stream (F), "P6" & ASCII.LF & "960 540" & ASCII.LF & "255" & ASCII.LF);
      IO.Write (F, RGB.all);
      IO.Close (F);
   end Write_PPM;

   First_Boxes : array (1 .. 3) of Box;
   Changed : Boolean;
   Clicked : Views.Hit;
   use type Views.Hit_Kind;
begin
   Add ("mic", 48_000);
   Add ("gain", 48_000);
   Add ("speaker", 48_000);
   Link (1, 2);
   Link (2, 3);
   Views.Update (V, M, Area);
   for N in 1 .. 3 loop
      First_Boxes (N) := Box_Of (V.Places, N);
   end loop;
   Check (First_Boxes (2).X > First_Boxes (1).X + First_Boxes (1).W
          and then First_Boxes (3).X > First_Boxes (2).X + First_Boxes (2).W,
          "a chain is laid out left to right");
   --  A new stage: the old ones stay put.
   Add ("meter", 10);
   Link (2, 4);
   Views.Update (V, M, Area);
   Check ((for all N in 1 .. 3 => Box_Of (V.Places, N) = First_Boxes (N)), "known nodes keep their places");
   Check ((for all A in 1 .. 4 =>
             (for all B in A + 1 .. 4 => not Overlaps (Box_Of (V.Places, A), Box_Of (V.Places, B), 0))),
          "no two nodes overlap");
   --  A pinned node survives a relayout.
   Pin (V.Places, 4, 700, 400);
   M.S (4).Pinned := True;
   Views.Relayout (V, M, Area);
   Check (Box_Of (V.Places, 4).X = 700 and then Box_Of (V.Places, 4).Y = 400, "a pinned node stays put");
   --  Wrap: a narrow area puts the third stage below the second.
   declare
      Narrow : constant Rect := (0, 0, 420, 900);
      V2 : Views.View;
   begin
      M.S (4).Pinned := False;
      Views.Update (V2, M, Narrow);
      Check (Box_Of (V2.Places, 3).Y > Box_Of (V2.Places, 2).Y, "past the wrap width a node goes below");
   end;
   --  Fit, draw, and a click on an open out-port.
   Views.Fit (V, M, Area);
   Check (V.Fitted and then V.Zoom >= Views.FIT_ZOOM_MIN, "fit chooses a readable zoom");
   M.S (2).Changed := True;
   Views.Draw (C, Area, M, V, CuBit.UI.Instrument.Instrument);
   Check ((for some P of Buffer.all => P = CuBit.UI.Instrument.Instrument.Edge), "edges are drawn");
   Check ((for some P of Buffer.all => P = CuBit.UI.Instrument.Instrument.Changed_Fill), "a changed row is marked");
   Write_PPM ("build/graphs.ppm");
   V.Zoom := UNIT_ZOOM;
   V.Pan_X := 0;
   V.Pan_Y := 0;
   declare
      B : constant Box := Box_Of (V.Places, 3);
   begin
      Views.Pointer_Down (V, B.X + 20, B.Y + Row_Center (2));
      Views.Pointer_Up (V, M, Area, B.X + 20, B.Y + Row_Center (2), Clicked, Changed);
      Check (Clicked.Kind = Views.Hit_Port and then Clicked.Node = 3 and then Clicked.Row = 2,
             "a click on an open out-port asks to follow it");
      Views.Pointer_Down (V, 5, 5);
      Views.Pointer_Move (V, 105, 55, Changed);
      Check (Changed and then V.Pan_X = -100 and then V.Pan_Y = -50, "dragging pans");
   end;
   if Failures > 0 then
      Put_Line ("graphs:" & Natural'Image (Failures) & " FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("graphs: all checks passed");
   end if;
end Graphs_Tests;
