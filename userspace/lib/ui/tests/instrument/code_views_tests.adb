------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CuBit.UI.Code_Views: markers in the gutter, coloured runs from the
--  caller, annotations, following and scrolling.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Code_Views; use CuBit.UI.Code_Views;
with CuBit.UI.Code_Views.Lines;
with CuBit.UI.Instrument;

procedure Code_Views_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS " else "FAIL ") & What);
      if not Condition then Failures := Failures + 1; end if;
   end Check;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;

   type Source is record
      Current, Stop : Natural := 0;
   end record;
   TEXT : constant array (1 .. 40) of String (1 .. 12) := [others => "x := y + 1; "];
   function Line_Total (M : Source) return Natural is (TEXT'Length);
   function Line (M : Source; N : Positive) return String is (TEXT (N));
   function Marker (M : Source; N : Positive) return Line_Marker is
     (if N = M.Current then Current_Line elsif N = M.Stop then Breakpoint else No_Marker);
   procedure Colorize (M : Source; N : Positive; Text : String;
                       Paint : not null access procedure (First, Last : Positive; Style : Token_Style)) is
   begin
      Paint (Text'First + 3, Text'First + 3, Keyword);   --  ":"
      Paint (Text'First + 9, Text'First + 9, Number);    --  "1"
   end Colorize;
   function Annotation (M : Source; N : Positive) return String is (if N = M.Current then "y = 41" else "");
   function Annotation_Marked (M : Source; N : Positive) return Boolean is (True);
   package Views is new CuBit.UI.Code_Views.Lines
     (Source, Line_Total, Line, Marker, Colorize, Annotation, Annotation_Marked);

   W : constant := 600;
   H : constant := 300;
   type Pixels is array (0 .. W * H - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels'(others => 0);
   C : constant Canvas := (addr => Buffer.all'Address, width => W, height => H, pitch => W * 4, others => <>);
   Area : constant Rect := (0, 0, W, H);
   M : constant Source := (Current => 20, Stop => 22);
   V : View;
   function Has (Col : Color) return Boolean is (for some X of Buffer.all => X = Col);
begin
   Show (V, M.Current, Area);
   Check (V.Top < M.Current and then M.Current <= V.Top + Rows_In (Area), "following keeps the current line in view");
   Views.Draw (C, Area, M, V, P);
   Check (Has (P.Current_Line) and then Has (P.Current_Mark), "the current line is banded and marked");
   Check (Has (P.Danger), "a breakpoint shows in the gutter");
   Check (Has (P.Changed_Fill), "a marked annotation is drawn");
   Scroll (V, -1_000, Line_Total (M));
   Check (V.Top = 0 and then not V.Follow, "scrolling stops at the top and stops following");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   Put_Line ("code views:" & (if Failures = 0 then " all checks passed" else Natural'Image (Failures) & " FAILED"));
end Code_Views_Tests;
