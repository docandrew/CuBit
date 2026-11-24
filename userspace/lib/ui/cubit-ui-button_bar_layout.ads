------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Where a button bar's buttons go (CuBit.UI.Button_Bars): each button
--  keeps the width its caption needs, clusters are kept together where they
--  fit, and a bar too narrow for one row wraps onto more rows. Nothing is
--  ever narrowed, truncated or hidden.
--
--  Proved (tests/ui-keys, level 2): every button keeps its width; a button
--  ends inside the bar unless it is the first of its row (a button wider
--  than the whole bar); buttons on a row are in order and never overlap,
--  with at least Gap between them; rows go down in order.
------------------------------------------------------------------------------
package CuBit.UI.Button_Bar_Layout with SPARK_Mode is
   MAXIMUM_COORDINATE : constant := 1_000_000;
   subtype Coordinate is Natural range 0 .. MAXIMUM_COORDINATE;
   --  A bar's buttons: a named ceiling (a keyboard's function keys and a
   --  toolbar fit with room to spare).
   MAXIMUM_BUTTONS : constant := 32;
   subtype Button_Count is Natural range 0 .. MAXIMUM_BUTTONS;
   subtype Button_Index is Button_Count range 1 .. MAXIMUM_BUTTONS;
   subtype Row_Number is Natural range 0 .. MAXIMUM_BUTTONS;

   type Width_Table is array (Button_Index) of Coordinate;
   --  A cluster starts at this button (a wider gap before it, F5 after F4).
   type Start_Table is array (Button_Index) of Boolean;
   type Place is record
      Row : Row_Number := 0;
      X : Coordinate := 0;   --  from the bar's left edge
   end record;
   type Place_Table is array (Button_Index) of Place;

   function Ends (P : Place; Width : Coordinate) return Natural is (P.X + Width);

   procedure Arrange
     (Widths : Width_Table; Starts : Start_Table; Count : Button_Count;
      Available, Gap, Cluster_Gap : Coordinate;
      Places : out Place_Table; Rows : out Row_Number)
   with Post =>
     (if Count = 0 then Rows = 0 else Rows in 1 .. Count)
     and then (for all K in 1 .. Count =>
                 Places (K).Row < Rows
                 and then (Ends (Places (K), Widths (K)) <= Available
                           or else Places (K).X = 0))
     and then (for all K in 2 .. Count =>
                 Places (K).Row in Places (K - 1).Row .. Places (K - 1).Row + 1
                 and then (if Places (K).Row = Places (K - 1).Row then
                             Places (K).X >= Ends (Places (K - 1), Widths (K - 1)) + Gap
                           else Places (K).X = 0))
     and then (if Count > 0 then Places (1) = (Row => 0, X => 0));

end CuBit.UI.Button_Bar_Layout;
