------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Table controls
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.State;

package CuBit.UI.Tables is
   --  Draw a three-column header and make both dividers draggable.  Layout is
   --  retained by the caller, while capture, damage, and cursor feedback are
   --  ordinary toolkit behavior.
   procedure Resizable_Header
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       firstDividerId, secondDividerId : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       c1, c2, c3 : String;
       layout : in out CuBit.UI.Table_Column_Layout;
       minimumFirst, minimumSecond, minimumThird : Natural := 32;
       retainedInput : Boolean := False);

   procedure Row
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       c1, c2, c3 : String;
       rowIndex : Natural;
       selectedIndex : in out Natural;
       result : out CuBit.UI.Widget_Result);

   --  Tables of up to MAX_COLUMNS columns. Every column but the last has a
   --  draggable right edge; the last takes the remaining width. With
   --  Sortable, a click on a header cell makes that column the sort key
   --  (ascending), and a second click reverses it. The caller sorts its rows.
   MAX_COLUMNS : constant := 8;
   subtype Column_Count is Natural range 0 .. MAX_COLUMNS;
   subtype Column_Index is Positive range 1 .. MAX_COLUMNS;
   type Column_Widths is array (Column_Index) of Natural;
   type Sort_Order is (Ascending, Descending);
   type Column_Layout is record
      Count : Column_Count := 0;
      Width : Column_Widths := [others => 96];
      Minimum : Column_Widths := [others => 32];
      Sortable : Boolean := False;
      --  0: rows in the caller's own order.
      Sort_Column : Column_Count := 0;
      Order : Sort_Order := Ascending;
      Cell_Padding : Natural := 5;
   end record;

   --  Control IDs from Base: Base .. Base + MAX_COLUMNS - 1 are the column
   --  edges, the next MAX_COLUMNS the header cells.
   RESERVED_IDS : constant := 2 * MAX_COLUMNS;
   subtype Column_ID_Base is CuBit.UI.Controls.Control_ID
     range 1 .. CuBit.UI.Controls.Control_ID'Last - RESERVED_IDS;

   --  Where column Column starts and how wide it is within Width pixels.
   function Column_Left (Layout : Column_Layout; Column : Column_Index; Width : Natural) return Natural
     with Pre => Column <= Layout.Count;
   function Column_Width (Layout : Column_Layout; Column : Column_Index; Width : Natural) return Natural
     with Pre => Column <= Layout.Count;

   procedure Toggle_Sort (Layout : in out Column_Layout; Column : Column_Index)
     with Pre => Column <= Layout.Count;
   --  From Handle_Event on pointer release over Target: a header click sorts.
   procedure Handle_Header_Release
     (Layout : in out Column_Layout;
      controls : in out CuBit.UI.Controls.Control_Map;
      Base : Column_ID_Base;
      Target : CuBit.UI.Controls.Control_ID;
      Changed : out Boolean);

   generic
      with function Title (Column : Column_Index) return String;
   procedure Columns_Header
      (c : CuBit.UI.Canvas;
       st : CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       Base : Column_ID_Base;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       Layout : in out Column_Layout);

   --  One row: Cell gives each column's text, Ink its color given the
   --  row's ordinary text color (selected rows pass the selection text).
   generic
      with function Cell (Column : Column_Index) return String;
      with function Ink (Column : Column_Index; Default : CuBit.UI.Color) return CuBit.UI.Color;
   procedure Draw_Columns_Row
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       Layout : Column_Layout;
       selected, hot : Boolean;
       textStyle : CuBit.UI.Table_Text_Style := CuBit.UI.Table_Interface_Text);
end CuBit.UI.Tables;
