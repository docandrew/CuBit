------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a dashboard's panels go on its 12-column grid (MET-002's app):
--  skyline packing, each panel in order at the lowest place its width
--  fits, leftmost on a tie. The window lays panels out by it, and a new
--  panel is put where it takes the first free place without moving any
--  other.
------------------------------------------------------------------------------
pragma Ada_2022;
with Dash_Model;

package Dash_Grid is
   subtype Column_Index is Natural range 0 .. Dash_Model.Grid_Columns - 1;
   --  A row is a grid unit from the top; a dashboard is at most every
   --  panel stacked at its tallest.
   Maximum_Rows : constant := Dash_Model.Maximum_Panels * Dash_Model.Maximum_Grid_Height;
   subtype Row_Count is Natural range 0 .. Maximum_Rows;

   type Placement is record
      Column : Column_Index := 0;
      Row : Row_Count := 0;
   end record;
   type Placements is array (Dash_Model.Panel_Index) of Placement;

   --  The panels' sizes in order (packing needs nothing else).
   type Size is record
      Width : Dash_Model.Grid_Width := Dash_Model.Grid_Columns;
      Height : Dash_Model.Grid_Height := 1;
   end record;
   type Size_List is array (Dash_Model.Panel_Index) of Size;
   function Sizes_Of (D : Dash_Model.Dashboard) return Size_List;

   --  The first Count panels' places, and the rows they take in all.
   procedure Pack (Sizes : Size_List; Count : Dash_Model.Panel_Count; Placed : out Placements; Rows : out Row_Count);

   --  Where a Width by Height panel goes in D's order: the index (from 1
   --  to D.Panels_Used + 1) at which inserting it leaves every other
   --  panel where it is and puts it highest, then leftmost. Appending
   --  always qualifies, so there is always one.
   procedure First_Free
     (D : Dash_Model.Dashboard; Width : Dash_Model.Grid_Width; Height : Dash_Model.Grid_Height;
      At_Index : out Dash_Model.Panel_Index; Where : out Placement)
     with Pre => D.Panels_Used < Dash_Model.Maximum_Panels;
end Dash_Grid;
