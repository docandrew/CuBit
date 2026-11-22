------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Parts side by side with draggable dividers between them (file panes,
--  editor splits). The application keeps the weights; Track lays the parts
--  out, registers each divider as a retained horizontal drag and applies a
--  drag before anything is drawn (Client_Split_Layout, proved).
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with Client_Split_Layout;

package CuBit.UI.Splits is
   MAXIMUM_PARTS : constant := Client_Split_Layout.MAXIMUM_PARTS;
   subtype Part_Count is Client_Split_Layout.Part_Count;
   subtype Part_Index is Client_Split_Layout.Part_Index;
   subtype Weights is Client_Split_Layout.Lengths;
   type Rect_Table is array (Part_Index) of Rect;

   DIVIDER_WIDTH : constant := 6;

   --  Count parts across Area: Parts (I) and Dividers (I) (after part I).
   --  Registers Base + I - 1 for divider I. After a drag, Shares holds the
   --  parts' lengths, so the split survives resizes proportionally.
   procedure Track
     (Map : in out Controls.Control_Map; Area : Rect; Count : Part_Count; Shares : in out Weights;
      Minimum : Natural; Base : Controls.Control_ID; Parts, Dividers : out Rect_Table);
   procedure Draw_Dividers
     (C : Canvas; Map : Controls.Control_Map; Count : Part_Count; Dividers : Rect_Table; Colors : Theme;
      Base : Controls.Control_ID; Hot_X, Hot_Y : Natural; Hover : Boolean);
end CuBit.UI.Splits;
