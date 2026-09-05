------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tree controls
------------------------------------------------------------------------------
with CuBit.UI.Controls;
with CuBit.UI.State;
with Interfaces; use Interfaces;

package CuBit.UI.Trees is
   TREE_ROW_HEIGHT : constant Positive := 24;
   TREE_INDENT     : constant Positive := 18;

   type Tree_Item_Icon is
     (No_Icon, Computer_Icon, Bus_Icon, Device_Icon, Input_Icon,
      Storage_Icon, Network_Icon, Display_Icon, Audio_Icon,
      Service_Icon, Warning_Icon);

   procedure View_Frame
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       focused : Boolean;
       content : out CuBit.UI.Rect);

   procedure Tree_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       itemIndex : Natural;
       selectedIndex : in out Natural;
       depth : Natural := 0;
       expanded : Boolean := False;
       hasChildren : Boolean := False;
       icon : Tree_Item_Icon := No_Icon;
       focused : Boolean := True;
       lastSibling : Boolean := False;
       ancestorBranches : Unsigned_64 := 0;
       result : out CuBit.UI.Widget_Result);
end CuBit.UI.Trees;
