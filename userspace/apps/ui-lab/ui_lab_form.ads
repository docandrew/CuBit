------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Static form metadata for UI Lab
------------------------------------------------------------------------------
with CuBit.UI.Widgets;

package UI_Lab_Form is
   subtype Tab_Title is CuBit.UI.Widgets.Tab_Title;

   TAB_BASICS   : aliased constant Tab_Title := "Basics";
   TAB_DATA     : aliased constant Tab_Title := "Data";
   TAB_APPS     : aliased constant Tab_Title := "Apps";
   TAB_COMMANDS : aliased constant Tab_Title := "Commands";

   TAB_LABELS : constant CuBit.UI.Widgets.Tab_Title_List :=
     (TAB_BASICS'Access,
      TAB_DATA'Access,
      TAB_APPS'Access,
      TAB_COMMANDS'Access);
end UI_Lab_Form;
