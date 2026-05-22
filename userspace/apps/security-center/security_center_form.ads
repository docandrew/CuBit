------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Static form metadata for Security Center
------------------------------------------------------------------------------
with CuBit.UI.Widgets;

package Security_Center_Form is
   subtype Tab_Title is CuBit.UI.Widgets.Tab_Title;

   TAB_OVERVIEW : aliased constant Tab_Title := "Overview";
   TAB_FILES     : aliased constant Tab_Title := "Files";
   TAB_CAPS      : aliased constant Tab_Title := "Caps";
   TAB_IPC       : aliased constant Tab_Title := "IPC";
   TAB_STREAMS   : aliased constant Tab_Title := "Streams";
   TAB_LAUNCH    : aliased constant Tab_Title := "Launch";

   TAB_LABELS : constant CuBit.UI.Widgets.Tab_Title_List :=
     (TAB_OVERVIEW'Access,
      TAB_FILES'Access,
      TAB_CAPS'Access,
      TAB_IPC'Access,
      TAB_STREAMS'Access,
      TAB_LAUNCH'Access);
end Security_Center_Form;
