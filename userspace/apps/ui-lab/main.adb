------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UI primitive exercise app
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Labels;
with CuBit.UI.Layout;
with CuBit.UI.Lists;
with CuBit.UI.State;
with CuBit.UI.Tables;
with CuBit.UI.Trees;
with CuBit.UI.Widgets;
with UI_Lab_Form;

procedure main is
   use ASCII;

   --  Keep UI Lab roomy enough for new controls while the app harness still
   --  uses a fixed backing buffer. True resize support belongs in CuBit.UI.App
   --  so apps can receive configure events and attach a correctly-sized buffer.
   bufferW : constant Natural := 720;
   bufferH : constant Natural := 500;

   win : CuBit.UI.App.Window;
   ignore : Unsigned_64;
   ui : CuBit.UI.State.UI_State;
   demoChecked : Boolean := True;
   demoValue : Natural := 42;
   clickCount : Natural := 0;
   CONTROL_ACTION : constant CuBit.UI.Controls.Control_ID := 1;
   CONTROL_CHECKBOX : constant CuBit.UI.Controls.Control_ID := 2;
   CONTROL_SLIDER : constant CuBit.UI.Controls.Control_ID := 3;
   CONTROL_TEXT_FIELD : constant CuBit.UI.Controls.Control_ID := 4;
   CONTROL_RADIO_FAST : constant CuBit.UI.Controls.Control_ID := 5;
   CONTROL_RADIO_SAFE : constant CuBit.UI.Controls.Control_ID := 6;
   CONTROL_RADIO_PROOF : constant CuBit.UI.Controls.Control_ID := 7;
   CONTROL_LIST_DESKTOP : constant CuBit.UI.Controls.Control_ID := 8;
   CONTROL_LIST_SECURITY : constant CuBit.UI.Controls.Control_ID := 9;
   CONTROL_LIST_CONSOLE : constant CuBit.UI.Controls.Control_ID := 10;
   CONTROL_LIST_DOOM : constant CuBit.UI.Controls.Control_ID := 11;
   CONTROL_LIST_SCROLL : constant CuBit.UI.Controls.Control_ID := 12;
   CONTROL_MENU_BUTTON : constant CuBit.UI.Controls.Control_ID := 13;
   CONTROL_MENU_APPS : constant CuBit.UI.Controls.Control_ID := 14;
   CONTROL_MENU_SYSTEM : constant CuBit.UI.Controls.Control_ID := 15;
   CONTROL_MENU_LOCKED : constant CuBit.UI.Controls.Control_ID := 16;
   CONTROL_BAR_FILE : constant CuBit.UI.Controls.Control_ID := 17;
   CONTROL_BAR_VIEW : constant CuBit.UI.Controls.Control_ID := 18;
   CONTROL_BAR_HELP : constant CuBit.UI.Controls.Control_ID := 19;
   CONTROL_FILE_RUN : constant CuBit.UI.Controls.Control_ID := 20;
   CONTROL_FILE_EXIT : constant CuBit.UI.Controls.Control_ID := 21;
   CONTROL_STREAM_ROW1 : constant CuBit.UI.Controls.Control_ID := 22;
   CONTROL_STREAM_ROW2 : constant CuBit.UI.Controls.Control_ID := 23;
   CONTROL_STREAM_ROW3 : constant CuBit.UI.Controls.Control_ID := 24;
   CONTROL_TAB_BASE : constant CuBit.UI.Controls.Control_ID := 25;
   CONTROL_DATA_SPLIT : constant CuBit.UI.Controls.Control_ID := 29;
   CONTROL_COMMAND_SCROLL : constant CuBit.UI.Controls.Control_ID := 30;
   CONTROL_TREE_ROOT : constant CuBit.UI.Controls.Control_ID := 31;
   KEY_UP : constant Unsigned_64 := 16#48#;
   KEY_DOWN : constant Unsigned_64 := 16#50#;
   KEY_HOME : constant Unsigned_64 := 16#47#;
   KEY_END : constant Unsigned_64 := 16#4F#;

   controls : CuBit.UI.Controls.Control_Map;
   lastHoverControl : CuBit.UI.Controls.Control_ID :=
      CuBit.UI.Controls.NO_CONTROL;
   TEXT_MAX : constant Natural := 24;
   sampleText : String (1 .. TEXT_MAX) := "CuBit UI                ";
   sampleTextLen : Natural := 8;
   demoMode : Natural := 2;
   selectedApp : Natural := 1;
   listScroll : Natural := 0;
   listFocused : Boolean := False;
   menuOpen : Boolean := False;
   fileMenuOpen : Boolean := False;
   menuChoice : Natural := 0;
   selectedStream : Natural := 1;
   activeTab : Natural := 1;
   dataSplit : Natural := 420;
   commandScroll : Natural := 0;
   selectedTree : Natural := 1;
   APP_COUNT : constant Natural := 8;
   LIST_VISIBLE : constant Natural := 4;

   PANEL_RECT : constant CuBit.UI.Rect :=
      (x => 18, y => 18, w => bufferW - 36, h => bufferH - 36);
   HEADER_RECT : constant CuBit.UI.Rect :=
      (x => 18, y => 18, w => bufferW - 36, h => 34);

   type Lab_Layout is record
      content : CuBit.UI.Rect := (others => 0);
      menuBar : CuBit.UI.Rect := (others => 0);
      fileTitle : CuBit.UI.Rect := (others => 0);
      viewTitle : CuBit.UI.Rect := (others => 0);
      helpTitle : CuBit.UI.Rect := (others => 0);
      fileMenuBox : CuBit.UI.Rect := (others => 0);
      fileRun : CuBit.UI.Rect := (others => 0);
      fileExit : CuBit.UI.Rect := (others => 0);
      footer : CuBit.UI.Rect := (others => 0);
      statusBar : CuBit.UI.Rect := (others => 0);
      tabPage : CuBit.UI.Rect := (others => 0);
      buttonLabel : CuBit.UI.Rect := (others => 0);
      actionButton : CuBit.UI.Rect := (others => 0);
      disabledButton : CuBit.UI.Rect := (others => 0);
      controlsLabel : CuBit.UI.Rect := (others => 0);
      checkbox : CuBit.UI.Rect := (others => 0);
      slider : CuBit.UI.Rect := (others => 0);
      counter : CuBit.UI.Rect := (others => 0);
      textField : CuBit.UI.Rect := (others => 0);
      tableLabel : CuBit.UI.Rect := (others => 0);
      dataSplit : CuBit.UI.Rect := (others => 0);
      streamPane : CuBit.UI.Rect := (others => 0);
      streamHeader : CuBit.UI.Rect := (others => 0);
      streamRow1 : CuBit.UI.Rect := (others => 0);
      streamRow2 : CuBit.UI.Rect := (others => 0);
      streamRow3 : CuBit.UI.Rect := (others => 0);
      radioLabel : CuBit.UI.Rect := (others => 0);
      radioFast : CuBit.UI.Rect := (others => 0);
      radioSafe : CuBit.UI.Rect := (others => 0);
      radioProof : CuBit.UI.Rect := (others => 0);
      radioGroup : CuBit.UI.Rect := (others => 0);
      listLabel : CuBit.UI.Rect := (others => 0);
      listBox : CuBit.UI.Rect := (others => 0);
      listSlot1 : CuBit.UI.Rect := (others => 0);
      listSlot2 : CuBit.UI.Rect := (others => 0);
      listSlot3 : CuBit.UI.Rect := (others => 0);
      listSlot4 : CuBit.UI.Rect := (others => 0);
      listScrollBar : CuBit.UI.Rect := (others => 0);
      menuLabel : CuBit.UI.Rect := (others => 0);
      menuButton : CuBit.UI.Rect := (others => 0);
      menuBox : CuBit.UI.Rect := (others => 0);
      menuApps : CuBit.UI.Rect := (others => 0);
      menuSystem : CuBit.UI.Rect := (others => 0);
      menuLocked : CuBit.UI.Rect := (others => 0);
      commandScrollArea : CuBit.UI.Rect := (others => 0);
   end record;

   function unpackLo32 (x : Unsigned_64) return Natural is
   begin
      return Natural (x and 16#FFFF_FFFF#);
   end unpackLo32;

   function unpackHi32 (x : Unsigned_64) return Natural is
   begin
      return Natural (Shift_Right (x, 32));
   end unpackHi32;

   function unpackSignedLo32 (x : Unsigned_64) return Integer is
      v : constant Unsigned_64 := x and 16#FFFF_FFFF#;
   begin
      if (v and 16#8000_0000#) /= 0 then
         return Integer (Integer_64 (v) - 16#1_0000_0000#);
      else
         return Integer (v);
      end if;
   end unpackSignedLo32;

   function appLabel (idx : Natural) return String is
   begin
      case idx is
         when 1 => return "Desktop";
         when 2 => return "Security Center";
         when 3 => return "CuBASIC";
         when 4 => return "DOOM";
         when 5 => return "UI Lab";
         when 6 => return "Mixer";
         when 7 => return "Trace Viewer";
         when others => return "Settings";
      end case;
   end appLabel;

   function isListControl (id : CuBit.UI.Controls.Control_ID) return Boolean is
   begin
      return id = CONTROL_LIST_DESKTOP or else
             id = CONTROL_LIST_SECURITY or else
             id = CONTROL_LIST_CONSOLE or else
             id = CONTROL_LIST_DOOM or else
             id = CONTROL_LIST_SCROLL;
   end isListControl;

   function isMenuControl (id : CuBit.UI.Controls.Control_ID) return Boolean is
   begin
      return id = CONTROL_MENU_BUTTON or else
             id = CONTROL_MENU_APPS or else
             id = CONTROL_MENU_SYSTEM or else
             id = CONTROL_MENU_LOCKED;
   end isMenuControl;

   function isFileMenuControl
      (id : CuBit.UI.Controls.Control_ID) return Boolean
   is
   begin
      return id = CONTROL_BAR_FILE or else
             id = CONTROL_FILE_RUN or else
             id = CONTROL_FILE_EXIT;
   end isFileMenuControl;

   procedure ensureSelectedVisible is
   begin
      if selectedApp <= listScroll then
         listScroll := selectedApp - 1;
      elsif selectedApp > listScroll + LIST_VISIBLE then
         listScroll := selectedApp - LIST_VISIBLE;
      end if;

      if listScroll > APP_COUNT - LIST_VISIBLE then
         listScroll := APP_COUNT - LIST_VISIBLE;
      end if;
   end ensureSelectedVisible;

   procedure scrollByWheel
      (value : in out Natural;
       maxValue : Natural;
       wheelDelta : Integer)
   is
      step : constant Natural := 1;
   begin
      if wheelDelta > 0 then
         if value > step then
            value := value - step;
         else
            value := 0;
         end if;
      elsif wheelDelta < 0 then
         value := Natural'Min (value + step, maxValue);
      end if;
   end scrollByWheel;

   function handleListKey (key : Unsigned_64) return Boolean is
   begin
      if not listFocused or else activeTab /= 3 then
         return False;
      end if;

      if key = KEY_UP then
         if selectedApp > 1 then
            selectedApp := selectedApp - 1;
         end if;
      elsif key = KEY_DOWN then
         if selectedApp < APP_COUNT then
            selectedApp := selectedApp + 1;
         end if;
      elsif key = KEY_HOME then
         selectedApp := 1;
      elsif key = KEY_END then
         selectedApp := APP_COUNT;
      else
         return False;
      end if;

      ensureSelectedVisible;
      return True;
   end handleListKey;

   function computeLayout return Lab_Layout is
      ret : Lab_Layout;
      l : CuBit.UI.Layout.Cursor;
   begin
      ret.menuBar :=
        (x => PANEL_RECT.x + 1, y => HEADER_RECT.y + HEADER_RECT.h,
         w => PANEL_RECT.w - 2, h => 24);
      ret.fileTitle :=
        (x => ret.menuBar.x + 4, y => ret.menuBar.y + 2, w => 48, h => 20);
      ret.viewTitle :=
        (x => ret.fileTitle.x + ret.fileTitle.w + 2,
         y => ret.menuBar.y + 2, w => 52, h => 20);
      ret.helpTitle :=
        (x => ret.viewTitle.x + ret.viewTitle.w + 2,
         y => ret.menuBar.y + 2, w => 52, h => 20);
      ret.fileMenuBox :=
        (x => ret.fileTitle.x, y => ret.menuBar.y + ret.menuBar.h,
         w => 196, h => 50);
      ret.fileRun :=
        (x => ret.fileMenuBox.x + 2, y => ret.fileMenuBox.y + 2,
         w => ret.fileMenuBox.w - 4, h => 22);
      ret.fileExit :=
        (x => ret.fileMenuBox.x + 2, y => ret.fileRun.y + 22,
         w => ret.fileMenuBox.w - 4, h => 22);
      declare
         frame : CuBit.UI.Layout.Dock_Frame :=
            CuBit.UI.Layout.Begin_Dock
              (CuBit.UI.Layout.Inset (PANEL_RECT, 1, 34, 1, 1));
      begin
         ret.menuBar := CuBit.UI.Layout.Dock_Top (frame, 24);
         ret.statusBar := CuBit.UI.Layout.Dock_Bottom (frame, 23);
      ret.content := CuBit.UI.Layout.Inset
           (CuBit.UI.Layout.Fill (frame), 15, 24, 15, 20);
      end;
      ret.tabPage :=
        (x => ret.content.x, y => ret.content.y + 30,
         w => ret.content.w,
         h => (if ret.content.h > 30 then ret.content.h - 30 else 0));
      ret.footer :=
        (x => ret.content.x, y => ret.content.y + ret.content.h - 16,
         w => ret.content.w, h => 16);

      l := CuBit.UI.Layout.Start (ret.tabPage, 18, 18);
      ret.buttonLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 1);
      ret.actionButton := CuBit.UI.Layout.Take (l, 108, 26);
      ret.disabledButton := CuBit.UI.Layout.Take (l, 108, 26);
      CuBit.UI.Layout.New_Row (l, 14);
      ret.controlsLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 6);
      ret.checkbox := CuBit.UI.Layout.Take (l, 20, 20);
      CuBit.UI.Layout.New_Row (l, 14);
      ret.slider := CuBit.UI.Layout.Take (l, 210, 24);
      CuBit.UI.Layout.New_Row (l, 10);
      ret.counter := CuBit.UI.Layout.Take (l, 160, 18);
      CuBit.UI.Layout.New_Row (l, 12);
      ret.textField := CuBit.UI.Layout.Take (l, 250, 24);
      CuBit.UI.Layout.New_Row (l, 14);
      ret.radioLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.radioFast := CuBit.UI.Layout.Take (l, 152, 20);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.radioSafe := CuBit.UI.Layout.Take (l, 152, 20);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.radioProof := CuBit.UI.Layout.Take (l, 152, 20);
      ret.radioGroup :=
        CuBit.UI.Inflate_Rect
          ((x => ret.radioFast.x,
            y => ret.radioFast.y,
            w => 172,
            h => ret.radioProof.y + ret.radioProof.h - ret.radioFast.y),
           2);

      l := CuBit.UI.Layout.Start (ret.tabPage, 18, 18);
      ret.tableLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.dataSplit := CuBit.UI.Layout.Take (l, ret.tabPage.w - 28, 170);
      ret.streamPane := ret.dataSplit;
      ret.streamHeader :=
        (x => ret.streamPane.x + 8, y => ret.streamPane.y + 18,
         w => ret.streamPane.w - 16, h => 22);
      ret.streamRow1 :=
        (x => ret.streamHeader.x, y => ret.streamHeader.y + 22,
         w => ret.streamHeader.w, h => 20);
      ret.streamRow2 :=
        (x => ret.streamHeader.x, y => ret.streamRow1.y + 20,
         w => ret.streamHeader.w, h => 20);
      ret.streamRow3 :=
        (x => ret.streamHeader.x, y => ret.streamRow2.y + 20,
         w => ret.streamHeader.w, h => 20);

      l := CuBit.UI.Layout.Start (ret.tabPage, 18, 18);
      ret.listLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.listBox := CuBit.UI.Layout.Take (l, 246, 118);
      ret.listScrollBar :=
        (x => ret.listBox.x + ret.listBox.w - 16,
         y => ret.listBox.y + 2,
         w => 14,
         h => ret.listBox.h - 4);
      ret.listSlot1 :=
        (x => ret.listBox.x + 2, y => ret.listBox.y + 2,
         w => ret.listBox.w - 20, h => 22);
      ret.listSlot2 :=
        (x => ret.listBox.x + 2, y => ret.listSlot1.y + 22,
         w => ret.listBox.w - 20, h => 22);
      ret.listSlot3 :=
        (x => ret.listBox.x + 2, y => ret.listSlot2.y + 22,
         w => ret.listBox.w - 20, h => 22);
      ret.listSlot4 :=
        (x => ret.listBox.x + 2, y => ret.listSlot3.y + 22,
         w => ret.listBox.w - 20, h => 22);

      l := CuBit.UI.Layout.Start (ret.tabPage, 18, 18);
      ret.menuLabel := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ret.menuButton := CuBit.UI.Layout.Take (l, 132, 28);
      CuBit.UI.Layout.New_Row (l, 2);
      ret.menuBox := CuBit.UI.Layout.Take (l, 190, 74);
      ret.menuApps :=
        (x => ret.menuBox.x + 2, y => ret.menuBox.y + 2,
         w => ret.menuBox.w - 4, h => 22);
      ret.menuSystem :=
        (x => ret.menuBox.x + 2, y => ret.menuApps.y + 22,
         w => ret.menuBox.w - 4, h => 22);
      ret.menuLocked :=
        (x => ret.menuBox.x + 2, y => ret.menuSystem.y + 22,
         w => ret.menuBox.w - 4, h => 22);
      CuBit.UI.Layout.New_Row (l, 12);
      ret.commandScrollArea := CuBit.UI.Layout.Take (l, 310, 86);
      return ret;
   end computeLayout;

   layout : constant Lab_Layout := computeLayout;

   function menuDamage return CuBit.UI.Rect is
   begin
      return CuBit.UI.Inflate_Rect
        (CuBit.UI.Union_Rect (layout.menuButton, layout.menuBox), 2);
   end menuDamage;

   function fileMenuDamage return CuBit.UI.Rect is
   begin
      return CuBit.UI.Inflate_Rect
        (CuBit.UI.Union_Rect (layout.menuBar, layout.fileMenuBox), 2);
   end fileMenuDamage;

   function tabDamage return CuBit.UI.Rect is
   begin
      return CuBit.UI.Inflate_Rect (layout.content, 2);
   end tabDamage;

   procedure drawLabel
      (c : CuBit.UI.Canvas; x, y : Natural; text : String)
   is
   begin
      CuBit.UI.Labels.Label
        (c,
         (x => x, y => y, w => CuBit.UI.UI_Text_Width (text),
          h => CuBit.UI.UI_Text_Height),
         CuBit.UI.Classic,
         text);
   end drawLabel;

   procedure markDirty
      (dirty : in out CuBit.UI.Rect; id : CuBit.UI.Controls.Control_ID)
   is
   begin
      CuBit.UI.Controls.Mark_Dirty (dirty, controls, id);
   end markDirty;

   procedure render
      (win : in out CuBit.UI.App.Window; damage : CuBit.UI.Rect)
   is
      c : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (win, damage);
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      actionButton : CuBit.UI.Widget_Result;
      checkBox : CuBit.UI.Widget_Result;
      slider : CuBit.UI.Widget_Result;
      textField : CuBit.UI.Widget_Result;
      radio : CuBit.UI.Widget_Result;
      listItem : CuBit.UI.Widget_Result;
      scrollBar : CuBit.UI.Widget_Result;
      menuTitle : CuBit.UI.Widget_Result;
      menuButton : CuBit.UI.Widget_Result;
      menuItem : CuBit.UI.Widget_Result;
      tableRow : CuBit.UI.Widget_Result;
      treeItem : CuBit.UI.Widget_Result;
      page : CuBit.UI.Rect;
      tabChanged : Boolean;
      dataLeft : CuBit.UI.Rect;
      dataRight : CuBit.UI.Rect;
      streamContent : CuBit.UI.Rect;
      detailContent : CuBit.UI.Rect;
      listContent : CuBit.UI.Rect;
      streamHeader : CuBit.UI.Rect;
      streamRow1 : CuBit.UI.Rect;
      streamRow2 : CuBit.UI.Rect;
      streamRow3 : CuBit.UI.Rect;
      commandViewport : CuBit.UI.Rect;
      commandOriginY : Natural;
      commandCanvas : CuBit.UI.Canvas;
      streamFrame : CuBit.UI.Layout.Container;
      detailFrame : CuBit.UI.Layout.Container;
      commandFrame : CuBit.UI.Layout.Container;
      labelRect : CuBit.UI.Rect;
      firstApp : constant Natural := listScroll + 1;
   begin
      CuBit.UI.Controls.Clear (controls);
      CuBit.UI.State.Begin_Frame (ui);
      CuBit.UI.State.Enter_Scope (ui);

      CuBit.UI.Fill_Rect (c, (x => 0, y => 0, w => bufferW, h => bufferH),
                          colors.desktop);
      CuBit.UI.Fill_Rect (c, PANEL_RECT, colors.panel);
      CuBit.UI.Stroke_Rect (c, PANEL_RECT, colors.edge, colors.shadow);
      CuBit.UI.Fill_Rect (c, HEADER_RECT, colors.face);
      CuBit.UI.Draw_UI_Text (c, 32, 27, "CuBit UI Lab",
                             colors.text,
                             colors.face);
      CuBit.UI.Draw_UI_Text (c, 438, 27, "SPAWN UI-LAB",
                             colors.accent,
                             colors.face);
      CuBit.UI.Draw_Menu_Bar (c, layout.menuBar, colors);
      CuBit.UI.Widgets.Menu_Title
        (c, ui, controls, CONTROL_BAR_FILE,
         layout.fileTitle, fileMenuDamage, colors,
         "File", fileMenuOpen, menuTitle);
      CuBit.UI.Widgets.Menu_Title
        (c, ui, controls, CONTROL_BAR_VIEW,
         layout.viewTitle, layout.menuBar, colors,
         "View", False, menuTitle);
      CuBit.UI.Widgets.Menu_Title
        (c, ui, controls, CONTROL_BAR_HELP,
         layout.helpTitle, layout.menuBar, colors,
         "Help", False, menuTitle);

      CuBit.UI.Widgets.Tab_Panel
        (c, ui, controls, CONTROL_TAB_BASE,
         layout.content, tabDamage, colors,
         UI_Lab_Form.TAB_LABELS, activeTab, page, tabChanged);

      if activeTab = 1 then
         drawLabel (c, layout.buttonLabel.x, layout.buttonLabel.y, "Button");
         CuBit.UI.Widgets.Button
           (c, ui, controls, CONTROL_ACTION,
            layout.actionButton,
            CuBit.UI.Union_Rect
              (CuBit.UI.Inflate_Rect (layout.actionButton, 4),
               layout.counter),
            colors, "Run", actionButton);
         if actionButton.activated then
            clickCount := clickCount + 1;
         end if;

         CuBit.UI.Widgets.Disabled_Button
           (c, layout.disabledButton, colors, "Disabled");

         drawLabel (c, layout.controlsLabel.x,
                    layout.controlsLabel.y, "Stateful controls");
         CuBit.UI.Widgets.Checkbox
           (c, ui, controls, CONTROL_CHECKBOX,
            layout.checkbox,
            CuBit.UI.Inflate_Rect
              ((x => layout.checkbox.x, y => layout.checkbox.y - 4,
                w => 148, h => 30), 2),
            colors, demoChecked, checkBox);
         CuBit.UI.Draw_UI_Text
           (c, layout.checkbox.x + 30, layout.checkbox.y + 2,
            "checkbox", colors.text, colors.face);

         CuBit.UI.Widgets.Horizontal_Slider
           (c, ui, controls, CONTROL_SLIDER,
            layout.slider,
            CuBit.UI.Inflate_Rect
              ((x => layout.slider.x, y => layout.slider.y - 4,
                w => 304, h => 34), 2),
            colors, 0, 100, demoValue, slider);
         CuBit.UI.Draw_UI_Text
           (c, layout.slider.x + 234, layout.slider.y + 2,
            "value", colors.muted, colors.face);
         CuBit.UI.Draw_Natural_Value
           (c, (x => layout.slider.x + 282, y => layout.slider.y + 2,
                w => 48, h => CuBit.UI.UI_Text_Height),
            colors, demoValue);

         CuBit.UI.Draw_UI_Text
           (c, layout.counter.x, layout.counter.y,
            "clicks", colors.muted, colors.face);
         CuBit.UI.Draw_Natural_Value
           (c, (x => layout.counter.x + 58, y => layout.counter.y,
                w => 48, h => CuBit.UI.UI_Text_Height),
            colors, clickCount);

         CuBit.UI.Widgets.Text_Field
           (c, ui, controls, CONTROL_TEXT_FIELD,
            layout.textField,
            CuBit.UI.Inflate_Rect (layout.textField, 4),
            colors,
            (if sampleTextLen = 0 then ""
             else sampleText (1 .. sampleTextLen)),
            textField);

         drawLabel (c, layout.radioLabel.x, layout.radioLabel.y, "Radio group");
         CuBit.UI.Widgets.Radio_Button
           (c, ui, controls, CONTROL_RADIO_FAST,
            layout.radioFast, layout.radioGroup, colors,
            "fast", 1, demoMode, radio);
         CuBit.UI.Widgets.Radio_Button
           (c, ui, controls, CONTROL_RADIO_SAFE,
            layout.radioSafe, layout.radioGroup, colors,
            "safe", 2, demoMode, radio);
         CuBit.UI.Widgets.Radio_Button
           (c, ui, controls, CONTROL_RADIO_PROOF,
            layout.radioProof, layout.radioGroup, colors,
            "proof", 3, demoMode, radio);
      elsif activeTab = 2 then
         drawLabel (c, layout.tableLabel.x, layout.tableLabel.y,
                    "Split pane + table");
         CuBit.UI.Widgets.Split_Pane
           (c, ui, controls, CONTROL_DATA_SPLIT,
            layout.dataSplit, tabDamage, colors,
            True, dataSplit, dataLeft, dataRight);
         CuBit.UI.Widgets.Group_Box
           (c, dataLeft, colors, "Streams", streamContent, 8);
         CuBit.UI.Widgets.Group_Box
           (c, dataRight, colors, "Inspector", detailContent, 8);
         streamFrame := CuBit.UI.Layout.Root (streamContent);
         detailFrame := CuBit.UI.Layout.Root (detailContent);
         streamHeader :=
            CuBit.UI.Layout.Resolve
              (streamFrame, (x => 0, y => 0, w => streamContent.w, h => 22));
         streamRow1 :=
            CuBit.UI.Layout.Resolve
              (streamFrame, (x => 0, y => 22, w => streamContent.w, h => 20));
         streamRow2 :=
            CuBit.UI.Layout.Resolve
              (streamFrame, (x => 0, y => 42, w => streamContent.w, h => 20));
         streamRow3 :=
            CuBit.UI.Layout.Resolve
              (streamFrame, (x => 0, y => 62, w => streamContent.w, h => 20));
         CuBit.UI.Draw_Table_Header
           (c, streamHeader, colors, "Name", "Type", "State");
         CuBit.UI.Tables.Row
           (c, ui, controls, CONTROL_STREAM_ROW1,
            streamRow1, dataLeft, colors,
            "audit", "log", "open", 1, selectedStream, tableRow);
         CuBit.UI.Tables.Row
           (c, ui, controls, CONTROL_STREAM_ROW2,
            streamRow2, dataLeft, colors,
            "metrics", "counter", "hot", 2, selectedStream, tableRow);
         CuBit.UI.Tables.Row
           (c, ui, controls, CONTROL_STREAM_ROW3,
            streamRow3, dataLeft, colors,
            "alerts", "event", "idle", 3, selectedStream, tableRow);
         labelRect :=
            CuBit.UI.Layout.Resolve
              (detailFrame,
               (x => 0, y => 0,
                w => detailContent.w, h => CuBit.UI.UI_Text_Height));
         CuBit.UI.Labels.Label
           (c, labelRect, colors, "Selected stream", muted => True);
         CuBit.UI.Draw_Natural_Value
           (c,
            CuBit.UI.Layout.Resolve
              (detailFrame,
               (x => 0, y => 22, w => 42, h => CuBit.UI.UI_Text_Height)),
            colors, selectedStream);
         CuBit.UI.Trees.Tree_Item
           (c, ui, controls, CONTROL_TREE_ROOT,
            CuBit.UI.Layout.Resolve
              (detailFrame,
               (x => 0, y => 50, w => detailContent.w, h => 22)),
            dataRight, colors,
            "session.streams", 1, selectedTree,
            depth => 0, expanded => True, hasChildren => True,
            result => treeItem);
      elsif activeTab = 3 then
         drawLabel (c, layout.listLabel.x, layout.listLabel.y, "List box");
         CuBit.UI.Widgets.Panel
           (c, layout.listBox, colors, listContent, 2);
         CuBit.UI.Lists.List_Item
           (c, ui, controls, CONTROL_LIST_DESKTOP,
            layout.listSlot1, layout.listBox, colors,
            appLabel (firstApp), firstApp, selectedApp, listItem);
         CuBit.UI.Lists.List_Item
           (c, ui, controls, CONTROL_LIST_SECURITY,
            layout.listSlot2, layout.listBox, colors,
            appLabel (firstApp + 1), firstApp + 1, selectedApp, listItem);
         CuBit.UI.Lists.List_Item
           (c, ui, controls, CONTROL_LIST_CONSOLE,
            layout.listSlot3, layout.listBox, colors,
            appLabel (firstApp + 2), firstApp + 2, selectedApp, listItem);
         CuBit.UI.Lists.List_Item
           (c, ui, controls, CONTROL_LIST_DOOM,
            layout.listSlot4, layout.listBox, colors,
            appLabel (firstApp + 3), firstApp + 3, selectedApp, listItem);
         CuBit.UI.Widgets.Vertical_Scrollbar
           (c, ui, controls, CONTROL_LIST_SCROLL,
            layout.listScrollBar, layout.listBox, colors,
            0, APP_COUNT - LIST_VISIBLE, listScroll, scrollBar);
         if listFocused then
            CuBit.UI.Stroke_Rect
              (c, CuBit.UI.Inflate_Rect (layout.listBox, 1),
               colors.accent,
               colors.accent);
         end if;
      else
         drawLabel (c, layout.menuLabel.x, layout.menuLabel.y, "Command menu");
         CuBit.UI.Widgets.Button
           (c, ui, controls, CONTROL_MENU_BUTTON,
            layout.menuButton, menuDamage, colors, "Menu", menuButton);
         if menuOpen then
            CuBit.UI.Fill_Rect (c, layout.menuBox, colors.panel);
            CuBit.UI.Stroke_Rect
              (c, layout.menuBox, colors.edge, colors.shadow);
            CuBit.UI.Widgets.Menu_Item
              (c, ui, controls, CONTROL_MENU_APPS,
               layout.menuApps, layout.menuBox, colors,
               "Programs", True, menuItem);
            CuBit.UI.Widgets.Menu_Item
              (c, ui, controls, CONTROL_MENU_SYSTEM,
               layout.menuSystem, layout.menuBox, colors,
               "System", True, menuItem);
            CuBit.UI.Widgets.Menu_Item
              (c, ui, controls, CONTROL_MENU_LOCKED,
               layout.menuLocked, layout.menuBox, colors,
               "Admin Tools", False, menuItem);
         end if;
         if menuChoice /= 0 then
            CuBit.UI.Draw_Natural_Value
              (c, (x => layout.menuLabel.x + 118,
                   y => layout.menuLabel.y,
                   w => 28, h => CuBit.UI.UI_Text_Height),
               colors, menuChoice);
         end if;
         CuBit.UI.Widgets.Scroll_Area
           (c, ui, controls, CONTROL_COMMAND_SCROLL,
            layout.commandScrollArea, tabDamage, colors,
            150, commandScroll, commandViewport, commandOriginY);
         commandFrame :=
            (bounds =>
                (x => commandViewport.x, y => commandOriginY,
                 w => commandViewport.w, h => 150),
             clip => commandViewport);
         commandCanvas := CuBit.UI.Layout.Canvas_For (c, commandFrame);
         CuBit.UI.Labels.Label
           (commandCanvas,
            CuBit.UI.Layout.Resolve
              (commandFrame,
               (x => 2, y => 0,
                w => commandViewport.w - 2, h => CuBit.UI.UI_Text_Height)),
            colors, "Scroll Area");
         CuBit.UI.Labels.Label
           (commandCanvas,
            CuBit.UI.Layout.Resolve
              (commandFrame,
               (x => 2, y => 22,
                w => commandViewport.w - 2, h => CuBit.UI.UI_Text_Height)),
            colors, "Owns border + scrollbar", muted => True);
         CuBit.UI.Labels.Label
           (commandCanvas,
            CuBit.UI.Layout.Resolve
              (commandFrame,
               (x => 2, y => 44,
                w => commandViewport.w - 2, h => CuBit.UI.UI_Text_Height)),
            colors, "Children draw into viewport", muted => True);
         CuBit.UI.Labels.Label
           (commandCanvas,
            CuBit.UI.Layout.Resolve
              (commandFrame,
               (x => 2, y => 66,
                w => commandViewport.w - 2, h => CuBit.UI.UI_Text_Height)),
            colors, "Clipped by container clip", muted => True);
         CuBit.UI.Labels.Label
           (commandCanvas,
            CuBit.UI.Layout.Resolve
              (commandFrame,
               (x => 2, y => 88,
                w => commandViewport.w - 2, h => CuBit.UI.UI_Text_Height)),
            colors, "This is the seed of forms", muted => True);
      end if;

      if fileMenuOpen then
         CuBit.UI.Fill_Rect (c, layout.fileMenuBox, colors.panel);
         CuBit.UI.Stroke_Rect
           (c, layout.fileMenuBox, colors.edge, colors.shadow);
         CuBit.UI.Widgets.Menu_Item
           (c, ui, controls, CONTROL_FILE_RUN,
            layout.fileRun, layout.fileMenuBox, colors,
            "Run action   Ctrl+R", True, menuItem);
         CuBit.UI.Widgets.Menu_Item
           (c, ui, controls, CONTROL_FILE_EXIT,
            layout.fileExit, layout.fileMenuBox, colors,
            "Exit         Esc", True, menuItem);
      end if;

      CuBit.UI.Draw_Status_Bar
        (c, layout.statusBar, colors,
         "Ready. Alt+F opens File, Ctrl+M toggles Menu, Ctrl+R runs.",
         (if fileMenuOpen then "File menu"
          elsif menuOpen then "Command menu"
          else "UI Lab"));
      CuBit.UI.State.Exit_Scope (ui);
      CuBit.UI.State.Finish_Frame (ui);
   end render;

   procedure handleEvent
      (win : in out CuBit.UI.App.Window;
       ev : CuBit.UI.App.Input_Event;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean)
   is
      pragma Unreferenced (win);
      newX : Natural;
      newY : Natural;
      newDown : Boolean;
      newHover : CuBit.UI.Controls.Control_ID;
      wheelDelta : Integer;
      ctrl : Boolean;
      alt : Boolean;
   begin
      if ev.kind = CuBit.UI.App.INPUT_KEY_DOWN then
         ctrl := (ev.payload1 and CuBit.UI.App.KEYMOD_CTRL) /= 0;
         alt := (ev.payload1 and CuBit.UI.App.KEYMOD_ALT) /= 0;
         if ev.payload0 = CuBit.UI.App.KEY_ESC then
            running := False;
         elsif ctrl and then ev.payload0 = CuBit.UI.App.KEY_R then
            clickCount := clickCount + 1;
            dirty := CuBit.UI.Union_Rect (dirty, layout.counter);
            dirty := CuBit.UI.Union_Rect (dirty, layout.statusBar);
         elsif ctrl and then ev.payload0 = CuBit.UI.App.KEY_M then
            activeTab := 4;
            menuOpen := not menuOpen;
            dirty := CuBit.UI.Union_Rect (dirty, menuDamage);
            dirty := CuBit.UI.Union_Rect (dirty, tabDamage);
            dirty := CuBit.UI.Union_Rect (dirty, layout.statusBar);
         elsif alt and then ev.payload0 = CuBit.UI.App.KEY_F then
            fileMenuOpen := not fileMenuOpen;
            dirty := CuBit.UI.Union_Rect (dirty, fileMenuDamage);
            dirty := CuBit.UI.Union_Rect (dirty, layout.statusBar);
         else
            if CuBit.UI.State.Text_Field_Key
              (ui,
               sampleText,
               sampleTextLen,
               Natural (ev.payload0),
               Natural (ev.payload1))
            then
               markDirty (dirty, CONTROL_TEXT_FIELD);
            elsif handleListKey (ev.payload0) then
               markDirty (dirty, CONTROL_LIST_DESKTOP);
            elsif ev.payload0 = CuBit.UI.App.KEY_Q then
               running := False;
            end if;
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_TEXT then
         if CuBit.UI.State.Text_Field_Text
           (ui,
            sampleText,
            sampleTextLen,
            Natural (ev.payload0))
         then
            markDirty (dirty, CONTROL_TEXT_FIELD);
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_MOVE then
         newX := unpackLo32 (ev.payload0);
         newY := unpackHi32 (ev.payload0);
         newDown := (ev.payload1 and 1) /= 0;
         newHover := CuBit.UI.Controls.Hit (controls, newX, newY);
         CuBit.UI.State.Set_Pointer (ui, newX, newY, newDown);

         if newDown and then
            (lastHoverControl = CONTROL_SLIDER or else
             newHover = CONTROL_SLIDER or else
             lastHoverControl = CONTROL_LIST_SCROLL or else
             newHover = CONTROL_LIST_SCROLL or else
             lastHoverControl = CONTROL_DATA_SPLIT or else
             newHover = CONTROL_DATA_SPLIT or else
             lastHoverControl = CONTROL_COMMAND_SCROLL or else
             newHover = CONTROL_COMMAND_SCROLL or else
             lastHoverControl = CONTROL_TEXT_FIELD or else
             newHover = CONTROL_TEXT_FIELD)
         then
            markDirty (dirty, lastHoverControl);
            markDirty (dirty, newHover);
         elsif newHover /= lastHoverControl then
            markDirty (dirty, lastHoverControl);
            markDirty (dirty, newHover);
         end if;

         lastHoverControl := newHover;
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_WHEEL then
         newX := unpackLo32 (ev.payload0);
         newY := unpackHi32 (ev.payload0);
         wheelDelta := unpackSignedLo32 (ev.payload1);
         newHover := CuBit.UI.Controls.Hit (controls, newX, newY);

         if activeTab = 3 and then
            (newHover = CONTROL_LIST_SCROLL or else
             CuBit.UI.Point_In_Rect (newX, newY, layout.listBox))
         then
            scrollByWheel (listScroll, APP_COUNT - LIST_VISIBLE, wheelDelta);
            ensureSelectedVisible;
            dirty := CuBit.UI.Union_Rect (dirty, layout.listBox);
         elsif activeTab = 4 and then
            (newHover = CONTROL_COMMAND_SCROLL or else
             CuBit.UI.Point_In_Rect (newX, newY, layout.commandScrollArea))
         then
            scrollByWheel (commandScroll, 64, wheelDelta);
            dirty := CuBit.UI.Union_Rect
              (dirty, layout.commandScrollArea);
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_DOWN then
         newX := unpackLo32 (ev.payload0);
         newY := unpackHi32 (ev.payload0);
         newHover := CuBit.UI.Controls.Hit (controls, newX, newY);
         if menuOpen and then
            not isMenuControl (newHover)
         then
            menuOpen := False;
            dirty := CuBit.UI.Union_Rect (dirty, menuDamage);
         end if;
         if fileMenuOpen and then
            not isFileMenuControl (newHover)
         then
            fileMenuOpen := False;
            dirty := CuBit.UI.Union_Rect (dirty, fileMenuDamage);
         end if;
         if listFocused /= isListControl (newHover) then
            listFocused := isListControl (newHover);
            markDirty (dirty, CONTROL_LIST_DESKTOP);
         end if;
         if newHover /= CONTROL_TEXT_FIELD and then
            CuBit.UI.State.Is_Last_Widget_Focused (ui)
         then
            CuBit.UI.State.Clear_Keyboard_Focus (ui);
            markDirty (dirty, CONTROL_TEXT_FIELD);
         end if;
         CuBit.UI.State.Set_Pointer
           (ui, newX, newY, True, pressed => True);
         lastHoverControl := newHover;
         markDirty (dirty, newHover);
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_UP then
         newX := unpackLo32 (ev.payload0);
         newY := unpackHi32 (ev.payload0);
         newHover := CuBit.UI.Controls.Hit (controls, newX, newY);
         case newHover is
            when CONTROL_RADIO_FAST =>
               demoMode := 1;
            when CONTROL_RADIO_SAFE =>
               demoMode := 2;
            when CONTROL_RADIO_PROOF =>
               demoMode := 3;
            when CONTROL_LIST_DESKTOP =>
               selectedApp := listScroll + 1;
            when CONTROL_LIST_SECURITY =>
               selectedApp := listScroll + 2;
            when CONTROL_LIST_CONSOLE =>
               selectedApp := listScroll + 3;
            when CONTROL_LIST_DOOM =>
               selectedApp := listScroll + 4;
            when CONTROL_STREAM_ROW1 =>
               selectedStream := 1;
            when CONTROL_STREAM_ROW2 =>
               selectedStream := 2;
            when CONTROL_STREAM_ROW3 =>
               selectedStream := 3;
            when CONTROL_MENU_BUTTON =>
               menuOpen := not menuOpen;
               dirty := CuBit.UI.Union_Rect (dirty, menuDamage);
            when CONTROL_BAR_FILE =>
               fileMenuOpen := not fileMenuOpen;
               dirty := CuBit.UI.Union_Rect (dirty, fileMenuDamage);
            when CONTROL_FILE_RUN =>
               clickCount := clickCount + 1;
               fileMenuOpen := False;
               dirty := CuBit.UI.Union_Rect (dirty, layout.counter);
               dirty := CuBit.UI.Union_Rect (dirty, fileMenuDamage);
            when CONTROL_FILE_EXIT =>
               running := False;
            when CONTROL_MENU_APPS =>
               menuChoice := 1;
               menuOpen := False;
               dirty := CuBit.UI.Union_Rect (dirty, menuDamage);
            when CONTROL_MENU_SYSTEM =>
               menuChoice := 2;
               menuOpen := False;
               dirty := CuBit.UI.Union_Rect (dirty, menuDamage);
            when others =>
               null;
         end case;
         if newHover >= CONTROL_TAB_BASE and then
            newHover < CONTROL_TAB_BASE + UI_Lab_Form.TAB_LABELS'Length
         then
            activeTab := newHover - CONTROL_TAB_BASE + 1;
            dirty := CuBit.UI.Union_Rect (dirty, tabDamage);
         end if;
         ensureSelectedVisible;
         CuBit.UI.State.Set_Pointer
           (ui, newX, newY, False, released => True);
         lastHoverControl := newHover;
         markDirty (dirty, newHover);
         if newHover = CONTROL_ACTION then
            dirty := CuBit.UI.Union_Rect (dirty, layout.counter);
         end if;
      end if;
   end handleEvent;

   procedure runUI is new CuBit.UI.App.Run
      (Render       => render,
       Handle_Event => handleEvent);

begin
   debugPrint ("ui-lab: starting" & LF);

   declare
      ok : Boolean;
      flags : constant Unsigned_64 :=
         CuBit.UI.App.WINDOW_FLAG_DECORATED or
         CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or
         CuBit.UI.App.WINDOW_FLAG_CLOSEABLE or
         CuBit.UI.App.WINDOW_FLAG_FIXED_SIZE;
   begin
      CuBit.UI.App.Open (win, bufferW, bufferH, flags, ok);
      if not ok then
         debugPrint ("ui-lab: window open failed" & LF);
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   runUI (win);

   CuBit.UI.App.Close (win);
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
