------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UI primitive exercise app
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.Layout;
with CuBit.UI.State;

procedure main is
   use ASCII;
   use type CuBit.UI.State.Widget_ID;
   use type CuBit.UI.State.Scope_ID;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := 16#0800#;
   OP_DESKTOP_BYE      : constant Unsigned_32 := 16#0801#;
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := 16#0802#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_ATTACH_BUFFER : constant Unsigned_32 := 16#0814#;
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := 16#0841#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;
   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 := 1;

   WINDOW_FLAG_DECORATED   : constant Unsigned_64 := 1;
   WINDOW_FLAG_MINIMIZABLE : constant Unsigned_64 := 4;
   WINDOW_FLAG_CLOSEABLE   : constant Unsigned_64 := 16;
   WINDOW_FLAG_FIXED_SIZE  : constant Unsigned_64 := 128;

   INPUT_NONE     : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN : constant Unsigned_64 := 1;
   INPUT_POINTER_MOVE : constant Unsigned_64 := 3;
   INPUT_POINTER_DOWN : constant Unsigned_64 := 4;
   INPUT_POINTER_UP   : constant Unsigned_64 := 5;
   INPUT_TEXT     : constant Unsigned_64 := 6;

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;
   PROTOCOL_VERSION : constant Unsigned_64 :=
      0 or Shift_Left (Unsigned_64'(1), 32);

   bufferW : constant Natural := 640;
   bufferH : constant Natural := 420;
   bufferPitch : constant Natural := bufferW * 4;
   windowW : constant Unsigned_64 := Unsigned_64 (bufferW + 20);
   windowH : constant Unsigned_64 := Unsigned_64 (bufferH + 44);

   windowId : Unsigned_64 := 0;
   bufferAddr : System.Address := System.Null_Address;
   bufferGrant : Unsigned_64 := 0;
   lastEvent : Unsigned_64 := 0;
   running : Boolean := True;
   sentBye : Boolean := False;
   ignore : Unsigned_64;
   ui : CuBit.UI.State.UI_State;
   demoChecked : Boolean := True;
   demoValue : Natural := 42;
   clickCount : Natural := 0;
   lastHoverControl : Natural := 0;
   TEXT_MAX : constant Natural := 24;
   sampleText : String (1 .. TEXT_MAX) := "CuBit UI                ";
   sampleTextLen : Natural := 8;

   PANEL_RECT : constant CuBit.UI.Rect :=
      (x => 18, y => 18, w => bufferW - 36, h => bufferH - 36);
   HEADER_RECT : constant CuBit.UI.Rect :=
      (x => 18, y => 18, w => bufferW - 36, h => 34);

   function callDesktop
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label  => label,
                      length => 4,
                      flags  => 0,
                      badge  => 0),
         capBadge => 0,
         words    => (w0, w1, w2, w3));
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_DESKTOP, msg);
      msg.tag := tag;
      return msg;
   end callDesktop;

   function packU32Pair (lo, hi : Unsigned_64) return Unsigned_64 is
   begin
      return (lo and 16#FFFF_FFFF#) or Shift_Left (hi and 16#FFFF_FFFF#, 32);
   end packU32Pair;

   function unpackLo32 (x : Unsigned_64) return Natural is
   begin
      return Natural (x and 16#FFFF_FFFF#);
   end unpackLo32;

   function unpackHi32 (x : Unsigned_64) return Natural is
   begin
      return Natural (Shift_Right (x, 32));
   end unpackHi32;

   function alignUpPage (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end alignUpPage;

   function canvas return CuBit.UI.Canvas is
   begin
      return
        (addr        => bufferAddr,
         width       => bufferW,
         height      => bufferH,
         pitch       => bufferPitch,
         clipEnabled => False,
         clip        => (others => 0));
   end canvas;

   function clippedCanvas (clip : CuBit.UI.Rect) return CuBit.UI.Canvas is
   begin
      return
        (addr        => bufferAddr,
         width       => bufferW,
         height      => bufferH,
         pitch       => bufferPitch,
         clipEnabled => True,
         clip        => CuBit.UI.Clamp_Rect (canvas, clip));
   end clippedCanvas;

   function fullRect return CuBit.UI.Rect is
   begin
      return (x => 0, y => 0, w => bufferW, h => bufferH);
   end fullRect;

   function contentRect return CuBit.UI.Rect is
   begin
      return CuBit.UI.Layout.Inset (PANEL_RECT, 16, 52, 16, 16);
   end contentRect;

   function leftColumnRect return CuBit.UI.Rect is
      content : constant CuBit.UI.Rect := contentRect;
   begin
      return (x => content.x, y => content.y, w => 304, h => content.h);
   end leftColumnRect;

   function rightColumnRect return CuBit.UI.Rect is
      content : constant CuBit.UI.Rect := contentRect;
      left    : constant CuBit.UI.Rect := leftColumnRect;
      x       : constant Natural := left.x + left.w + 14;
   begin
      return (x => x, y => content.y + 82,
              w => content.x + content.w - x, h => content.h - 82);
   end rightColumnRect;

   function buttonLabelRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
   begin
      return CuBit.UI.Layout.Take_Remaining (l, 18);
   end buttonLabelRect;

   function actionButtonRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      return CuBit.UI.Layout.Take (l, 108, 30);
   end actionButtonRect;

   function disabledButtonRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      return CuBit.UI.Layout.Take (l, 108, 30);
   end disabledButtonRect;

   function controlsLabelRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      CuBit.UI.Layout.New_Row (l, 16);
      return CuBit.UI.Layout.Take_Remaining (l, 18);
   end controlsLabelRect;

   function checkboxRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      CuBit.UI.Layout.New_Row (l, 16);
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      return CuBit.UI.Layout.Take (l, 22, 22);
   end checkboxRect;

   function sliderRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      CuBit.UI.Layout.New_Row (l, 16);
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ignore := CuBit.UI.Layout.Take (l, 22, 22);
      CuBit.UI.Layout.New_Row (l, 12);
      return CuBit.UI.Layout.Take (l, 210, 26);
   end sliderRect;

   function counterRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      CuBit.UI.Layout.New_Row (l, 16);
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ignore := CuBit.UI.Layout.Take (l, 22, 22);
      CuBit.UI.Layout.New_Row (l, 12);
      ignore := CuBit.UI.Layout.Take (l, 210, 26);
      CuBit.UI.Layout.New_Row (l, 12);
      return CuBit.UI.Layout.Take (l, 160, 18);
   end counterRect;

   function textFieldRect return CuBit.UI.Rect is
      l : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (leftColumnRect, 12, 8);
      ignore : CuBit.UI.Rect;
   begin
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 2);
      ignore := CuBit.UI.Layout.Take (l, 108, 30);
      CuBit.UI.Layout.New_Row (l, 16);
      ignore := CuBit.UI.Layout.Take_Remaining (l, 18);
      CuBit.UI.Layout.New_Row (l, 4);
      ignore := CuBit.UI.Layout.Take (l, 22, 22);
      CuBit.UI.Layout.New_Row (l, 12);
      ignore := CuBit.UI.Layout.Take (l, 210, 26);
      CuBit.UI.Layout.New_Row (l, 12);
      ignore := CuBit.UI.Layout.Take (l, 160, 18);
      CuBit.UI.Layout.New_Row (l, 12);
      return CuBit.UI.Layout.Take (l, 250, 28);
   end textFieldRect;

   function hitControl (x, y : Natural) return Natural is
   begin
      if CuBit.UI.Point_In_Rect (x, y, actionButtonRect) then
         return 1;
      elsif CuBit.UI.Point_In_Rect (x, y, checkboxRect) then
         return 2;
      elsif CuBit.UI.Point_In_Rect (x, y, sliderRect) then
         return 3;
      elsif CuBit.UI.Point_In_Rect (x, y, textFieldRect) then
         return 4;
      else
         return 0;
      end if;
   end hitControl;

   function controlDamage (code : Natural) return CuBit.UI.Rect is
   begin
      case code is
         when 1 =>
            return CuBit.UI.Union_Rect
              (CuBit.UI.Inflate_Rect (actionButtonRect, 4), counterRect);
         when 2 =>
            return CuBit.UI.Inflate_Rect
              ((x => checkboxRect.x, y => checkboxRect.y - 4,
                w => 148, h => 30), 2);
         when 3 =>
            return CuBit.UI.Inflate_Rect
              ((x => sliderRect.x, y => sliderRect.y - 4,
                w => 304, h => 34), 2);
         when 4 =>
            return CuBit.UI.Inflate_Rect (textFieldRect, 4);
         when others =>
            return (others => 0);
      end case;
   end controlDamage;

   procedure drawLabel
      (c : CuBit.UI.Canvas; x, y : Natural; text : String)
   is
   begin
      CuBit.UI.Draw_UI_Text (c, x, y, text,
                             CuBit.UI.Mirage.text,
                             CuBit.UI.Mirage.panel);
   end drawLabel;

   procedure drawMuted
      (c : CuBit.UI.Canvas; x, y : Natural; text : String)
   is
   begin
      CuBit.UI.Draw_UI_Text (c, x, y, text,
                             CuBit.UI.Mirage.muted,
                             CuBit.UI.Mirage.panel);
   end drawMuted;

   procedure render (damage : CuBit.UI.Rect := fullRect) is
      c : constant CuBit.UI.Canvas :=
         (if CuBit.UI.Is_Empty (damage) then canvas
          elsif damage.x = 0 and then damage.y = 0 and then
             damage.w = bufferW and then damage.h = bufferH
          then canvas
          else clippedCanvas (damage));
      colors : constant CuBit.UI.Theme := CuBit.UI.Mirage;
      buttonLabel : constant CuBit.UI.Rect := buttonLabelRect;
      controlsLabel : constant CuBit.UI.Rect := controlsLabelRect;
      rightColumn : constant CuBit.UI.Rect := rightColumnRect;
      footer : constant CuBit.UI.Rect :=
        (x => contentRect.x,
         y => contentRect.y + contentRect.h - 16,
         w => contentRect.w,
         h => 16);
      actionButton : CuBit.UI.Widget_Result;
      checkBox : CuBit.UI.Widget_Result;
      slider : CuBit.UI.Widget_Result;
      textField : CuBit.UI.Widget_Result;
      layout : CuBit.UI.Layout.Cursor :=
         CuBit.UI.Layout.Start (rightColumn, 8, 8);
      label : CuBit.UI.Rect;
      bar : CuBit.UI.Rect;
      progress : CuBit.UI.Rect;
      buttonStyle : CuBit.UI.Button_Style;
   begin
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

      drawLabel (c, buttonLabel.x, buttonLabel.y, "Button");
      actionButton :=
         CuBit.UI.State.Button (ui, actionButtonRect);
      buttonStyle :=
         (if actionButton.active then CuBit.UI.Button_Pressed
          elsif actionButton.hot then CuBit.UI.Button_Hot
          else CuBit.UI.Button_Normal);
      CuBit.UI.Draw_Button (c, actionButtonRect, colors, buttonStyle, "Run");
      if actionButton.activated then
         clickCount := clickCount + 1;
      end if;
      if ui.keyboardItem = ui.lastWidget and then
         ui.keyboardScope = ui.lastScope
      then
         CuBit.UI.Stroke_Rect
           (c, CuBit.UI.Inflate_Rect (actionButtonRect, 1),
            colors.accent,
            colors.accent);
      end if;

      CuBit.UI.Draw_Button
        (c, disabledButtonRect, colors, CuBit.UI.Button_Disabled,
         "Disabled");

      drawLabel (c, controlsLabel.x, controlsLabel.y, "Stateful controls");
      checkBox :=
         CuBit.UI.State.Checkbox (ui, checkboxRect, demoChecked);
      CuBit.UI.Draw_Checkbox (c, checkboxRect, colors, demoChecked,
                              checkBox.hot, checkBox.active);
      CuBit.UI.Draw_UI_Text (c, 68, 185, "checkbox",
                             colors.text,
                             colors.panel);

      slider :=
         CuBit.UI.State.Horizontal_Slider
           (ui, sliderRect, demoValue, 0, 100);
      CuBit.UI.Draw_Horizontal_Slider
        (c, sliderRect, colors, 0, 100, demoValue, slider.hot,
         slider.active);
      CuBit.UI.Draw_UI_Text (c, 258, 229, "value",
                             colors.muted,
                             colors.panel);
      CuBit.UI.Draw_Natural_Value
        (c, (x => 306, y => 229, w => 48, h => CuBit.UI.UI_Text_Height),
         colors, demoValue);

      CuBit.UI.Draw_UI_Text (c, counterRect.x, counterRect.y, "clicks",
                             colors.muted,
                             colors.panel);
      CuBit.UI.Draw_Natural_Value
        (c, (x => 90, y => counterRect.y,
             w => 48, h => CuBit.UI.UI_Text_Height),
         colors, clickCount);

      textField := CuBit.UI.State.Text_Field (ui, textFieldRect);
      CuBit.UI.Draw_Text_Field
        (c, textFieldRect, colors,
         (if sampleTextLen = 0 then "" else sampleText (1 .. sampleTextLen)),
         CuBit.UI.State.Is_Last_Widget_Focused (ui),
         textField.hot);

      label := CuBit.UI.Layout.Take_Remaining (layout, 18);
      drawLabel (c, label.x, label.y, "Layout blocks");
      CuBit.UI.Layout.New_Row (layout, 4);
      bar := CuBit.UI.Layout.Take (layout, 150, 24);
      CuBit.UI.Fill_Rect (c, bar, colors.face);
      CuBit.UI.Layout.New_Row (layout, 0);
      bar := CuBit.UI.Layout.Take (layout, 118, 24);
      CuBit.UI.Fill_Rect (c, bar, colors.face);
      CuBit.UI.Layout.New_Row (layout, 0);
      bar := CuBit.UI.Layout.Take (layout, 74, 24);
      CuBit.UI.Fill_Rect (c, bar, colors.face);
      CuBit.UI.Layout.New_Row (layout, 8);
      progress := CuBit.UI.Layout.Take (layout, 150, 10);
      CuBit.UI.Draw_Progress_Bar (c, progress, colors, 0, 150, 108);
      CuBit.UI.Layout.New_Row (layout, 10);
      bar := CuBit.UI.Layout.Take (layout, 120, 18);
      CuBit.UI.Draw_Swatch (c, bar, colors, colors.danger, "danger");

      CuBit.UI.Draw_Swatch
        (c, (x => 190, y => checkboxRect.y, w => 120, h => 18),
         colors, colors.accent, "accent");
      CuBit.UI.Draw_Swatch
        (c, (x => 190, y => counterRect.y + 4, w => 120, h => 18),
         colors, colors.good, "good");

      drawMuted (c, footer.x, footer.y,
                 "Esc/Q exits. This app exists to grow CuBit.UI.");
      CuBit.UI.State.Exit_Scope (ui);
      CuBit.UI.State.Finish_Frame (ui);
   end render;

   procedure present (damage : CuBit.UI.Rect := fullRect) is
      reply : Message;
      r : constant CuBit.UI.Rect :=
         CuBit.UI.Clamp_Rect (canvas, damage);
   begin
      if CuBit.UI.Is_Empty (r) then
         return;
      end if;

      render (r);
      reply := callDesktop
        (OP_SURFACE_PRESENT,
         windowId,
         packU32Pair (Unsigned_64 (r.x), Unsigned_64 (r.y)),
         packU32Pair (Unsigned_64 (r.w), Unsigned_64 (r.h)),
         0);
   end present;

   procedure closeSession is
      reply : Message;
   begin
      if sentBye then
         return;
      end if;

      reply := callDesktop (OP_DESKTOP_BYE);
      sentBye := True;
   end closeSession;

   procedure attachPixelBuffer is
      raw : Unsigned_64;
      pages : constant Unsigned_64 :=
         (Unsigned_64 (bufferPitch * bufferH) + 4095) / 4096;
      grantOk : Boolean;
      reply : Message;
   begin
      raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
      if raw = Unsigned_64'Last then
         debugPrint ("ui-lab: pixel buffer alloc failed" & LF);
         return;
      end if;

      bufferAddr := To_Address (Integer_Address (alignUpPage (raw)));
      render;

      createGrantViaCap
        (slot      => CAP_SLOT_DESKTOP,
         localAddr => bufferAddr,
         numPages  => Natural (pages),
         readWrite => False,
         grantId   => bufferGrant,
         success   => grantOk);
      if not grantOk then
         debugPrint ("ui-lab: pixel grant failed" & LF);
         return;
      end if;

      reply := callDesktop
        (OP_SURFACE_ATTACH_BUFFER,
         windowId,
         bufferGrant,
         packU32Pair (Unsigned_64 (bufferW), Unsigned_64 (bufferH)),
         Unsigned_64 (bufferPitch) or
            Shift_Left (PIXEL_FORMAT_BGRA8888, 32));
      if reply.words (0) /= 0 then
         debugPrint ("ui-lab: pixel attach failed" & LF);
      end if;
   end attachPixelBuffer;

begin
   debugPrint ("ui-lab: starting" & LF);

   declare
      hello : constant Message :=
         callDesktop (OP_DESKTOP_HELLO, PROTOCOL_VERSION, 0, 0, 0);
   begin
      if hello.words (0) = 0 then
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      info : constant Message := callDesktop (OP_DESKTOP_GET_INFO);
   begin
      if info.words (0) = 0 then
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      created : constant Message :=
         callDesktop (OP_SURFACE_CREATE,
                      windowW,
                      windowH,
                      SURFACE_FLAG_WINDOW,
                      0);
   begin
      windowId := created.words (0);
      if windowId = 0 then
         closeSession;
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      flags : constant Unsigned_64 :=
         WINDOW_FLAG_DECORATED or WINDOW_FLAG_MINIMIZABLE or
         WINDOW_FLAG_CLOSEABLE or WINDOW_FLAG_FIXED_SIZE;
      reply : Message;
   begin
      reply := callDesktop (OP_WINDOW_SET_LIMITS,
                            windowId,
                            packU32Pair (windowW, windowH),
                            packU32Pair (windowW, windowH),
                            flags);
   end;

   attachPixelBuffer;
   present;

   while running loop
      declare
         dirty : CuBit.UI.Rect := (others => 0);
         sawInput : Boolean := False;
         drainLimit : constant Natural := 32;
         newX : Natural;
         newY : Natural;
         newDown : Boolean;
         newHover : Natural;

         procedure markDirty (r : CuBit.UI.Rect) is
         begin
            dirty := CuBit.UI.Union_Rect (dirty, r);
         end markDirty;
      begin
         for i in 1 .. drainLimit loop
            declare
               ev : constant Message :=
                  callDesktop (OP_INPUT_POLL, windowId, lastEvent, 0, 0);
            begin
               exit when ev.words (0) = INPUT_NONE;

               sawInput := True;
               lastEvent := ev.words (1);
               if ev.words (0) = INPUT_KEY_DOWN then
                  if ev.words (2) = KEY_ESC then
                     running := False;
                     dirty := (others => 0);
                     exit;
                  else
                     if CuBit.UI.State.Text_Field_Key
                       (ui,
                        sampleText,
                        sampleTextLen,
                        Natural (ev.words (2)),
                        Natural (ev.words (3)))
                     then
                        markDirty (controlDamage (4));
                     elsif ev.words (2) = KEY_Q then
                        running := False;
                        dirty := (others => 0);
                        exit;
                     end if;
                  end if;
               elsif ev.words (0) = INPUT_TEXT then
                  if CuBit.UI.State.Text_Field_Text
                    (ui,
                     sampleText,
                     sampleTextLen,
                     Natural (ev.words (2)))
                  then
                     markDirty (controlDamage (4));
                  end if;
               elsif ev.words (0) = INPUT_POINTER_MOVE then
                  newX := unpackLo32 (ev.words (2));
                  newY := unpackHi32 (ev.words (2));
                  newDown := (ev.words (3) and 1) /= 0;
                  newHover := hitControl (newX, newY);
                  CuBit.UI.State.Set_Pointer
                    (ui,
                     newX,
                     newY,
                     newDown);

                  --  Plain cursor motion is handled by the compositor cursor
                  --  overlay. The app only redraws when a widget's visual
                  --  state can actually change: hover enter/leave or active
                  --  slider dragging.
                  if newDown and then
                     (lastHoverControl = 3 or else newHover = 3)
                  then
                     markDirty (controlDamage (3));
                  elsif newHover /= lastHoverControl then
                     markDirty (controlDamage (lastHoverControl));
                     markDirty (controlDamage (newHover));
                  end if;

                  lastHoverControl := newHover;
               elsif ev.words (0) = INPUT_POINTER_DOWN then
                  newX := unpackLo32 (ev.words (2));
                  newY := unpackHi32 (ev.words (2));
                  newHover := hitControl (newX, newY);
                  if newHover /= 4 and then
                     CuBit.UI.State.Is_Last_Widget_Focused (ui)
                  then
                     CuBit.UI.State.Clear_Keyboard_Focus (ui);
                     markDirty (controlDamage (4));
                  end if;
                  CuBit.UI.State.Set_Pointer
                    (ui,
                     newX,
                     newY,
                     True,
                     pressed => True);
                  lastHoverControl := newHover;
                  markDirty (controlDamage (newHover));
               elsif ev.words (0) = INPUT_POINTER_UP then
                  newX := unpackLo32 (ev.words (2));
                  newY := unpackHi32 (ev.words (2));
                  newHover := hitControl (newX, newY);
                  CuBit.UI.State.Set_Pointer
                    (ui,
                     newX,
                     newY,
                     False,
                     released => True);
                  lastHoverControl := newHover;
                  markDirty (controlDamage (newHover));
                  if newHover = 1 then
                     markDirty (counterRect);
                  end if;
               end if;
            end;
         end loop;

         if not CuBit.UI.Is_Empty (dirty) then
            present (dirty);
         end if;

         if sawInput or else ui.pointer.down then
            ignore := syscall (SYSCALL_SLEEP, 1);
         else
            ignore := syscall (SYSCALL_SLEEP, 5);
         end if;
      end;
   end loop;

   closeSession;
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
