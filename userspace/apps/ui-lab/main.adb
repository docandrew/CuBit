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

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;

   PROTOCOL_VERSION : constant Unsigned_64 :=
      0 or Shift_Left (Unsigned_64'(1), 32);

   bufferW : constant Natural := 560;
   bufferH : constant Natural := 360;
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

   procedure drawLabel
      (c : CuBit.UI.Canvas; x, y : Natural; text : String)
   is
   begin
      CuBit.UI.Draw_Text (c, x, y, text,
                          CuBit.UI.Mirage.text,
                          CuBit.UI.Mirage.panel);
   end drawLabel;

   procedure drawMuted
      (c : CuBit.UI.Canvas; x, y : Natural; text : String)
   is
   begin
      CuBit.UI.Draw_Text (c, x, y, text,
                          CuBit.UI.Mirage.muted,
                          CuBit.UI.Mirage.panel);
   end drawMuted;

   procedure drawSwatch
      (c : CuBit.UI.Canvas;
       x, y : Natural;
       color : CuBit.UI.Color;
       name : String)
   is
   begin
      CuBit.UI.Fill_Rect (c, (x => x, y => y, w => 28, h => 18), color);
      CuBit.UI.Stroke_Rect (c, (x => x, y => y, w => 28, h => 18),
                            CuBit.UI.Mirage.edge,
                            CuBit.UI.Mirage.shadow);
      CuBit.UI.Draw_Text (c, x + 36, y + 1, name,
                          CuBit.UI.Mirage.text,
                          CuBit.UI.Mirage.panel);
   end drawSwatch;

   procedure render is
      c : constant CuBit.UI.Canvas := canvas;
      colors : constant CuBit.UI.Theme := CuBit.UI.Mirage;
      normalButton : CuBit.UI.Widget_Result;
      hotButton : CuBit.UI.Widget_Result;
      pressedButton : CuBit.UI.Widget_Result;
      disabledButton : CuBit.UI.Widget_Result;
   begin
      CuBit.UI.State.Begin_Frame (ui);
      CuBit.UI.State.Enter_Scope (ui);

      CuBit.UI.Fill_Rect (c, (x => 0, y => 0, w => bufferW, h => bufferH),
                          colors.desktop);
      CuBit.UI.Fill_Rect (c, (x => 18, y => 18, w => 524, h => 324),
                          colors.panel);
      CuBit.UI.Stroke_Rect (c, (x => 18, y => 18, w => 524, h => 324),
                            colors.edge,
                            colors.shadow);
      CuBit.UI.Fill_Rect (c, (x => 18, y => 18, w => 524, h => 34),
                          colors.face);
      CuBit.UI.Draw_Text (c, 32, 27, "CuBit UI Lab",
                          colors.text,
                          colors.face);
      CuBit.UI.Draw_Text (c, 392, 27, "SPAWN UI-LAB",
                          colors.accent,
                          colors.face);

      drawLabel (c, 34, 70, "Button frames");
      normalButton :=
         CuBit.UI.State.Button
           (ui, (x => 34, y => 98, w => 108, h => 30));
      CuBit.UI.Draw_Button_Frame (c, (x => 34, y => 98, w => 108, h => 30),
                                  colors,
                                  (if normalButton.active then
                                      CuBit.UI.Button_Pressed
                                   elsif normalButton.hot then
                                      CuBit.UI.Button_Hot
                                   else
                                      CuBit.UI.Button_Normal));
      CuBit.UI.Draw_Text (c, 52, 105, "Normal", colors.text, colors.face);
      if ui.keyboardItem = ui.lastWidget and then
         ui.keyboardScope = ui.lastScope
      then
         CuBit.UI.Stroke_Rect
           (c, (x => 31, y => 95, w => 114, h => 36),
            colors.accent,
            colors.accent);
      end if;

      hotButton :=
         CuBit.UI.State.Button
           (ui, (x => 154, y => 98, w => 108, h => 30));
      CuBit.UI.Draw_Button_Frame (c, (x => 154, y => 98, w => 108, h => 30),
                                  colors,
                                  (if hotButton.active then
                                      CuBit.UI.Button_Pressed
                                   elsif hotButton.hot then
                                      CuBit.UI.Button_Hot
                                   else
                                      CuBit.UI.Button_Hot));
      CuBit.UI.Draw_Text (c, 188, 105, "Hot", colors.text, colors.panel);
      pressedButton :=
         CuBit.UI.State.Button
           (ui, (x => 274, y => 98, w => 108, h => 30));
      CuBit.UI.Draw_Button_Frame (c, (x => 274, y => 98, w => 108, h => 30),
                                  colors,
                                  (if pressedButton.activated then
                                      CuBit.UI.Button_Active
                                   else
                                      CuBit.UI.Button_Pressed));
      CuBit.UI.Draw_Text (c, 296, 105, "Pressed", colors.text, colors.face);
      disabledButton :=
         CuBit.UI.State.Button
           (ui, (x => 394, y => 98, w => 108, h => 30));
      CuBit.UI.Draw_Button_Frame (c, (x => 394, y => 98, w => 108, h => 30),
                                  colors,
                                  (if disabledButton.hot then
                                      CuBit.UI.Button_Hot
                                   else
                                      CuBit.UI.Button_Disabled));
      CuBit.UI.Draw_Text (c, 416, 105, "Disabled", colors.muted,
                          colors.shadow);

      drawLabel (c, 34, 152, "Theme swatches");
      drawSwatch (c, 34, 182, colors.desktop, "desktop");
      drawSwatch (c, 34, 210, colors.panel, "panel");
      drawSwatch (c, 34, 238, colors.face, "face");
      drawSwatch (c, 190, 182, colors.accent, "accent");
      drawSwatch (c, 190, 210, colors.good, "good");
      drawSwatch (c, 190, 238, colors.danger, "danger");

      drawLabel (c, 352, 152, "Layout blocks");
      CuBit.UI.Fill_Rect (c, (x => 352, y => 182, w => 150, h => 24),
                          colors.face);
      CuBit.UI.Fill_Rect (c, (x => 352, y => 214, w => 118, h => 24),
                          colors.face);
      CuBit.UI.Fill_Rect (c, (x => 352, y => 246, w => 74, h => 24),
                          colors.face);
      CuBit.UI.Fill_Rect (c, (x => 352, y => 286, w => 150, h => 10),
                          colors.shadow);
      CuBit.UI.Fill_Rect (c, (x => 352, y => 286, w => 108, h => 10),
                          colors.good);

      drawMuted (c, 34, 310, "Esc/Q exits. This app exists to grow CuBit.UI.");
      CuBit.UI.State.Exit_Scope (ui);
      CuBit.UI.State.Finish_Frame (ui);
   end render;

   procedure present is
      reply : Message;
   begin
      render;
      reply := callDesktop (OP_SURFACE_PRESENT, windowId, 0, 0, 0);
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
         ev : constant Message :=
            callDesktop (OP_INPUT_POLL, windowId, lastEvent, 0, 0);
      begin
         if ev.words (0) /= INPUT_NONE then
            lastEvent := ev.words (1);
            if ev.words (0) = INPUT_KEY_DOWN and then
              (ev.words (2) = KEY_ESC or else ev.words (2) = KEY_Q)
            then
               running := False;
            end if;
         end if;
      end;

      ignore := syscall (SYSCALL_SLEEP, 25);
   end loop;

   closeSession;
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
