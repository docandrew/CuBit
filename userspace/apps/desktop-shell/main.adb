------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  First desktop shell client
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;

procedure main is
   use ASCII;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := 16#0800#;
   OP_DESKTOP_BYE      : constant Unsigned_32 := 16#0801#;
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := 16#0802#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_RESIZE   : constant Unsigned_32 := 16#0813#;
   OP_SURFACE_ATTACH_BUFFER : constant Unsigned_32 := 16#0814#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   SURFACE_FLAG_SHELL  : constant Unsigned_64 := 1;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;
   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 := 1;

   INPUT_NONE      : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN  : constant Unsigned_64 := 1;
   INPUT_CONFIGURE : constant Unsigned_64 := 8;

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;
   KEY_R : constant Unsigned_64 := 16#13#;

   PROTOCOL_VERSION : constant Unsigned_64 :=
      0 or Shift_Left (Unsigned_64'(1), 32);

   surfaceId : Unsigned_64 := 0;
   windowId  : Unsigned_64 := 0;
   width     : Unsigned_64 := 0;
   height    : Unsigned_64 := 0;
   windowW   : Unsigned_64 := 340;
   windowH   : Unsigned_64 := 220;
   bufferW   : constant Natural := 320;
   bufferH   : constant Natural := 176;
   bufferPitch : constant Natural := bufferW * 4;
   bufferAddr : System.Address := System.Null_Address;
   bufferGrant : Unsigned_64 := 0;
   lastEvent : Unsigned_64 := 0;
   compact   : Boolean := False;
   running   : Boolean := True;
   sentBye   : Boolean := False;
   ignore    : Unsigned_64;

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
      --  All shell protocol traffic goes through the desktop endpoint cap.
      --  The app never maps the framebuffer and never registers for raw
      --  keyboard/mouse input; desktop.svc remains the display/input boundary.
      tag := capCall (CAP_SLOT_DESKTOP, msg);
      msg.tag := tag;
      return msg;
   end callDesktop;

   function alignUpPage (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end alignUpPage;

   procedure writePixel (x, y : Natural; color : Unsigned_32) is
   begin
      CuBit.UI.Set_Pixel (canvas, x, y, color);
   end writePixel;

   procedure drawPixelTest (phase : Unsigned_64) is
      c : constant CuBit.UI.Canvas := canvas;
      r : Unsigned_32;
      g : Unsigned_32;
      b : Unsigned_32;
   begin
      for y in 0 .. bufferH - 1 loop
         for x in 0 .. bufferW - 1 loop
            r := Unsigned_32 ((x + Natural (phase mod 64)) mod 256);
            g := Unsigned_32 ((y * 2) mod 256);
            b := Unsigned_32 (((x / 8 + y / 8) * 24) mod 256);
            writePixel
              (x, y,
               Shift_Left (r, 16) or Shift_Left (g, 8) or b);
         end loop;
      end loop;

      CuBit.UI.Draw_Button_Frame
        (c,
         (x => 8, y => 8, w => 132, h => 28),
         CuBit.UI.Mirage,
         CuBit.UI.Button_Active);
      CuBit.UI.Draw_Button_Frame
        (c,
         (x => bufferW - 46, y => 8, w => 38, h => 28),
         CuBit.UI.Mirage,
         CuBit.UI.Button_Normal);
   end drawPixelTest;

   procedure attachPixelBuffer is
      raw : Unsigned_64;
      pages : constant Unsigned_64 :=
         (Unsigned_64 (bufferPitch * bufferH) + 4095) / 4096;
      grantOk : Boolean;
      reply : Message;
   begin
      raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
      if raw = Unsigned_64'Last then
         debugPrint ("desktop-shell: pixel buffer alloc failed" & LF);
         return;
      end if;

      bufferAddr := To_Address (Integer_Address (alignUpPage (raw)));
      drawPixelTest (0);

      createGrantViaCap
        (slot      => CAP_SLOT_DESKTOP,
         localAddr => bufferAddr,
         numPages  => Natural (pages),
         readWrite => False,
         grantId   => bufferGrant,
         success   => grantOk);
      if not grantOk then
         debugPrint ("desktop-shell: pixel grant failed" & LF);
         return;
      end if;

      reply := callDesktop
        (OP_SURFACE_ATTACH_BUFFER,
         windowId,
         bufferGrant,
         Unsigned_64 (bufferW) or Shift_Left (Unsigned_64 (bufferH), 32),
         Unsigned_64 (bufferPitch) or
            Shift_Left (PIXEL_FORMAT_BGRA8888, 32));
      if reply.words (0) /= 0 then
         debugPrint ("desktop-shell: pixel attach failed" & LF);
      else
         debugPrint ("desktop-shell: pixel buffer attached" & LF);
      end if;
   end attachPixelBuffer;

   procedure present is
      reply : Message;
      target : Unsigned_64 := surfaceId;
   begin
      if windowId /= 0 then
         target := windowId;
      end if;

      if bufferAddr /= System.Null_Address then
         drawPixelTest (lastEvent);
      end if;
      reply := callDesktop (OP_SURFACE_PRESENT, target, 0, 0, 0);
   end present;

   procedure requestResize is
      reply : Message;
      nextW : Unsigned_64 := windowW;
      nextH : Unsigned_64 := windowH;
   begin
      --  Bring-up resize exercise: press R to toggle a single child window.
      --  This proves resize belongs to a surface, not the whole desktop.
      if compact then
         compact := False;
      else
         compact := True;
      end if;

      if compact then
         nextW := 520;
         nextH := 320;
      else
         nextW := 360;
         nextH := 220;
      end if;

      reply := callDesktop (OP_SURFACE_RESIZE, windowId, nextW, nextH, 0);
      if reply.words (0) = 0 then
         windowW := reply.words (1);
         windowH := reply.words (2);
         present;
      end if;
   end requestResize;

   procedure closeSession is
      reply : Message;
   begin
      if sentBye then
         return;
      end if;

      --  This is the graceful prototype exit path. The shell client asks the
      --  desktop service to remove its surface, then exits so the CLI shell's
      --  foreground-child logic can reclaim keyboard/mouse focus and redraw.
      reply := callDesktop (OP_DESKTOP_BYE);
      sentBye := True;
   end closeSession;

begin
   debugPrint ("desktop-shell: starting" & LF);

   declare
      hello : constant Message :=
         callDesktop (OP_DESKTOP_HELLO, PROTOCOL_VERSION, 0, 0, 0);
   begin
      if hello.words (0) = 0 then
         debugPrint ("desktop-shell: hello failed" & LF);
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      info : constant Message := callDesktop (OP_DESKTOP_GET_INFO);
   begin
      width := info.words (0);
      height := info.words (1);
   end;

   declare
      created : constant Message :=
         callDesktop (OP_SURFACE_CREATE,
                      width,
                      height,
                      SURFACE_FLAG_SHELL,
                      0);
   begin
      surfaceId := created.words (0);
      width := created.words (1);
      height := created.words (2);
      if surfaceId = 0 then
         debugPrint ("desktop-shell: create surface failed" & LF);
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
                      surfaceId);
   begin
      windowId := created.words (0);
      windowW := created.words (1);
      windowH := created.words (2);
      if windowId = 0 then
         debugPrint ("desktop-shell: create window failed" & LF);
         closeSession;
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   debugPrint ("desktop-shell: connected" & LF);
   attachPixelBuffer;
   present;

   while running loop
      declare
         ev : constant Message :=
            callDesktop (OP_INPUT_POLL, windowId, lastEvent, 0, 0);
      begin
         if ev.words (0) /= INPUT_NONE then
            lastEvent := ev.words (1);

            if ev.words (0) = INPUT_CONFIGURE then
               if ev.words (1) /= 0 then
                  windowW := ev.words (2);
                  windowH := ev.words (3);
               end if;
            elsif ev.words (0) = INPUT_KEY_DOWN then
               if ev.words (2) = KEY_ESC or else ev.words (2) = KEY_Q then
                  closeSession;
                  running := False;
               elsif ev.words (2) = KEY_R then
                  requestResize;
               end if;
            end if;
         end if;
      end;

      ignore := syscall (SYSCALL_SLEEP, 25);
   end loop;

   declare
   begin
      closeSession;
   end;

   ignore := syscall (SYSCALL_EXIT, 0);
end main;
