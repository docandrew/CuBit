------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  First desktop shell client
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := 16#0800#;
   OP_DESKTOP_BYE      : constant Unsigned_32 := 16#0801#;
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := 16#0802#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_RESIZE   : constant Unsigned_32 := 16#0813#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   SURFACE_FLAG_SHELL  : constant Unsigned_64 := 1;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;

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
   windowW   : Unsigned_64 := 360;
   windowH   : Unsigned_64 := 220;
   lastEvent : Unsigned_64 := 0;
   compact   : Boolean := False;
   running   : Boolean := True;
   sentBye   : Boolean := False;
   ignore    : Unsigned_64;

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

   procedure present is
      reply : Message;
      target : Unsigned_64 := surfaceId;
   begin
      if windowId /= 0 then
         target := windowId;
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
