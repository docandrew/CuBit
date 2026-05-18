------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop compositor/session service prototype
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Font8x16;

procedure main is
   use ASCII;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;

   EVENT_KEYBOARD : constant Unsigned_32 := 1;
   EVENT_MOUSE    : constant Unsigned_32 := 2;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := 16#0800#;
   OP_DESKTOP_BYE      : constant Unsigned_32 := 16#0801#;
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := 16#0802#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_DESTROY  : constant Unsigned_32 := 16#0811#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_RESIZE   : constant Unsigned_32 := 16#0813#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   UI_OK              : constant Unsigned_64 := 0;
   UI_ERR_DENIED      : constant Unsigned_64 := 1;
   UI_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   UI_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   UI_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   SURFACE_FLAG_SHELL : constant Unsigned_64 := 1;

   INPUT_NONE      : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN  : constant Unsigned_64 := 1;
   INPUT_KEY_UP    : constant Unsigned_64 := 2;
   INPUT_CONFIGURE : constant Unsigned_64 := 8;

   PROTOCOL_MAJOR : constant Unsigned_64 := 0;
   PROTOCOL_MINOR : constant Unsigned_64 := 1;
   PROTOCOL_VERSION : constant Unsigned_64 :=
      PROTOCOL_MAJOR or Shift_Left (PROTOCOL_MINOR, 32);

   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   fbAddr   : System.Address := System.Null_Address;

   type Surface is record
      used      : Boolean := False;
      owner     : ProcessID := NO_PROCESS;
      id        : Unsigned_64 := 0;
      x         : Natural := 0;
      y         : Natural := 0;
      w         : Natural := 0;
      h         : Natural := 0;
      flags     : Unsigned_64 := 0;
      serial    : Unsigned_64 := 0;
      dirty     : Boolean := True;
   end record;

   MAX_SURFACES : constant Natural := 8;
   subtype SurfaceIndex is Natural range 0 .. MAX_SURFACES - 1;
   type SurfaceTable is array (SurfaceIndex) of Surface;

   surfaces : SurfaceTable;
   nextSurfaceId : Unsigned_64 := 1;
   focusSurface  : Unsigned_64 := 0;
   nextInputSerial : Unsigned_64 := 1;
   inputOwned : Boolean := False;

   type PendingInput is record
      valid   : Boolean := False;
      serial  : Unsigned_64 := 0;
      kind    : Unsigned_64 := INPUT_NONE;
      target  : Unsigned_64 := 0;
      payload0 : Unsigned_64 := 0;
      payload1 : Unsigned_64 := 0;
   end record;

   inputEvent : PendingInput;

   C_BG     : constant Unsigned_32 := 16#0013_1518#;
   C_PANEL  : constant Unsigned_32 := 16#0022_272D#;
   C_TEXT   : constant Unsigned_32 := 16#00E8_ECEF#;
   C_MUTED  : constant Unsigned_32 := 16#0098_A2AD#;
   C_ACCENT : constant Unsigned_32 := 16#0037_B4D8#;
   C_GOOD   : constant Unsigned_32 := 16#0049_C070#;
   C_WHITE  : constant Unsigned_32 := 16#00FF_FFFF#;
   C_BLACK  : constant Unsigned_32 := 16#0000_0000#;
   C_DESK   : constant Unsigned_32 := 16#0000_8080#;
   C_BAR    : constant Unsigned_32 := 16#00C0_C0C0#;
   C_DARK   : constant Unsigned_32 := 16#0080_8080#;
   C_BLUE   : constant Unsigned_32 := 16#0000_0080#;

   procedure putPixel (x, y : Natural; color : Unsigned_32) is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
      pixel : Unsigned_32 with Import, Address => fbAddr + offset;
   begin
      if x < fbWidth and then y < fbHeight then
         pixel := color;
      end if;
   end putPixel;

   procedure fillRect (x, y, w, h : Natural; color : Unsigned_32) is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
   begin
      if w = 0 or else h = 0 or else x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      if maxX > fbWidth then
         maxX := fbWidth;
      end if;
      if maxY > fbHeight then
         maxY := fbHeight;
      end if;

      for yy in y .. maxY - 1 loop
         for xx in x .. maxX - 1 loop
            putPixel (xx, yy, color);
         end loop;
      end loop;
   end fillRect;

   procedure strokeRect
      (x, y, w, h : Natural; light : Unsigned_32; dark : Unsigned_32)
   is
   begin
      if w < 2 or else h < 2 then
         return;
      end if;

      fillRect (x, y, w, 1, light);
      fillRect (x, y, 1, h, light);
      fillRect (x, y + h - 1, w, 1, dark);
      fillRect (x + w - 1, y, 1, h, dark);
   end strokeRect;

   procedure drawGlyph
      (x, y : Natural;
       ch   : Character;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      glyph : Font8x16.GlyphData renames Font8x16.font (Character'Pos (ch));
   begin
      for row in 0 .. Font8x16.GLYPH_HEIGHT - 1 loop
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for bit in 0 .. Font8x16.GLYPH_WIDTH - 1 loop
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                  putPixel (x + bit, y + row, fg);
               else
                  putPixel (x + bit, y + row, bg);
               end if;
            end loop;
         end;
      end loop;
   end drawGlyph;

   procedure drawText
      (x, y : Natural;
       s    : String;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      cx : Natural := x;
   begin
      for i in s'Range loop
         if cx + Font8x16.GLYPH_WIDTH <= fbWidth then
            drawGlyph (cx, y, s (i), fg, bg);
         end if;
         cx := cx + Font8x16.GLYPH_WIDTH;
      end loop;
   end drawText;

   procedure drawSplash is
      panelW : constant Natural := 560;
      panelH : constant Natural := 170;
      x      : Natural := 40;
      y      : Natural := 40;
   begin
      if fbWidth > panelW then
         x := (fbWidth - panelW) / 2;
      end if;
      if fbHeight > panelH then
         y := (fbHeight - panelH) / 2;
      end if;

      fillRect (0, 0, fbWidth, fbHeight, C_BG);
      fillRect (x, y, panelW, panelH, C_PANEL);
      fillRect (x, y, panelW, 4, C_ACCENT);
      drawText (x + 24, y + 28, "CuBit desktop.svc", C_TEXT, C_PANEL);
      drawText (x + 24, y + 58,
                "display/input/session authority boundary",
                C_MUTED, C_PANEL);
      drawText (x + 24, y + 94,
                "registered as DRIVER_DESKTOP / @desktop",
                C_GOOD, C_PANEL);
      drawText (x + 24, y + 126,
                "Q or Esc exits this prototype owner",
                C_TEXT, C_PANEL);
   end drawSplash;

   procedure drawDesktopShell is
      taskbarH : constant Natural := 36;
      barY     : Natural := 0;
      shellW   : Natural := fbWidth;
      shellH   : Natural := fbHeight;
      startW   : constant Natural := 88;
      panelW   : constant Natural := 310;
      panelH   : constant Natural := 150;
      px       : Natural := 24;
      py       : Natural := 24;
   begin
      if fbWidth = 0 or else fbHeight = 0 then
         return;
      end if;

      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
         then
            shellW := surfaces (i).w;
            shellH := surfaces (i).h;
            exit;
         end if;
      end loop;

      if shellW > fbWidth then
         shellW := fbWidth;
      end if;
      if shellH > fbHeight then
         shellH := fbHeight;
      end if;

      if shellH > taskbarH then
         barY := shellH - taskbarH;
      end if;

      --  First shell renderer: deliberately Win95-simple. The compositor owns
      --  pixels for now; the shell owns policy and talks through the protocol.
      --  Shared client buffers can replace this drawing path later without
      --  changing the surface/session shape.
      fillRect (0, 0, fbWidth, fbHeight, C_BG);
      fillRect (0, 0, shellW, shellH, C_DESK);
      fillRect (0, barY, shellW, taskbarH, C_BAR);
      strokeRect (0, barY, shellW, taskbarH, C_WHITE, C_DARK);

      fillRect (6, barY + 6, startW, 24, C_BAR);
      strokeRect (6, barY + 6, startW, 24, C_WHITE, C_DARK);
      drawText (20, barY + 10, "Start", C_BLACK, C_BAR);

      if shellW > panelW + 48 and then shellH > panelH + taskbarH + 48 then
         px := (shellW - panelW) / 2;
         py := (shellH - taskbarH - panelH) / 2;
      end if;

      fillRect (px, py, panelW, panelH, C_BAR);
      strokeRect (px, py, panelW, panelH, C_WHITE, C_DARK);
      fillRect (px + 3, py + 3, panelW - 6, 22, C_BLUE);
      drawText (px + 10, py + 7, "CuBit Desktop", C_WHITE, C_BLUE);
      drawText (px + 18, py + 44, "desktop-shell.app connected",
                C_BLACK, C_BAR);
      drawText (px + 18, py + 70, "surface protocol: session + resize",
                C_BLACK, C_BAR);
      drawText (px + 18, py + 96, "Q or Esc exits desktop.svc",
                C_BLACK, C_BAR);

      drawText (16, 18, "Computer", C_WHITE, C_DESK);
      drawText (16, 48, "Security", C_WHITE, C_DESK);
   end drawDesktopShell;

   procedure redraw is
      shellVisible : Boolean := False;
   begin
      if fbBpp /= 32 then
         return;
      end if;

      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
         then
            shellVisible := True;
         end if;
      end loop;

      if shellVisible then
         drawDesktopShell;
      else
         drawSplash;
      end if;
   end redraw;

   function findSurface (id : Unsigned_64) return Integer is
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then surfaces (i).id = id then
            return Integer (i);
         end if;
      end loop;

      return -1;
   end findSurface;

   procedure queueConfigure (surfaceId, w, h : Unsigned_64) is
   begin
      inputEvent :=
        (valid    => True,
         serial   => nextInputSerial,
         kind     => INPUT_CONFIGURE,
         target   => surfaceId,
         payload0 => w,
         payload1 => h);
      nextInputSerial := nextInputSerial + 1;
   end queueConfigure;

   procedure queueKey (raw : Unsigned_8) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_64 := Unsigned_64 (raw and 16#7F#);
   begin
      if focusSurface = 0 then
         return;
      end if;

      inputEvent :=
        (valid    => True,
         serial   => nextInputSerial,
         kind     => (if release then INPUT_KEY_UP else INPUT_KEY_DOWN),
         target   => focusSurface,
         payload0 => code,
         payload1 => 0);
      nextInputSerial := nextInputSerial + 1;
   end queueKey;

   procedure claimInput is
      r : Unsigned_64;
   begin
      if inputOwned then
         return;
      end if;

      --  Manual bring-up rule: desktop.svc registers as the desktop endpoint
      --  at startup, but it does not steal keyboard/mouse focus until a real
      --  shell/client surface connects. That lets the CLI shell stay usable
      --  long enough to run `spawn desktop-shell.app`.
      r := registerDriver (DRIVER_KEYBOARD);
      if r = Unsigned_64'Last then
         debugPrint ("desktop: register keyboard failed" & LF);
      else
         debugPrint ("desktop: registered keyboard" & LF);
      end if;

      r := registerDriver (DRIVER_MOUSE);
      if r = Unsigned_64'Last then
         debugPrint ("desktop: register mouse failed" & LF);
      else
         debugPrint ("desktop: registered mouse" & LF);
      end if;

      inputOwned := True;
   end claimInput;

   procedure handleRequest (from : ProcessID; request : Message) is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      case request.tag.label is
         when OP_DESKTOP_HELLO =>
            replyMsg.tag := (label  => OP_DESKTOP_HELLO,
                             length => 4,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := 1; -- session id
            replyMsg.words (1) := 0; -- compositor flags
            replyMsg.words (2) := 8; -- max surfaces
            replyMsg.words (3) := PROTOCOL_VERSION;

         when OP_DESKTOP_GET_INFO =>
            replyMsg.tag := (label  => OP_DESKTOP_GET_INFO,
                             length => 4,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := Unsigned_64 (fbWidth);
            replyMsg.words (1) := Unsigned_64 (fbHeight);
            replyMsg.words (2) := 1; -- BGRA8888
            replyMsg.words (3) := 16#0001_0000#; -- scale 1.0 in 16.16

         when OP_SURFACE_CREATE =>
            declare
               slot : Integer := -1;
               reqW : Natural := Natural (request.words (0));
               reqH : Natural := Natural (request.words (1));
            begin
               for i in surfaces'Range loop
                  if not surfaces (i).used then
                     slot := Integer (i);
                     exit;
                  end if;
               end loop;

               if slot < 0 then
                  replyMsg.tag := (label  => OP_SURFACE_CREATE,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               else
                  if reqW = 0 or else reqW > fbWidth then
                     reqW := fbWidth;
                  end if;
                  if reqH = 0 or else reqH > fbHeight then
                     reqH := fbHeight;
                  end if;

                  surfaces (SurfaceIndex (slot)) :=
                    (used   => True,
                     owner  => from,
                     id     => nextSurfaceId,
                     x      => 0,
                     y      => 0,
                     w      => reqW,
                     h      => reqH,
                     flags  => request.words (2),
                     serial => 1,
                     dirty  => True);
                  focusSurface := nextSurfaceId;

                  replyMsg.tag := (label  => OP_SURFACE_CREATE,
                                   length => 4,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := nextSurfaceId;
                  replyMsg.words (1) := Unsigned_64 (reqW);
                  replyMsg.words (2) := Unsigned_64 (reqH);
                  replyMsg.words (3) := 1;

                  queueConfigure (nextSurfaceId,
                                  Unsigned_64 (reqW),
                                  Unsigned_64 (reqH));
                  nextSurfaceId := nextSurfaceId + 1;
                  claimInput;
                  redraw;
               end if;
            end;

         when OP_SURFACE_RESIZE =>
            declare
               idx  : constant Integer := findSurface (request.words (0));
               newW : Natural := Natural (request.words (1));
               newH : Natural := Natural (request.words (2));
            begin
               if idx < 0 then
                  replyMsg.tag := (label  => OP_SURFACE_RESIZE,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.tag := (label  => OP_SURFACE_RESIZE,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_DENIED;
               else
                  if newW = 0 or else newW > fbWidth then
                     newW := fbWidth;
                  end if;
                  if newH = 0 or else newH > fbHeight then
                     newH := fbHeight;
                  end if;

                  surfaces (SurfaceIndex (idx)).w := newW;
                  surfaces (SurfaceIndex (idx)).h := newH;
                  surfaces (SurfaceIndex (idx)).serial :=
                     surfaces (SurfaceIndex (idx)).serial + 1;
                  surfaces (SurfaceIndex (idx)).dirty := True;

                  replyMsg.tag := (label  => OP_SURFACE_RESIZE,
                                   length => 4,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_OK;
                  replyMsg.words (1) := Unsigned_64 (newW);
                  replyMsg.words (2) := Unsigned_64 (newH);
                  replyMsg.words (3) := surfaces (SurfaceIndex (idx)).serial;

                  queueConfigure (request.words (0),
                                  Unsigned_64 (newW),
                                  Unsigned_64 (newH));
                  redraw;
               end if;
            end;

         when OP_SURFACE_PRESENT =>
            replyMsg.tag := (label  => OP_SURFACE_PRESENT,
                             length => 1,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := UI_OK;
            redraw;

         when OP_SURFACE_DESTROY =>
            declare
               idx : constant Integer := findSurface (request.words (0));
            begin
               if idx < 0 then
                  replyMsg.tag := (label  => OP_SURFACE_DESTROY,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.tag := (label  => OP_SURFACE_DESTROY,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_DENIED;
               else
                  surfaces (SurfaceIndex (idx)) := (others => <>);
                  if focusSurface = request.words (0) then
                     focusSurface := 0;
                  end if;
                  replyMsg.tag := (label  => OP_SURFACE_DESTROY,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_OK;
                  redraw;
               end if;
            end;

         when OP_INPUT_POLL =>
            replyMsg.tag := (label  => OP_INPUT_POLL,
                             length => 4,
                             flags  => 0,
                             badge  => 0);
            if inputEvent.valid and then
               inputEvent.target = request.words (0) and then
               inputEvent.serial > request.words (1)
            then
               replyMsg.words (0) := inputEvent.kind;
               replyMsg.words (1) := inputEvent.serial;
               replyMsg.words (2) := inputEvent.payload0;
               replyMsg.words (3) := inputEvent.payload1;
               inputEvent.valid := False;
            else
               replyMsg.words (0) := INPUT_NONE;
               replyMsg.words (1) := request.words (1);
            end if;

         when OP_DESKTOP_BYE =>
            for i in surfaces'Range loop
               if surfaces (i).used and then surfaces (i).owner = from then
                  surfaces (i) := (others => <>);
               end if;
            end loop;
            if focusSurface /= 0 and then findSurface (focusSurface) < 0 then
               focusSurface := 0;
            end if;
            replyMsg.tag := (label  => OP_DESKTOP_BYE,
                             length => 1,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := UI_OK;
            redraw;

         when others =>
            replyMsg.tag := (label  => request.tag.label,
                             length => 1,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := UI_ERR_UNSUPPORTED;
      end case;

      ignore := reply (from, replyMsg);
   end handleRequest;

   function shouldExitKey (raw : Unsigned_8) return Boolean is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
   begin
      return (not release) and then (code = 16#01# or else code = 16#10#);
   end shouldExitKey;

   procedure handleEvent (eventMsg : Message; running : in out Boolean) is
      raw : Unsigned_8;
   begin
      if eventMsg.tag.label = EVENT_KEYBOARD then
         raw := Unsigned_8 (eventMsg.words (0) and 16#FF#);

         --  Once a shell/client surface has focus, keyboard events belong to
         --  that surface. The service-level Q/Esc escape remains available
         --  only before a client has connected, which keeps early bring-up
         --  recoverable without stealing application quit keys.
         if focusSurface /= 0 then
            queueKey (raw);
         elsif shouldExitKey (raw) then
            debugPrint ("desktop: exit key" & LF);
            running := False;
         end if;
      elsif eventMsg.tag.label = EVENT_MOUSE then
         null;
      end if;
   end handleEvent;

   ret      : Unsigned_64;
   from     : ProcessID;
   msg      : Message;
   found    : Boolean;
   running  : Boolean := True;
begin
   debugPrint ("desktop: starting" & LF);

   ret := registerDriver (DRIVER_DESKTOP);
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: register failed" & LF);
   end if;

   ret := syscall (SYSCALL_MAPFB);
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: MAPFB failed" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;

   fbAddr   := To_Address (Integer_Address (ret));
   fbWidth  := Natural (getInfo (SYSINFO_FB_WIDTH));
   fbHeight := Natural (getInfo (SYSINFO_FB_HEIGHT));
   fbPitch  := Natural (getInfo (SYSINFO_FB_PITCH));
   fbBpp    := Natural (getInfo (SYSINFO_FB_BPP));

   debugPrint ("desktop: waiting for shell client" & LF);

   while running loop
      declare
         eventMsg   : Message;
         eventFound : constant Boolean := Poll_Event (eventMsg);
      begin
         if eventFound then
            handleEvent (eventMsg, running);
         else
            Poll_Service_Request (from, msg, found);
            if found then
               handleRequest (from, msg);
            elsif syscall (SYSCALL_SLEEP, 10) = Unsigned_64'Last then
               null;
            end if;
         end if;
      end;
   end loop;

   if fbBpp = 32 then
      fillRect (0, 0, fbWidth, fbHeight, C_BG);
   end if;

   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
