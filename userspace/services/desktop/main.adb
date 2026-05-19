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

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := 16#0900#;
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := 16#0901#;
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := 16#0902#;
   OP_DISPLAY_CLEAR         : constant Unsigned_32 := 16#0903#;
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := 16#0904#;
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := 16#0905#;
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := 16#0906#;

   UI_OK              : constant Unsigned_64 := 0;
   UI_ERR_DENIED      : constant Unsigned_64 := 1;
   UI_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   UI_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   UI_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   SURFACE_FLAG_SHELL  : constant Unsigned_64 := 1;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;

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
   backBufferAddr : System.Address := System.Null_Address;
   backBufferGrant : Unsigned_64 := 0;

   --  Prototype compositor shadow framebuffer. All desktop drawing goes here
   --  first, then a completed frame is copied to the real framebuffer in one
   --  pass. That avoids exposing intermediate clear/background/window phases
   --  to the display and is the first step toward real compositor buffering.
   backBufferReady : Boolean := False;
   drawingBackBuffer : Boolean := False;

   type Rect is record
      x : Natural := 0;
      y : Natural := 0;
      w : Natural := 0;
      h : Natural := 0;
   end record;

   --  Damage clipping for compositor redraws. A full scene redraw with a clip
   --  rectangle lets existing drawing code repaint correct background/window
   --  ordering while touching only the region that changed.
   clipEnabled : Boolean := False;
   clipRect    : Rect;
   framePending : Boolean := False;
   frameDamage  : Rect;
   frameDueMs   : Unsigned_64 := 0;
   FRAME_INTERVAL_MS : constant Unsigned_64 := 16;

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

   cursorX : Natural := 80;
   cursorY : Natural := 80;
   lastButtons : Unsigned_64 := 0;
   launchMenuOpen : Boolean := False;

   DRAG_NONE        : constant Natural := 0;
   DRAG_MOVE        : constant Natural := 1;
   DRAG_RESIZE_E    : constant Natural := 2;
   DRAG_RESIZE_S    : constant Natural := 3;
   DRAG_RESIZE_SE   : constant Natural := 4;
   dragMode         : Natural := DRAG_NONE;
   dragSurfaceId    : Unsigned_64 := 0;
   dragOffsetX      : Natural := 0;
   dragOffsetY      : Natural := 0;
   dragPreviewValid : Boolean := False;
   dragPreviewRect  : Rect;

   TITLE_HEIGHT : constant Natural := 24;
   BORDER_SIZE  : constant Natural := 6;
   TASKBAR_H    : constant Natural := 36;
   LAUNCH_W     : constant Natural := 88;
   LAUNCH_H     : constant Natural := 24;
   MENU_W       : constant Natural := 232;
   MENU_H       : constant Natural := 154;
   CURSOR_W     : constant Natural := 12;
   CURSOR_H     : constant Natural := 18;
   CURSOR_PIXELS : constant Natural := CURSOR_W * CURSOR_H;
   MIN_WIN_W    : constant Natural := 120;
   MIN_WIN_H    : constant Natural := 80;

   type CursorSaveBuffer is array (Natural range 0 .. CURSOR_PIXELS - 1)
      of Unsigned_32;
   cursorSave      : CursorSaveBuffer := (others => 0);
   cursorSaveValid : Boolean := False;
   cursorSaveRect  : Rect;

   C_BG     : constant Unsigned_32 := 16#001F_2430#;
   C_PANEL  : constant Unsigned_32 := 16#0024_2936#;
   C_TEXT   : constant Unsigned_32 := 16#00CB_CAC2#;
   C_MUTED  : constant Unsigned_32 := 16#0070_7A8C#;
   C_ACCENT : constant Unsigned_32 := 16#00FF_CC66#;
   C_GOOD   : constant Unsigned_32 := 16#00BA_E67E#;
   C_WHITE  : constant Unsigned_32 := 16#00FF_FFFF#;
   C_BLACK  : constant Unsigned_32 := 16#0000_0000#;
   C_DESK   : constant Unsigned_32 := 16#001B_202B#;
   C_BAR    : constant Unsigned_32 := 16#0024_2936#;
   C_BLUE   : constant Unsigned_32 := 16#003F_6380#;
   C_WIN    : constant Unsigned_32 := 16#0028_2E3D#;
   C_EDGE   : constant Unsigned_32 := 16#004B_5263#;
   C_SHADOW : constant Unsigned_32 := 16#0014_1820#;

   statsStartMs      : Unsigned_64 := 0;
   statsEvents       : Unsigned_64 := 0;
   statsMouseEvents  : Unsigned_64 := 0;
   statsRequests     : Unsigned_64 := 0;
   statsFrames       : Unsigned_64 := 0;
   statsFullFrames   : Unsigned_64 := 0;
   statsDrawMs       : Unsigned_64 := 0;
   statsPresentOps   : Unsigned_64 := 0;
   statsPresentMs    : Unsigned_64 := 0;
   statsDamagePixels : Unsigned_64 := 0;

   procedure printDec (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;

      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;

      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   procedure maybePrintStats is
      now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      if now = Unsigned_64'Last then
         return;
      end if;

      if statsStartMs = 0 then
         statsStartMs := now;
         return;
      end if;

      if now < statsStartMs or else now - statsStartMs < 1000 then
         return;
      end if;

      if statsFrames > 0 or else statsEvents > 0 then
         debugPrint ("desktop: stats ev=");
         printDec (statsEvents);
         debugPrint (" mouse=");
         printDec (statsMouseEvents);
         debugPrint (" req=");
         printDec (statsRequests);
         debugPrint (" frames=");
         printDec (statsFrames);
         debugPrint (" full=");
         printDec (statsFullFrames);
         debugPrint (" draw_ms=");
         printDec (statsDrawMs);
         debugPrint (" submit=");
         printDec (statsPresentOps);
         debugPrint (" submit_ms=");
         printDec (statsPresentMs);
         debugPrint (" px=");
         printDec (statsDamagePixels);
         debugPrint ("" & LF);
      end if;

      statsStartMs := now;
      statsEvents := 0;
      statsMouseEvents := 0;
      statsRequests := 0;
      statsFrames := 0;
      statsFullFrames := 0;
      statsDrawMs := 0;
      statsPresentOps := 0;
      statsPresentMs := 0;
      statsDamagePixels := 0;
   end maybePrintStats;

   function nowMs return Unsigned_64 is
   begin
      return syscall (SYSCALL_GETTIME);
   end nowMs;

   procedure putPixel (x, y : Natural; color : Unsigned_32) is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
   begin
      if x < fbWidth and then y < fbHeight then
         if clipEnabled and then
            (x < clipRect.x or else y < clipRect.y or else
             x >= clipRect.x + clipRect.w or else
             y >= clipRect.y + clipRect.h)
         then
            return;
         end if;

         if drawingBackBuffer then
            declare
               pixel : Unsigned_32 with
                  Import, Address => backBufferAddr + offset;
            begin
               pixel := color;
            end;
         elsif backBufferAddr /= System.Null_Address then
            declare
               pixel : Unsigned_32 with
                  Import, Address => backBufferAddr + offset;
            begin
               pixel := color;
            end;
         else
            --  No display buffer is attached yet. This should only happen if
            --  desktop.svc was started before display.svc or grant setup
            --  failed; keep the compositor alive so bring-up remains debuggable.
            null;
         end if;
      end if;
   end putPixel;

   function readBackPixel (x, y : Natural) return Unsigned_32 is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
   begin
      if backBufferAddr = System.Null_Address or else
         x >= fbWidth or else y >= fbHeight
      then
         return 0;
      end if;

      declare
         pixel : Unsigned_32 with
            Import, Address => backBufferAddr + offset;
      begin
         return pixel;
      end;
   end readBackPixel;

   procedure writeBackPixel (x, y : Natural; color : Unsigned_32) is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
   begin
      if backBufferAddr = System.Null_Address or else
         x >= fbWidth or else y >= fbHeight
      then
         return;
      end if;

      declare
         pixel : Unsigned_32 with
            Import, Address => backBufferAddr + offset;
      begin
         pixel := color;
      end;
   end writeBackPixel;

   function Cursor_Core (xx, yy : Integer) return Boolean is
   begin
      if xx < 0 or else yy < 0 or else
         xx >= Integer (CURSOR_W) or else yy >= Integer (CURSOR_H)
      then
         return False;
      end if;

      --  Keep the cursor intentionally chunky while the compositor is still
      --  using software damage tracking. A one-pixel cursor is easy to lose
      --  on light UI chrome and makes partial-present bugs hard to see.
      return
         (yy >= 1 and then yy < 15 and then
          xx >= 1 and then xx <= yy / 2 + 1) or else
         (yy >= 10 and then yy < 18 and then
          xx >= 4 and then xx <= 6);
   end Cursor_Core;

   function Cursor_Near_Core (xx, yy : Integer) return Boolean is
   begin
      for oy in -1 .. 1 loop
         for ox in -1 .. 1 loop
            if Cursor_Core (xx + ox, yy + oy) then
               return True;
            end if;
         end loop;
      end loop;

      return False;
   end Cursor_Near_Core;

   function isEmpty (r : Rect) return Boolean is
   begin
      return r.w = 0 or else r.h = 0;
   end isEmpty;

   function clampRect (r : Rect) return Rect is
      x2 : Natural := r.x + r.w;
      y2 : Natural := r.y + r.h;
   begin
      if r.w = 0 or else r.h = 0 or else
         r.x >= fbWidth or else r.y >= fbHeight
      then
         return (others => 0);
      end if;

      if x2 > fbWidth then
         x2 := fbWidth;
      end if;
      if y2 > fbHeight then
         y2 := fbHeight;
      end if;

      return (x => r.x, y => r.y, w => x2 - r.x, h => y2 - r.y);
   end clampRect;

   function cursorRect return Rect is
   begin
      return clampRect ((x => cursorX, y => cursorY,
                         w => CURSOR_W, h => CURSOR_H));
   end cursorRect;

   function taskbarY return Natural is
   begin
      if fbHeight > TASKBAR_H then
         return fbHeight - TASKBAR_H;
      else
         return 0;
      end if;
   end taskbarY;

   function launchButtonRect return Rect is
   begin
      return clampRect ((x => 6, y => taskbarY + 6,
                         w => LAUNCH_W, h => LAUNCH_H));
   end launchButtonRect;

   function launchMenuRect return Rect is
      y : Natural := 0;
   begin
      if taskbarY > MENU_H then
         y := taskbarY - MENU_H;
      end if;

      return clampRect ((x => 6, y => y, w => MENU_W, h => MENU_H));
   end launchMenuRect;

   function pointInRect (x, y : Natural; r : Rect) return Boolean is
   begin
      return not isEmpty (r) and then
         x >= r.x and then y >= r.y and then
         x < r.x + r.w and then y < r.y + r.h;
   end pointInRect;

   function unionRect (a, b : Rect) return Rect is
      ax2 : constant Natural := a.x + a.w;
      ay2 : constant Natural := a.y + a.h;
      bx2 : constant Natural := b.x + b.w;
      by2 : constant Natural := b.y + b.h;
      x1  : Natural;
      y1  : Natural;
      x2  : Natural;
      y2  : Natural;
   begin
      if isEmpty (a) then
         return b;
      elsif isEmpty (b) then
         return a;
      end if;

      x1 := Natural'Min (a.x, b.x);
      y1 := Natural'Min (a.y, b.y);
      x2 := Natural'Max (ax2, bx2);
      y2 := Natural'Max (ay2, by2);
      return (x => x1, y => y1, w => x2 - x1, h => y2 - y1);
   end unionRect;

   function inflateRect (r : Rect; amount : Natural) return Rect is
      x1 : Natural := r.x;
      y1 : Natural := r.y;
      x2 : Natural := r.x + r.w;
      y2 : Natural := r.y + r.h;
   begin
      if isEmpty (r) then
         return r;
      end if;

      if x1 > amount then
         x1 := x1 - amount;
      else
         x1 := 0;
      end if;

      if y1 > amount then
         y1 := y1 - amount;
      else
         y1 := 0;
      end if;

      x2 := Natural'Min (fbWidth, x2 + amount);
      y2 := Natural'Min (fbHeight, y2 + amount);

      return (x => x1, y => y1, w => x2 - x1, h => y2 - y1);
   end inflateRect;

   function surfaceRect (s : Surface) return Rect is
   begin
      return clampRect ((x => s.x, y => s.y, w => s.w, h => s.h));
   end surfaceRect;

   function signed12 (x : Unsigned_64) return Integer is
      v : constant Unsigned_64 := x and 16#FFF#;
   begin
      if (v and 16#800#) /= 0 then
         return Integer (v) - 4096;
      else
         return Integer (v);
      end if;
   end signed12;

   function hitSurface (x, y : Natural) return Integer is
   begin
      for i in reverse surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0 and then
            x >= surfaces (i).x and then y >= surfaces (i).y and then
            x < surfaces (i).x + surfaces (i).w and then
            y < surfaces (i).y + surfaces (i).h
         then
            return Integer (i);
         end if;
      end loop;

      return -1;
   end hitSurface;

   function hitMode (s : Surface; x, y : Natural) return Natural is
      onRight  : constant Boolean :=
         x + BORDER_SIZE >= s.x + s.w;
      onBottom : constant Boolean :=
         y + BORDER_SIZE >= s.y + s.h;
      inTitle  : constant Boolean :=
         y >= s.y and then y < s.y + TITLE_HEIGHT;
   begin
      if onRight and then onBottom then
         return DRAG_RESIZE_SE;
      elsif onRight then
         return DRAG_RESIZE_E;
      elsif onBottom then
         return DRAG_RESIZE_S;
      elsif inTitle then
         return DRAG_MOVE;
      else
         return DRAG_NONE;
      end if;
   end hitMode;

   procedure flushBackBufferRect (dirty : Rect) is
      r : constant Rect := clampRect (dirty);
      msg : Message :=
        (tag      => (label  => OP_DISPLAY_PRESENT_RECT,
                      length => 4,
                      flags  => 0,
                      badge  => 0),
         capBadge => 0,
         words    => (Unsigned_64 (r.x),
                      Unsigned_64 (r.y),
                      Unsigned_64 (r.w),
                      Unsigned_64 (r.h)));
      t0 : Unsigned_64;
      t1 : Unsigned_64;
   begin
      if not backBufferReady or else fbBpp /= 32 then
         return;
      end if;
      if isEmpty (r) then
         return;
      end if;

      --  Present is delegated to display.svc, the sole scanout owner. Keeping
      --  display timing in one service gives us a clean place for vblank waits,
      --  page flips, and frame-deadline scheduling.
      --
      --  This service currently has one mutable backbuffer, so use the
      --  synchronous present form. Async present needs either buffer rotation
      --  or a returned fence/completion before we can safely draw the next
      --  frame without smearing cursor/window damage.
      t0 := syscall (SYSCALL_GETTIME);
      msg.tag := capCall (CAP_SLOT_DISPLAY, msg);
      t1 := syscall (SYSCALL_GETTIME);

      statsPresentOps := statsPresentOps + 1;
      if t0 /= Unsigned_64'Last and then t1 /= Unsigned_64'Last and then
         t1 >= t0
      then
         statsPresentMs := statsPresentMs + (t1 - t0);
      end if;
   end flushBackBufferRect;

   procedure fillRect (x, y, w, h : Natural; color : Unsigned_32) is
      minX : Natural := x;
      minY : Natural := y;
      maxX : Natural := x + w;
      maxY : Natural := y + h;
   begin
      if w = 0 or else h = 0 or else x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      if clipEnabled then
         if minX < clipRect.x then
            minX := clipRect.x;
         end if;
         if minY < clipRect.y then
            minY := clipRect.y;
         end if;
         if maxX > clipRect.x + clipRect.w then
            maxX := clipRect.x + clipRect.w;
         end if;
         if maxY > clipRect.y + clipRect.h then
            maxY := clipRect.y + clipRect.h;
         end if;
      end if;

      if maxX > fbWidth then
         maxX := fbWidth;
      end if;
      if maxY > fbHeight then
         maxY := fbHeight;
      end if;
      if minX >= maxX or else minY >= maxY then
         return;
      end if;

      if backBufferAddr = System.Null_Address then
         return;
      end if;

      --  Rect fills dominate compositor redraws. Do clipping and target
      --  selection once, then write the clipped rows directly instead of
      --  paying putPixel's bounds/clip checks for every pixel.
      for yy in minY .. maxY - 1 loop
         declare
            rowOffset : constant Storage_Offset :=
               Storage_Offset (yy * fbPitch + minX * 4);
         begin
         for xx in minX .. maxX - 1 loop
            declare
               pixel : Unsigned_32 with
                  Import, Address =>
                     backBufferAddr + rowOffset +
                     Storage_Offset ((xx - minX) * 4);
            begin
               pixel := color;
            end;
         end loop;
         end;
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

   procedure restoreCursorOverlay is
   begin
      if not cursorSaveValid then
         return;
      end if;

      --  The cursor is an overlay, not part of the scene. Restore the saved
      --  pixels before repainting scene damage or moving the cursor, so window
      --  redraws never have to know where the pointer was.
      for yy in 0 .. cursorSaveRect.h - 1 loop
         for xx in 0 .. cursorSaveRect.w - 1 loop
            writeBackPixel
              (cursorSaveRect.x + xx,
               cursorSaveRect.y + yy,
               cursorSave (yy * CURSOR_W + xx));
         end loop;
      end loop;

      cursorSaveValid := False;
   end restoreCursorOverlay;

   procedure drawCursorOverlay is
      r : constant Rect := cursorRect;
   begin
      if isEmpty (r) then
         return;
      end if;

      --  Save the clean scene pixels under the cursor before drawing it. This
      --  is the software version of a hardware cursor plane: motion restores a
      --  tiny old rectangle and draws a tiny new one instead of repainting the
      --  full compositor scene.
      for yy in 0 .. r.h - 1 loop
         for xx in 0 .. r.w - 1 loop
            cursorSave (yy * CURSOR_W + xx) :=
               readBackPixel (r.x + xx, r.y + yy);
         end loop;
      end loop;

      cursorSaveRect := r;
      cursorSaveValid := True;

      for yy in 0 .. r.h - 1 loop
         for xx in 0 .. r.w - 1 loop
            if Cursor_Core (xx, yy) then
               writeBackPixel (r.x + xx, r.y + yy, C_WHITE);
            elsif Cursor_Near_Core (xx, yy) then
               writeBackPixel (r.x + xx, r.y + yy, C_BLACK);
            end if;
         end loop;
      end loop;
   end drawCursorOverlay;

   procedure moveCursorOverlay (oldCursor : Rect) is
      newCursor : constant Rect := cursorRect;
      damage    : Rect := unionRect (oldCursor, newCursor);
   begin
      restoreCursorOverlay;
      drawCursorOverlay;
      damage := inflateRect (damage, 1);
      flushBackBufferRect (damage);
   end moveCursorOverlay;

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

   procedure drawWindow (s : Surface) is
      titleH : constant Natural := 24;
      minW   : constant Natural := 80;
      minH   : constant Natural := 60;
      x      : Natural := s.x;
      y      : Natural := s.y;
      w      : Natural := s.w;
      h      : Natural := s.h;
   begin
      if w < minW then
         w := minW;
      end if;
      if h < minH then
         h := minH;
      end if;

      if x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      fillRect (x + 3, y + 3, w, h, C_SHADOW);
      fillRect (x, y, w, h, C_WIN);
      strokeRect (x, y, w, h, C_EDGE, C_SHADOW);
      fillRect (x + 3, y + 3, w - 6, titleH, C_BLUE);
      drawText (x + 10, y + 7, "Demo Window", C_WHITE, C_BLUE);

      --  Close box placeholder. It is drawn by the compositor because close,
      --  move, resize, and focus are all authority-sensitive window mechanics.
      fillRect (x + w - 23, y + 6, 14, 14, C_BAR);
      strokeRect (x + w - 23, y + 6, 14, 14, C_EDGE, C_SHADOW);
      drawText (x + w - 20, y + 5, "x", C_TEXT, C_BAR);

      drawText (x + 18, y + 44, "This is a real child surface.",
                C_TEXT, C_WIN);
      drawText (x + 18, y + 70, "Drag title bar to move.",
                C_TEXT, C_WIN);
      drawText (x + 18, y + 96, "Drag edges to resize.",
                C_TEXT, C_WIN);
   end drawWindow;

   procedure drawDragOutline is
      r : constant Rect := clampRect (dragPreviewRect);
   begin
      if not dragPreviewValid or else isEmpty (r) then
         return;
      end if;

      --  Classic low-cost shell behavior: during move/resize we draw a
      --  compositor-owned preview rectangle and commit the real surface only
      --  when the button is released. That keeps interactive drag latency from
      --  depending on repainting and copying the entire window every tick.
      strokeRect (r.x, r.y, r.w, r.h, C_WHITE, C_BLACK);
      if r.w > 4 and then r.h > 4 then
         strokeRect (r.x + 2, r.y + 2, r.w - 4, r.h - 4, C_BLACK, C_WHITE);
      end if;
   end drawDragOutline;

   procedure drawLaunchMenu is
      r : constant Rect := launchMenuRect;
   begin
      if not launchMenuOpen or else isEmpty (r) then
         return;
      end if;

      fillRect (r.x + 3, r.y + 3, r.w, r.h, C_SHADOW);
      fillRect (r.x, r.y, r.w, r.h, C_PANEL);
      strokeRect (r.x, r.y, r.w, r.h, C_EDGE, C_SHADOW);
      fillRect (r.x, r.y, 4, r.h, C_ACCENT);

      drawText (r.x + 18, r.y + 14, "CuBit", C_TEXT, C_PANEL);
      drawText (r.x + 18, r.y + 42, "Terminal", C_TEXT, C_PANEL);
      drawText (r.x + 18, r.y + 68, "Files", C_MUTED, C_PANEL);
      drawText (r.x + 18, r.y + 94, "Security Center", C_MUTED, C_PANEL);
      fillRect (r.x + 12, r.y + 122, r.w - 24, 1, C_EDGE);
      drawText (r.x + 18, r.y + 130, "Power", C_MUTED, C_PANEL);
   end drawLaunchMenu;

   procedure drawDesktopShell is
      barY     : constant Natural := taskbarY;
      launch   : constant Rect := launchButtonRect;
      panelW   : constant Natural := 310;
      panelH   : constant Natural := 150;
      px       : Natural := 24;
      py       : Natural := 24;
   begin
      if fbWidth = 0 or else fbHeight = 0 then
         return;
      end if;

      --  First shell renderer: deliberately Win95-simple. The compositor owns
      --  pixels for now; the shell owns policy and talks through the protocol.
      --  Shared client buffers can replace this drawing path later without
      --  changing the surface/session shape.
      fillRect (0, 0, fbWidth, fbHeight, C_DESK);
      fillRect (0, barY, fbWidth, TASKBAR_H, C_BAR);
      strokeRect (0, barY, fbWidth, TASKBAR_H, C_EDGE, C_SHADOW);
      fillRect (0, barY, fbWidth, 2, C_ACCENT);

      fillRect (launch.x, launch.y, launch.w, launch.h, C_BAR);
      if launchMenuOpen then
         strokeRect (launch.x, launch.y, launch.w, launch.h, C_SHADOW, C_EDGE);
      else
         strokeRect (launch.x, launch.y, launch.w, launch.h, C_EDGE, C_SHADOW);
      end if;
      drawText (18, barY + 10, "Launch", C_TEXT, C_BAR);

      if fbWidth > panelW + 48 and then fbHeight > panelH + TASKBAR_H + 48 then
         px := (fbWidth - panelW) / 2;
         py := (fbHeight - TASKBAR_H - panelH) / 2;
      end if;

      fillRect (px, py, panelW, panelH, C_BAR);
      strokeRect (px, py, panelW, panelH, C_EDGE, C_SHADOW);
      fillRect (px + 3, py + 3, panelW - 6, 22, C_BLUE);
      drawText (px + 10, py + 7, "CuBit Desktop", C_WHITE, C_BLUE);
      drawText (px + 18, py + 44, "desktop-shell.app connected",
                C_TEXT, C_BAR);
      drawText (px + 18, py + 70, "surface protocol: child window",
                C_TEXT, C_BAR);
      drawText (px + 18, py + 96, "Q or Esc exits desktop shell",
                C_TEXT, C_BAR);

      drawText (16, 18, "System", C_WHITE, C_DESK);
      drawText (16, 48, "Authority", C_WHITE, C_DESK);

      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            drawWindow (surfaces (i));
         end if;
      end loop;

      drawLaunchMenu;
   end drawDesktopShell;

   procedure drawCurrentScene is
      shellVisible : Boolean := False;
   begin
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
      drawDragOutline;
   end drawCurrentScene;

   procedure redraw is
   begin
      if fbBpp /= 32 then
         return;
      end if;

      restoreCursorOverlay;
      clipEnabled := False;
      drawingBackBuffer := backBufferReady;

      drawCurrentScene;
      drawCursorOverlay;

      if drawingBackBuffer then
         drawingBackBuffer := False;
         flushBackBufferRect ((x => 0, y => 0, w => fbWidth, h => fbHeight));
      end if;
   end redraw;

   procedure redrawRect (dirty : Rect) is
      r : constant Rect := clampRect (dirty);
   begin
      if fbBpp /= 32 or else isEmpty (r) then
         return;
      end if;

      restoreCursorOverlay;
      clipRect := r;
      clipEnabled := True;
      drawingBackBuffer := backBufferReady;

      drawCurrentScene;
      drawCursorOverlay;

      if drawingBackBuffer then
         drawingBackBuffer := False;
         flushBackBufferRect (unionRect (r, cursorRect));
      end if;

      clipEnabled := False;
   end redrawRect;

   procedure scheduleRedraw is
      now : constant Unsigned_64 := nowMs;
   begin
      framePending := True;
      frameDamage := (x => 0, y => 0, w => fbWidth, h => fbHeight);
      if now /= Unsigned_64'Last then
         frameDueMs := now;
      else
         frameDueMs := 0;
      end if;
   end scheduleRedraw;

   procedure scheduleRedrawRect (dirty : Rect; defer : Boolean := False) is
      r : constant Rect := clampRect (dirty);
      now : constant Unsigned_64 := nowMs;
      due : Unsigned_64 := 0;
   begin
      if isEmpty (r) then
         return;
      end if;

      if now /= Unsigned_64'Last then
         if defer then
            due := now + FRAME_INTERVAL_MS;
         else
            due := now;
         end if;
      end if;

      if framePending then
         frameDamage := unionRect (frameDamage, r);
         --  Non-deferred damage, such as surface creation/resize, should not
         --  wait behind a mouse coalescing deadline.
         if not defer then
            frameDueMs := due;
         end if;
      else
         frameDamage := r;
         framePending := True;
         frameDueMs := due;
      end if;
   end scheduleRedrawRect;

   procedure flushFrame is
      damage : constant Rect := frameDamage;
      now : constant Unsigned_64 := nowMs;
      t0 : Unsigned_64;
      t1 : Unsigned_64;
      full : constant Boolean :=
         damage.x = 0 and then damage.y = 0 and then
         damage.w = fbWidth and then damage.h = fbHeight;
   begin
      if not framePending then
         return;
      end if;
      if now /= Unsigned_64'Last and then frameDueMs /= 0 and then
         now < frameDueMs
      then
         return;
      end if;

      framePending := False;
      frameDamage := (others => 0);
      frameDueMs := 0;

      t0 := syscall (SYSCALL_GETTIME);
      if full then
         redraw;
      else
         redrawRect (damage);
      end if;
      t1 := syscall (SYSCALL_GETTIME);

      statsFrames := statsFrames + 1;
      if full then
         statsFullFrames := statsFullFrames + 1;
      end if;
      statsDamagePixels :=
         statsDamagePixels + Unsigned_64 (damage.w) * Unsigned_64 (damage.h);
      if t0 /= Unsigned_64'Last and then t1 /= Unsigned_64'Last and then
         t1 >= t0
      then
         statsDrawMs := statsDrawMs + (t1 - t0);
      end if;
   end flushFrame;

   function findSurface (id : Unsigned_64) return Integer is
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then surfaces (i).id = id then
            return Integer (i);
         end if;
      end loop;

      return -1;
   end findSurface;

   function anySurfaceUsed return Boolean is
   begin
      for i in surfaces'Range loop
         if surfaces (i).used then
            return True;
         end if;
      end loop;

      return False;
   end anySurfaceUsed;

   function shellSurfaceVisible return Boolean is
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
         then
            return True;
         end if;
      end loop;

      return False;
   end shellSurfaceVisible;

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

   function callDisplay
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message;

   procedure setupDisplayBuffer (ok : out Boolean);
   procedure releaseDisplayBuffer;

   procedure handleRequest (from : ProcessID; request : Message) is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      statsRequests := statsRequests + 1;

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
               surfX : Natural := 0;
               surfY : Natural := 0;
               displayReady : Boolean := True;
            begin
               if not backBufferReady then
                  setupDisplayBuffer (displayReady);
               end if;

               for i in surfaces'Range loop
                  if not surfaces (i).used then
                     slot := Integer (i);
                     exit;
                  end if;
               end loop;

               if not displayReady then
                  replyMsg.tag := (label  => OP_SURFACE_CREATE,
                                   length => 1,
                                   flags  => 0,
                                   badge  => 0);
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               elsif slot < 0 then
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

                  if (request.words (2) and SURFACE_FLAG_WINDOW) /= 0 then
                     surfX := 80 + Natural (slot) * 18;
                     surfY := 64 + Natural (slot) * 18;
                     if reqW = fbWidth or else reqW < 220 then
                        reqW := 360;
                     end if;
                     if reqH = fbHeight or else reqH < 140 then
                        reqH := 220;
                     end if;
                     if surfX + reqW > fbWidth then
                        reqW := fbWidth - surfX;
                     end if;
                     if surfY + reqH > fbHeight then
                        reqH := fbHeight - surfY;
                     end if;
                  end if;

                  surfaces (SurfaceIndex (slot)) :=
                    (used   => True,
                     owner  => from,
                     id     => nextSurfaceId,
                     x      => surfX,
                     y      => surfY,
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
                  scheduleRedraw;
               end if;
            end;

         when OP_SURFACE_RESIZE =>
            declare
               idx  : constant Integer := findSurface (request.words (0));
               newW : Natural := Natural (request.words (1));
               newH : Natural := Natural (request.words (2));
               oldBounds : Rect;
               newBounds : Rect;
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
                  if surfaces (SurfaceIndex (idx)).x + newW > fbWidth then
                     newW := fbWidth - surfaces (SurfaceIndex (idx)).x;
                  end if;
                  if surfaces (SurfaceIndex (idx)).y + newH > fbHeight then
                     newH := fbHeight - surfaces (SurfaceIndex (idx)).y;
                  end if;

                  oldBounds := surfaceRect (surfaces (SurfaceIndex (idx)));
                  surfaces (SurfaceIndex (idx)).w := newW;
                  surfaces (SurfaceIndex (idx)).h := newH;
                  surfaces (SurfaceIndex (idx)).serial :=
                     surfaces (SurfaceIndex (idx)).serial + 1;
                  surfaces (SurfaceIndex (idx)).dirty := True;
                  newBounds := surfaceRect (surfaces (SurfaceIndex (idx)));

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
                  scheduleRedrawRect
                    (inflateRect (unionRect (oldBounds, newBounds), 4));
               end if;
            end;

         when OP_SURFACE_PRESENT =>
            replyMsg.tag := (label  => OP_SURFACE_PRESENT,
                             length => 1,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := UI_OK;
            declare
               idx : constant Integer := findSurface (request.words (0));
            begin
               if idx >= 0 and then
                  (surfaces (SurfaceIndex (idx)).flags and
                   SURFACE_FLAG_SHELL) = 0
               then
                  scheduleRedrawRect
                    (inflateRect
                       (surfaceRect (surfaces (SurfaceIndex (idx))), 4));
               else
                  scheduleRedraw;
               end if;
            end;

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
                  if anySurfaceUsed then
                     scheduleRedraw;
                  else
                     releaseDisplayBuffer;
                  end if;
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
            if anySurfaceUsed then
               scheduleRedraw;
            else
               releaseDisplayBuffer;
            end if;

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

   function clampPointerCoord (value, maxValue : Integer) return Natural is
   begin
      if value < 0 then
         return 0;
      elsif value > maxValue then
         return Natural (maxValue);
      else
         return Natural (value);
      end if;
   end clampPointerCoord;

   function clampWindowRect (r : Rect) return Rect is
      ret : Rect := r;
   begin
      if ret.w < MIN_WIN_W then
         ret.w := MIN_WIN_W;
      end if;
      if ret.h < MIN_WIN_H then
         ret.h := MIN_WIN_H;
      end if;

      if ret.w > fbWidth then
         ret.w := fbWidth;
      end if;
      if ret.h > fbHeight then
         ret.h := fbHeight;
      end if;

      if ret.x + ret.w > fbWidth then
         ret.x := fbWidth - ret.w;
      end if;
      if ret.y + ret.h > fbHeight then
         ret.y := fbHeight - ret.h;
      end if;

      return ret;
   end clampWindowRect;

   function previewRectFromPointer (s : Surface) return Rect is
      r : Rect := dragPreviewRect;
   begin
      case dragMode is
         when DRAG_MOVE =>
            if cursorX > dragOffsetX then
               r.x := cursorX - dragOffsetX;
            else
               r.x := 0;
            end if;
            if cursorY > dragOffsetY then
               r.y := cursorY - dragOffsetY;
            else
               r.y := 0;
            end if;

         when DRAG_RESIZE_E | DRAG_RESIZE_SE =>
            if cursorX > s.x + MIN_WIN_W then
               r.w := cursorX - s.x;
            else
               r.w := MIN_WIN_W;
            end if;

         when others =>
            null;
      end case;

      if dragMode = DRAG_RESIZE_S or else dragMode = DRAG_RESIZE_SE then
         if cursorY > s.y + MIN_WIN_H then
            r.h := cursorY - s.y;
         else
            r.h := MIN_WIN_H;
         end if;
      end if;

      return clampWindowRect (r);
   end previewRectFromPointer;

   procedure handleMouseMotion
      (buttons : Unsigned_64;
       dx      : Integer;
       dy      : Integer)
   is
      oldCursor : constant Rect := cursorRect;
      oldBounds : Rect := (others => 0);
      newBounds : Rect := (others => 0);
      damage    : Rect := oldCursor;
      idx       : Integer;
      leftDown  : constant Boolean := (buttons and 1) /= 0;
      leftWasDown : constant Boolean := (lastButtons and 1) /= 0;
      sceneDamage : constant Boolean := leftDown or else leftWasDown;
      handledChromeClick : Boolean := False;
      maxX      : Integer := 0;
      maxY      : Integer := 0;
   begin
      if fbWidth > CURSOR_W then
         maxX := Integer (fbWidth - CURSOR_W);
      end if;
      if fbHeight > CURSOR_H then
         maxY := Integer (fbHeight - CURSOR_H);
      end if;

      --  PS/2 reports positive Y as upward motion; screen coordinates grow
      --  downward.
      cursorX := clampPointerCoord (Integer (cursorX) + dx, maxX);
      cursorY := clampPointerCoord (Integer (cursorY) - dy, maxY);
      damage := unionRect (damage, cursorRect);

      if leftDown and then not leftWasDown then
         if shellSurfaceVisible and then
            pointInRect (cursorX, cursorY, launchButtonRect)
         then
            launchMenuOpen := not launchMenuOpen;
            damage := unionRect
              (damage,
               inflateRect (unionRect (launchButtonRect, launchMenuRect), 4));
            handledChromeClick := True;
         elsif launchMenuOpen and then
            pointInRect (cursorX, cursorY, launchMenuRect)
         then
            --  Menu entries are visual placeholders until desktop-shell.app
            --  grows a launcher protocol. Close the menu to acknowledge the
            --  click without granting any process-launch authority yet.
            launchMenuOpen := False;
            damage := unionRect (damage, inflateRect (launchMenuRect, 4));
            handledChromeClick := True;
         elsif launchMenuOpen then
            launchMenuOpen := False;
            damage := unionRect (damage, inflateRect (launchMenuRect, 4));
         end if;

         idx := hitSurface (cursorX, cursorY);
         if not handledChromeClick and then idx >= 0 then
            focusSurface := surfaces (SurfaceIndex (idx)).id;
            dragMode := hitMode (surfaces (SurfaceIndex (idx)), cursorX, cursorY);
            dragSurfaceId := focusSurface;
            dragOffsetX := cursorX - surfaces (SurfaceIndex (idx)).x;
            dragOffsetY := cursorY - surfaces (SurfaceIndex (idx)).y;
            dragPreviewRect := surfaceRect (surfaces (SurfaceIndex (idx)));
            dragPreviewValid := dragMode /= DRAG_NONE;
            if dragPreviewValid then
               damage := unionRect (damage, inflateRect (dragPreviewRect, 4));
            end if;
         end if;
      elsif not leftDown and then leftWasDown and then
            dragMode /= DRAG_NONE and then dragSurfaceId /= 0
      then
         idx := findSurface (dragSurfaceId);
         if idx >= 0 and then dragPreviewValid then
            oldBounds := surfaceRect (surfaces (SurfaceIndex (idx)));
            newBounds := clampWindowRect (dragPreviewRect);

            surfaces (SurfaceIndex (idx)).x := newBounds.x;
            surfaces (SurfaceIndex (idx)).y := newBounds.y;
            surfaces (SurfaceIndex (idx)).w := newBounds.w;
            surfaces (SurfaceIndex (idx)).h := newBounds.h;
            surfaces (SurfaceIndex (idx)).serial :=
               surfaces (SurfaceIndex (idx)).serial + 1;
            queueConfigure (surfaces (SurfaceIndex (idx)).id,
                            Unsigned_64 (newBounds.w),
                            Unsigned_64 (newBounds.h));

            damage := unionRect (damage,
                       inflateRect (unionRect (oldBounds, newBounds), 4));
         end if;

         dragPreviewValid := False;
         dragMode := DRAG_NONE;
         dragSurfaceId := 0;
      elsif not leftDown then
         dragPreviewValid := False;
         dragMode := DRAG_NONE;
         dragSurfaceId := 0;
      end if;

      if leftDown and then dragMode /= DRAG_NONE and then dragSurfaceId /= 0 then
         idx := findSurface (dragSurfaceId);
         if idx >= 0 then
            oldBounds := dragPreviewRect;
            dragPreviewRect :=
               previewRectFromPointer (surfaces (SurfaceIndex (idx)));
            newBounds := dragPreviewRect;
            damage := unionRect (damage,
                       inflateRect (unionRect (oldBounds, newBounds), 4));
         end if;
      end if;

      lastButtons := buttons;
      if sceneDamage or else framePending then
         scheduleRedrawRect (inflateRect (damage, 2), defer => True);
      else
         moveCursorOverlay (oldCursor);
      end if;
   end handleMouseMotion;

   procedure handleEvent (eventMsg : Message; running : in out Boolean) is
      raw : Unsigned_8;
      packed : Unsigned_64;
   begin
      statsEvents := statsEvents + 1;

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
         statsMouseEvents := statsMouseEvents + 1;
         packed := eventMsg.words (0);
         handleMouseMotion
           (buttons => packed and 16#FF#,
            dx      => signed12 (Shift_Right (packed, 8)),
            dy      => signed12 (Shift_Right (packed, 20)));
      end if;
   end handleEvent;

   function callDisplay
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label => label, length => 4, flags => 0, badge => 0),
         capBadge => 0,
         words    => (w0, w1, w2, w3));
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_DISPLAY, msg);
      msg.tag := tag;
      return msg;
   end callDisplay;

   procedure releaseDisplayBuffer is
      released : Message;
   begin
      if not backBufferReady then
         return;
      end if;

      restoreCursorOverlay;
      released := callDisplay (OP_DISPLAY_RELEASE);
      if released.tag.length >= 1 and then released.words (0) /= 0 then
         debugPrint ("desktop: display release failed" & LF);
      end if;

      backBufferReady := False;
      drawingBackBuffer := False;
      cursorSaveValid := False;
      framePending := False;
      frameDamage := (others => 0);
      frameDueMs := 0;
      inputEvent.valid := False;
   end releaseDisplayBuffer;

   function alignUpPage (addr : Unsigned_64) return Unsigned_64 is
   begin
      return (addr + 4095) and not Unsigned_64'(4095);
   end alignUpPage;

   procedure setupDisplayBuffer (ok : out Boolean) is
      info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
      acquire : Message;
      bytes : Unsigned_64;
      pages : Unsigned_64;
      raw   : Unsigned_64;
      aligned : Unsigned_64;
      grantOk : Boolean;
      attach  : Message;
      status  : Message;
   begin
      ok := False;

      acquire := callDisplay (OP_DISPLAY_ACQUIRE);
      if acquire.tag.length < 1 or else acquire.words (0) /= 0 then
         debugPrint ("desktop: display acquire failed" & LF);
         return;
      end if;

      fbWidth  := Natural (info.words (0));
      fbHeight := Natural (info.words (1));
      fbPitch  := Natural (info.words (2));
      fbBpp    := Natural (info.words (3));

      if fbWidth = 0 or else fbHeight = 0 or else fbPitch = 0 or else
         fbBpp /= 32
      then
         debugPrint ("desktop: display info unsupported" & LF);
         status := callDisplay (OP_DISPLAY_RELEASE);
         return;
      end if;

      bytes := Unsigned_64 (fbPitch * fbHeight);
      pages := (bytes + 4095) / 4096;
      raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
      if raw = Unsigned_64'Last then
         debugPrint ("desktop: backbuffer alloc failed" & LF);
         status := callDisplay (OP_DISPLAY_RELEASE);
         return;
      end if;

      aligned := alignUpPage (raw);
      backBufferAddr := To_Address (Integer_Address (aligned));
      createGrantViaCap
        (slot      => CAP_SLOT_DISPLAY,
         localAddr => backBufferAddr,
         numPages  => Natural (pages),
         readWrite => True,
         grantId   => backBufferGrant,
         success   => grantOk);
      if not grantOk then
         debugPrint ("desktop: display grant failed" & LF);
         status := callDisplay (OP_DISPLAY_RELEASE);
         return;
      end if;

      attach := callDisplay
        (OP_DISPLAY_ATTACH_BUFFER,
         backBufferGrant,
         Unsigned_64 (fbWidth),
         Unsigned_64 (fbHeight),
         Unsigned_64 (fbPitch));
      if attach.words (0) /= 0 then
         debugPrint ("desktop: display attach failed" & LF);
         status := callDisplay (OP_DISPLAY_RELEASE);
         return;
      end if;

      backBufferReady := True;
      status := callDisplay (OP_DISPLAY_GET_STATUS);
      if status.tag.length >= 2 then
         debugPrint ("desktop: display backend=");
         printDec (status.words (0));
         debugPrint (" caps=");
         printDec (status.words (1));
         debugPrint ("" & LF);
      end if;
      ok := True;
   end setupDisplayBuffer;

   procedure queryDisplayInfo (ok : out Boolean) is
      info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
   begin
      ok := False;
      if info.tag.length < 4 then
         debugPrint ("desktop: display info unsupported" & LF);
         return;
      end if;

      fbWidth  := Natural (info.words (0));
      fbHeight := Natural (info.words (1));
      fbPitch  := Natural (info.words (2));
      fbBpp    := Natural (info.words (3));

      if fbWidth = 0 or else fbHeight = 0 or else fbPitch = 0 or else
         fbBpp /= 32
      then
         debugPrint ("desktop: display info unsupported" & LF);
         return;
      end if;

      ok := True;
   end queryDisplayInfo;

   ret      : Unsigned_64;
   from     : ProcessID;
   msg      : Message;
   found    : Boolean;
   running  : Boolean := True;
   displayInfoOk : Boolean := False;
begin
   debugPrint ("desktop: starting" & LF);

   ret := registerDriver (DRIVER_DESKTOP);
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: register failed" & LF);
   end if;

   --  Do not attach a display buffer yet. During manual bring-up the CLI
   --  shell needs to remain visible long enough for the user to run
   --  `spawn desktop-shell.app`; the desktop takes over scanout lazily when
   --  the first real surface is created.
   queryDisplayInfo (displayInfoOk);
   if not displayInfoOk then
      debugPrint ("desktop: display setup failed" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;
   debugPrint ("desktop: display info ready" & LF);

   debugPrint ("desktop: waiting for shell client" & LF);

   while running loop
      declare
         eventMsg   : Message;
         eventFound : Boolean;
      begin
         loop
            eventFound := Poll_Event (eventMsg);
            exit when not eventFound;
            handleEvent (eventMsg, running);
            exit when not running;
         end loop;

         loop
            Poll_Service_Request (from, msg, found);
            exit when not found;
            handleRequest (from, msg);
            exit when not running;
         end loop;

         flushFrame;
         maybePrintStats;

         if not eventFound and then not found then
            declare
               now : constant Unsigned_64 := nowMs;
               sleepMs : Unsigned_64 := 2;
            begin
               if framePending and then now /= Unsigned_64'Last and then
                  frameDueMs /= 0 and then now < frameDueMs
               then
                  sleepMs := frameDueMs - now;
                  if sleepMs > 2 then
                     sleepMs := 2;
                  end if;
               elsif framePending then
                  sleepMs := 0;
               end if;

               if sleepMs > 0 and then
                  syscall (SYSCALL_SLEEP, sleepMs) = Unsigned_64'Last
               then
                  null;
               end if;
            end;
         end if;
      end;
   end loop;

   if fbBpp = 32 then
      declare
         cleared : constant Message :=
            callDisplay (OP_DISPLAY_CLEAR, Unsigned_64 (C_BG), 0, 0, 0);
      begin
         if cleared.words (0) /= 0 then
            null;
         end if;
      end;
   end if;

   declare
      released : constant Message := callDisplay (OP_DISPLAY_RELEASE);
   begin
      if released.words (0) /= 0 then
         null;
      end if;
   end;

   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
