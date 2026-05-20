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
   OP_SPAWN            : constant Unsigned_32 := 16#0100#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_DESTROY  : constant Unsigned_32 := 16#0811#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_RESIZE   : constant Unsigned_32 := 16#0813#;
   OP_SURFACE_ATTACH_BUFFER : constant Unsigned_32 := 16#0814#;
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := 16#0841#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := 16#0900#;
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := 16#0901#;
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := 16#0902#;
   OP_DISPLAY_CLEAR         : constant Unsigned_32 := 16#0903#;
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := 16#0904#;
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := 16#0905#;
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := 16#0906#;
   OP_DISPLAY_MAP_BACKBUFFER : constant Unsigned_32 := 16#0907#;

   UI_OK              : constant Unsigned_64 := 0;
   UI_ERR_DENIED      : constant Unsigned_64 := 1;
   UI_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   UI_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   UI_ERR_UNSUPPORTED : constant Unsigned_64 := 5;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;

   SURFACE_FLAG_SHELL  : constant Unsigned_64 := 1;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;

   WINDOW_FLAG_DECORATED      : constant Unsigned_64 := 1;
   WINDOW_FLAG_RESIZABLE      : constant Unsigned_64 := 2;
   WINDOW_FLAG_MINIMIZABLE    : constant Unsigned_64 := 4;
   WINDOW_FLAG_MAXIMIZABLE    : constant Unsigned_64 := 8;
   WINDOW_FLAG_CLOSEABLE      : constant Unsigned_64 := 16;
   WINDOW_FLAG_FULLSCREENABLE : constant Unsigned_64 := 32;
   WINDOW_FLAG_POINTER_CAPTURE : constant Unsigned_64 := 64;
   WINDOW_FLAG_FIXED_SIZE     : constant Unsigned_64 := 128;
   WINDOW_FLAGS_DEFAULT : constant Unsigned_64 :=
      WINDOW_FLAG_DECORATED or WINDOW_FLAG_RESIZABLE or
      WINDOW_FLAG_MINIMIZABLE or WINDOW_FLAG_MAXIMIZABLE or
      WINDOW_FLAG_CLOSEABLE;

   INPUT_NONE      : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN  : constant Unsigned_64 := 1;
   INPUT_KEY_UP    : constant Unsigned_64 := 2;
   INPUT_POINTER_MOVE : constant Unsigned_64 := 3;
   INPUT_POINTER_DOWN : constant Unsigned_64 := 4;
   INPUT_POINTER_UP   : constant Unsigned_64 := 5;
   INPUT_CONFIGURE : constant Unsigned_64 := 8;

   PROTOCOL_MAJOR : constant Unsigned_64 := 0;
   PROTOCOL_MINOR : constant Unsigned_64 := 1;
   PROTOCOL_VERSION : constant Unsigned_64 :=
      PROTOCOL_MAJOR or Shift_Left (PROTOCOL_MINOR, 32);

   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 := 1;
   SCALE_1_0_16_16       : constant Unsigned_64 := 16#0001_0000#;
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB
   DISPLAY_CAP_DIRECT_BACKBUFFER : constant Unsigned_64 := 16#0008#;
   PS_BUF_SIZE : constant Unsigned_64 := 8192;
   PS_ENTRY_SIZE : constant Storage_Offset := 32;

   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   backBufferAddr : System.Address := System.Null_Address;
   backBufferGrant : Unsigned_64 := 0;
   spawnGrantAddr : System.Address := System.Null_Address;
   spawnGrantId   : Unsigned_64 := 0;
   spawnGrantReady : Boolean := False;
   psBufAddr : System.Address := System.Null_Address;

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

   function memcpy
      (dest : System.Address;
       src  : System.Address;
       len  : Storage_Count)
      return System.Address with
      Import => True,
      Convention => C,
      External_Name => "memcpy";

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
      minimized : Boolean := False;
      appKind   : Natural := 0;
      maximized : Boolean := False;
      restoreX  : Natural := 0;
      restoreY  : Natural := 0;
      restoreW  : Natural := 0;
      restoreH  : Natural := 0;
      minW      : Natural := 120;
      minH      : Natural := 80;
      maxW      : Natural := 0;
      maxH      : Natural := 0;
      windowFlags : Unsigned_64 := WINDOW_FLAGS_DEFAULT;
      bufferAttached : Boolean := False;
      bufferGrant    : Unsigned_64 := 0;
      bufferAddr     : System.Address := System.Null_Address;
      bufferW        : Natural := 0;
      bufferH        : Natural := 0;
      bufferPitch    : Natural := 0;
      bufferFormat   : Unsigned_64 := 0;
   end record;

   MAX_SURFACES : constant Natural := 8;
   subtype SurfaceIndex is Natural range 0 .. MAX_SURFACES - 1;
   type SurfaceTable is array (SurfaceIndex) of Surface;

   surfaces : SurfaceTable;
   nextSurfaceId : Unsigned_64 := 1;
   focusSurface  : Unsigned_64 := 0;
   internalShellSurface : Unsigned_64 := 0;
   internalDemoWindow   : Unsigned_64 := 0;
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

   INPUT_QUEUE_SIZE : constant Natural := 64;
   subtype InputQueueIndex is Natural range 0 .. INPUT_QUEUE_SIZE - 1;
   type PendingInputQueue is array (InputQueueIndex) of PendingInput;

   inputEvents : PendingInputQueue;

   cursorX : Natural := 80;
   cursorY : Natural := 80;
   lastButtons : Unsigned_64 := 0;
   pointerSurfaceId : Unsigned_64 := 0;
   launchMenuOpen : Boolean := False;
   desktopShiftDown : Boolean := False;
   desktopCapsLockOn : Boolean := False;

   CONSOLE_INPUT_MAX : constant Natural := 56;
   consoleInput : String (1 .. CONSOLE_INPUT_MAX) := (others => ' ');
   consoleInputLen : Natural := 0;
   consoleLast : String (1 .. CONSOLE_INPUT_MAX) := (others => ' ');
   consoleLastLen : Natural := 0;
   consoleResult : String (1 .. 72) := (others => ' ');
   consoleResultLen : Natural := 0;
   CONSOLE_LINE_MAX : constant Natural := 72;
   CONSOLE_HISTORY_ROWS : constant Natural := 5;
   subtype ConsoleLine is String (1 .. CONSOLE_LINE_MAX);
   type ConsoleHistoryTable is array (1 .. CONSOLE_HISTORY_ROWS) of ConsoleLine;
   type ConsoleHistoryLengths is array (1 .. CONSOLE_HISTORY_ROWS) of Natural;
   consoleHistory : ConsoleHistoryTable := (others => (others => ' '));
   consoleHistoryLen : ConsoleHistoryLengths := (others => 0);

   type ScanTable is array (Unsigned_8 range 0 .. 16#39#) of Unsigned_8;
   scancodeNormal : constant ScanTable :=
     (16#02# => Character'Pos ('1'),
      16#03# => Character'Pos ('2'),
      16#04# => Character'Pos ('3'),
      16#05# => Character'Pos ('4'),
      16#06# => Character'Pos ('5'),
      16#07# => Character'Pos ('6'),
      16#08# => Character'Pos ('7'),
      16#09# => Character'Pos ('8'),
      16#0A# => Character'Pos ('9'),
      16#0B# => Character'Pos ('0'),
      16#0C# => Character'Pos ('-'),
      16#0D# => Character'Pos ('='),
      16#0E# => 8,
      16#10# => Character'Pos ('q'),
      16#11# => Character'Pos ('w'),
      16#12# => Character'Pos ('e'),
      16#13# => Character'Pos ('r'),
      16#14# => Character'Pos ('t'),
      16#15# => Character'Pos ('y'),
      16#16# => Character'Pos ('u'),
      16#17# => Character'Pos ('i'),
      16#18# => Character'Pos ('o'),
      16#19# => Character'Pos ('p'),
      16#1C# => 10,
      16#1E# => Character'Pos ('a'),
      16#1F# => Character'Pos ('s'),
      16#20# => Character'Pos ('d'),
      16#21# => Character'Pos ('f'),
      16#22# => Character'Pos ('g'),
      16#23# => Character'Pos ('h'),
      16#24# => Character'Pos ('j'),
      16#25# => Character'Pos ('k'),
      16#26# => Character'Pos ('l'),
      16#2C# => Character'Pos ('z'),
      16#2D# => Character'Pos ('x'),
      16#2E# => Character'Pos ('c'),
      16#2F# => Character'Pos ('v'),
      16#30# => Character'Pos ('b'),
      16#31# => Character'Pos ('n'),
      16#32# => Character'Pos ('m'),
      16#33# => Character'Pos (','),
      16#34# => Character'Pos ('.'),
      16#35# => Character'Pos ('/'),
      16#39# => Character'Pos (' '),
      others => 0);

   scancodeShifted : constant ScanTable :=
     (16#02# => Character'Pos ('!'),
      16#03# => Character'Pos ('@'),
      16#04# => Character'Pos ('#'),
      16#05# => Character'Pos ('$'),
      16#06# => Character'Pos ('%'),
      16#07# => Character'Pos ('^'),
      16#08# => Character'Pos ('&'),
      16#09# => Character'Pos ('*'),
      16#0A# => Character'Pos ('('),
      16#0B# => Character'Pos (')'),
      16#0C# => Character'Pos ('_'),
      16#0D# => Character'Pos ('+'),
      16#0E# => 8,
      16#10# => Character'Pos ('Q'),
      16#11# => Character'Pos ('W'),
      16#12# => Character'Pos ('E'),
      16#13# => Character'Pos ('R'),
      16#14# => Character'Pos ('T'),
      16#15# => Character'Pos ('Y'),
      16#16# => Character'Pos ('U'),
      16#17# => Character'Pos ('I'),
      16#18# => Character'Pos ('O'),
      16#19# => Character'Pos ('P'),
      16#1C# => 10,
      16#1E# => Character'Pos ('A'),
      16#1F# => Character'Pos ('S'),
      16#20# => Character'Pos ('D'),
      16#21# => Character'Pos ('F'),
      16#22# => Character'Pos ('G'),
      16#23# => Character'Pos ('H'),
      16#24# => Character'Pos ('J'),
      16#25# => Character'Pos ('K'),
      16#26# => Character'Pos ('L'),
      16#2C# => Character'Pos ('Z'),
      16#2D# => Character'Pos ('X'),
      16#2E# => Character'Pos ('C'),
      16#2F# => Character'Pos ('V'),
      16#30# => Character'Pos ('B'),
      16#31# => Character'Pos ('N'),
      16#32# => Character'Pos ('M'),
      16#33# => Character'Pos ('<'),
      16#34# => Character'Pos ('>'),
      16#35# => Character'Pos ('?'),
      16#39# => Character'Pos (' '),
      others => 0);

   APP_CLIENT   : constant Natural := 0;
   APP_DEMO     : constant Natural := 1;
   APP_CONSOLE  : constant Natural := 2;
   APP_SECURITY : constant Natural := 3;

   LAUNCH_NONE     : constant Natural := 0;
   LAUNCH_CONSOLE : constant Natural := 1;
   LAUNCH_FILES    : constant Natural := 2;
   LAUNCH_SECURITY : constant Natural := 3;
   LAUNCH_POWER    : constant Natural := 4;

   DRAG_NONE        : constant Natural := 0;
   DRAG_MOVE        : constant Natural := 1;
   DRAG_RESIZE_E    : constant Natural := 2;
   DRAG_RESIZE_S    : constant Natural := 3;
   DRAG_RESIZE_SE   : constant Natural := 4;
   HIT_MINIMIZE     : constant Natural := 5;
   HIT_CLOSE        : constant Natural := 6;
   HIT_MAXIMIZE     : constant Natural := 7;
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
   TASK_BUTTON_W : constant Natural := 156;
   TASK_BUTTON_H : constant Natural := 24;
   TASK_BUTTON_GAP : constant Natural := 6;
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

   function alignUpPage (addr : Unsigned_64) return Unsigned_64 is
   begin
      return (addr + 4095) and not Unsigned_64'(4095);
   end alignUpPage;

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

   function launchItemRect (action : Natural) return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural := menu.y + 34;
   begin
      if isEmpty (menu) or else action = LAUNCH_NONE or else menu.w <= 16 then
         return (others => 0);
      end if;

      case action is
         when LAUNCH_CONSOLE =>
            y := menu.y + 34;
         when LAUNCH_FILES =>
            y := menu.y + 60;
         when LAUNCH_SECURITY =>
            y := menu.y + 86;
         when LAUNCH_POWER =>
            y := menu.y + 122;
         when others =>
            return (others => 0);
      end case;

      return clampRect ((x => menu.x + 8, y => y,
                         w => menu.w - 16, h => 24));
   end launchItemRect;

   function taskButtonOrdinal (slot : SurfaceIndex) return Natural is
      ordinal : Natural := 0;
   begin
      for i in surfaces'Range loop
         exit when i = slot;
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            ordinal := ordinal + 1;
         end if;
      end loop;

      return ordinal;
   end taskButtonOrdinal;

   function taskButtonRect (slot : SurfaceIndex) return Rect is
      ordinal : constant Natural := taskButtonOrdinal (slot);
      x       : Natural := 104 + ordinal * (TASK_BUTTON_W + TASK_BUTTON_GAP);
      maxW    : Natural := TASK_BUTTON_W;
   begin
      if x >= fbWidth then
         return (others => 0);
      end if;

      if x + maxW + 6 > fbWidth then
         maxW := fbWidth - x;
      end if;

      return clampRect ((x => x, y => taskbarY + 6,
                         w => maxW, h => TASK_BUTTON_H));
   end taskButtonRect;

   function pointInRect (x, y : Natural; r : Rect) return Boolean is
   begin
      return not isEmpty (r) and then
         x >= r.x and then y >= r.y and then
         x < r.x + r.w and then y < r.y + r.h;
   end pointInRect;

   function hitLaunchItem (x, y : Natural) return Natural is
   begin
      if pointInRect (x, y, launchItemRect (LAUNCH_CONSOLE)) then
         return LAUNCH_CONSOLE;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_FILES)) then
         return LAUNCH_FILES;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_SECURITY)) then
         return LAUNCH_SECURITY;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_POWER)) then
         return LAUNCH_POWER;
      else
         return LAUNCH_NONE;
      end if;
   end hitLaunchItem;

   function hitTaskButton (x, y : Natural) return Integer is
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0 and then
            pointInRect (x, y, taskButtonRect (i))
         then
            return Integer (i);
         end if;
      end loop;

      return -1;
   end hitTaskButton;

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

   function clientRect (s : Surface) return Rect is
   begin
      if s.w <= 20 or else s.h <= 44 then
         return (others => 0);
      end if;

      return clampRect ((x => s.x + 10, y => s.y + 34,
                         w => s.w - 20, h => s.h - 44));
   end clientRect;

   function unpackLo32 (x : Unsigned_64) return Natural is
   begin
      return Natural (x and 16#FFFF_FFFF#);
   end unpackLo32;

   function unpackHi32 (x : Unsigned_64) return Natural is
   begin
      return Natural (Shift_Right (x, 32));
   end unpackHi32;

   function packU32Pair (lo, hi : Natural) return Unsigned_64 is
   begin
      return Unsigned_64 (lo) or Shift_Left (Unsigned_64 (hi), 32);
   end packU32Pair;

   function ensureProcessListBuffer return Boolean is
      raw : Unsigned_64;
   begin
      if psBufAddr /= System.Null_Address then
         return True;
      end if;

      raw := syscall (SYSCALL_SBRK, PS_BUF_SIZE);
      if raw = Unsigned_64'Last then
         debugPrint ("desktop: ps buffer alloc failed" & LF);
         return False;
      end if;

      psBufAddr := To_Address (Integer_Address (raw));
      return True;
   end ensureProcessListBuffer;

   function processAlive (pid : ProcessID) return Boolean is
      count : Unsigned_64;
      entryAddr : System.Address;
      pidVal : Unsigned_16;
   begin
      if pid = NO_PROCESS then
         return True;
      end if;
      if not ensureProcessListBuffer then
         --  Fail open: losing the process list should not destroy a valid
         --  client window just because the diagnostic buffer could not grow.
         return True;
      end if;

      count := syscall (SYSCALL_PROCLIST,
                        Unsigned_64 (To_Integer (psBufAddr)),
                        PS_BUF_SIZE);
      if count = Unsigned_64'Last then
         return True;
      end if;

      for i in 0 .. count - 1 loop
         entryAddr := psBufAddr + Storage_Offset (i) * PS_ENTRY_SIZE;
         declare
            p : Unsigned_16 with Import, Address => entryAddr;
         begin
            pidVal := p;
         end;
         if ProcessID (pidVal) = pid then
            return True;
         end if;
      end loop;

      return False;
   end processAlive;

   function hasWindowFlag (s : Surface; flag : Unsigned_64) return Boolean is
   begin
      return (s.windowFlags and flag) /= 0;
   end hasWindowFlag;

   procedure clampSurfaceSize
      (s : Surface;
       w : in out Natural;
       h : in out Natural)
   is
   begin
      if w < s.minW then
         w := s.minW;
      end if;
      if h < s.minH then
         h := s.minH;
      end if;

      if s.maxW /= 0 and then w > s.maxW then
         w := s.maxW;
      end if;
      if s.maxH /= 0 and then h > s.maxH then
         h := s.maxH;
      end if;
   end clampSurfaceSize;

   function minimizeButtonRect (s : Surface) return Rect is
   begin
      if not hasWindowFlag (s, WINDOW_FLAG_MINIMIZABLE) or else s.w < 66 then
         return (others => 0);
      end if;

      return clampRect ((x => s.x + s.w - 59,
                         y => s.y + 6,
                         w => 14,
                         h => 14));
   end minimizeButtonRect;

   function maximizeButtonRect (s : Surface) return Rect is
   begin
      if not hasWindowFlag (s, WINDOW_FLAG_MAXIMIZABLE) or else
         hasWindowFlag (s, WINDOW_FLAG_FIXED_SIZE) or else s.w < 48
      then
         return (others => 0);
      end if;

      return clampRect ((x => s.x + s.w - 41,
                         y => s.y + 6,
                         w => 14,
                         h => 14));
   end maximizeButtonRect;

   function closeButtonRect (s : Surface) return Rect is
   begin
      if not hasWindowFlag (s, WINDOW_FLAG_CLOSEABLE) or else s.w < 30 then
         return (others => 0);
      end if;

      return clampRect ((x => s.x + s.w - 23,
                         y => s.y + 6,
                         w => 14,
                         h => 14));
   end closeButtonRect;

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
            not surfaces (i).minimized and then
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
      if pointInRect (x, y, closeButtonRect (s)) then
         return HIT_CLOSE;
      elsif pointInRect (x, y, maximizeButtonRect (s)) then
         return HIT_MAXIMIZE;
      elsif pointInRect (x, y, minimizeButtonRect (s)) then
         return HIT_MINIMIZE;
      elsif s.maximized then
         return DRAG_NONE;
      elsif not hasWindowFlag (s, WINDOW_FLAG_RESIZABLE) or else
         hasWindowFlag (s, WINDOW_FLAG_FIXED_SIZE)
      then
         if inTitle then
            return DRAG_MOVE;
         else
            return DRAG_NONE;
         end if;
      elsif onRight and then onBottom then
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

   procedure drawSurfaceTitle
      (s       : Surface;
       x, y    : Natural;
       fg, bg  : Unsigned_32)
   is
   begin
      case s.appKind is
         when APP_CONSOLE =>
            drawText (x, y, "CuBASIC Console", fg, bg);
         when APP_SECURITY =>
            drawText (x, y, "Security Center", fg, bg);
         when APP_DEMO =>
            drawText (x, y, "Demo Window", fg, bg);
         when others =>
            drawText (x, y, "Application", fg, bg);
      end case;
   end drawSurfaceTitle;

   procedure drawConsoleText (x, y : Natural; bg : Unsigned_32) is
   begin
      drawText (x, y, "CuBASIC 0.1", C_ACCENT, bg);
      drawText (x, y + 24, "READY.", C_GOOD, bg);

      for row in 1 .. CONSOLE_HISTORY_ROWS loop
         if consoleHistoryLen (row) > 0 then
            drawText
              (x, y + 48 + (row - 1) * 18,
               consoleHistory (row) (1 .. consoleHistoryLen (row)),
               C_TEXT, bg);
         end if;
      end loop;

      drawText (x, y + 148, "]", C_GOOD, bg);
      if consoleInputLen > 0 then
         drawText (x + 24, y + 148, consoleInput (1 .. consoleInputLen),
                   C_TEXT, bg);
         drawText (x + 24 + consoleInputLen * Font8x16.GLYPH_WIDTH,
                   y + 148, "_", C_GOOD, bg);
      else
         drawText (x + 24, y + 148, "_", C_GOOD, bg);
      end if;

      if consoleLastLen > 0 then
         drawText (x, y + 182, "last:", C_MUTED, bg);
         drawText (x + 48, y + 182, consoleLast (1 .. consoleLastLen),
                   C_TEXT, bg);
      else
         drawText (x, y + 182, "type HELP, LIST SERVICES, SHOW CAPS",
                   C_MUTED, bg);
      end if;
   end drawConsoleText;

   procedure drawClientBuffer (s : Surface; x, y, w, h : Natural) is
      copyW : Natural := w;
      copyH : Natural := h;
      minX  : Natural := x;
      minY  : Natural := y;
      maxX  : Natural;
      maxY  : Natural;
   begin
      if not s.bufferAttached or else
         s.bufferAddr = System.Null_Address or else
         backBufferAddr = System.Null_Address or else
         s.bufferFormat /= PIXEL_FORMAT_BGRA8888 or else
         s.bufferPitch < s.bufferW * 4
      then
         return;
      end if;

      if copyW > s.bufferW then
         copyW := s.bufferW;
      end if;
      if copyH > s.bufferH then
         copyH := s.bufferH;
      end if;
      if copyW = 0 or else copyH = 0 or else
         x >= fbWidth or else y >= fbHeight
      then
         return;
      end if;

      maxX := x + copyW;
      maxY := y + copyH;

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

      --  Client buffers are already BGRA8888, matching the compositor
      --  backbuffer. Clip once, then copy rows directly; bitmap-heavy clients
      --  such as DOOM should not pay the cost of putPixel/readClientPixel for
      --  every pixel in a full-frame present.
      for yy in minY .. maxY - 1 loop
         declare
            srcY : constant Natural := yy - y;
            srcX : constant Natural := minX - x;
            bytes : constant Storage_Count := Storage_Count ((maxX - minX) * 4);
            ignore : System.Address;
         begin
            ignore := memcpy
              (backBufferAddr + Storage_Offset (yy * fbPitch + minX * 4),
               s.bufferAddr +
                  Storage_Offset (srcY * s.bufferPitch + srcX * 4),
               bytes);
         end;
      end loop;
   end drawClientBuffer;

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
      active  : constant Boolean := s.id = focusSurface;
      frame   : Surface := s;
      minBtn  : Rect;
      maxBtn  : Rect;
      closeBtn : Rect;
      titleColor : Unsigned_32 := C_EDGE;
      x      : Natural := s.x;
      y      : Natural := s.y;
      w      : Natural := s.w;
      h      : Natural := s.h;
   begin
      if s.minimized then
         return;
      end if;

      if w < minW then
         w := minW;
      end if;
      if h < minH then
         h := minH;
      end if;

      if x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      if active then
         titleColor := C_BLUE;
      end if;

      frame.x := x;
      frame.y := y;
      frame.w := w;
      frame.h := h;

      fillRect (x + 3, y + 3, w, h, C_SHADOW);
      fillRect (x, y, w, h, C_WIN);
      strokeRect (x, y, w, h, C_EDGE, C_SHADOW);
      fillRect (x + 3, y + 3, w - 6, titleH, titleColor);
      drawSurfaceTitle (s, x + 10, y + 7, C_WHITE, titleColor);

      --  Window controls are compositor-owned because they mutate focus,
      --  visibility, and eventually client lifecycle authority.
      minBtn := minimizeButtonRect (frame);
      maxBtn := maximizeButtonRect (frame);
      closeBtn := closeButtonRect (frame);

      if not isEmpty (minBtn) then
         fillRect (minBtn.x, minBtn.y, minBtn.w, minBtn.h, C_BAR);
         strokeRect (minBtn.x, minBtn.y, minBtn.w, minBtn.h,
                     C_EDGE, C_SHADOW);
         drawText (minBtn.x + 4, minBtn.y + 1, "_", C_TEXT, C_BAR);
      end if;

      if not isEmpty (maxBtn) then
         fillRect (maxBtn.x, maxBtn.y, maxBtn.w, maxBtn.h, C_BAR);
         strokeRect (maxBtn.x, maxBtn.y, maxBtn.w, maxBtn.h,
                     C_EDGE, C_SHADOW);
         if s.maximized then
            strokeRect (maxBtn.x + 3, maxBtn.y + 4, 7, 6, C_TEXT, C_TEXT);
            strokeRect (maxBtn.x + 5, maxBtn.y + 2, 7, 6, C_TEXT, C_TEXT);
         else
            strokeRect (maxBtn.x + 3, maxBtn.y + 3, 8, 8, C_TEXT, C_TEXT);
         end if;
      end if;

      if not isEmpty (closeBtn) then
         fillRect (closeBtn.x, closeBtn.y, closeBtn.w, closeBtn.h, C_BAR);
         strokeRect (closeBtn.x, closeBtn.y, closeBtn.w, closeBtn.h,
                     C_EDGE, C_SHADOW);
         drawText (closeBtn.x + 3, closeBtn.y - 1, "x", C_TEXT, C_BAR);
      end if;

      case s.appKind is
         when APP_CONSOLE =>
            fillRect (x + 14, y + 40, w - 28, h - 56, C_SHADOW);
            drawConsoleText (x + 24, y + 50, C_SHADOW);
         when APP_SECURITY =>
            drawText (x + 18, y + 44, "Capability map", C_TEXT, C_WIN);
            drawText (x + 18, y + 70, "Processes: procmgr, display, desktop",
                      C_GOOD, C_WIN);
            drawText (x + 18, y + 96, "Policy: least authority by default",
                      C_TEXT, C_WIN);
            drawText (x + 18, y + 122, "Live inspection hooks come next",
                      C_MUTED, C_WIN);
         when others =>
            if s.bufferAttached then
               if s.bufferW < w - 20 or else s.bufferH < h - 44 then
                  fillRect (x + 10, y + 34, w - 20, h - 44, C_SHADOW);
               end if;
               drawClientBuffer (s, x + 10, y + 34, w - 20, h - 44);
            else
               drawText (x + 18, y + 44, "This is a real child surface.",
                         C_TEXT, C_WIN);
               drawText (x + 18, y + 70, "Drag title bar to move.",
                         C_TEXT, C_WIN);
               drawText (x + 18, y + 96, "Drag edges to resize.",
                         C_TEXT, C_WIN);
            end if;
      end case;
   end drawWindow;

   procedure drawTaskButtons is
      r : Rect;
      face : Unsigned_32;
      light : Unsigned_32;
      dark : Unsigned_32;
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            r := taskButtonRect (i);
            if not isEmpty (r) then
               face := C_BAR;
               light := C_EDGE;
               dark := C_SHADOW;

               if surfaces (i).id = focusSurface and then
                  not surfaces (i).minimized
               then
                  face := C_WIN;
                  light := C_SHADOW;
                  dark := C_EDGE;
               end if;

               fillRect (r.x, r.y, r.w, r.h, face);
               strokeRect (r.x, r.y, r.w, r.h, light, dark);
               drawSurfaceTitle (surfaces (i), r.x + 10, r.y + 6,
                                 C_TEXT, face);
            end if;
         end if;
      end loop;
   end drawTaskButtons;

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
      drawText (r.x + 18, r.y + 42, "CuBASIC", C_TEXT, C_PANEL);
      drawText (r.x + 18, r.y + 68, "Files", C_MUTED, C_PANEL);
      drawText (r.x + 18, r.y + 94, "Security Center", C_TEXT, C_PANEL);
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
      drawTaskButtons;

      if fbWidth > panelW + 48 and then fbHeight > panelH + TASKBAR_H + 48 then
         px := (fbWidth - panelW) / 2;
         py := (fbHeight - TASKBAR_H - panelH) / 2;
      end if;

      fillRect (px, py, panelW, panelH, C_BAR);
      strokeRect (px, py, panelW, panelH, C_EDGE, C_SHADOW);
      fillRect (px + 3, py + 3, panelW - 6, 22, C_BLUE);
      drawText (px + 10, py + 7, "CuBit Desktop", C_WHITE, C_BLUE);
      drawText (px + 18, py + 44, "desktop.svc owns the session",
                C_TEXT, C_BAR);
      drawText (px + 18, py + 70, "Launch opens desktop windows",
                C_TEXT, C_BAR);
      drawText (px + 18, py + 96, "Q or Esc exits desktop shell",
                C_TEXT, C_BAR);

      drawText (16, 18, "System", C_WHITE, C_DESK);
      drawText (16, 48, "Authority", C_WHITE, C_DESK);

      for i in surfaces'Range loop
         if surfaces (i).used and then
            not surfaces (i).minimized and then
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

   procedure reapDeadClientSurfaces (damage : in out Rect);

   procedure flushFrame is
      damage : Rect := frameDamage;
      now : constant Unsigned_64 := nowMs;
      t0 : Unsigned_64;
      t1 : Unsigned_64;
      full : Boolean;
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

      reapDeadClientSurfaces (damage);
      full :=
         damage.x = 0 and then damage.y = 0 and then
         damage.w = fbWidth and then damage.h = fbHeight;

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
      if internalShellSurface /= 0 then
         return True;
      end if;

      for i in surfaces'Range loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
         then
            return True;
         end if;
      end loop;

      return False;
   end shellSurfaceVisible;

   procedure queueConfigure (surfaceId, w, h : Unsigned_64);
   procedure restoreSurface (idx : SurfaceIndex; damage : in out Rect);

   procedure focusTopmostVisibleWindow (damage : in out Rect) is
   begin
      for i in reverse surfaces'Range loop
         if surfaces (i).used and then
            not surfaces (i).minimized and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            focusSurface := surfaces (i).id;
            damage := unionRect
              (damage, inflateRect (surfaceRect (surfaces (i)), 4));
            damage := unionRect
              (damage, inflateRect (taskButtonRect (i), 4));
            return;
         end if;
      end loop;

      focusSurface := 0;
   end focusTopmostVisibleWindow;

   procedure raiseSurface (idx : SurfaceIndex) is
      moved : Surface := surfaces (idx);
      last  : SurfaceIndex := idx;
   begin
      if (surfaces (idx).flags and SURFACE_FLAG_WINDOW) = 0 then
         return;
      end if;
      if idx = surfaces'Last then
         return;
      end if;

      for i in idx + 1 .. surfaces'Last loop
         if surfaces (i).used and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            surfaces (last) := surfaces (i);
            last := i;
         end if;
      end loop;

      surfaces (last) := moved;
   end raiseSurface;

   procedure focusAndRaiseSurface
      (idx    : SurfaceIndex;
       damage : in out Rect)
   is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      oldTask   : constant Rect := taskButtonRect (idx);
      id        : constant Unsigned_64 := surfaces (idx).id;
      raisedIdx : Integer;
   begin
      focusSurface := id;
      raiseSurface (idx);
      raisedIdx := findSurface (id);

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (oldTask, 4));

      if raisedIdx >= 0 then
         damage := unionRect
           (damage,
            inflateRect (surfaceRect (surfaces (SurfaceIndex (raisedIdx))), 4));
         damage := unionRect
           (damage,
            inflateRect (taskButtonRect (SurfaceIndex (raisedIdx)), 4));
      end if;
   end focusAndRaiseSurface;

   procedure cycleFocus (damage : in out Rect) is
      current : Integer := findSurface (focusSurface);
      base    : Natural := 0;
      probe   : Natural;
      chosen  : Integer := -1;
   begin
      if current >= 0 then
         base := Natural (current) + 1;
      end if;

      for step in 0 .. MAX_SURFACES - 1 loop
         probe := (base + step) mod MAX_SURFACES;
         if surfaces (SurfaceIndex (probe)).used and then
            (surfaces (SurfaceIndex (probe)).flags and
             SURFACE_FLAG_WINDOW) /= 0
         then
            chosen := Integer (probe);
            exit;
         end if;
      end loop;

      if chosen >= 0 then
         if surfaces (SurfaceIndex (chosen)).minimized then
            restoreSurface (SurfaceIndex (chosen), damage);
         end if;
         focusAndRaiseSurface (SurfaceIndex (chosen), damage);
      end if;
   end cycleFocus;

   procedure minimizeSurface (idx : SurfaceIndex; damage : in out Rect) is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      button    : constant Rect := taskButtonRect (idx);
   begin
      surfaces (idx).minimized := True;
      surfaces (idx).dirty := True;

      if focusSurface = surfaces (idx).id then
         focusSurface := 0;
      end if;

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));

      if focusSurface = 0 then
         focusTopmostVisibleWindow (damage);
      end if;
   end minimizeSurface;

   procedure restoreSurface (idx : SurfaceIndex; damage : in out Rect) is
      bounds : Rect;
      button : constant Rect := taskButtonRect (idx);
   begin
      surfaces (idx).minimized := False;
      surfaces (idx).dirty := True;
      focusSurface := surfaces (idx).id;
      bounds := surfaceRect (surfaces (idx));

      damage := unionRect (damage, inflateRect (bounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));
   end restoreSurface;

   procedure toggleMaximizeSurface
      (idx    : SurfaceIndex;
       damage : in out Rect)
   is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      newBounds : Rect;
      workH     : Natural := fbHeight;
      nextW     : Natural;
      nextH     : Natural;
   begin
      if not hasWindowFlag (surfaces (idx), WINDOW_FLAG_MAXIMIZABLE) or else
         hasWindowFlag (surfaces (idx), WINDOW_FLAG_FIXED_SIZE)
      then
         return;
      end if;

      if fbHeight > TASKBAR_H then
         workH := fbHeight - TASKBAR_H;
      end if;

      if surfaces (idx).maximized then
         surfaces (idx).x := surfaces (idx).restoreX;
         surfaces (idx).y := surfaces (idx).restoreY;
         surfaces (idx).w := surfaces (idx).restoreW;
         surfaces (idx).h := surfaces (idx).restoreH;
         surfaces (idx).maximized := False;
      else
         surfaces (idx).restoreX := surfaces (idx).x;
         surfaces (idx).restoreY := surfaces (idx).y;
         surfaces (idx).restoreW := surfaces (idx).w;
         surfaces (idx).restoreH := surfaces (idx).h;
         nextW := fbWidth;
         nextH := workH;
         clampSurfaceSize (surfaces (idx), nextW, nextH);
         surfaces (idx).x := 0;
         surfaces (idx).y := 0;
         surfaces (idx).w := nextW;
         surfaces (idx).h := nextH;
         surfaces (idx).maximized := True;
      end if;

      surfaces (idx).minimized := False;
      surfaces (idx).dirty := True;
      surfaces (idx).serial := surfaces (idx).serial + 1;
      focusSurface := surfaces (idx).id;
      newBounds := surfaceRect (surfaces (idx));

      if surfaces (idx).owner /= NO_PROCESS then
         queueConfigure (surfaces (idx).id,
                         Unsigned_64 (newBounds.w),
                         Unsigned_64 (newBounds.h));
      end if;

      damage := unionRect
        (damage, inflateRect (unionRect (oldBounds, newBounds), 4));
      damage := unionRect
        (damage, inflateRect ((x => 0, y => taskbarY,
                               w => fbWidth, h => TASKBAR_H), 2));
   end toggleMaximizeSurface;

   procedure closeSurface (idx : SurfaceIndex; damage : in out Rect) is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      oldId     : constant Unsigned_64 := surfaces (idx).id;
      button    : constant Rect := taskButtonRect (idx);
   begin
      --  Internal demo windows can disappear immediately. For client-owned
      --  windows this is a temporary hard close; the protocol should later
      --  grow a close-request event so clients can save state or refuse.
      if oldId = internalDemoWindow then
         internalDemoWindow := 0;
      end if;

      surfaces (idx) := (others => <>);

      if focusSurface = oldId then
         focusSurface := 0;
      end if;

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));

      if focusSurface = 0 then
         focusTopmostVisibleWindow (damage);
      end if;
   end closeSurface;

   procedure reapDeadClientSurfaces (damage : in out Rect) is
      oldBounds : Rect;
      oldTask   : Rect;
      changed   : Boolean := False;
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            surfaces (i).owner /= NO_PROCESS and then
            not processAlive (surfaces (i).owner)
         then
            --  Client-owned surface buffers are grants from the client. When
            --  the client exits, the kernel revokes those grants. Reap the
            --  stale surface before the compositor tries to blit from the
            --  now-unmapped address.
            oldBounds := surfaceRect (surfaces (i));
            oldTask := taskButtonRect (i);
            if focusSurface = surfaces (i).id then
               focusSurface := 0;
            end if;
            surfaces (i) := (others => <>);
            damage := unionRect (damage, inflateRect (oldBounds, 4));
            damage := unionRect (damage, inflateRect (oldTask, 4));
            changed := True;
         end if;
      end loop;

      if changed then
         if focusSurface = 0 then
            focusTopmostVisibleWindow (damage);
         end if;
         damage := unionRect
           (damage, inflateRect ((x => 0, y => taskbarY,
                                  w => fbWidth, h => TASKBAR_H), 2));
      end if;
   end reapDeadClientSurfaces;

   procedure createInternalSurface
      (flags : Unsigned_64;
       x, y  : Natural;
       w, h  : Natural;
       appKind : Natural;
       id    : out Unsigned_64);

   procedure openInternalApp (appKind : Natural; damage : in out Rect) is
      existing : Integer := -1;
      id       : Unsigned_64 := 0;
      winX     : Natural := 96;
      winY     : Natural := 72;
      winW     : Natural := 420;
      winH     : Natural := 240;
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            surfaces (i).owner = NO_PROCESS and then
            surfaces (i).appKind = appKind
         then
            existing := Integer (i);
            exit;
         end if;
      end loop;

      if existing >= 0 then
         restoreSurface (SurfaceIndex (existing), damage);
         existing := findSurface (surfaces (SurfaceIndex (existing)).id);
         if existing >= 0 then
            focusAndRaiseSurface (SurfaceIndex (existing), damage);
         end if;
         return;
      end if;

      case appKind is
         when APP_CONSOLE =>
            winX := 86;
            winY := 76;
            winW := 620;
            winH := 330;
         when APP_SECURITY =>
            winX := 132;
            winY := 104;
            winW := 480;
            winH := 280;
         when others =>
            null;
      end case;

      if winX + winW > fbWidth then
         winW := Natural'Max (MIN_WIN_W, fbWidth - winX);
      end if;
      if winY + winH > fbHeight then
         winH := Natural'Max (MIN_WIN_H, fbHeight - winY);
      end if;

      createInternalSurface
        (SURFACE_FLAG_WINDOW, winX, winY, winW, winH, appKind, id);

      if id /= 0 then
         declare
            idx : constant Integer := findSurface (id);
         begin
            if idx >= 0 then
               focusAndRaiseSurface (SurfaceIndex (idx), damage);
            else
               focusSurface := id;
            end if;
         end;
         damage := unionRect
           (damage,
            inflateRect ((x => winX, y => winY, w => winW, h => winH), 4));
         damage := unionRect
           (damage,
            inflateRect ((x => 0, y => taskbarY, w => fbWidth,
                          h => TASKBAR_H), 2));
      end if;
   end openInternalApp;

   procedure createInternalSurface
      (flags : Unsigned_64;
       x, y  : Natural;
       w, h  : Natural;
       appKind : Natural;
       id    : out Unsigned_64)
   is
      slot : Integer := -1;
   begin
      id := 0;
      for i in surfaces'Range loop
         if not surfaces (i).used then
            slot := Integer (i);
            exit;
         end if;
      end loop;

      if slot < 0 then
         return;
      end if;

      surfaces (SurfaceIndex (slot)) :=
        (used   => True,
         owner  => NO_PROCESS,
         id     => nextSurfaceId,
         x      => x,
         y      => y,
         w      => w,
         h      => h,
         flags  => flags,
         serial => 1,
         dirty  => True,
         minimized => False,
         appKind => appKind,
         maximized => False,
         restoreX => x,
         restoreY => y,
         restoreW => w,
         restoreH => h,
         minW => MIN_WIN_W,
         minH => MIN_WIN_H,
         maxW => 0,
         maxH => 0,
         windowFlags => WINDOW_FLAGS_DEFAULT,
         bufferAttached => False,
         bufferGrant => 0,
         bufferAddr => System.Null_Address,
         bufferW => 0,
         bufferH => 0,
         bufferPitch => 0,
         bufferFormat => 0);
      id := nextSurfaceId;
      nextSurfaceId := nextSurfaceId + 1;
   end createInternalSurface;

   procedure enqueueInput
      (kind : Unsigned_64;
       target : Unsigned_64;
       payload0 : Unsigned_64;
       payload1 : Unsigned_64);

   procedure dequeueInput
      (target : Unsigned_64;
       afterSerial : Unsigned_64;
       found : out Boolean;
       event : out PendingInput);

   procedure clearInputQueue;

   procedure queueConfigure (surfaceId, w, h : Unsigned_64) is
   begin
      enqueueInput (INPUT_CONFIGURE, surfaceId, w, h);
   end queueConfigure;

   procedure enqueueInput
      (kind : Unsigned_64;
       target : Unsigned_64;
       payload0 : Unsigned_64;
       payload1 : Unsigned_64)
   is
      slot : Integer := -1;
      oldest : Integer := -1;
      oldestSerial : Unsigned_64 := Unsigned_64'Last;
   begin
      if target = 0 then
         return;
      end if;

      for i in inputEvents'Range loop
         if not inputEvents (i).valid and then slot < 0 then
            slot := Integer (i);
         elsif inputEvents (i).valid and then
            inputEvents (i).serial < oldestSerial
         then
            oldest := Integer (i);
            oldestSerial := inputEvents (i).serial;
         end if;
      end loop;

      if slot < 0 then
         --  Keep accepting fresh input rather than letting an unresponsive
         --  client stall the compositor. With a 64-event queue this should be
         --  rare, and dropping oldest preserves key releases much better than
         --  overwriting the single pending event slot did.
         slot := oldest;
      end if;

      if slot < 0 then
         return;
      end if;

      inputEvents (InputQueueIndex (slot)) :=
        (valid    => True,
         serial   => nextInputSerial,
         kind     => kind,
         target   => target,
         payload0 => payload0,
         payload1 => payload1);
      nextInputSerial := nextInputSerial + 1;
   end enqueueInput;

   procedure dequeueInput
      (target : Unsigned_64;
       afterSerial : Unsigned_64;
       found : out Boolean;
       event : out PendingInput)
   is
      best : Integer := -1;
      bestSerial : Unsigned_64 := Unsigned_64'Last;
   begin
      found := False;
      event := (others => <>);

      for i in inputEvents'Range loop
         if inputEvents (i).valid and then
            inputEvents (i).target = target and then
            inputEvents (i).serial > afterSerial and then
            inputEvents (i).serial < bestSerial
         then
            best := Integer (i);
            bestSerial := inputEvents (i).serial;
         end if;
      end loop;

      if best >= 0 then
         event := inputEvents (InputQueueIndex (best));
         inputEvents (InputQueueIndex (best)).valid := False;
         found := True;
      end if;
   end dequeueInput;

   procedure clearInputQueue is
   begin
      inputEvents := (others => (others => <>));
   end clearInputQueue;

   procedure queueKey (raw : Unsigned_8) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_64 := Unsigned_64 (raw and 16#7F#);
   begin
      if focusSurface = 0 then
         return;
      end if;

      enqueueInput ((if release then INPUT_KEY_UP else INPUT_KEY_DOWN),
                    focusSurface,
                    code,
                    0);
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
   procedure activateInternalSession (ok : out Boolean);

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
            replyMsg.words (2) := PIXEL_FORMAT_BGRA8888;
            replyMsg.words (3) := SCALE_1_0_16_16;

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
                     dirty  => True,
                     minimized => False,
                     appKind => APP_CLIENT,
                     maximized => False,
                     restoreX => surfX,
                     restoreY => surfY,
                     restoreW => reqW,
                     restoreH => reqH,
                     minW => MIN_WIN_W,
                     minH => MIN_WIN_H,
                     maxW => 0,
                     maxH => 0,
                     windowFlags => WINDOW_FLAGS_DEFAULT,
                     bufferAttached => False,
                     bufferGrant => 0,
                     bufferAddr => System.Null_Address,
                     bufferW => 0,
                     bufferH => 0,
                     bufferPitch => 0,
                     bufferFormat => 0);
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
                  clampSurfaceSize (surfaces (SurfaceIndex (idx)), newW, newH);
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

         when OP_WINDOW_SET_LIMITS =>
            declare
               idx  : constant Integer := findSurface (request.words (0));
               minW : Natural :=
                  Natural (request.words (1) and 16#FFFF_FFFF#);
               minH : Natural := Natural (Shift_Right (request.words (1), 32));
               maxW : Natural :=
                  Natural (request.words (2) and 16#FFFF_FFFF#);
               maxH : Natural := Natural (Shift_Right (request.words (2), 32));
               winFlags : constant Unsigned_64 := request.words (3);
               oldBounds : Rect;
               newBounds : Rect;
               nextW : Natural;
               nextH : Natural;
            begin
               replyMsg.tag := (label  => OP_WINDOW_SET_LIMITS,
                                length => 4,
                                flags  => 0,
                                badge  => 0);

               if idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               else
                  if minW < MIN_WIN_W then
                     minW := MIN_WIN_W;
                  end if;
                  if minH < MIN_WIN_H then
                     minH := MIN_WIN_H;
                  end if;
                  if maxW /= 0 and then maxW < minW then
                     maxW := minW;
                  end if;
                  if maxH /= 0 and then maxH < minH then
                     maxH := minH;
                  end if;

                  if (winFlags and WINDOW_FLAG_FIXED_SIZE) /= 0 then
                     maxW := minW;
                     maxH := minH;
                  end if;

                  oldBounds := surfaceRect (surfaces (SurfaceIndex (idx)));
                  surfaces (SurfaceIndex (idx)).minW := minW;
                  surfaces (SurfaceIndex (idx)).minH := minH;
                  surfaces (SurfaceIndex (idx)).maxW := maxW;
                  surfaces (SurfaceIndex (idx)).maxH := maxH;
                  surfaces (SurfaceIndex (idx)).windowFlags := winFlags;
                  nextW := surfaces (SurfaceIndex (idx)).w;
                  nextH := surfaces (SurfaceIndex (idx)).h;
                  clampSurfaceSize (surfaces (SurfaceIndex (idx)),
                                    nextW, nextH);
                  surfaces (SurfaceIndex (idx)).w := nextW;
                  surfaces (SurfaceIndex (idx)).h := nextH;
                  surfaces (SurfaceIndex (idx)).serial :=
                     surfaces (SurfaceIndex (idx)).serial + 1;
                  surfaces (SurfaceIndex (idx)).dirty := True;
                  newBounds := surfaceRect (surfaces (SurfaceIndex (idx)));

                  queueConfigure (surfaces (SurfaceIndex (idx)).id,
                                  Unsigned_64 (newBounds.w),
                                  Unsigned_64 (newBounds.h));

                  replyMsg.words (0) := UI_OK;
                  replyMsg.words (1) := Unsigned_64 (minW) or
                     Shift_Left (Unsigned_64 (minH), 32);
                  replyMsg.words (2) := Unsigned_64 (maxW) or
                     Shift_Left (Unsigned_64 (maxH), 32);
                  replyMsg.words (3) := surfaces (SurfaceIndex (idx)).serial;

                  scheduleRedrawRect
                    (inflateRect (unionRect (oldBounds, newBounds), 4));
               end if;
            end;

         when OP_SURFACE_ATTACH_BUFFER =>
            declare
               idx    : constant Integer := findSurface (request.words (0));
               grant  : constant Unsigned_64 := request.words (1);
               bufW   : Natural :=
                  Natural (request.words (2) and 16#FFFF_FFFF#);
               bufH   : Natural := Natural (Shift_Right (request.words (2), 32));
               pitch  : Natural :=
                  Natural (request.words (3) and 16#FFFF_FFFF#);
               format : constant Unsigned_64 := Shift_Right (request.words (3), 32);
               pages  : Unsigned_64;
            begin
               replyMsg.tag := (label  => OP_SURFACE_ATTACH_BUFFER,
                                length => 4,
                                flags  => 0,
                                badge  => 0);

               if idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif bufW = 0 or else bufH = 0 or else
                  pitch < bufW * 4 or else format /= PIXEL_FORMAT_BGRA8888
               then
                  replyMsg.words (0) := UI_ERR_UNSUPPORTED;
               else
                  pages :=
                    (Unsigned_64 (pitch) * Unsigned_64 (bufH) + 4095) / 4096;
                  if pages > 4096 then
                     replyMsg.words (0) := UI_ERR_BAD_STATE;
                  else
                     surfaces (SurfaceIndex (idx)).bufferAttached := True;
                     surfaces (SurfaceIndex (idx)).bufferGrant := grant;
                     surfaces (SurfaceIndex (idx)).bufferAddr :=
                        To_Address
                          (Integer_Address
                             (GRANT_REGION_BASE + grant * GRANT_SLOT_SIZE));
                     surfaces (SurfaceIndex (idx)).bufferW := bufW;
                     surfaces (SurfaceIndex (idx)).bufferH := bufH;
                     surfaces (SurfaceIndex (idx)).bufferPitch := pitch;
                     surfaces (SurfaceIndex (idx)).bufferFormat := format;
                     surfaces (SurfaceIndex (idx)).dirty := True;
                     surfaces (SurfaceIndex (idx)).serial :=
                        surfaces (SurfaceIndex (idx)).serial + 1;

                     replyMsg.words (0) := UI_OK;
                     replyMsg.words (1) := grant;
                     replyMsg.words (2) := Unsigned_64 (bufW) or
                        Shift_Left (Unsigned_64 (bufH), 32);
                     replyMsg.words (3) :=
                        surfaces (SurfaceIndex (idx)).serial;

                     scheduleRedrawRect
                       (inflateRect
                          (surfaceRect (surfaces (SurfaceIndex (idx))), 4));
                  end if;
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
                  --  A client present changes the client buffer contents, not
                  --  the compositor-owned decoration. Keep high-rate surfaces
                  --  such as DOOM clipped to their content area so every tick
                  --  does the minimum useful work.
                  scheduleRedrawRect
                    (clientRect (surfaces (SurfaceIndex (idx))));
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
            declare
               found : Boolean;
               event : PendingInput;
            begin
               dequeueInput (request.words (0),
                             request.words (1),
                             found,
                             event);
               if found then
                  replyMsg.words (0) := event.kind;
                  replyMsg.words (1) := event.serial;
                  replyMsg.words (2) := event.payload0;
                  replyMsg.words (3) := event.payload1;
               else
                  replyMsg.words (0) := INPUT_NONE;
                  replyMsg.words (1) := request.words (1);
               end if;
            end;

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

   function shouldCycleFocusKey (raw : Unsigned_8) return Boolean is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
   begin
      --  Plain Tab is a prototype stand-in for Alt+Tab until modifier state
      --  is represented in the input model.
      return (not release) and then code = 16#0F#;
   end shouldCycleFocusKey;

   function updateDesktopModifierKey (raw : Unsigned_8) return Boolean is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
   begin
      --  Keep compositor-side modifier state for internal desktop surfaces.
      --  Client surfaces still receive the raw key events; this state is only
      --  used by compositor-owned widgets such as the CuBASIC prototype.
      if code = 16#2A# or else code = 16#36# then
         desktopShiftDown := not release;
         return True;
      elsif code = 16#3A# then
         if not release then
            desktopCapsLockOn := not desktopCapsLockOn;
         end if;
         return True;
      end if;

      return False;
   end updateDesktopModifierKey;

   function keyChar (code : Unsigned_8) return Character is
      pos : Unsigned_8 := 0;
      ch  : Character := Character'Val (0);
   begin
      if code > scancodeNormal'Last then
         return Character'Val (0);
      end if;

      ch := Character'Val (scancodeNormal (code));
      if ch >= 'a' and then ch <= 'z' then
         if desktopShiftDown xor desktopCapsLockOn then
            return Character'Val
              (Character'Pos (ch) - Character'Pos ('a') + Character'Pos ('A'));
         else
            return ch;
         end if;
      elsif desktopShiftDown then
         pos := scancodeShifted (code);
      else
         pos := scancodeNormal (code);
      end if;

      return Character'Val (pos);
   end keyChar;

   function upperChar (ch : Character) return Character is
   begin
      if ch >= 'a' and then ch <= 'z' then
         return Character'Val
           (Character'Pos (ch) - Character'Pos ('a') + Character'Pos ('A'));
      end if;

      return ch;
   end upperChar;

   function consoleMatches (pattern : String) return Boolean is
   begin
      if consoleInputLen /= pattern'Length then
         return False;
      end if;

      for i in pattern'Range loop
         if upperChar (consoleInput (i)) /= pattern (i) then
            return False;
         end if;
      end loop;

      return True;
   end consoleMatches;

   function consoleStartsWith (pattern : String) return Boolean is
   begin
      if consoleInputLen < pattern'Length then
         return False;
      end if;

      for i in pattern'Range loop
         if upperChar (consoleInput (i)) /= pattern (i) then
            return False;
         end if;
      end loop;

      return True;
   end consoleStartsWith;

   procedure setConsoleResult (text : String) is
      count : Natural := text'Length;
   begin
      if count > consoleResult'Length then
         count := consoleResult'Length;
      end if;

      consoleResult := (others => ' ');
      consoleResultLen := count;
      if count > 0 then
         consoleResult (1 .. count) := text (text'First .. text'First + count - 1);
      end if;
   end setConsoleResult;

   procedure setConsoleSpawned (pid : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := pid;
   begin
      if v = 0 then
         setConsoleResult ("SPAWNED PID 0");
         return;
      end if;

      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;

      setConsoleResult ("SPAWNED PID " & buf (pos + 1 .. buf'Last));
   end setConsoleSpawned;

   procedure ensureSpawnGrant is
      raw : Unsigned_64;
      aligned : Unsigned_64;
      grantOk : Boolean;
   begin
      if spawnGrantReady then
         return;
      end if;

      raw := syscall (SYSCALL_SBRK, 8192);
      if raw = Unsigned_64'Last then
         setConsoleResult ("SPAWN BUFFER ALLOCATION FAILED");
         return;
      end if;

      aligned := alignUpPage (raw);
      spawnGrantAddr := To_Address (Integer_Address (aligned));
      createGrantViaCap
        (slot      => CAP_SLOT_PROCMGR,
         localAddr => spawnGrantAddr,
         numPages  => 1,
         readWrite => True,
         grantId   => spawnGrantId,
         success   => grantOk);

      if grantOk then
         spawnGrantReady := True;
      else
         setConsoleResult ("SPAWN GRANT TO PROCMGR FAILED");
      end if;
   end ensureSpawnGrant;

   function lowerChar (ch : Character) return Character is
   begin
      if ch >= 'A' and then ch <= 'Z' then
         return Character'Val
           (Character'Pos (ch) - Character'Pos ('A') + Character'Pos ('a'));
      end if;

      return ch;
   end lowerChar;

   procedure normalizeAppName
      (source : String;
       dest   : out String;
       len    : out Natural)
   is
      first : Natural := source'First;
      last  : Natural := source'Last;
   begin
      dest := (others => ' ');
      len := 0;

      while first <= source'Last and then source (first) = ' ' loop
         first := first + 1;
      end loop;
      while last >= first and then source (last) = ' ' loop
         last := last - 1;
      end loop;

      if first > last then
         return;
      end if;

      for i in first .. last loop
         exit when len = dest'Length;
         len := len + 1;
         dest (len) := lowerChar (source (i));
      end loop;
   end normalizeAppName;

   function hasExtension (name : String) return Boolean is
   begin
      for i in name'Range loop
         if name (i) = '.' then
            return True;
         end if;
      end loop;
      return False;
   end hasExtension;

   procedure trySpawnFromConsole (name : String; ok : out Boolean) is
      msg : Message := NULL_MESSAGE;
      tag : MessageTag;
      len : Natural := name'Length;
   begin
      ok := False;
      if len = 0 or else len > 255 then
         return;
      end if;

      ensureSpawnGrant;
      if not spawnGrantReady then
         return;
      end if;

      declare
         buf : array (0 .. 4095) of Unsigned_8 with
            Import, Address => spawnGrantAddr;
      begin
         for i in 0 .. len - 1 loop
            buf (i) := Unsigned_8
              (Character'Pos (name (name'First + i)));
         end loop;
      end;

      msg.tag := (label  => OP_SPAWN,
                  length => Unsigned_8 (len),
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := spawnGrantId;
      msg.words (1) := 5;
      msg.words (2) := 0;
      msg.words (3) := 0;
      tag := capCall (CAP_SLOT_PROCMGR, msg);

      if tag.label = REPLY_OK then
         setConsoleSpawned (msg.words (0));
         ok := True;
      end if;
   end trySpawnFromConsole;

   procedure spawnFromConsole (name : String) is
      normalized : String (1 .. 64);
      len : Natural;
      ok : Boolean := False;
   begin
      normalizeAppName (name, normalized, len);
      if len = 0 then
         setConsoleResult ("SPAWN NEEDS AN APP NAME");
         return;
      elsif len > 60 then
         setConsoleResult ("SPAWN NAME TOO LONG");
         return;
      end if;

      if hasExtension (normalized (1 .. len)) then
         trySpawnFromConsole (normalized (1 .. len), ok);
      else
         trySpawnFromConsole (normalized (1 .. len) & ".app", ok);
         if not ok then
            trySpawnFromConsole (normalized (1 .. len) & ".elf", ok);
         end if;
      end if;

      if not ok then
         setConsoleResult ("SPAWN FAILED: " & normalized (1 .. len));
      end if;
   end spawnFromConsole;

   procedure pushConsoleLine (text : String) is
      count : Natural := text'Length;
   begin
      if count > CONSOLE_LINE_MAX then
         count := CONSOLE_LINE_MAX;
      end if;

      for row in 1 .. CONSOLE_HISTORY_ROWS - 1 loop
         consoleHistory (row) := consoleHistory (row + 1);
         consoleHistoryLen (row) := consoleHistoryLen (row + 1);
      end loop;

      consoleHistory (CONSOLE_HISTORY_ROWS) := (others => ' ');
      consoleHistoryLen (CONSOLE_HISTORY_ROWS) := count;
      if count > 0 then
         consoleHistory (CONSOLE_HISTORY_ROWS) (1 .. count) :=
            text (text'First .. text'First + count - 1);
      end if;
   end pushConsoleLine;

   procedure pushConsoleInputLine is
      line : ConsoleLine := (others => ' ');
      count : Natural := consoleInputLen + 2;
   begin
      if count > CONSOLE_LINE_MAX then
         count := CONSOLE_LINE_MAX;
      end if;

      line (1) := ']';
      line (2) := ' ';
      if count > 2 then
         line (3 .. count) := consoleInput (1 .. count - 2);
      end if;

      pushConsoleLine (line (1 .. count));
   end pushConsoleInputLine;

   procedure evalConsoleLine is
   begin
      if consoleInputLen = 0 then
         setConsoleResult ("READY.");
      elsif consoleMatches ("HELP") then
         setConsoleResult ("TRY: SERVICES, CAPS, SECRETS, SPAWN <APP>");
      elsif consoleMatches ("LIST SERVICES") then
         setConsoleResult ("desktop.svc display.svc procmgr secrets.svc");
      elsif consoleMatches ("SERVICES") then
         setConsoleResult ("desktop.svc display.svc procmgr secrets.svc");
      elsif consoleMatches ("SHOW CAPS") then
         setConsoleResult ("CAP DISPLAY.INPUT CAP DISPLAY.PRESENT CAP SESSION.OWN");
      elsif consoleMatches ("CAPS") then
         setConsoleResult ("CAP DISPLAY.INPUT CAP DISPLAY.PRESENT CAP SESSION.OWN");
      elsif consoleMatches ("SECRETS") then
         setConsoleResult ("SECRET VALUES ARE OBJECTS, NOT STRINGS");
      elsif consoleStartsWith ("PRINT ") then
         consoleResult := (others => ' ');
         consoleResultLen := consoleInputLen - 6;
         if consoleResultLen > consoleResult'Length then
            consoleResultLen := consoleResult'Length;
         end if;
         if consoleResultLen > 0 then
            consoleResult (1 .. consoleResultLen) :=
               consoleInput (7 .. 6 + consoleResultLen);
         end if;
      elsif consoleStartsWith ("LET ") then
         setConsoleResult ("BOUND VALUE IN THIS REPL SESSION");
      elsif consoleStartsWith ("SPAWN ") then
         spawnFromConsole (consoleInput (7 .. consoleInputLen));
      elsif consoleMatches ("CLS") then
         consoleLast := (others => ' ');
         consoleLastLen := 0;
         consoleHistory := (others => (others => ' '));
         consoleHistoryLen := (others => 0);
         setConsoleResult ("READY.");
      else
         setConsoleResult ("?SYNTAX ERROR");
      end if;
   end evalConsoleLine;

   procedure handleConsoleKey (raw : Unsigned_8; damage : in out Rect) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
      ch      : Character;
      idx     : constant Integer := findSurface (focusSurface);
   begin
      if release then
         return;
      end if;

      ch := keyChar (code);
      if ch = Character'Val (0) then
         return;
      elsif ch = Character'Val (8) then
         if consoleInputLen > 0 then
            consoleInput (consoleInputLen) := ' ';
            consoleInputLen := consoleInputLen - 1;
         end if;
      elsif ch = LF then
         pushConsoleInputLine;
         consoleLast := (others => ' ');
         consoleLastLen := consoleInputLen;
         if consoleInputLen > 0 then
            consoleLast (1 .. consoleInputLen) :=
               consoleInput (1 .. consoleInputLen);
         end if;
         evalConsoleLine;
         if consoleResultLen > 0 then
            pushConsoleLine (consoleResult (1 .. consoleResultLen));
         end if;
         consoleInput := (others => ' ');
         consoleInputLen := 0;
      elsif consoleInputLen < CONSOLE_INPUT_MAX then
         consoleInputLen := consoleInputLen + 1;
         consoleInput (consoleInputLen) := ch;
      end if;

      if idx >= 0 then
         damage := unionRect
           (damage,
            inflateRect (surfaceRect (surfaces (SurfaceIndex (idx))), 4));
      end if;
   end handleConsoleKey;

   function handleInternalKey (raw : Unsigned_8; damage : in out Rect)
      return Boolean
   is
      idx : constant Integer := findSurface (focusSurface);
   begin
      if idx < 0 then
         return False;
      end if;

      if surfaces (SurfaceIndex (idx)).owner /= NO_PROCESS then
         return False;
      end if;

      case surfaces (SurfaceIndex (idx)).appKind is
         when APP_CONSOLE =>
            handleConsoleKey (raw, damage);
            return True;
         when APP_SECURITY | APP_DEMO =>
            return True;
         when others =>
            return False;
      end case;
   end handleInternalKey;

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

   function clampWindowRect (s : Surface; r : Rect) return Rect is
      ret : Rect := r;
   begin
      clampSurfaceSize (s, ret.w, ret.h);

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
            if cursorX > s.x + s.minW then
               r.w := cursorX - s.x;
            else
               r.w := s.minW;
            end if;

         when others =>
            null;
      end case;

      if dragMode = DRAG_RESIZE_S or else dragMode = DRAG_RESIZE_SE then
         if cursorY > s.y + s.minH then
            r.h := cursorY - s.y;
         else
            r.h := s.minH;
         end if;
      end if;

      return clampWindowRect (s, r);
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
      taskIdx   : Integer;
      launchAction : Natural;
      clickedId : Unsigned_64;
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
            launchAction := hitLaunchItem (cursorX, cursorY);
            launchMenuOpen := False;
            damage := unionRect (damage, inflateRect (launchMenuRect, 4));
            case launchAction is
               when LAUNCH_CONSOLE =>
                  openInternalApp (APP_CONSOLE, damage);
               when LAUNCH_SECURITY =>
                  openInternalApp (APP_SECURITY, damage);
               when others =>
                  null;
            end case;
            handledChromeClick := True;
         elsif launchMenuOpen then
            launchMenuOpen := False;
            damage := unionRect (damage, inflateRect (launchMenuRect, 4));
         end if;

         taskIdx := hitTaskButton (cursorX, cursorY);
         if not handledChromeClick and then taskIdx >= 0 then
            clickedId := surfaces (SurfaceIndex (taskIdx)).id;
            if surfaces (SurfaceIndex (taskIdx)).minimized then
               restoreSurface (SurfaceIndex (taskIdx), damage);
            end if;
            taskIdx := findSurface (clickedId);
            if taskIdx >= 0 then
               focusAndRaiseSurface (SurfaceIndex (taskIdx), damage);
            end if;
            handledChromeClick := True;
         end if;

         idx := hitSurface (cursorX, cursorY);
         if not handledChromeClick and then idx >= 0 then
            clickedId := surfaces (SurfaceIndex (idx)).id;
            dragMode := hitMode (surfaces (SurfaceIndex (idx)), cursorX, cursorY);

            if dragMode = HIT_CLOSE then
               --  Window buttons are evaluated against the surface that was
               --  under the pointer at mouse-down. Raising can reshuffle the
               --  surface table, so always refind by id before mutating the
               --  window. This keeps a stale slot from closing/minimizing the
               --  wrong window when several windows overlap.
               focusAndRaiseSurface (SurfaceIndex (idx), damage);
               idx := findSurface (clickedId);
               if idx >= 0 then
                  closeSurface (SurfaceIndex (idx), damage);
               end if;
               dragMode := DRAG_NONE;
            elsif dragMode = HIT_MINIMIZE then
               focusAndRaiseSurface (SurfaceIndex (idx), damage);
               idx := findSurface (clickedId);
               if idx >= 0 then
                  minimizeSurface (SurfaceIndex (idx), damage);
               end if;
               dragMode := DRAG_NONE;
            else
               focusAndRaiseSurface (SurfaceIndex (idx), damage);
               idx := findSurface (clickedId);
               if idx >= 0 then
                  if dragMode = HIT_MAXIMIZE then
                     toggleMaximizeSurface (SurfaceIndex (idx), damage);
                     dragMode := DRAG_NONE;
                  else
                     dragSurfaceId := clickedId;
                     dragOffsetX := cursorX - surfaces (SurfaceIndex (idx)).x;
                     dragOffsetY := cursorY - surfaces (SurfaceIndex (idx)).y;
                     dragPreviewRect := surfaceRect (surfaces (SurfaceIndex (idx)));
                     dragPreviewValid := dragMode /= DRAG_NONE;
                     if dragPreviewValid then
                        damage := unionRect
                          (damage, inflateRect (dragPreviewRect, 4));
                     end if;
                  end if;
               end if;
            end if;
         end if;
      elsif not leftDown and then leftWasDown and then
            dragMode /= DRAG_NONE and then dragSurfaceId /= 0
      then
         idx := findSurface (dragSurfaceId);
         if idx >= 0 and then dragPreviewValid then
            oldBounds := surfaceRect (surfaces (SurfaceIndex (idx)));
            newBounds := clampWindowRect
              (surfaces (SurfaceIndex (idx)), dragPreviewRect);

            surfaces (SurfaceIndex (idx)).x := newBounds.x;
            surfaces (SurfaceIndex (idx)).y := newBounds.y;
            surfaces (SurfaceIndex (idx)).w := newBounds.w;
            surfaces (SurfaceIndex (idx)).h := newBounds.h;
            surfaces (SurfaceIndex (idx)).serial :=
               surfaces (SurfaceIndex (idx)).serial + 1;
            if surfaces (SurfaceIndex (idx)).owner /= NO_PROCESS then
               queueConfigure (surfaces (SurfaceIndex (idx)).id,
                               Unsigned_64 (newBounds.w),
                               Unsigned_64 (newBounds.h));
            end if;

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
         if updateDesktopModifierKey (raw) then
            null;
         end if;

         --  Once a shell/client surface has focus, keyboard events belong to
         --  that surface. The service-level Q/Esc escape remains available
         --  only before a client has connected, which keeps early bring-up
         --  recoverable without stealing application quit keys.
         if shouldCycleFocusKey (raw) then
            declare
               damage : Rect := cursorRect;
            begin
               cycleFocus (damage);
               scheduleRedrawRect (inflateRect (damage, 2));
            end;
         elsif focusSurface /= 0 then
            declare
               damage : Rect := cursorRect;
            begin
               if handleInternalKey (raw, damage) then
                  scheduleRedrawRect (inflateRect (damage, 2));
               else
                  queueKey (raw);
               end if;
            end;
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
      clearInputQueue;
   end releaseDisplayBuffer;

   procedure setupDisplayBuffer (ok : out Boolean) is
      info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
      acquire : Message;
      ignored : Unsigned_64;
      bytes : Unsigned_64;
      pages : Unsigned_64;
      raw   : Unsigned_64;
      aligned : Unsigned_64;
      grantOk : Boolean;
      attach  : Message;
      status  : Message;
      direct : Message;
   begin
      ok := False;

      --  When desktop.svc is spawned from the CLI shell, both processes run
      --  briefly in parallel: the shell releases its display lease only after
      --  procmgr returns the spawn reply. Retry for a short bounded window so
      --  normal foreground handoff is race-free without making display.svc
      --  block indefinitely on a stale owner.
      for attempt in 1 .. 100 loop
         acquire := callDisplay (OP_DISPLAY_ACQUIRE);
         exit when acquire.tag.length >= 1 and then acquire.words (0) = 0;
         ignored := syscall (SYSCALL_SLEEP, 2);
      end loop;

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

      status := callDisplay (OP_DISPLAY_GET_STATUS);
      if status.tag.length >= 2 and then
         (status.words (1) and DISPLAY_CAP_DIRECT_BACKBUFFER) /= 0
      then
         direct := callDisplay (OP_DISPLAY_MAP_BACKBUFFER);
         if direct.tag.length >= 4 and then direct.words (0) = 0 then
            backBufferGrant := direct.words (1);
            backBufferAddr := To_Address
              (Integer_Address
                 (GRANT_REGION_BASE + backBufferGrant * GRANT_SLOT_SIZE));
            fbWidth := unpackLo32 (direct.words (2));
            fbHeight := unpackHi32 (direct.words (2));
            fbPitch := Natural (direct.words (3));
            backBufferReady := True;

            debugPrint ("desktop: direct gpu backbuffer" & LF);
            ok := True;
            return;
         end if;

         debugPrint ("desktop: direct backbuffer unavailable" & LF);
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

   procedure activateInternalSession (ok : out Boolean) is
      displayReady : Boolean := True;
      demoX : Natural := 98;
      demoY : Natural := 72;
      demoW : Natural := 360;
      demoH : Natural := 220;
   begin
      ok := False;

      if not backBufferReady then
         setupDisplayBuffer (displayReady);
      end if;
      if not displayReady then
         return;
      end if;

      if internalShellSurface = 0 then
         createInternalSurface
           (SURFACE_FLAG_SHELL, 0, 0, fbWidth, fbHeight,
            APP_CLIENT, internalShellSurface);
      end if;

      if internalDemoWindow = 0 then
         if demoX + demoW > fbWidth then
            demoW := Natural'Max (MIN_WIN_W, fbWidth - demoX);
         end if;
         if demoY + demoH > fbHeight then
            demoH := Natural'Max (MIN_WIN_H, fbHeight - demoY);
         end if;

         createInternalSurface
           (SURFACE_FLAG_WINDOW, demoX, demoY, demoW, demoH,
            APP_DEMO, internalDemoWindow);
         focusSurface := internalDemoWindow;
      end if;

      if internalShellSurface = 0 then
         return;
      end if;

      claimInput;
      scheduleRedraw;
      ok := True;
   end activateInternalSession;

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

   declare
      activeOk : Boolean;
   begin
      activateInternalSession (activeOk);
      if activeOk then
         debugPrint ("desktop: internal shell active" & LF);
      else
         debugPrint ("desktop: waiting for shell client" & LF);
      end if;
   end;

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
