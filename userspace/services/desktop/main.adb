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
with CuBit.Input;
with CuBit.Theme;
with Desktop_Cursors;
with Desktop_Icons;
with Desktop_UI_Font;
with Desktop_Window_Icons;
with Font8x16;

procedure main is
   use ASCII;
   use type CuBit.Input.Device_Class;
   use type CuBit.Input.Delivery_Class;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;
   SYSINFO_EVENT_DROPS_SELF : constant Unsigned_64 := 1401;

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
   OP_SURFACE_SET_POINTER_CURSOR : constant Unsigned_32 := 16#0815#;
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := 16#0841#;
   OP_WINDOW_SET_TITLE  : constant Unsigned_32 := 16#0842#;
   OP_STREAM_AVAILABLE  : constant Unsigned_32 := 16#0706#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;
   OP_INPUT_WAIT       : constant Unsigned_32 := 16#0822#;
   INPUT_REPLY_MORE_PENDING : constant Unsigned_8 := 1;

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := 16#0900#;
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := 16#0901#;
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := 16#0902#;
   OP_DISPLAY_PRESENT_IMMEDIATE_RECT : constant Unsigned_32 := 16#0908#;
   OP_DISPLAY_PRESENT_REGION : constant Unsigned_32 := 16#0909#;
   OP_DISPLAY_PRESENT_IMMEDIATE_REGION : constant Unsigned_32 := 16#090A#;
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
   INPUT_TEXT      : constant Unsigned_64 := 6;
   INPUT_POINTER_WHEEL : constant Unsigned_64 := 7;
   INPUT_CONFIGURE : constant Unsigned_64 := 8;
   INPUT_RESYNC    : constant Unsigned_64 := 9;

   REQUEST_BUDGET_FRAME : constant Natural := 32;
   REQUEST_BUDGET_IDLE  : constant Natural := 96;

   KEYMOD_SHIFT : constant Unsigned_64 := 1;
   KEYMOD_CTRL  : constant Unsigned_64 := 2;
   KEYMOD_ALT   : constant Unsigned_64 := 4;
   KEYMOD_CAPS  : constant Unsigned_64 := 8;

   KEY_ESCAPE         : constant Unsigned_8 := 16#01#;
   KEY_ENTER          : constant Unsigned_8 := 16#1C#;
   KEY_UP             : constant Unsigned_8 := 16#48#;
   KEY_DOWN           : constant Unsigned_8 := 16#50#;
   KEY_LEFT_SUPER     : constant Unsigned_8 := 16#5B#;
   KEY_RIGHT_SUPER    : constant Unsigned_8 := 16#5C#;
   KEY_EXTENDED_PREFIX : constant Unsigned_8 := 16#E0#;

   PROTOCOL_MAJOR : constant Unsigned_64 := 0;
   PROTOCOL_MINOR : constant Unsigned_64 := 1;
   PROTOCOL_VERSION : constant Unsigned_64 :=
      PROTOCOL_MAJOR or Shift_Left (PROTOCOL_MINOR, 32);

   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 := 1;
   SCALE_1_0_16_16       : constant Unsigned_64 := 16#0001_0000#;
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB
   --  Reserved for a future explicitly loaned display buffer.  Page flipping
   --  is a display-backend detail and occupies 0x0008 in the public protocol.
   DISPLAY_CAP_DIRECT_BACKBUFFER : constant Unsigned_64 := 16#0010#;
   PS_BUF_SIZE : constant Unsigned_64 := 8192;
   PS_ENTRY_SIZE : constant Storage_Offset := 32;

   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   backBufferAddr : System.Address := System.Null_Address;
   backBufferGrant : Unsigned_64 := 0;
   dragBaseBufferAddr : System.Address := System.Null_Address;
   dragBaseReady : Boolean := False;
   dragCacheAnnounced : Boolean := False;
   compositionExcludedSurface : Unsigned_64 := 0;
   spawnGrantAddr : System.Address := System.Null_Address;
   spawnGrantId   : Unsigned_64 := 0;
   spawnGrantReady : Boolean := False;
   lastSpawnedPid : ProcessID := NO_PROCESS;
   doomPid : ProcessID := NO_PROCESS;
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

   --  Relative input may arrive substantially faster than scanout.  Keep
   --  consuming and dispatching every report, but bound software-cursor
   --  presents so a 500/1000 Hz USB mouse cannot serialize the desktop on
   --  synchronous display IPC.  A hardware cursor plane can eventually make
   --  this interval unnecessary; four milliseconds still gives a 250 Hz
   --  visual update budget on the framebuffer path.
   cursorPresentPending : Boolean := False;
   cursorPresentDueMs   : Unsigned_64 := 0;
   lastCursorPresentMs  : Unsigned_64 := 0;
   CURSOR_PRESENT_INTERVAL_MS : constant Unsigned_64 := 4;

   function memcpy
      (dest : System.Address;
       src  : System.Address;
       len  : Storage_Count)
      return System.Address with
      Import => True,
      Convention => C,
      External_Name => "memcpy";

   type App_Kind is (APP_CLIENT, APP_CONSOLE, APP_DOOM);
   for App_Kind use
     (APP_CLIENT => 0, APP_CONSOLE => 1, APP_DOOM => 2);
   for App_Kind'Size use 8;

   type Pointer_Cursor_Style is
     (POINTER_DEFAULT, POINTER_TEXT, POINTER_RESIZE_HORIZONTAL,
      POINTER_RESIZE_VERTICAL, POINTER_RESIZE_DIAGONAL);
   for Pointer_Cursor_Style use
     (POINTER_DEFAULT => 0, POINTER_TEXT => 1,
      POINTER_RESIZE_HORIZONTAL => 2, POINTER_RESIZE_VERTICAL => 3,
      POINTER_RESIZE_DIAGONAL => 4);
   for Pointer_Cursor_Style'Size use 8;

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
      appKind   : App_Kind := APP_CLIENT;
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
      title       : String (1 .. 23) := (others => ' ');
      titleLen    : Natural range 0 .. 23 := 0;
      bufferAttached : Boolean := False;
      bufferGrant    : Unsigned_64 := 0;
      bufferAddr     : System.Address := System.Null_Address;
      bufferW        : Natural := 0;
      bufferH        : Natural := 0;
      bufferPitch    : Natural := 0;
      bufferFormat   : Unsigned_64 := 0;
      pointerCursor  : Pointer_Cursor_Style := POINTER_DEFAULT;
   end record;

   MAX_SURFACES : constant Natural := 8;
   subtype SurfaceIndex is Natural range 0 .. MAX_SURFACES - 1;
   type SurfaceTable is array (SurfaceIndex) of Surface;

   surfaces : SurfaceTable;

   MAX_STREAM_ANNOUNCEMENTS : constant Natural := 16;
   subtype StreamAnnouncementIndex is Natural
      range 0 .. MAX_STREAM_ANNOUNCEMENTS - 1;
   type StreamAnnouncement is record
      used : Boolean := False;
      pid  : ProcessID := NO_PROCESS;
      mask : Unsigned_64 := 0;
   end record;
   type StreamAnnouncementTable is array (StreamAnnouncementIndex) of
      StreamAnnouncement;

   streamAnnouncements : StreamAnnouncementTable;

   nextSurfaceId : Unsigned_64 := 1;
   focusSurface  : Unsigned_64 := 0;
   internalShellSurface : Unsigned_64 := 0;
   inputOwned : Boolean := False;

   type PendingInput is record
      valid   : Boolean := False;
      serial  : Unsigned_64 := 0;
      kind    : Unsigned_64 := INPUT_NONE;
      target  : Unsigned_64 := 0;
      payload0 : Unsigned_64 := 0;
      payload1 : Unsigned_64 := 0;
   end record;

   INPUT_QUEUE_SIZE : constant Natural := 32;
   subtype InputQueueIndex is Natural range 0 .. INPUT_QUEUE_SIZE - 1;
   type PendingInputQueue is array (InputQueueIndex) of PendingInput;

   type InputSnapshot is record
      pointerPosition : Unsigned_64 := 0;
      buttons         : Unsigned_64 := 0;
      modifiers       : Unsigned_64 := 0;
      generation      : Unsigned_64 := 0;
   end record;

   --  A client blocks in OP_INPUT_WAIT while desktop retains the kernel-minted
   --  one-use reply capability. One slot per surface makes waiter ownership
   --  explicit and prevents one client from consuming another client's wake.
   INPUT_REPLY_SLOT_FIRST : constant CapabilitySlot := 32;
   type InputWaiter is record
      active      : Boolean := False;
      owner       : ProcessID := NO_PROCESS;
      target      : Unsigned_64 := 0;
      afterSerial : Unsigned_64 := 0;
      replySlot   : CapabilitySlot := INPUT_REPLY_SLOT_FIRST;
   end record;

   --  Input channels are keyed by stable surface ID, not z-order table index.
   --  Raising a window reorders Surface records; it must not move a pending
   --  reply capability or accidentally attach queued input to another window.
   type SurfaceInputChannel is record
      target   : Unsigned_64 := 0;
      nextSerial : Unsigned_64 := 1;
      events   : PendingInputQueue := (others => (others => <>));
      snapshot : InputSnapshot;
      waiter   : InputWaiter;
   end record;
   type SurfaceInputChannelTable is
     array (SurfaceIndex) of SurfaceInputChannel;
   inputChannels : SurfaceInputChannelTable;
   inputQueueOverflows : Unsigned_64 := 0;

   cursorX : Natural := 80;
   cursorY : Natural := 80;
   cursorStyle : Pointer_Cursor_Style := POINTER_DEFAULT;
   lastButtons : Unsigned_64 := 0;

   MAX_INPUT_SOURCES : constant Natural := 8;
   subtype InputSourceIndex is Natural range 0 .. MAX_INPUT_SOURCES - 1;
   type InputSourceState is record
      used       : Boolean := False;
      badge      : Unsigned_64 := 0;
      device     : CuBit.Input.Device_Class := CuBit.Input.KEYBOARD;
      generation : CuBit.Input.Source_Generation := 0;
      sequence   : CuBit.Input.Source_Sequence := 0;
      buttons    : Unsigned_64 := 0;
   end record;
   type InputSourceTable is array (InputSourceIndex) of InputSourceState;
   inputSources : InputSourceTable := (others => (others => <>));

   pointerSurfaceId : Unsigned_64 := 0;
   launchMenuOpen : Boolean := False;
   desktopExtendedPrefix : Boolean := False;
   desktopShiftDown : Boolean := False;
   desktopCtrlDown  : Boolean := False;
   desktopAltDown   : Boolean := False;
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

   type Launch_Action is
     (LAUNCH_NONE, LAUNCH_CONSOLE, LAUNCH_WORKBENCH, LAUNCH_DOOM,
      LAUNCH_DEVICES, LAUNCH_BROWSER, LAUNCH_FILES, LAUNCH_POWER);
   for Launch_Action use
     (LAUNCH_NONE => 0, LAUNCH_CONSOLE => 1, LAUNCH_WORKBENCH => 2,
      LAUNCH_DOOM => 3, LAUNCH_DEVICES => 4, LAUNCH_BROWSER => 5,
      LAUNCH_FILES => 6, LAUNCH_POWER => 7);
   for Launch_Action'Size use 8;

   launchMenuSelection : Launch_Action := LAUNCH_CONSOLE;

   function nextLaunchSelection
      (current : Launch_Action;
       upward  : Boolean) return Launch_Action
   is
   begin
      if upward then
         case current is
            when LAUNCH_CONSOLE   => return LAUNCH_FILES;
            when LAUNCH_WORKBENCH => return LAUNCH_CONSOLE;
            when LAUNCH_DOOM      => return LAUNCH_WORKBENCH;
            when LAUNCH_DEVICES   => return LAUNCH_DOOM;
            when LAUNCH_BROWSER   => return LAUNCH_DEVICES;
            when LAUNCH_FILES     => return LAUNCH_BROWSER;
            when others           => return LAUNCH_CONSOLE;
         end case;
      else
         case current is
            when LAUNCH_CONSOLE   => return LAUNCH_WORKBENCH;
            when LAUNCH_WORKBENCH => return LAUNCH_DOOM;
            when LAUNCH_DOOM      => return LAUNCH_DEVICES;
            when LAUNCH_DEVICES   => return LAUNCH_BROWSER;
            when LAUNCH_BROWSER   => return LAUNCH_FILES;
            when LAUNCH_FILES     => return LAUNCH_CONSOLE;
            when others           => return LAUNCH_CONSOLE;
         end case;
      end if;
   end nextLaunchSelection;

   type Pointer_Action is
     (DRAG_NONE, DRAG_MOVE, DRAG_RESIZE_E, DRAG_RESIZE_S, DRAG_RESIZE_SE,
      HIT_MINIMIZE, HIT_CLOSE, HIT_MAXIMIZE);
   for Pointer_Action use
     (DRAG_NONE => 0, DRAG_MOVE => 1, DRAG_RESIZE_E => 2,
      DRAG_RESIZE_S => 3, DRAG_RESIZE_SE => 4, HIT_MINIMIZE => 5,
      HIT_CLOSE => 6, HIT_MAXIMIZE => 7);
   for Pointer_Action'Size use 8;
   dragMode         : Pointer_Action := DRAG_NONE;
   dragSurfaceId    : Unsigned_64 := 0;
   dragOffsetX      : Natural := 0;
   dragOffsetY      : Natural := 0;
   dragPreviewValid : Boolean := False;
   dragPreviewRect  : Rect;
   --  The preview visible in the last completed compositor frame can lag the
   --  latest pointer report.  Keep it separately so a burst of reports only
   --  erases the outline that was actually scanned out, not every
   --  intermediate position in the burst.
   dragPresentedValid : Boolean := False;
   dragPresentedRect  : Rect;

   TITLE_HEIGHT : constant Natural := 24;
   BORDER_SIZE  : constant Natural := 6;
   CLIENT_INSET_X : constant Natural := 4;
   CLIENT_INSET_TOP : constant Natural := 30;
   CLIENT_INSET_BOTTOM : constant Natural := 4;
   DROP_SHADOW_DEPTH : constant Positive := 3;
   --  Compositor decorations extend beyond the client-visible window
   --  geometry. Damage retains one extra pixel beyond the shadow so every
   --  pixel painted by the old geometry is restored during movement.
   WINDOW_VISUAL_MARGIN : constant Natural := DROP_SHADOW_DEPTH + 1;
   TASKBAR_H    : constant Natural := 36;
   LAUNCH_W     : constant Natural := 88;
   LAUNCH_H     : constant Natural := 24;
   MENU_W       : constant Natural := 250;
   MENU_H       : constant Natural := 286;
   TASK_BUTTON_W : constant Natural := 156;
   TASK_BUTTON_H : constant Natural := 24;
   TASK_BUTTON_GAP : constant Natural := 6;
   CURSOR_SAVE_STRIDE : constant Positive := Desktop_Cursors.MAX_WIDTH;
   CURSOR_PIXELS : constant Positive :=
      Desktop_Cursors.MAX_WIDTH * Desktop_Cursors.MAX_HEIGHT;
   MIN_WIN_W    : constant Natural := 120;
   MIN_WIN_H    : constant Natural := 80;

   type CursorSaveBuffer is array (Natural range 0 .. CURSOR_PIXELS - 1)
      of Unsigned_32;
   cursorSave      : CursorSaveBuffer := (others => 0);
   cursorSaveValid : Boolean := False;
   cursorSaveRect  : Rect;

   C_BG     : constant Unsigned_32 := CuBit.Theme.Desktop;
   C_PANEL  : constant Unsigned_32 := CuBit.Theme.Panel;
   C_TEXT   : constant Unsigned_32 := CuBit.Theme.Text;
   C_MUTED  : constant Unsigned_32 := CuBit.Theme.Muted;
   C_ACCENT : constant Unsigned_32 := CuBit.Theme.Accent;
   C_GOOD   : constant Unsigned_32 := CuBit.Theme.Good;
   C_WHITE  : constant Unsigned_32 := CuBit.Theme.White;
   C_BLACK  : constant Unsigned_32 := CuBit.Theme.Black;
   C_DESK   : constant Unsigned_32 := CuBit.Theme.Desktop;
   C_BAR    : constant Unsigned_32 := CuBit.Theme.Panel;
   C_BLUE   : constant Unsigned_32 := CuBit.Theme.Accent;
   C_WIN    : constant Unsigned_32 := CuBit.Theme.Face;
   C_EDGE   : constant Unsigned_32 := CuBit.Theme.Edge;
   C_SHADOW : constant Unsigned_32 := CuBit.Theme.Shadow;

   statsStartMs      : Unsigned_64 := 0;
   statsEvents       : Unsigned_64 := 0;
   statsKeyboardEvents : Unsigned_64 := 0;
   statsMouseEvents  : Unsigned_64 := 0;
   statsButtonTransitions : Unsigned_64 := 0;
   statsWheelEvents  : Unsigned_64 := 0;
   statsRequests     : Unsigned_64 := 0;
   statsFrames       : Unsigned_64 := 0;
   statsFastFrames   : Unsigned_64 := 0;
   statsFullFrames   : Unsigned_64 := 0;
   statsPresentReq   : Unsigned_64 := 0;
   statsInputReq     : Unsigned_64 := 0;
   statsOtherReq     : Unsigned_64 := 0;
   statsDrawMs       : Unsigned_64 := 0;
   statsPresentOps   : Unsigned_64 := 0;
   statsPresentMs    : Unsigned_64 := 0;
   statsDamagePixels : Unsigned_64 := 0;
   statsSourceGaps   : Unsigned_64 := 0;
   statsSourceRejects : Unsigned_64 := 0;
   lastEventDrops    : Unsigned_64 := 0;
   lastInputQueueOverflows : Unsigned_64 := 0;
   inputTraceBudget  : Natural := 64;

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
      eventDrops : constant Unsigned_64 :=
         getInfo (SYSINFO_EVENT_DROPS_SELF);
      eventDropsThisPeriod : Unsigned_64 := 0;
      inputOverflowsThisPeriod : Unsigned_64 := 0;
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

      if eventDrops /= Unsigned_64'Last then
         if eventDrops >= lastEventDrops then
            eventDropsThisPeriod := eventDrops - lastEventDrops;
         else
            --  Unsigned telemetry wrapped between observations.
            eventDropsThisPeriod := eventDrops;
         end if;
         lastEventDrops := eventDrops;
      end if;

      if inputQueueOverflows >= lastInputQueueOverflows then
         inputOverflowsThisPeriod :=
           inputQueueOverflows - lastInputQueueOverflows;
      else
         inputOverflowsThisPeriod := inputQueueOverflows;
      end if;
      lastInputQueueOverflows := inputQueueOverflows;

      if statsFrames > 0 or else statsEvents > 0 then
         debugPrint ("desktop: stats ev=");
         printDec (statsEvents);
         debugPrint (" key=");
         printDec (statsKeyboardEvents);
         debugPrint (" mouse=");
         printDec (statsMouseEvents);
         debugPrint (" button=");
         printDec (statsButtonTransitions);
         debugPrint (" wheel=");
         printDec (statsWheelEvents);
         debugPrint (" event_drop=");
         printDec (eventDropsThisPeriod);
         debugPrint (" input_resync=");
         printDec (inputOverflowsThisPeriod);
         debugPrint (" source_gap=");
         printDec (statsSourceGaps);
         debugPrint (" source_reject=");
         printDec (statsSourceRejects);
         debugPrint (" req=");
         printDec (statsRequests);
         debugPrint (" frames=");
         printDec (statsFrames);
         debugPrint (" fast=");
         printDec (statsFastFrames);
         debugPrint (" full=");
         printDec (statsFullFrames);
         debugPrint (" present_req=");
         printDec (statsPresentReq);
         debugPrint (" input_req=");
         printDec (statsInputReq);
         debugPrint (" other_req=");
         printDec (statsOtherReq);
         debugPrint (" draw_ms=");
         printDec (statsDrawMs);
         debugPrint (" submit=");
         printDec (statsPresentOps);
         debugPrint (" submit_ms=");
         printDec (statsPresentMs);
         debugPrint (" px=");
         printDec (statsDamagePixels);
         debugPrint (" cursor_x=");
         printDec (Unsigned_64 (cursorX));
         debugPrint (" cursor_y=");
         printDec (Unsigned_64 (cursorY));
         debugPrint ("" & LF);
      end if;

      statsStartMs := now;
      statsEvents := 0;
      statsKeyboardEvents := 0;
      statsMouseEvents := 0;
      statsButtonTransitions := 0;
      statsWheelEvents := 0;
      statsRequests := 0;
      statsFrames := 0;
      statsFastFrames := 0;
      statsFullFrames := 0;
      statsPresentReq := 0;
      statsInputReq := 0;
      statsOtherReq := 0;
      statsDrawMs := 0;
      statsPresentOps := 0;
      statsPresentMs := 0;
      statsDamagePixels := 0;
      statsSourceGaps := 0;
      statsSourceRejects := 0;
   end maybePrintStats;

   procedure tracePointer
      (label : String;
       a, b, c : Unsigned_64 := 0)
   is
   begin
      if inputTraceBudget = 0 then
         return;
      end if;

      inputTraceBudget := inputTraceBudget - 1;
      debugPrint ("desktop: ptr ");
      debugPrint (label);
      debugPrint (" ");
      printDec (a);
      debugPrint (" ");
      printDec (b);
      debugPrint (" ");
      printDec (c);
      debugPrint ("" & LF);
   end tracePointer;

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

   function cursorAsset return Desktop_Cursors.Cursor_ID is
   begin
      case cursorStyle is
         when POINTER_DEFAULT =>
            return Desktop_Cursors.Arrow;
         when POINTER_TEXT =>
            return Desktop_Cursors.Text;
         when POINTER_RESIZE_HORIZONTAL =>
            return Desktop_Cursors.Horizontal_Resize;
         when POINTER_RESIZE_VERTICAL =>
            return Desktop_Cursors.Vertical_Resize;
         when POINTER_RESIZE_DIAGONAL =>
            return Desktop_Cursors.Diagonal_Resize;
      end case;
   end cursorAsset;

   function cursorWidth return Positive is
     (Desktop_Cursors.Metadata (cursorAsset).Width);

   function cursorHeight return Positive is
     (Desktop_Cursors.Metadata (cursorAsset).Height);

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

   function cursorHotX return Natural is
   begin
      return Desktop_Cursors.Metadata (cursorAsset).Hotspot_X;
   end cursorHotX;

   function cursorHotY return Natural is
   begin
      return Desktop_Cursors.Metadata (cursorAsset).Hotspot_Y;
   end cursorHotY;

   function cursorOriginX return Integer is
     (Integer (cursorX) - Integer (cursorHotX));

   function cursorOriginY return Integer is
     (Integer (cursorY) - Integer (cursorHotY));

   function cursorRect return Rect is
      originX : constant Integer := cursorOriginX;
      originY : constant Integer := cursorOriginY;
      left : constant Natural := Natural (Integer'Max (0, originX));
      top : constant Natural := Natural (Integer'Max (0, originY));
      right : constant Natural := Natural'Min
        (fbWidth,
         Natural (Integer'Max (0, originX + Integer (cursorWidth))));
      bottom : constant Natural := Natural'Min
        (fbHeight,
         Natural (Integer'Max (0, originY + Integer (cursorHeight))));
   begin
      if right <= left or else bottom <= top then
         return (others => 0);
      end if;
      return (x => left, y => top, w => right - left, h => bottom - top);
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

   function launchItemRect (action : Launch_Action) return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural := menu.y + 34;
   begin
      if isEmpty (menu) or else action = LAUNCH_NONE or else menu.w <= 16 then
         return (others => 0);
      end if;

      case action is
         when LAUNCH_CONSOLE =>
            y := menu.y + 42;
         when LAUNCH_WORKBENCH =>
            y := menu.y + 76;
         when LAUNCH_DOOM =>
            y := menu.y + 110;
         when LAUNCH_DEVICES =>
            y := menu.y + 144;
         when LAUNCH_BROWSER =>
            y := menu.y + 178;
         when LAUNCH_FILES =>
            y := menu.y + 212;
         when LAUNCH_POWER =>
            y := menu.y + 252;
         when others =>
            return (others => 0);
      end case;

      return clampRect ((x => menu.x + 8, y => y,
                         w => menu.w - 16, h => 30));
   end launchItemRect;

   function launchSeparatorRect return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural := 0;
   begin
      if isEmpty (menu) or else menu.w <= 24 then
         return (others => 0);
      end if;
      y := menu.y + 244;
      return clampRect ((x => menu.x + 12, y => y,
                         w => menu.w - 24, h => 1));
   end launchSeparatorRect;

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

   function hitLaunchItem (x, y : Natural) return Launch_Action is
   begin
      if pointInRect (x, y, launchItemRect (LAUNCH_CONSOLE)) then
         return LAUNCH_CONSOLE;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_WORKBENCH)) then
         return LAUNCH_WORKBENCH;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_DOOM)) then
         return LAUNCH_DOOM;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_DEVICES)) then
         return LAUNCH_DEVICES;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_BROWSER)) then
         return LAUNCH_BROWSER;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_FILES)) then
         return LAUNCH_FILES;
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

   function rectContains (outer, inner : Rect) return Boolean is
   begin
      return not isEmpty (outer) and then
         not isEmpty (inner) and then
         inner.x >= outer.x and then
         inner.y >= outer.y and then
         inner.x + inner.w <= outer.x + outer.w and then
         inner.y + inner.h <= outer.y + outer.h;
   end rectContains;

   function rectIntersects (a, b : Rect) return Boolean is
   begin
      return not isEmpty (a) and then
         not isEmpty (b) and then
         a.x < b.x + b.w and then
         b.x < a.x + a.w and then
         a.y < b.y + b.h and then
         b.y < a.y + a.h;
   end rectIntersects;

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

   function windowVisualRect (r : Rect) return Rect is
   begin
      return inflateRect (r, WINDOW_VISUAL_MARGIN);
   end windowVisualRect;

   function clientRect (s : Surface) return Rect is
   begin
      if s.w <= CLIENT_INSET_X * 2 or else
         s.h <= CLIENT_INSET_TOP + CLIENT_INSET_BOTTOM
      then
         return (others => 0);
      end if;

      return clampRect
        ((x => s.x + CLIENT_INSET_X,
          y => s.y + CLIENT_INSET_TOP,
          w => s.w - CLIENT_INSET_X * 2,
          h => s.h - CLIENT_INSET_TOP - CLIENT_INSET_BOTTOM));
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

   function signed8 (x : Unsigned_64) return Integer is
      v : constant Unsigned_64 := x and 16#FF#;
   begin
      if (v and 16#80#) /= 0 then
         return Integer (v) - 256;
      else
         return Integer (v);
      end if;
   end signed8;

   function packI32Buttons
      (value : Integer;
       buttons : Unsigned_64) return Unsigned_64
   is
      low : Unsigned_64;
   begin
      if value < 0 then
         low := 16#1_0000_0000# - Unsigned_64 (-value);
      else
         low := Unsigned_64 (value);
      end if;

      return (low and 16#FFFF_FFFF#) or
             Shift_Left (buttons and 16#FFFF_FFFF#, 32);
   end packI32Buttons;

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

   function hitMode (s : Surface; x, y : Natural) return Pointer_Action is
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

   function cursorStyleAtPointer return Pointer_Cursor_Style is
      idx : constant Integer := hitSurface (cursorX, cursorY);
      action : Pointer_Action := DRAG_NONE;
   begin
      if dragMode /= DRAG_NONE then
         action := dragMode;
      elsif idx >= 0 then
         action := hitMode
           (surfaces (SurfaceIndex (idx)), cursorX, cursorY);
      end if;

      case action is
         when DRAG_RESIZE_E => return POINTER_RESIZE_HORIZONTAL;
         when DRAG_RESIZE_S => return POINTER_RESIZE_VERTICAL;
         when DRAG_RESIZE_SE => return POINTER_RESIZE_DIAGONAL;
         when others => null;
      end case;

      if idx >= 0 and then
        pointInRect
          (cursorX, cursorY, clientRect (surfaces (SurfaceIndex (idx))))
      then
         return surfaces (SurfaceIndex (idx)).pointerCursor;
      end if;
      return POINTER_DEFAULT;
   end cursorStyleAtPointer;

   type Present_Timing is (PRESENT_AT_VBLANK, PRESENT_IMMEDIATELY);

   procedure flushBackBufferRect
      (dirty  : Rect;
       timing : Present_Timing := PRESENT_AT_VBLANK)
   is
      r : constant Rect := clampRect (dirty);
      msg : Message :=
        (tag      =>
           (label  =>
              (if timing = PRESENT_IMMEDIATELY
               then OP_DISPLAY_PRESENT_IMMEDIATE_RECT
               else OP_DISPLAY_PRESENT_RECT),
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
      pairColor : constant Unsigned_64 :=
         Shift_Left (Unsigned_64 (color), 32) or Unsigned_64 (color);
      startX : Natural;
      endX : Natural;
      offset : Storage_Offset;
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
         startX := minX;
         endX := maxX;

         if startX < endX and then startX mod 2 /= 0 then
            declare
               offset : constant Storage_Offset :=
                  Storage_Offset (yy * fbPitch + startX * 4);
               pixel : Unsigned_32 with
                  Import, Address => backBufferAddr + offset;
            begin
               pixel := color;
            end;
            startX := startX + 1;
         end if;

         while startX + 1 < endX loop
            offset := Storage_Offset (yy * fbPitch + startX * 4);
            declare
               pixels : Unsigned_64 with
                  Import, Address => backBufferAddr + offset;
            begin
               pixels := pairColor;
            end;
            startX := startX + 2;
         end loop;

         if startX < endX then
            offset := Storage_Offset (yy * fbPitch + startX * 4);
            declare
               pixel : Unsigned_32 with
                  Import, Address => backBufferAddr + offset;
            begin
               pixel := color;
            end;
         end if;
      end loop;
   end fillRect;

   procedure drawDappledShadow (x, y, w, h : Natural) is
   begin
      if w = 0 or else h = 0 then
         return;
      end if;

      --  A screen-anchored checker leaves half of the underlying scene
      --  visible. Besides looking lighter than a solid slab, keeping parity
      --  anchored to absolute coordinates prevents the pattern itself from
      --  crawling as a window moves.
      for yy in y + DROP_SHADOW_DEPTH .. y + h + DROP_SHADOW_DEPTH - 1 loop
         for xx in x + w .. x + w + DROP_SHADOW_DEPTH - 1 loop
            if (xx + yy) mod 2 = 0 then
               putPixel (xx, yy, C_BLACK);
            end if;
         end loop;
      end loop;

      for yy in y + h .. y + h + DROP_SHADOW_DEPTH - 1 loop
         for xx in x + DROP_SHADOW_DEPTH .. x + w - 1 loop
            if (xx + yy) mod 2 = 0 then
               putPixel (xx, yy, C_BLACK);
            end if;
         end loop;
      end loop;
   end drawDappledShadow;

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
      minX : Natural := x;
      minY : Natural := y;
      maxX : Natural := x + Font8x16.GLYPH_WIDTH;
      maxY : Natural := y + Font8x16.GLYPH_HEIGHT;
      offset : Storage_Offset;
      row : Natural;
      bit : Natural;
   begin
      if backBufferAddr = System.Null_Address or else
         x >= fbWidth or else y >= fbHeight
      then
         return;
      end if;

      if maxX > fbWidth then
         maxX := fbWidth;
      end if;
      if maxY > fbHeight then
         maxY := fbHeight;
      end if;

      for yy in minY .. maxY - 1 loop
         row := yy - y;
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for xx in minX .. maxX - 1 loop
               bit := xx - x;
               offset := Storage_Offset (yy * fbPitch + xx * 4);
               declare
                  pixel : Unsigned_32 with
                     Import, Address => backBufferAddr + offset;
               begin
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                     pixel := fg;
               else
                     pixel := bg;
               end if;
               end;
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

   function blendPixel
      (src : Unsigned_32;
       dst : Unsigned_32;
       alpha : Natural) return Unsigned_32
   is
      inv  : constant Natural := 255 - alpha;
      sr   : constant Natural := Natural (Shift_Right (src, 16) and 16#FF#);
      sg   : constant Natural := Natural (Shift_Right (src, 8) and 16#FF#);
      sb   : constant Natural := Natural (src and 16#FF#);
      dr   : constant Natural := Natural (Shift_Right (dst, 16) and 16#FF#);
      dg   : constant Natural := Natural (Shift_Right (dst, 8) and 16#FF#);
      db   : constant Natural := Natural (dst and 16#FF#);
      rr   : constant Natural := (sr * alpha + dr * inv + 127) / 255;
      rg   : constant Natural := (sg * alpha + dg * inv + 127) / 255;
      rb   : constant Natural := (sb * alpha + db * inv + 127) / 255;
   begin
      return Shift_Left (Unsigned_32 (rr), 16) or
             Shift_Left (Unsigned_32 (rg), 8) or
             Unsigned_32 (rb);
   end blendPixel;

   function iconPixelOver
      (srcARGB : Unsigned_32;
       bg      : Unsigned_32) return Unsigned_32
   is
      alpha : constant Natural :=
         Natural (Shift_Right (srcARGB, 24) and 16#FF#);
      src   : constant Unsigned_32 := srcARGB and 16#00FF_FFFF#;
   begin
      if alpha = 0 then
         return bg;
      elsif alpha = 255 then
         return src;
      else
         return blendPixel (src, bg, alpha);
      end if;
   end iconPixelOver;

   function premultipliedPixelOver
      (srcARGB : Unsigned_32;
       bg      : Unsigned_32) return Unsigned_32
   is
      alpha : constant Natural :=
         Natural (Shift_Right (srcARGB, 24) and 16#FF#);
      inv   : constant Natural := 255 - alpha;
      sr    : constant Natural :=
         Natural (Shift_Right (srcARGB, 16) and 16#FF#);
      sg    : constant Natural :=
         Natural (Shift_Right (srcARGB, 8) and 16#FF#);
      sb    : constant Natural := Natural (srcARGB and 16#FF#);
      dr    : constant Natural := Natural (Shift_Right (bg, 16) and 16#FF#);
      dg    : constant Natural := Natural (Shift_Right (bg, 8) and 16#FF#);
      db    : constant Natural := Natural (bg and 16#FF#);
      rr    : constant Natural := sr + (dr * inv + 127) / 255;
      rg    : constant Natural := sg + (dg * inv + 127) / 255;
      rb    : constant Natural := sb + (db * inv + 127) / 255;
   begin
      if alpha = 0 then
         return bg;
      elsif alpha = 255 then
         return srcARGB and 16#00FF_FFFF#;
      else
         return Shift_Left (Unsigned_32 (Natural'Min (rr, 255)), 16) or
                Shift_Left (Unsigned_32 (Natural'Min (rg, 255)), 8) or
                Unsigned_32 (Natural'Min (rb, 255));
      end if;
   end premultipliedPixelOver;

   procedure drawIcon
      (id : Desktop_Icons.Icon_ID;
       x, y : Natural;
       bg   : Unsigned_32)
   is
      pixel : Unsigned_32;
   begin
      for yy in 0 .. Desktop_Icons.ICON_SIZE - 1 loop
         for xx in 0 .. Desktop_Icons.ICON_SIZE - 1 loop
            pixel := Desktop_Icons.Pixels (id)
              (yy * Desktop_Icons.ICON_SIZE + xx);
            if Shift_Right (pixel, 24) /= 0 then
               putPixel (x + xx, y + yy, iconPixelOver (pixel, bg));
            end if;
         end loop;
      end loop;
   end drawIcon;

   procedure drawWindowIcon
      (id : Desktop_Window_Icons.Icon_ID;
       x, y : Natural;
       bg   : Unsigned_32)
   is
      pixel : Unsigned_32;
   begin
      for yy in 0 .. Desktop_Window_Icons.ICON_SIZE - 1 loop
         for xx in 0 .. Desktop_Window_Icons.ICON_SIZE - 1 loop
            pixel := Desktop_Window_Icons.Pixels (id)
              (yy * Desktop_Window_Icons.ICON_SIZE + xx);
            if Shift_Right (pixel, 24) /= 0 then
               putPixel (x + xx, y + yy, iconPixelOver (pixel, bg));
            end if;
         end loop;
      end loop;
   end drawWindowIcon;

   procedure drawWindowButtonIcon
      (button : Rect;
       id     : Desktop_Window_Icons.Icon_ID;
       bg     : Unsigned_32)
   is
      iconX : Natural := button.x;
      iconY : Natural := button.y;
   begin
      if button.w > Desktop_Window_Icons.ICON_SIZE then
         iconX := button.x + (button.w - Desktop_Window_Icons.ICON_SIZE) / 2;
      end if;
      if button.h > Desktop_Window_Icons.ICON_SIZE then
         iconY := button.y + (button.h - Desktop_Window_Icons.ICON_SIZE) / 2;
      end if;

      drawWindowIcon (id, iconX, iconY, bg);
   end drawWindowButtonIcon;

   function uiTextWidth (s : String) return Natural is
      width : Natural := 0;
      code  : Natural;
   begin
      for i in s'Range loop
         code := Character'Pos (s (i));
         if code >= Desktop_UI_Font.FIRST_GLYPH and then
            code <= Desktop_UI_Font.LAST_GLYPH
         then
            width := width + Desktop_UI_Font.Widths (code);
         else
            width := width + Desktop_UI_Font.Widths (Character'Pos ('?'));
         end if;
      end loop;
      return width;
   end uiTextWidth;

   procedure drawUIGlyph
      (x, y : Natural;
       ch   : Character;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      code  : Natural := Character'Pos (ch);
      width : Natural;
      alpha : Natural;
   begin
      if code < Desktop_UI_Font.FIRST_GLYPH or else
         code > Desktop_UI_Font.LAST_GLYPH
      then
         code := Character'Pos ('?');
      end if;

      width := Desktop_UI_Font.Widths (code);
      fillRect (x, y, width, Desktop_UI_Font.LINE_HEIGHT, bg);

      for yy in 0 .. Desktop_UI_Font.GLYPH_HEIGHT - 1 loop
         for xx in 0 .. width - 1 loop
            alpha := Natural (Desktop_UI_Font.Alpha (code) (yy) (xx));
            if alpha = 255 then
               putPixel (x + xx, y + yy, fg);
            elsif alpha /= 0 then
               putPixel (x + xx, y + yy, blendPixel (fg, bg, alpha));
            end if;
         end loop;
      end loop;
   end drawUIGlyph;

   procedure drawUIText
      (x, y : Natural;
       s    : String;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      cx : Natural := x;
   begin
      for i in s'Range loop
         drawUIGlyph (cx, y, s (i), fg, bg);
         cx := cx + uiTextWidth (s (i .. i));
      end loop;
   end drawUIText;

   procedure drawSurfaceTitle
      (s       : Surface;
       x, y    : Natural;
       fg, bg  : Unsigned_32)
   is
   begin
      case s.appKind is
         when APP_CONSOLE =>
            drawUIText (x, y, "CuBASIC Console", fg, bg);
         when APP_DOOM =>
            drawUIText (x, y, "DOOM", fg, bg);
         when others =>
            if s.titleLen > 0 then
               drawUIText (x, y, s.title (1 .. s.titleLen), fg, bg);
            else
               drawUIText (x, y, "Application", fg, bg);
            end if;
      end case;
   end drawSurfaceTitle;

   function streamName (id : Natural) return String is
   begin
      case id is
         when 1 => return "in";
         when 2 => return "out";
         when 3 => return "err";
         when 4 => return "audit";
         when others => return "s";
      end case;
   end streamName;

   function streamMaskForPID (pid : ProcessID) return Unsigned_64 is
   begin
      for i in streamAnnouncements'Range loop
         if streamAnnouncements (i).used and then
            streamAnnouncements (i).pid = pid
         then
            return streamAnnouncements (i).mask;
         end if;
      end loop;
      return 0;
   end streamMaskForPID;

   procedure rememberStreams (pid : ProcessID; mask : Unsigned_64) is
      firstFree : Integer := -1;
   begin
      if pid = NO_PROCESS then
         return;
      end if;

      for i in streamAnnouncements'Range loop
         if streamAnnouncements (i).used and then
            streamAnnouncements (i).pid = pid
         then
            streamAnnouncements (i).mask := mask;
            return;
         elsif not streamAnnouncements (i).used and then firstFree < 0 then
            firstFree := Integer (i);
         end if;
      end loop;

      if firstFree < 0 then
         firstFree := 0;
      end if;

      streamAnnouncements (StreamAnnouncementIndex (firstFree)) :=
        (used => True, pid => pid, mask => mask);
   end rememberStreams;

   procedure drawStreamBadges
      (s : Surface;
       titleY : Natural)
   is
      mask : constant Unsigned_64 := streamMaskForPID (s.owner);
      closeBtn : constant Rect := closeButtonRect (s);
      maxBtn : constant Rect := maximizeButtonRect (s);
      rightLimit : Natural := s.x + s.w - 8;
      badgeW : constant Natural := 38;
      badgeH : constant Natural := 14;
      x : Natural;
      drawn : Natural := 0;
      r : Rect;
   begin
      if mask = 0 or else s.w < 190 then
         return;
      end if;

      if not isEmpty (closeBtn) then
         rightLimit := closeBtn.x - 6;
      elsif not isEmpty (maxBtn) then
         rightLimit := maxBtn.x - 6;
      end if;

      if rightLimit < s.x + 120 then
         return;
      end if;

      x := rightLimit;
      for bit in 1 .. 7 loop
         exit when drawn >= 3;
         if (mask and Shift_Left (Unsigned_64'(1), bit)) /= 0 then
            exit when x < s.x + 120 + badgeW;
            x := x - badgeW;
            r := (x => x, y => titleY + 5, w => badgeW, h => badgeH);
            fillRect (r.x, r.y, r.w, r.h, C_BAR);
            strokeRect (r.x, r.y, r.w, r.h, C_EDGE, C_SHADOW);
            drawUIText (r.x + 4, r.y + 2, streamName (bit), C_TEXT, C_BAR);
            x := x - 4;
            drawn := drawn + 1;
         end if;
      end loop;
   end drawStreamBadges;

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
               cursorSave (yy * CURSOR_SAVE_STRIDE + xx));
         end loop;
      end loop;

      cursorSaveValid := False;
   end restoreCursorOverlay;

   procedure drawCursorOverlay is
      r : constant Rect := cursorRect;
      originX : constant Integer := cursorOriginX;
      originY : constant Integer := cursorOriginY;
      asset : constant Desktop_Cursors.Cursor_ID := cursorAsset;
      metadata : constant Desktop_Cursors.Cursor_Metadata :=
         Desktop_Cursors.Metadata (asset);
      shapeX, shapeY : Integer;
      pixel : Unsigned_32;
      background : Unsigned_32;
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
            cursorSave (yy * CURSOR_SAVE_STRIDE + xx) :=
               readBackPixel (r.x + xx, r.y + yy);
         end loop;
      end loop;

      cursorSaveRect := r;
      cursorSaveValid := True;

      for yy in 0 .. r.h - 1 loop
         for xx in 0 .. r.w - 1 loop
            shapeX := Integer (r.x + xx) - originX;
            shapeY := Integer (r.y + yy) - originY;
            if shapeX >= 0 and then shapeY >= 0 and then
              shapeX < Integer (metadata.Width) and then
              shapeY < Integer (metadata.Height)
            then
               pixel := Desktop_Cursors.Pixels
                 (metadata.Offset + Natural (shapeY) * metadata.Width +
                    Natural (shapeX));
               if Shift_Right (pixel, 24) /= 0 then
                  background := cursorSave (yy * CURSOR_SAVE_STRIDE + xx);
                  writeBackPixel
                    (r.x + xx, r.y + yy,
                     premultipliedPixelOver (pixel, background));
               end if;
            end if;
         end loop;
      end loop;
   end drawCursorOverlay;

   procedure noteCursorPresented is
      now : constant Unsigned_64 := nowMs;
   begin
      cursorPresentPending := False;
      cursorPresentDueMs := 0;
      if now /= Unsigned_64'Last then
         lastCursorPresentMs := now;
      end if;
   end noteCursorPresented;

   procedure presentCursorOverlay is
      newCursor : constant Rect := cursorRect;
      oldCursor : Rect := newCursor;
      damage    : Rect;
   begin
      if cursorSaveValid then
         oldCursor := cursorSaveRect;
      end if;
      damage := unionRect (oldCursor, newCursor);
      restoreCursorOverlay;
      drawCursorOverlay;
      damage := inflateRect (damage, 1);
      --  Cursor feedback is latency-critical and this damage is only a small
      --  rectangle. Waiting for legacy VGA vertical blank for every input
      --  packet serializes the input path at the refresh rate and can fill
      --  the bounded event ring. display.svc still owns scanout, but copies
      --  this explicitly marked damage immediately.
      flushBackBufferRect (damage, PRESENT_IMMEDIATELY);
      noteCursorPresented;
   end presentCursorOverlay;

   procedure scheduleCursorPresent is
      now : constant Unsigned_64 := nowMs;
      due : Unsigned_64;
   begin
      if now = Unsigned_64'Last or else lastCursorPresentMs = 0 or else
         now < lastCursorPresentMs or else
         now - lastCursorPresentMs >= CURSOR_PRESENT_INTERVAL_MS
      then
         presentCursorOverlay;
         return;
      end if;

      due := lastCursorPresentMs + CURSOR_PRESENT_INTERVAL_MS;
      if not cursorPresentPending or else cursorPresentDueMs = 0 or else
         due < cursorPresentDueMs
      then
         cursorPresentPending := True;
         cursorPresentDueMs := due;
      end if;
   end scheduleCursorPresent;

   procedure flushCursorPresent is
      now : constant Unsigned_64 := nowMs;
   begin
      if not cursorPresentPending then
         return;
      end if;

      if now = Unsigned_64'Last or else cursorPresentDueMs = 0 or else
         now < lastCursorPresentMs or else now >= cursorPresentDueMs
      then
         presentCursorOverlay;
      end if;
   end flushCursorPresent;

   function tryFastClientRedraw (dirty : Rect) return Boolean is
      r : constant Rect := clampRect (dirty);
      c : Rect;
      oldCursor : Rect := (others => 0);
      damage : Rect;
      occluded : Boolean;
   begin
      if isEmpty (r) or else launchMenuOpen or else
         not backBufferReady or else backBufferAddr = System.Null_Address
      then
         return False;
      end if;

      for i in surfaces'Range loop
         if surfaces (i).used and then
            not surfaces (i).minimized and then
            surfaces (i).bufferAttached and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            c := clientRect (surfaces (i));
            if rectContains (c, r) then
               occluded := False;

               if i < SurfaceIndex'Last then
                  for j in SurfaceIndex'Succ (i) .. SurfaceIndex'Last loop
                     if surfaces (j).used and then
                        not surfaces (j).minimized and then
                        (surfaces (j).flags and SURFACE_FLAG_WINDOW) /= 0
                        and then rectIntersects (r, surfaceRect (surfaces (j)))
                     then
                        occluded := True;
                        exit;
                     end if;
                  end loop;
               end if;

               if not occluded then
                  if cursorSaveValid then
                     oldCursor := cursorSaveRect;
                  end if;

                  statsFastFrames := statsFastFrames + 1;
                  restoreCursorOverlay;
                  drawClientBuffer (surfaces (i), c.x, c.y, c.w, c.h);
                  drawCursorOverlay;

                  damage := unionRect (r, cursorRect);
                  damage := unionRect (damage, oldCursor);
                  flushBackBufferRect (damage);
                  return True;
               end if;
            end if;
         end if;
      end loop;

      return False;
   end tryFastClientRedraw;

   procedure drawWindow (s : Surface) is
      titleH : constant Natural := 24;
      minW   : constant Natural := 80;
      minH   : constant Natural := 60;
      active  : constant Boolean := s.id = focusSurface;
      frame   : Surface := s;
      minBtn  : Rect;
      maxBtn  : Rect;
      closeBtn : Rect;
      titleColor : Unsigned_32 := C_PANEL;
      titleText  : Unsigned_32 := C_TEXT;
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
         titleText := C_WHITE;
      end if;

      frame.x := x;
      frame.y := y;
      frame.w := w;
      frame.h := h;

      drawDappledShadow (x, y, w, h);
      fillRect (x, y, w, h, C_WIN);
      strokeRect (x, y, w, h, C_EDGE, C_SHADOW);
      fillRect (x + 3, y + 3, w - 6, titleH, titleColor);
      drawSurfaceTitle (s, x + 10, y + 7, titleText, titleColor);
      drawStreamBadges (s, y + 3);

      --  Window controls are compositor-owned because they mutate focus,
      --  visibility, and eventually client lifecycle authority.
      minBtn := minimizeButtonRect (frame);
      maxBtn := maximizeButtonRect (frame);
      closeBtn := closeButtonRect (frame);

      if not isEmpty (minBtn) then
         fillRect (minBtn.x, minBtn.y, minBtn.w, minBtn.h, C_BAR);
         strokeRect (minBtn.x, minBtn.y, minBtn.w, minBtn.h,
                     C_EDGE, C_SHADOW);
         drawWindowButtonIcon
           (minBtn, Desktop_Window_Icons.Minimize, C_BAR);
      end if;

      if not isEmpty (maxBtn) then
         fillRect (maxBtn.x, maxBtn.y, maxBtn.w, maxBtn.h, C_BAR);
         strokeRect (maxBtn.x, maxBtn.y, maxBtn.w, maxBtn.h,
                     C_EDGE, C_SHADOW);
         if s.maximized then
            drawWindowButtonIcon
              (maxBtn, Desktop_Window_Icons.Restore, C_BAR);
         else
            drawWindowButtonIcon
              (maxBtn, Desktop_Window_Icons.Maximize, C_BAR);
         end if;
      end if;

      if not isEmpty (closeBtn) then
         fillRect (closeBtn.x, closeBtn.y, closeBtn.w, closeBtn.h, C_BAR);
         strokeRect (closeBtn.x, closeBtn.y, closeBtn.w, closeBtn.h,
                     C_EDGE, C_SHADOW);
         drawWindowButtonIcon
           (closeBtn, Desktop_Window_Icons.Close, C_BAR);
      end if;

      case s.appKind is
         when APP_CONSOLE =>
            fillRect (x + 14, y + 40, w - 28, h - 56, C_SHADOW);
            drawConsoleText (x + 24, y + 50, C_SHADOW);
         when others =>
            if s.bufferAttached then
               declare
                  c : constant Rect := clientRect (frame);
               begin
                  if not isEmpty (c) then
                     if s.bufferW < c.w or else s.bufferH < c.h then
                        fillRect (c.x, c.y, c.w, c.h, C_WIN);
                     end if;
                     drawClientBuffer (s, c.x, c.y, c.w, c.h);
                  end if;
               end;
            else
               drawUIText (x + 18, y + 44, "This is a real child surface.",
                           C_TEXT, C_WIN);
               drawUIText (x + 18, y + 70, "Drag title bar to move.",
                           C_TEXT, C_WIN);
               drawUIText (x + 18, y + 96, "Drag edges to resize.",
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

      procedure drawLaunchItem
         (action : Launch_Action;
          icon   : Desktop_Icons.Icon_ID;
          label  : String;
          fg     : Unsigned_32)
      is
         item : constant Rect := launchItemRect (action);
         selected : constant Boolean := launchMenuSelection = action;
         bg : constant Unsigned_32 :=
           (if selected then C_ACCENT else C_PANEL);
         textColor : constant Unsigned_32 :=
           (if selected then C_WHITE else fg);
      begin
         if selected then
            fillRect (item.x, item.y, item.w, item.h, bg);
         end if;
         drawIcon (icon, item.x + 8, item.y + 3, bg);
         drawUIText (item.x + 40, item.y + 7, label, textColor, bg);
      end drawLaunchItem;
   begin
      if not launchMenuOpen or else isEmpty (r) then
         return;
      end if;

      drawDappledShadow (r.x, r.y, r.w, r.h);
      fillRect (r.x, r.y, r.w, r.h, C_PANEL);
      strokeRect (r.x, r.y, r.w, r.h, C_EDGE, C_SHADOW);
      fillRect (r.x, r.y, 4, r.h, C_ACCENT);

      drawIcon (Desktop_Icons.Start, r.x + 12, r.y + 10, C_PANEL);
      drawUIText (r.x + 44, r.y + 14, "CuBit", C_TEXT, C_PANEL);
      drawLaunchItem
        (LAUNCH_CONSOLE, Desktop_Icons.Console, "CuBASIC", C_TEXT);
      drawLaunchItem
        (LAUNCH_WORKBENCH, Desktop_Icons.UILab, "CCL Workbench", C_TEXT);
      drawLaunchItem (LAUNCH_DOOM, Desktop_Icons.Doom, "DOOM", C_TEXT);
      drawLaunchItem
        (LAUNCH_DEVICES, Desktop_Icons.Files, "Devices", C_TEXT);
      drawLaunchItem
        (LAUNCH_BROWSER, Desktop_Icons.Files, "NetSurf", C_TEXT);
      drawLaunchItem
        (LAUNCH_FILES, Desktop_Icons.Files, "Files", C_TEXT);
      declare
         sep : constant Rect := launchSeparatorRect;
      begin
         fillRect (sep.x, sep.y, sep.w, sep.h, C_EDGE);
      end;
      drawLaunchItem
        (LAUNCH_POWER, Desktop_Icons.Power, "Power", C_MUTED);
   end drawLaunchMenu;

   procedure drawDesktopShell is
      barY     : constant Natural := taskbarY;
      launch   : constant Rect := launchButtonRect;
      launchIconY : constant Natural :=
         launch.y +
         (if launch.h > Desktop_Icons.ICON_SIZE
          then (launch.h - Desktop_Icons.ICON_SIZE) / 2
          else 0);
      launchTextY : constant Natural :=
         launch.y +
         (if launch.h > Desktop_UI_Font.LINE_HEIGHT
          then (launch.h - Desktop_UI_Font.LINE_HEIGHT) / 2
          else 0);
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
      drawIcon (Desktop_Icons.Start, launch.x + 5, launchIconY, C_BAR);
      drawUIText (launch.x + 34, launchTextY, "Apps", C_TEXT, C_BAR);
      drawTaskButtons;

      for i in surfaces'Range loop
         if surfaces (i).used and then
            not surfaces (i).minimized and then
            surfaces (i).id /= compositionExcludedSurface and then
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
         fillRect (0, 0, fbWidth, fbHeight, C_DESK);
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

      if tryFastClientRedraw (r) then
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

   function findSurface (id : Unsigned_64) return Integer;

   procedure prepareMoveBase (surfaceId : Unsigned_64) is
      savedBuffer : constant System.Address := backBufferAddr;
      savedClipEnabled : constant Boolean := clipEnabled;
      savedClip : constant Rect := clipRect;
      savedDrawing : constant Boolean := drawingBackBuffer;
      savedExcluded : constant Unsigned_64 := compositionExcludedSurface;
   begin
      dragBaseReady := False;
      if dragBaseBufferAddr = System.Null_Address or else
        backBufferAddr = System.Null_Address or else surfaceId = 0
      then
         return;
      end if;

      --  Build the stable scene underneath the moving top-level surface once
      --  at drag start. Subsequent pointer reports restore pixels from this
      --  retained layer and draw only the moving window; they never traverse
      --  and repaint the complete desktop scene.
      restoreCursorOverlay;
      backBufferAddr := dragBaseBufferAddr;
      drawingBackBuffer := True;
      clipEnabled := False;
      compositionExcludedSurface := surfaceId;
      drawCurrentScene;

      backBufferAddr := savedBuffer;
      drawingBackBuffer := savedDrawing;
      clipEnabled := savedClipEnabled;
      clipRect := savedClip;
      compositionExcludedSurface := savedExcluded;
      dragBaseReady := True;
      if not dragCacheAnnounced then
         debugPrint ("desktop: retained move path active" & LF);
         dragCacheAnnounced := True;
      end if;
   end prepareMoveBase;

   procedure redrawCachedMove
     (dirty : Rect;
      pixelCount : out Unsigned_64;
      handled : out Boolean)
   is
      idx : constant Integer := findSurface (dragSurfaceId);
      r : Rect := dirty;
      ignore : System.Address;
   begin
      pixelCount := 0;
      handled := False;
      if dragMode /= DRAG_MOVE or else not dragBaseReady or else
        dragBaseBufferAddr = System.Null_Address or else idx < 0
      then
         return;
      end if;

      if dragPresentedValid then
         r := unionRect (r, windowVisualRect (dragPresentedRect));
      end if;
      r := clampRect
        (unionRect (r, windowVisualRect (dragPreviewRect)));
      if isEmpty (r) then
         handled := True;
         return;
      end if;

      restoreCursorOverlay;
      for row in r.y .. r.y + r.h - 1 loop
         ignore := memcpy
           (backBufferAddr + Storage_Offset (row * fbPitch + r.x * 4),
            dragBaseBufferAddr +
              Storage_Offset (row * fbPitch + r.x * 4),
            Storage_Count (r.w * 4));
      end loop;

      clipRect := r;
      clipEnabled := True;
      drawingBackBuffer := backBufferReady;
      drawWindow (surfaces (SurfaceIndex (idx)));
      clipEnabled := False;
      drawCursorOverlay;
      drawingBackBuffer := False;
      flushBackBufferRect (unionRect (r, cursorRect));

      pixelCount := Unsigned_64 (r.w) * Unsigned_64 (r.h);
      handled := True;
   end redrawCachedMove;

   procedure redrawDragFrame
      (dirty : Rect;
       pixelCount : out Unsigned_64)
   is
      MAX_RECTS : constant Positive := 12;
      OUTLINE_THICKNESS : constant Positive := 4;
      subtype Damage_Index is Positive range 1 .. MAX_RECTS;
      type Damage_Array is array (Damage_Index) of Rect;
      regions : Damage_Array;
      count : Natural range 0 .. MAX_RECTS := 0;
      cachedHandled : Boolean;

      procedure Add (candidate : Rect) is
         r : constant Rect := clampRect (candidate);
      begin
         if isEmpty (r) then
            return;
         end if;

         --  One ordinary damage rectangle plus four old and four new outline
         --  strips fit by construction.  Retaining separate strips avoids
         --  turning a hollow wireframe into a window-sized bounding repaint.
         if count < MAX_RECTS then
            count := count + 1;
            regions (Damage_Index (count)) := r;
         else
            regions (Damage_Index'First) :=
              unionRect (regions (Damage_Index'First), r);
         end if;
      end Add;

      procedure Add_Outline (bounds : Rect) is
         r : constant Rect := clampRect (bounds);
         edgeW : Natural;
         edgeH : Natural;
      begin
         if isEmpty (r) then
            return;
         end if;
         edgeW := Natural'Min (OUTLINE_THICKNESS, r.w);
         edgeH := Natural'Min (OUTLINE_THICKNESS, r.h);
         Add ((x => r.x, y => r.y, w => r.w, h => edgeH));
         Add ((x => r.x, y => r.y + r.h - edgeH,
               w => r.w, h => edgeH));
         Add ((x => r.x, y => r.y, w => edgeW, h => r.h));
         Add ((x => r.x + r.w - edgeW, y => r.y,
               w => edgeW, h => r.h));
      end Add_Outline;

      function Pack_Rect (r : Rect) return Unsigned_64 is
      begin
         return
           Unsigned_64 (r.x) or
           Shift_Left (Unsigned_64 (r.y), 16) or
           Shift_Left (Unsigned_64 (r.w), 32) or
           Shift_Left (Unsigned_64 (r.h), 48);
      end Pack_Rect;

      procedure Present_Regions is
         first : Natural := 1;
         batchCount : Natural;
         request : Message := NULL_MESSAGE;
         t0, t1 : Unsigned_64;
      begin
         --  The packed display protocol supports framebuffer coordinates up
         --  to 65535 and four rectangles per IPC message. This is deliberately
         --  a scanout protocol limit, not a UI-coordinate limitation.
         if fbWidth > 16#FFFF# or else fbHeight > 16#FFFF# then
            for i in Damage_Index'First .. Damage_Index (count) loop
               flushBackBufferRect
                 (regions (i),
                  (if i = Damage_Index'First
                   then PRESENT_AT_VBLANK else PRESENT_IMMEDIATELY));
            end loop;
            return;
         end if;

         while first <= count loop
            batchCount := Natural'Min (4, count - first + 1);
            request := NULL_MESSAGE;
            request.tag :=
              (label =>
                 (if first = 1 then OP_DISPLAY_PRESENT_REGION
                  else OP_DISPLAY_PRESENT_IMMEDIATE_REGION),
               length => Unsigned_8 (batchCount), flags => 0, badge => 0);
            for offset in 0 .. batchCount - 1 loop
               request.words (offset) :=
                 Pack_Rect (regions (Damage_Index (first + offset)));
            end loop;

            t0 := syscall (SYSCALL_GETTIME);
            request.tag := capCall (CAP_SLOT_DISPLAY, request);
            t1 := syscall (SYSCALL_GETTIME);
            statsPresentOps := statsPresentOps + 1;
            if t0 /= Unsigned_64'Last and then t1 /= Unsigned_64'Last and then
              t1 >= t0
            then
               statsPresentMs := statsPresentMs + (t1 - t0);
            end if;
            first := first + batchCount;
         end loop;
      end Present_Regions;
   begin
      redrawCachedMove (dirty, pixelCount, cachedHandled);
      if cachedHandled then
         return;
      end if;

      pixelCount := 0;
      if dragMode = DRAG_MOVE and then dragPresentedValid then
         --  A complete moved window plus the pixels exposed at its old
         --  position cover essentially the union of the old and new bounds.
         --  Render that union once: multiple clipped scene traversals save no
         --  memory traffic here and can expose partially updated geometry.
         Add
           (unionRect
              (dirty,
               unionRect
                 (windowVisualRect (dragPresentedRect),
                  windowVisualRect (dragPreviewRect))));
      else
         Add (dirty);
         if dragPresentedValid then
            Add_Outline (dragPresentedRect);
         end if;
         if dragPreviewValid then
            Add_Outline (dragPreviewRect);
         end if;
      end if;

      if fbBpp /= 32 or else count = 0 then
         return;
      end if;

      restoreCursorOverlay;
      drawingBackBuffer := backBufferReady;
      for i in Damage_Index'First .. Damage_Index (count) loop
         clipRect := regions (i);
         clipEnabled := True;
         drawCurrentScene;
         pixelCount := pixelCount +
           Unsigned_64 (regions (i).w) * Unsigned_64 (regions (i).h);
      end loop;
      clipEnabled := False;
      drawCursorOverlay;

      if drawingBackBuffer then
         drawingBackBuffer := False;
         --  A region is one display transaction and one vblank decision, so
         --  scanout cannot expose each strip as a separate compositor frame.
         Present_Regions;
      end if;
   end redrawDragFrame;

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

   procedure clearInputForTarget (target : Unsigned_64);

   procedure reapDeadClientSurfaces (damage : in out Rect);

   procedure flushFrame is
      damage : Rect := frameDamage;
      now : constant Unsigned_64 := nowMs;
      t0 : Unsigned_64;
      t1 : Unsigned_64;
      full : Boolean;
      damagePixels : Unsigned_64 := 0;
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
         damagePixels := Unsigned_64 (damage.w) * Unsigned_64 (damage.h);
      elsif dragPresentedValid or else dragPreviewValid then
         redrawDragFrame (damage, damagePixels);
      else
         redrawRect (damage);
         damagePixels := Unsigned_64 (damage.w) * Unsigned_64 (damage.h);
      end if;
      dragPresentedValid :=
        (dragMode = DRAG_MOVE and then dragSurfaceId /= 0) or else
        dragPreviewValid;
      if dragPresentedValid then
         dragPresentedRect := dragPreviewRect;
      end if;
      noteCursorPresented;
      t1 := syscall (SYSCALL_GETTIME);

      statsFrames := statsFrames + 1;
      if full then
         statsFullFrames := statsFullFrames + 1;
      end if;
      statsDamagePixels := statsDamagePixels + damagePixels;
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
      oldOwner  : constant ProcessID := surfaces (idx).owner;
      button    : constant Rect := taskButtonRect (idx);
      ignore    : Unsigned_64;
   begin
      --  Internal windows can disappear immediately. For client-owned windows
      --  this is a temporary hard close. The protocol should later
      --  grow a close-request event so clients can save state or refuse, but
      --  removing only the surface leaves clients polling a dead object.
      if pointerSurfaceId = oldId then
         pointerSurfaceId := 0;
      end if;
      if dragSurfaceId = oldId then
         dragSurfaceId := 0;
         dragMode := DRAG_NONE;
         dragPreviewValid := False;
         dragPresentedValid := False;
      end if;
      clearInputForTarget (oldId);

      surfaces (idx) := (others => <>);

      if focusSurface = oldId then
         focusSurface := 0;
      end if;

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));

      if focusSurface = 0 then
         focusTopmostVisibleWindow (damage);
      end if;

      if oldOwner /= NO_PROCESS and then processAlive (oldOwner) then
         ignore := killProcess (oldOwner);
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
            if pointerSurfaceId = surfaces (i).id then
               pointerSurfaceId := 0;
            end if;
            if dragSurfaceId = surfaces (i).id then
               dragSurfaceId := 0;
               dragMode := DRAG_NONE;
               dragPreviewValid := False;
               dragPresentedValid := False;
            end if;
            clearInputForTarget (surfaces (i).id);
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
       appKind : App_Kind;
       id    : out Unsigned_64);

   procedure openInternalApp (appKind : App_Kind; damage : in out Rect) is
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
       appKind : App_Kind;
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
         title => (others => ' '),
         titleLen => 0,
         bufferAttached => False,
         bufferGrant => 0,
         bufferAddr => System.Null_Address,
         bufferW => 0,
         bufferH => 0,
         bufferPitch => 0,
         bufferFormat => 0,
         pointerCursor => POINTER_DEFAULT);
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

   procedure completeInputWaiter (target : Unsigned_64);

   procedure clearInputQueue;

   function findInputChannel (target : Unsigned_64) return Integer is
   begin
      for i in inputChannels'Range loop
         if inputChannels (i).target = target and then target /= 0 then
            return Integer (i);
         end if;
      end loop;
      return -1;
   end findInputChannel;

   procedure ensureInputChannel
      (target : Unsigned_64;
       channelSlot : out Integer)
   is
      surfaceSlot : constant Integer := findSurface (target);
      c : Rect;
      localX : Natural := 0;
      localY : Natural := 0;
      mods : Unsigned_64 := 0;
   begin
      channelSlot := findInputChannel (target);
      if channelSlot >= 0 or else target = 0 or else surfaceSlot < 0
      then
         return;
      end if;

      c := clientRect (surfaces (SurfaceIndex (surfaceSlot)));
      if cursorX >= c.x then
         localX := cursorX - c.x;
      end if;
      if cursorY >= c.y then
         localY := cursorY - c.y;
      end if;
      if desktopShiftDown then
         mods := mods or KEYMOD_SHIFT;
      end if;
      if desktopCtrlDown then
         mods := mods or KEYMOD_CTRL;
      end if;
      if desktopAltDown then
         mods := mods or KEYMOD_ALT;
      end if;
      if desktopCapsLockOn then
         mods := mods or KEYMOD_CAPS;
      end if;

      for i in inputChannels'Range loop
         if inputChannels (i).target = 0 then
            inputChannels (i) :=
              (target => target,
               snapshot =>
                 (pointerPosition => packU32Pair (localX, localY),
                  buttons => lastButtons,
                  modifiers => mods,
                  generation => 0),
               others => <>);
            channelSlot := Integer (i);
            return;
         end if;
      end loop;
   end ensureInputChannel;

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
      channelSlot : Integer;
      slot : Integer := -1;
      newestForTarget : Integer := -1;
      newestSerial : Unsigned_64 := 0;
   begin
      ensureInputChannel (target, channelSlot);
      if channelSlot < 0 then
         return;
      end if;

      declare
         idx : constant SurfaceIndex := SurfaceIndex (channelSlot);
         queue : PendingInputQueue renames inputChannels (idx).events;
         snapshot : InputSnapshot renames inputChannels (idx).snapshot;
      begin
         --  Maintain recoverable state before attempting bounded delivery.
         --  INPUT_RESYNC can therefore describe the state after the report
         --  whose insertion discovered overflow.
         if kind = INPUT_POINTER_MOVE or else
            kind = INPUT_POINTER_DOWN or else
            kind = INPUT_POINTER_UP
         then
            snapshot.pointerPosition := payload0;
            snapshot.buttons := payload1 and 16#FFFF_FFFF#;
         elsif kind = INPUT_POINTER_WHEEL then
            snapshot.pointerPosition := payload0;
            snapshot.buttons := Shift_Right (payload1, 32);
         elsif kind = INPUT_KEY_DOWN or else kind = INPUT_KEY_UP then
            snapshot.modifiers := payload1 and 16#FFFF_FFFF#;
         end if;

         if kind = INPUT_POINTER_MOVE then
            --  Collapse motion only when it is the newest pending event for
            --  this surface. Press, release, wheel, and key events are strict
            --  ordering barriers.
            for i in queue'Range loop
               if queue (i).valid and then
                  queue (i).serial > newestSerial
               then
                  newestForTarget := Integer (i);
                  newestSerial := queue (i).serial;
               end if;
            end loop;

            if newestForTarget >= 0 and then
               queue (InputQueueIndex (newestForTarget)).kind =
                 INPUT_POINTER_MOVE
            then
               queue (InputQueueIndex (newestForTarget)).payload0 := payload0;
               queue (InputQueueIndex (newestForTarget)).payload1 := payload1;
               completeInputWaiter (target);
               return;
            end if;
         end if;

         for i in queue'Range loop
            if not queue (i).valid and then slot < 0 then
               slot := Integer (i);
            end if;
         end loop;

         if slot < 0 then
            --  Never silently replace an ordered transition. The application
            --  is told that continuity was lost and receives authoritative
            --  pointer/button/modifier state. A stalled surface cannot consume
            --  another surface's queue capacity.
            queue := (others => (others => <>));
            snapshot.generation := snapshot.generation + 1;
            inputQueueOverflows := inputQueueOverflows + 1;
            slot := Integer (queue'First);
            queue (queue'First) :=
              (valid    => True,
               serial   => inputChannels (idx).nextSerial,
               kind     => INPUT_RESYNC,
               target   => target,
               payload0 => snapshot.pointerPosition,
               payload1 =>
                 (snapshot.buttons and 16#FFFF_FFFF#) or
                 Shift_Left
                   (snapshot.modifiers and 16#FFFF_FFFF#, 32));
         else
            queue (InputQueueIndex (slot)) :=
              (valid    => True,
               serial   => inputChannels (idx).nextSerial,
               kind     => kind,
               target   => target,
               payload0 => payload0,
               payload1 => payload1);
         end if;

         inputChannels (idx).nextSerial :=
           inputChannels (idx).nextSerial + 1;
      end;

      completeInputWaiter (target);
   end enqueueInput;

   procedure dequeueInput
      (target : Unsigned_64;
       afterSerial : Unsigned_64;
       found : out Boolean;
       event : out PendingInput)
   is
      channelSlot : constant Integer := findInputChannel (target);
      best : Integer := -1;
      bestSerial : Unsigned_64 := Unsigned_64'Last;
   begin
      found := False;
      event := (others => <>);

      if channelSlot < 0 then
         return;
      end if;

      declare
         queue : PendingInputQueue renames
           inputChannels (SurfaceIndex (channelSlot)).events;
      begin
         for i in queue'Range loop
            if queue (i).valid and then queue (i).serial <= afterSerial then
               --  A retried wait may name an event already consumed through
               --  another path. Stale entries can no longer be observed.
               queue (i).valid := False;
            elsif queue (i).valid and then
               queue (i).serial < bestSerial
            then
               best := Integer (i);
               bestSerial := queue (i).serial;
            end if;
         end loop;

         if best >= 0 then
            event := queue (InputQueueIndex (best));
            queue (InputQueueIndex (best)).valid := False;
            found := True;
         end if;
      end;
   end dequeueInput;

   function hasInputAfter
     (target : Unsigned_64; afterSerial : Unsigned_64) return Boolean
   is
      channelSlot : constant Integer := findInputChannel (target);
   begin
      if channelSlot < 0 then
         return False;
      end if;

      for item of inputChannels (SurfaceIndex (channelSlot)).events loop
         if item.valid and then item.serial > afterSerial then
            return True;
         end if;
      end loop;
      return False;
   end hasInputAfter;

   procedure completeInputWaiter (target : Unsigned_64) is
      channelSlot : constant Integer := findInputChannel (target);
      event : PendingInput;
      found : Boolean;
      response : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if channelSlot < 0 or else
         not inputChannels (SurfaceIndex (channelSlot)).waiter.active
      then
         return;
      end if;

      declare
         waiter : InputWaiter renames
           inputChannels (SurfaceIndex (channelSlot)).waiter;
      begin
         dequeueInput (target, waiter.afterSerial, found, event);
         if not found then
            return;
         end if;

         response.tag :=
           (label => OP_INPUT_WAIT, length => 4,
            flags =>
              (if hasInputAfter (target, event.serial)
               then INPUT_REPLY_MORE_PENDING else 0),
            badge => 0);
         response.words (0) := event.kind;
         response.words (1) := event.serial;
         response.words (2) := event.payload0;
         response.words (3) := event.payload1;

         --  Clear software ownership before consuming the one-use kernel
         --  authority. A failed reply cannot leave an immortal waiter.
         declare
            slot : constant CapabilitySlot := waiter.replySlot;
         begin
            waiter := (replySlot => slot, others => <>);
            ignore := replyCap (slot, response);
         end;
      end;
   end completeInputWaiter;

   --  A source-stream discontinuity invalidates transient ownership and every
   --  queued transition derived from the old stream history. Publish one
   --  authoritative state record per surface instead of attempting to guess
   --  which lost press/release events should be replayed.
   procedure forceInputResynchronization is
      mods : Unsigned_64 := 0;
   begin
      pointerSurfaceId := 0;
      dragSurfaceId := 0;
      dragMode := DRAG_NONE;
      dragPreviewValid := False;
      dragPresentedValid := False;
      desktopExtendedPrefix := False;

      if desktopShiftDown then
         mods := mods or KEYMOD_SHIFT;
      end if;
      if desktopCtrlDown then
         mods := mods or KEYMOD_CTRL;
      end if;
      if desktopAltDown then
         mods := mods or KEYMOD_ALT;
      end if;
      if desktopCapsLockOn then
         mods := mods or KEYMOD_CAPS;
      end if;

      for i in inputChannels'Range loop
         if inputChannels (i).target /= 0 then
            declare
               surfaceSlot : constant Integer :=
                 findSurface (inputChannels (i).target);
               localX : Natural := 0;
               localY : Natural := 0;
            begin
               if surfaceSlot >= 0 then
                  declare
                     c : constant Rect :=
                       clientRect (surfaces (SurfaceIndex (surfaceSlot)));
                  begin
                     if cursorX >= c.x then
                        localX := cursorX - c.x;
                     end if;
                     if cursorY >= c.y then
                        localY := cursorY - c.y;
                     end if;
                  end;
               end if;

               inputChannels (i).events := (others => (others => <>));
               inputChannels (i).snapshot.pointerPosition :=
                 packU32Pair (localX, localY);
               inputChannels (i).snapshot.buttons := lastButtons;
               inputChannels (i).snapshot.modifiers := mods;
               inputChannels (i).snapshot.generation :=
                 inputChannels (i).snapshot.generation + 1;
               inputChannels (i).events (InputQueueIndex'First) :=
                 (valid    => True,
                  serial   => inputChannels (i).nextSerial,
                  kind     => INPUT_RESYNC,
                  target   => inputChannels (i).target,
                  payload0 => inputChannels (i).snapshot.pointerPosition,
                  payload1 =>
                    (lastButtons and 16#FFFF_FFFF#) or
                    Shift_Left (mods and 16#FFFF_FFFF#, 32));
               inputChannels (i).nextSerial :=
                 inputChannels (i).nextSerial + 1;
               completeInputWaiter (inputChannels (i).target);
            end;
         end if;
      end loop;
   end forceInputResynchronization;

   procedure clearInputQueue is
   begin
      inputChannels := (others => (others => <>));
   end clearInputQueue;

   procedure clearInputForTarget (target : Unsigned_64) is
      channelSlot : constant Integer := findInputChannel (target);
      response : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if target = 0 or else channelSlot < 0 then
         return;
      end if;

      if inputChannels (SurfaceIndex (channelSlot)).waiter.active then
         response.tag :=
           (label => OP_INPUT_WAIT, length => 4, flags => 0, badge => 0);
         response.words (0) := INPUT_RESYNC;
         response.words (1) :=
           inputChannels (SurfaceIndex (channelSlot)).nextSerial;
         inputChannels (SurfaceIndex (channelSlot)).nextSerial :=
           inputChannels (SurfaceIndex (channelSlot)).nextSerial + 1;
         declare
            slot : constant CapabilitySlot :=
              inputChannels (SurfaceIndex (channelSlot)).waiter.replySlot;
         begin
            inputChannels (SurfaceIndex (channelSlot)).waiter :=
              (replySlot => slot, others => <>);
            ignore := replyCap (slot, response);
         end;
      end if;

      inputChannels (SurfaceIndex (channelSlot)) := (others => <>);
   end clearInputForTarget;

   function keyChar (code : Unsigned_8) return Character;

   function modifierState return Unsigned_64 is
      mods : Unsigned_64 := 0;
   begin
      if desktopShiftDown then
         mods := mods or KEYMOD_SHIFT;
      end if;
      if desktopCtrlDown then
         mods := mods or KEYMOD_CTRL;
      end if;
      if desktopAltDown then
         mods := mods or KEYMOD_ALT;
      end if;
      if desktopCapsLockOn then
         mods := mods or KEYMOD_CAPS;
      end if;
      return mods;
   end modifierState;

   procedure queueKey (raw : Unsigned_8) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_64 := Unsigned_64 (raw and 16#7F#);
      ch      : Character;
      mods    : constant Unsigned_64 := modifierState;
   begin
      if focusSurface = 0 then
         return;
      end if;

      enqueueInput ((if release then INPUT_KEY_UP else INPUT_KEY_DOWN),
                    focusSurface,
                    code,
                    mods);
      if not release and then
         (mods and (KEYMOD_CTRL or KEYMOD_ALT)) = 0
      then
         --  Key events preserve the physical key identity for shortcuts and
         --  games. Text input is a separate composed event so applications do
         --  not need to duplicate keyboard layout, Shift, and Caps Lock state.
         --  Ctrl/Alt combinations are shortcuts, not text-entry input.
         ch := keyChar (raw and 16#7F#);
         if ch >= ' ' and then ch < Character'Val (127) then
            enqueueInput (INPUT_TEXT,
                          focusSurface,
                          Unsigned_64 (Character'Pos (ch)),
                          0);
         end if;
      end if;
   end queueKey;

   procedure queuePointer
      (kind : Unsigned_64;
       target : Unsigned_64;
       screenX, screenY : Natural;
       buttons : Unsigned_64)
   is
      idx : constant Integer := findSurface (target);
      c   : Rect;
      localX : Natural := 0;
      localY : Natural := 0;
   begin
      if idx < 0 or else surfaces (SurfaceIndex (idx)).owner = NO_PROCESS then
         return;
      end if;

      c := clientRect (surfaces (SurfaceIndex (idx)));
      if screenX >= c.x then
         localX := screenX - c.x;
      end if;
      if screenY >= c.y then
         localY := screenY - c.y;
      end if;

      enqueueInput (kind,
                    target,
                    packU32Pair (localX, localY),
                    buttons);
      if kind = INPUT_POINTER_DOWN or else kind = INPUT_POINTER_UP then
         tracePointer
           ((if kind = INPUT_POINTER_DOWN then "queue-down" else "queue-up"),
            target,
            Unsigned_64 (localX),
            Unsigned_64 (localY));
      end if;
   end queuePointer;

   procedure queuePointerIfClient
      (kind : Unsigned_64;
       target : Unsigned_64;
       screenX, screenY : Natural;
       buttons : Unsigned_64)
   is
      idx : constant Integer := findSurface (target);
   begin
      if idx >= 0 and then
         surfaces (SurfaceIndex (idx)).owner /= NO_PROCESS and then
         pointInRect (screenX, screenY,
                      clientRect (surfaces (SurfaceIndex (idx))))
      then
         queuePointer (kind, target, screenX, screenY, buttons);
      end if;
   end queuePointerIfClient;

   procedure queuePointerWheel
      (target : Unsigned_64;
       screenX, screenY : Natural;
       buttons : Unsigned_64;
       dz : Integer)
   is
      idx : constant Integer := findSurface (target);
      c   : Rect;
      localX : Natural := 0;
      localY : Natural := 0;
   begin
      if idx < 0 or else surfaces (SurfaceIndex (idx)).owner = NO_PROCESS then
         return;
      end if;

      c := clientRect (surfaces (SurfaceIndex (idx)));
      if not pointInRect (screenX, screenY, c) then
         return;
      end if;
      if screenX >= c.x then
         localX := screenX - c.x;
      end if;
      if screenY >= c.y then
         localY := screenY - c.y;
      end if;

      enqueueInput
        (INPUT_POINTER_WHEEL,
         target,
         packU32Pair (localX, localY),
         packI32Buttons (dz, buttons));
   end queuePointerWheel;

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
      replyNow : Boolean := True;
   begin
      statsRequests := statsRequests + 1;
      case request.tag.label is
         when OP_SURFACE_PRESENT =>
            statsPresentReq := statsPresentReq + 1;
         when OP_INPUT_POLL | OP_INPUT_WAIT =>
            statsInputReq := statsInputReq + 1;
         when others =>
            statsOtherReq := statsOtherReq + 1;
      end case;

      if dragBaseReady and then
        request.tag.label /= OP_INPUT_POLL and then
        request.tag.label /= OP_INPUT_WAIT
      then
         --  The retained under-window layer is a snapshot. Any client or
         --  window-management mutation invalidates it; correctness falls
         --  back to normal scene composition for the rest of this drag.
         dragBaseReady := False;
      end if;

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
                     appKind =>
                       (if from = doomPid and then doomPid /= NO_PROCESS
                         and then processAlive (doomPid)
                        then APP_DOOM
                        else APP_CLIENT),
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
                     title => (others => ' '),
                     titleLen => 0,
                     bufferAttached => False,
                     bufferGrant => 0,
                     bufferAddr => System.Null_Address,
                     bufferW => 0,
                     bufferH => 0,
                     bufferPitch => 0,
                     bufferFormat => 0,
                     pointerCursor => POINTER_DEFAULT);
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

         when OP_SURFACE_SET_POINTER_CURSOR =>
            declare
               idx : constant Integer := findSurface (request.words (0));
               requested : constant Unsigned_64 := request.words (1);
               nextStyle : Pointer_Cursor_Style;
            begin
               replyMsg.tag :=
                 (label => OP_SURFACE_SET_POINTER_CURSOR,
                  length => 1, flags => 0, badge => 0);
               if idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif requested > Unsigned_64
                 (Pointer_Cursor_Style'Enum_Rep
                    (Pointer_Cursor_Style'Last))
               then
                  replyMsg.words (0) := UI_ERR_UNSUPPORTED;
               else
                  surfaces (SurfaceIndex (idx)).pointerCursor :=
                    Pointer_Cursor_Style'Enum_Val (Integer (requested));
                  nextStyle := cursorStyleAtPointer;
                  if nextStyle /= cursorStyle then
                     cursorStyle := nextStyle;
                     scheduleCursorPresent;
                  end if;
                  replyMsg.words (0) := UI_OK;
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

         when OP_WINDOW_SET_TITLE =>
            declare
               idx : constant Integer := findSurface (request.words (0));
               requestedLength : constant Natural := Natural
                 (Shift_Right (request.words (3), 56) and 16#FF#);
               sourceWord : Unsigned_64;
               byteIndex : Natural;
            begin
               replyMsg.tag :=
                 (label => OP_WINDOW_SET_TITLE,
                  length => 1, flags => 0, badge => 0);
               if idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif requestedLength > 23 then
                  replyMsg.words (0) := UI_ERR_UNSUPPORTED;
               else
                  surfaces (SurfaceIndex (idx)).title := (others => ' ');
                  surfaces (SurfaceIndex (idx)).titleLen := requestedLength;
                  if requestedLength > 0 then
                     for index in 0 .. requestedLength - 1 loop
                        case index / 8 is
                           when 0 => sourceWord := request.words (1);
                           when 1 => sourceWord := request.words (2);
                           when others => sourceWord := request.words (3);
                        end case;
                        byteIndex := index mod 8;
                        surfaces (SurfaceIndex (idx)).title (index + 1) :=
                          Character'Val
                            (Shift_Right (sourceWord, byteIndex * 8) and 16#FF#);
                     end loop;
                  end if;
                  surfaces (SurfaceIndex (idx)).dirty := True;
                  replyMsg.words (0) := UI_OK;
                  scheduleRedrawRect
                    (inflateRect
                       (surfaceRect (surfaces (SurfaceIndex (idx))), 2));
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
                  --  does the minimum useful work. Newer clients may also
                  --  pass a dirty rectangle in client-local coordinates:
                  --  word1 = x/y, word2 = w/h. Legacy zero/zero presents
                  --  still mean "the whole client area changed."
                  declare
                     client : constant Rect :=
                        clientRect (surfaces (SurfaceIndex (idx)));
                     localX : constant Natural := unpackLo32 (request.words (1));
                     localY : constant Natural := unpackHi32 (request.words (1));
                     localW : constant Natural := unpackLo32 (request.words (2));
                     localH : constant Natural := unpackHi32 (request.words (2));
                  begin
                     if localW = 0 or else localH = 0 then
                        scheduleRedrawRect (client);
                     else
                        scheduleRedrawRect
                          ((x => client.x + localX,
                            y => client.y + localY,
                            w => Natural'Min (localW,
                                  client.w - Natural'Min (localX, client.w)),
                            h => Natural'Min (localH,
                                  client.h - Natural'Min (localY, client.h))));
                     end if;
                  end;
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
                  if pointerSurfaceId = request.words (0) then
                     pointerSurfaceId := 0;
                  end if;
                  if dragSurfaceId = request.words (0) then
                     dragSurfaceId := 0;
                     dragMode := DRAG_NONE;
                     dragPreviewValid := False;
                     dragPresentedValid := False;
                  end if;
                  clearInputForTarget (request.words (0));
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

         when OP_INPUT_POLL | OP_INPUT_WAIT =>
            replyMsg.tag := (label  => request.tag.label,
                             length => 4,
                             flags  => 0,
                             badge  => 0);
            declare
               found : Boolean;
               event : PendingInput;
               idx   : constant Integer := findSurface (request.words (0));
               channelSlot : Integer := findInputChannel (request.words (0));
            begin
               --  A surface identifier names an object; it does not confer
               --  authority.  Only the creating process may consume the
               --  input stream routed to that surface.
               if idx >= 0 and then
                 surfaces (SurfaceIndex (idx)).owner = from
               then
                  dequeueInput (request.words (0),
                                request.words (1),
                                found,
                                event);
               else
                  found := False;
               end if;
               if found then
                  if hasInputAfter (request.words (0), event.serial) then
                     replyMsg.tag.flags := INPUT_REPLY_MORE_PENDING;
                  end if;
                  replyMsg.words (0) := event.kind;
                  replyMsg.words (1) := event.serial;
                  replyMsg.words (2) := event.payload0;
                  replyMsg.words (3) := event.payload1;
               elsif request.tag.label = OP_INPUT_WAIT and then
                 idx >= 0 and then
                 surfaces (SurfaceIndex (idx)).owner = from and then
                 channelSlot >= 0 and then
                 not inputChannels
                   (SurfaceIndex (channelSlot)).waiter.active
               then
                  declare
                     slot : constant CapabilitySlot :=
                       INPUT_REPLY_SLOT_FIRST + CapabilitySlot (channelSlot);
                  begin
                     if saveReplyCap (Unsigned_64 (slot)) = 1 then
                        inputChannels (SurfaceIndex (channelSlot)).waiter :=
                          (active      => True,
                           owner       => from,
                           target      => request.words (0),
                           afterSerial => request.words (1),
                           replySlot   => slot);
                        replyNow := False;
                     else
                        replyMsg.words (0) := INPUT_RESYNC;
                        replyMsg.words (1) := request.words (1);
                     end if;
                  end;
               else
                  replyMsg.words (0) := INPUT_NONE;
                  replyMsg.words (1) := request.words (1);
               end if;
            end;

         when OP_DESKTOP_BYE =>
            for i in surfaces'Range loop
               if surfaces (i).used and then surfaces (i).owner = from then
                  if pointerSurfaceId = surfaces (i).id then
                     pointerSurfaceId := 0;
                  end if;
                  if dragSurfaceId = surfaces (i).id then
                     dragSurfaceId := 0;
                     dragMode := DRAG_NONE;
                     dragPreviewValid := False;
                     dragPresentedValid := False;
                  end if;
                  clearInputForTarget (surfaces (i).id);
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

      if replyNow then
         ignore := reply (from, replyMsg);
      end if;
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
      elsif code = 16#1D# then
         desktopCtrlDown := not release;
         return True;
      elsif code = 16#38# then
         desktopAltDown := not release;
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
         lastSpawnedPid := ProcessID (msg.words (0) and 16#FFFF#);
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
         when others =>
            return False;
      end case;
   end handleInternalKey;

   procedure performLaunchAction
      (action : Launch_Action;
       damage : in out Rect)
   is
      ok : Boolean;
   begin
      case action is
         when LAUNCH_CONSOLE =>
            openInternalApp (APP_CONSOLE, damage);
         when LAUNCH_WORKBENCH =>
            trySpawnFromConsole ("ccl-workbench.app", ok);
         when LAUNCH_DOOM =>
            if doomPid /= NO_PROCESS and then processAlive (doomPid) then
               setConsoleResult ("DOOM IS ALREADY RUNNING");
            else
               trySpawnFromConsole ("doom.elf", ok);
               if ok then
                  doomPid := lastSpawnedPid;
               end if;
            end if;
         when LAUNCH_DEVICES =>
            trySpawnFromConsole ("devices.app", ok);
         when LAUNCH_BROWSER =>
            trySpawnFromConsole ("netsurf.app", ok);
         when LAUNCH_FILES =>
            trySpawnFromConsole ("files.app", ok);
         when others =>
            null;
      end case;
   end performLaunchAction;

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
       dy      : Integer;
       dz      : Integer)
   is
      oldCursor : constant Rect := cursorRect;
      oldBounds : Rect := (others => 0);
      newBounds : Rect := (others => 0);
      damage    : Rect := oldCursor;
      idx       : Integer;
      leftDown  : constant Boolean := (buttons and 1) /= 0;
      leftWasDown : constant Boolean := (lastButtons and 1) /= 0;
      leftTransition : constant Boolean := leftDown /= leftWasDown;
      pointerMoved : constant Boolean := dx /= 0 or else dy /= 0;
      deliverMove : constant Boolean :=
        not leftTransition and then
        (pointerMoved or else buttons /= lastButtons);
      --  A held client button does not inherently alter the compositor scene.
      --  Treating every held-motion packet as scene damage throttled the
      --  software cursor behind the 60 Hz frame scheduler.
      sceneDamage : Boolean := leftTransition;
      handledChromeClick : Boolean := False;
      taskIdx   : Integer;
      launchAction : Launch_Action;
      clickedId : Unsigned_64;
      maxX      : Integer := 0;
      maxY      : Integer := 0;
      wheelIdx  : Integer;
   begin
      if leftTransition then
         statsButtonTransitions := statsButtonTransitions + 1;
      end if;

      if fbWidth > 0 then
         maxX := Integer (fbWidth - 1);
      end if;
      if fbHeight > 0 then
         maxY := Integer (fbHeight - 1);
      end if;

      --  PS/2 reports positive Y as upward motion; screen coordinates grow
      --  downward.
      cursorX := clampPointerCoord (Integer (cursorX) + dx, maxX);
      cursorY := clampPointerCoord (Integer (cursorY) - dy, maxY);
      damage := unionRect (damage, cursorRect);

      if pointerSurfaceId /= 0 then
         if deliverMove then
            queuePointer (INPUT_POINTER_MOVE,
                          pointerSurfaceId,
                          cursorX,
                          cursorY,
                          buttons);
         end if;
         if dz /= 0 then
            queuePointerWheel
              (pointerSurfaceId, cursorX, cursorY, buttons, dz);
         end if;
      elsif focusSurface /= 0 then
         if deliverMove then
            queuePointerIfClient (INPUT_POINTER_MOVE,
                                  focusSurface,
                                  cursorX,
                                  cursorY,
                                  buttons);
         end if;
         if dz /= 0 then
            wheelIdx := hitSurface (cursorX, cursorY);
            if wheelIdx >= 0 then
               queuePointerWheel
                 (surfaces (SurfaceIndex (wheelIdx)).id,
                  cursorX,
                  cursorY,
                  buttons,
                  dz);
            else
               queuePointerWheel (focusSurface, cursorX, cursorY, buttons, dz);
            end if;
         end if;
      end if;

      if leftDown and then not leftWasDown then
         if shellSurfaceVisible and then
            pointInRect (cursorX, cursorY, launchButtonRect)
         then
            launchMenuOpen := not launchMenuOpen;
            if launchMenuOpen then
               launchMenuSelection := LAUNCH_CONSOLE;
            end if;
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
            performLaunchAction (launchAction, damage);
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
            tracePointer
              ("hit-down",
               clickedId,
               Unsigned_64 (idx),
               Unsigned_64 (Pointer_Action'Enum_Rep (dragMode)));

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
                  elsif dragMode = DRAG_NONE then
                     pointerSurfaceId := clickedId;
                     queuePointerIfClient (INPUT_POINTER_DOWN,
                                           clickedId,
                                           cursorX,
                                           cursorY,
                                           buttons);
                  else
                     dragSurfaceId := clickedId;
                     dragOffsetX := cursorX - surfaces (SurfaceIndex (idx)).x;
                     dragOffsetY := cursorY - surfaces (SurfaceIndex (idx)).y;
                     dragPreviewRect := surfaceRect (surfaces (SurfaceIndex (idx)));
                     if dragMode = DRAG_MOVE then
                        --  The existing surface is already the first
                        --  presented position. Moves can reuse its attached
                        --  buffer directly; resizes retain the outline preview
                        --  until a new client buffer is configured.
                        dragPreviewValid := False;
                        dragPresentedRect := dragPreviewRect;
                        dragPresentedValid := True;
                        prepareMoveBase (clickedId);
                     else
                        dragPreviewValid := dragMode /= DRAG_NONE;
                        dragPresentedValid := False;
                        dragBaseReady := False;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      elsif not leftDown and then leftWasDown
      then
         if pointerSurfaceId /= 0 then
            queuePointer (INPUT_POINTER_UP,
                          pointerSurfaceId,
                          cursorX,
                          cursorY,
                          buttons);
            pointerSurfaceId := 0;
         elsif dragMode /= DRAG_NONE and then dragSurfaceId /= 0 then
            idx := findSurface (dragSurfaceId);
            if idx >= 0 and then
              (dragPreviewValid or else dragMode = DRAG_MOVE)
            then
               oldBounds := surfaceRect (surfaces (SurfaceIndex (idx)));
               newBounds := clampWindowRect
                 (surfaces (SurfaceIndex (idx)), dragPreviewRect);

               surfaces (SurfaceIndex (idx)).x := newBounds.x;
               surfaces (SurfaceIndex (idx)).y := newBounds.y;
               surfaces (SurfaceIndex (idx)).w := newBounds.w;
               surfaces (SurfaceIndex (idx)).h := newBounds.h;
               surfaces (SurfaceIndex (idx)).serial :=
                  surfaces (SurfaceIndex (idx)).serial + 1;
               if dragMode /= DRAG_MOVE and then
                 surfaces (SurfaceIndex (idx)).owner /= NO_PROCESS
               then
                  queueConfigure (surfaces (SurfaceIndex (idx)).id,
                                  Unsigned_64 (newBounds.w),
                                  Unsigned_64 (newBounds.h));
               end if;

               --  Ensure a final position that arrived just before release is
               --  presented even if the deferred drag frame had not fired.
               damage := unionRect
                 (damage,
                  inflateRect
                    (unionRect
                       ((if dragPresentedValid
                         then dragPresentedRect else oldBounds),
                        newBounds), 4));
               tracePointer
                 ("drag-up", dragSurfaceId,
                  Unsigned_64 (newBounds.x), Unsigned_64 (newBounds.y));
            end if;
         end if;

         dragPreviewValid := False;
         dragPresentedValid := False;
         dragBaseReady := False;
         dragMode := DRAG_NONE;
         dragSurfaceId := 0;
      elsif not leftDown then
         dragPreviewValid := False;
         dragPresentedValid := False;
         dragBaseReady := False;
         dragMode := DRAG_NONE;
         dragSurfaceId := 0;
      end if;

      if leftDown and then dragMode /= DRAG_NONE and then dragSurfaceId /= 0 then
         idx := findSurface (dragSurfaceId);
         if idx >= 0 then
            dragPreviewRect :=
               previewRectFromPointer (surfaces (SurfaceIndex (idx)));
            if dragMode = DRAG_MOVE then
               surfaces (SurfaceIndex (idx)).x := dragPreviewRect.x;
               surfaces (SurfaceIndex (idx)).y := dragPreviewRect.y;
            end if;
            --  The compositor frame compares the last drag geometry actually
            --  presented with this latest position. Intermediate mouse
            --  positions drained in the same pass require no repaint at all.
            sceneDamage := True;
         end if;
      end if;

      cursorStyle := cursorStyleAtPointer;
      damage := unionRect (damage, cursorRect);
      lastButtons := buttons;
      if sceneDamage or else
        (dragMode /= DRAG_NONE and then dragSurfaceId /= 0)
      then
         scheduleRedrawRect
           (inflateRect (damage, 2),
            defer => False);
      else
         scheduleCursorPresent;
      end if;
   end handleMouseMotion;

   procedure acceptSourceReport
     (report        : CuBit.Input.Source_Report;
      accepted      : out Boolean;
      discontinuity : out Boolean;
      seatButtons   : out Unsigned_64)
   is
      slot : Integer := -1;
   begin
      accepted := False;
      discontinuity := False;
      seatButtons := lastButtons;

      for i in inputSources'Range loop
         if inputSources (i).used and then
            inputSources (i).badge = report.sourceBadge and then
            inputSources (i).device = report.device
         then
            slot := Integer (i);
            exit;
         end if;
      end loop;

      if slot < 0 then
         for i in inputSources'Range loop
            if not inputSources (i).used then
               slot := Integer (i);
               exit;
            end if;
         end loop;
      end if;

      if slot < 0 or else
         (report.device = CuBit.Input.KEYBOARD and then
          report.delivery /= CuBit.Input.ORDERED_TRANSITION) or else
         (report.device = CuBit.Input.RELATIVE_POINTER and then
          report.delivery /= CuBit.Input.ACCUMULABLE_DISPLACEMENT)
      then
         statsSourceRejects := statsSourceRejects + 1;
         return;
      end if;

      declare
         source : InputSourceState renames
           inputSources (InputSourceIndex (slot));
      begin
         --  Replaying a duplicate can synthesize a second key or button
         --  transition, so duplicates are rejected rather than resynchronized.
         if source.used and then
            source.generation = report.generation and then
            source.sequence = report.sequence
         then
            statsSourceRejects := statsSourceRejects + 1;
            return;
         end if;

         discontinuity := report.flags (CuBit.Input.RESYNCHRONIZE) or else
           (source.used and then
             (source.generation /= report.generation or else
              not CuBit.Input.Is_Immediate_Successor
                (source.sequence, report.sequence)));

         source.used := True;
         source.badge := report.sourceBadge;
         source.device := report.device;
         source.generation := report.generation;
         source.sequence := report.sequence;
         if report.device = CuBit.Input.RELATIVE_POINTER then
            source.buttons := report.snapshot and 16#FF#;
         end if;
      end;

      seatButtons := 0;
      for i in inputSources'Range loop
         if inputSources (i).used and then
            inputSources (i).device = CuBit.Input.RELATIVE_POINTER
         then
            seatButtons := seatButtons or inputSources (i).buttons;
         end if;
      end loop;

      if discontinuity then
         statsSourceGaps := statsSourceGaps + 1;
      end if;
      accepted := True;
   end acceptSourceReport;

   procedure handleEvent (eventMsg : Message; running : in out Boolean) is
      raw : Unsigned_8;
      packed : Unsigned_64;
      sourceReport : CuBit.Input.Source_Report :=
        CuBit.Input.NULL_SOURCE_REPORT;
      sourceValid : Boolean := False;
      sourceAccepted : Boolean := False;
      sourceDiscontinuity : Boolean := False;
      seatButtons : Unsigned_64 := lastButtons;
   begin
      statsEvents := statsEvents + 1;

      if eventMsg.tag.label = CuBit.Input.OP_SOURCE_REPORT then
         CuBit.Input.Decode (eventMsg, sourceReport, sourceValid);
         if sourceValid then
            acceptSourceReport
              (sourceReport, sourceAccepted, sourceDiscontinuity,
               seatButtons);
         else
            statsSourceRejects := statsSourceRejects + 1;
         end if;

         if not sourceAccepted then
            return;
         end if;
      end if;

      if eventMsg.tag.label = EVENT_KEYBOARD or else
         (sourceAccepted and then
          sourceReport.device = CuBit.Input.KEYBOARD)
      then
         statsKeyboardEvents := statsKeyboardEvents + 1;
         raw := Unsigned_8
           ((if sourceAccepted then sourceReport.payload
             else eventMsg.words (0)) and 16#FF#);

         if sourceAccepted and then sourceDiscontinuity then
            --  Held modifiers are transient state. Never carry them across a
            --  report gap; the future input service will install a complete
            --  keyboard snapshot here instead.
            desktopShiftDown := False;
            desktopCtrlDown := False;
            desktopAltDown := False;
            desktopExtendedPrefix := False;
         end if;

         --  Set-1 extended keys arrive as E0 followed by a normal press or
         --  release byte. Preserve that context at the desktop boundary so
         --  Super can remain a compositor shortcut even while an app owns
         --  keyboard focus.
         if raw = KEY_EXTENDED_PREFIX then
            desktopExtendedPrefix := True;
         else
            declare
               extended : constant Boolean := desktopExtendedPrefix;
               release  : constant Boolean := (raw and 16#80#) /= 0;
               code     : constant Unsigned_8 := raw and 16#7F#;
               damage   : Rect := cursorRect;
            begin
               desktopExtendedPrefix := False;
               if updateDesktopModifierKey (raw) then
                  null;
               end if;

               if extended and then
                  (code = KEY_LEFT_SUPER or else code = KEY_RIGHT_SUPER)
               then
                  if not release and then shellSurfaceVisible then
                     launchMenuOpen := not launchMenuOpen;
                     launchMenuSelection := LAUNCH_CONSOLE;
                     damage := unionRect
                       (damage,
                        inflateRect
                          (unionRect (launchButtonRect, launchMenuRect), 4));
                     scheduleRedrawRect (inflateRect (damage, 2));
                  end if;
               elsif launchMenuOpen then
                  --  The compositor owns keyboard input while its Apps menu
                  --  is open. Swallow both presses and releases so the focused
                  --  client cannot observe half of a key transition.
                  if not release then
                     if code = KEY_UP then
                        launchMenuSelection :=
                          nextLaunchSelection
                            (launchMenuSelection, upward => True);
                     elsif code = KEY_DOWN then
                        launchMenuSelection :=
                          nextLaunchSelection
                            (launchMenuSelection, upward => False);
                     elsif code = KEY_ENTER then
                        launchMenuOpen := False;
                        performLaunchAction (launchMenuSelection, damage);
                     elsif code = KEY_ESCAPE then
                        launchMenuOpen := False;
                     end if;

                     damage := unionRect
                       (damage,
                        inflateRect
                          (unionRect (launchButtonRect, launchMenuRect), 4));
                     scheduleRedrawRect (inflateRect (damage, 2));
                  end if;
               --  Once a shell/client surface has focus, keyboard events
               --  belong to that surface. The service-level Q/Esc escape
               --  remains available only before a client has connected.
               elsif shouldCycleFocusKey (raw) then
                  cycleFocus (damage);
                  scheduleRedrawRect (inflateRect (damage, 2));
               elsif focusSurface /= 0 then
                  if handleInternalKey (raw, damage) then
                     scheduleRedrawRect (inflateRect (damage, 2));
                  else
                     queueKey (raw);
                  end if;
               elsif shouldExitKey (raw) then
                  debugPrint ("desktop: exit key" & LF);
                  running := False;
               end if;
            end;
         end if;

         if sourceAccepted and then sourceDiscontinuity then
            forceInputResynchronization;
         end if;
      elsif eventMsg.tag.label = EVENT_MOUSE or else
         (sourceAccepted and then
          sourceReport.device = CuBit.Input.RELATIVE_POINTER)
      then
         statsMouseEvents := statsMouseEvents + 1;
         packed :=
           (if sourceAccepted then sourceReport.payload
            else eventMsg.words (0));
         if signed8 (Shift_Right (packed, 32)) /= 0 then
            statsWheelEvents := statsWheelEvents + 1;
         end if;
         handleMouseMotion
           (buttons =>
              (if sourceAccepted then seatButtons
               else packed and 16#FF#),
            dx      => signed12 (Shift_Right (packed, 8)),
            dy      => signed12 (Shift_Right (packed, 20)),
            dz      => signed8 (Shift_Right (packed, 32)));

         if sourceAccepted and then sourceDiscontinuity then
            forceInputResynchronization;
         end if;
      elsif eventMsg.tag.label = OP_STREAM_AVAILABLE then
         rememberStreams
           (ProcessID (eventMsg.words (0) and 16#FFFF#),
            eventMsg.words (1));
         scheduleRedrawRect ((x => 0, y => 0, w => fbWidth, h => fbHeight));
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
      dragRaw : Unsigned_64;
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

      --  A retained, compositor-private scene without the actively dragged
      --  window turns movement into bounded rectangle copies plus one window
      --  blit. Failure is non-fatal: the compositor retains its complete
      --  redraw fallback on memory-constrained systems.
      dragRaw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
      if dragRaw /= Unsigned_64'Last then
         dragBaseBufferAddr := To_Address
           (Integer_Address (alignUpPage (dragRaw)));
      else
         debugPrint ("desktop: retained drag layer unavailable" & LF);
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

   ret := setLatencyContract
      (LATENCY_INTERACTIVE,
       16_667,  --  Target one compositor frame per 60 Hz display period.
       4_000);  --  Budget hint for input dispatch and compositor drawing.
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: latency contract rejected" & LF);
   end if;

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
         requestsThisPass : Natural := 0;
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
            requestsThisPass := requestsThisPass + 1;
            exit when not running;
            --  Do not let a steady stream of synchronous input polls hold a
            --  completed client frame in the compositor. A small request
            --  budget keeps IPC responsive while preserving frame latency and
            --  prevents idle pollers from starving new window handshakes.
            exit when requestsThisPass >=
              (if framePending then REQUEST_BUDGET_FRAME
               else REQUEST_BUDGET_IDLE);
         end loop;

         flushFrame;
         flushCursorPresent;
         maybePrintStats;

         if not eventFound and then not found then
            if not framePending and then not cursorPresentPending then
               --  Idle input and service dispatch must be event-driven. The
               --  mixed receive primitive blocks on the unified mailbox and
               --  is woken directly by either an unsolicited device event or
               --  a client request; polling with a fixed sleep added up to
               --  two milliseconds before any useful work even began.
               receive (from, msg);
               if from = NO_PROCESS then
                  handleEvent (msg, running);
               else
                  handleRequest (from, msg);
               end if;
            else
               declare
                  now : constant Unsigned_64 := nowMs;
                  nextDueMs : Unsigned_64 := 0;
                  mayWait : Boolean := now /= Unsigned_64'Last;
                  received : Boolean;
               begin
                  if framePending and then now /= Unsigned_64'Last and then
                     frameDueMs /= 0 and then now < frameDueMs
                  then
                     nextDueMs := frameDueMs;
                  elsif framePending then
                     mayWait := False;
                  end if;

                  if cursorPresentPending and then now /= Unsigned_64'Last
                    and then cursorPresentDueMs /= 0
                    and then now < cursorPresentDueMs
                    and then
                      (nextDueMs = 0 or else cursorPresentDueMs < nextDueMs)
                  then
                     nextDueMs := cursorPresentDueMs;
                  elsif cursorPresentPending and then
                    (now = Unsigned_64'Last or else
                     cursorPresentDueMs = 0 or else
                     now >= cursorPresentDueMs)
                  then
                     mayWait := False;
                  end if;

                  if mayWait and then nextDueMs /= 0 then
                     --  A single kernel wait races IPC publication against
                     --  the absolute frame deadline atomically. Input wakes
                     --  this process immediately; there is no millisecond
                     --  sleep slice in the dispatch path.
                     receiveUntil (nextDueMs, from, msg, received);
                     if received then
                        if from = NO_PROCESS then
                           handleEvent (msg, running);
                        else
                           handleRequest (from, msg);
                        end if;
                     end if;
                  end if;
               end;
            end if;
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
