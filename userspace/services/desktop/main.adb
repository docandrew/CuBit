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
with CuBit.Display_Protocol;
with CuBit.Display_Layouts;
with CuBit.Desktop_Messages;
with CuBit.Input;
with CuBit.Audio_Control;
with CuBit.Clocks;
with CuBit.Click_Sequences;
with CuBit.Theme;
with Desktop_Cursors;
with Desktop_Icons;
with CuBit.Fonts;
with Desktop_Window_Icons;
with Desktop_Wallpaper;
with Desktop_Settings;
with CuBit.Appearance;
with CuBit.Config;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with CuBit.UI.Theme_Data;
with CuBit.Desktop_Protocol;
with CuBit.Memory_Grants;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with Presentation_Test_Policy;

procedure main is
   package DSP renames CuBit.Display_Protocol;
   package DL renames CuBit.Display_Layouts;
   package DG renames DL.G;
   use type DSP.Output_Number, DL.Admission_Status;
   use type DG.Pixel_Edge;
   package MG renames CuBit.Memory_Grants;
   package GM renames CuBit.Graphics_Metrics;
   stagingCopies : GM.Counter;
   stagingReporter : CuBit.Graphics_Metrics_IO.Reporter;
   use ASCII;
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Status_Code;
   use type DP.Damage_Mode;
   use type CuBit.Input.Device_Class;
   use type CuBit.Input.Delivery_Class;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;
   SYSINFO_EVENT_DROPS_SELF : constant Unsigned_64 := 1401;

   EVENT_KEYBOARD : constant Unsigned_32 := 1;
   EVENT_MOUSE    : constant Unsigned_32 := 2;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Hello);
   OP_DESKTOP_BYE      : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Goodbye);
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Get_Information);
   OP_SPAWN            : constant Unsigned_32 := 16#0100#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Create_Surface);
   OP_SURFACE_DESTROY  : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Destroy_Surface);
   OP_SURFACE_PRESENT  : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Present_Surface);
   OP_SURFACE_RESIZE   : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Resize_Surface);
   OP_SURFACE_ATTACH_BUFFER : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Attach_Buffer);
   OP_SURFACE_SET_POINTER_CURSOR : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Set_Pointer_Cursor);
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Set_Window_Limits);
   OP_WINDOW_SET_TITLE  : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Set_Window_Title);
   OP_STREAM_AVAILABLE  : constant Unsigned_32 := 16#0706#;
   OP_INPUT_POLL       : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Poll_Input);
   OP_INPUT_WAIT       : constant Unsigned_32 := DP.Operation'Enum_Rep (DP.Wait_Input);
   INPUT_REPLY_MORE_PENDING : constant Unsigned_8 := 1;

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Get_Information);
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Attach_Buffer);
   OP_DISPLAY_CLEAR         : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Clear);
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Get_Status);
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Acquire_Display);
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Release_Display);

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
   EVENT_BUDGET : constant Positive := 64;

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

   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 :=
     DP.Pixel_Format'Enum_Rep (DP.BGRA_8888);
   PS_BUF_SIZE : constant Unsigned_64 := 8192;
   PS_ENTRY_SIZE : constant Storage_Offset := 32;

   -- Once the session is active these describe the private logical scene,
   -- not any one scanout. Output-local pitches and storage live below.
   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   backBufferAddr : System.Address := System.Null_Address;
   type Transfer_Phase is (Available, In_Flight, Quarantined);
   frameSequence : Unsigned_64 := 0;
   asyncAnnounced, releaseAnnounced : Boolean := False;
   inputWhileHeldFrame : Unsigned_64 := 0;
   retiredThrough : Unsigned_64 := 0;
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
   --  Initial native arrangement: adjacent, equal-size, unit-scale outputs.
   --  Scene storage is private. Each output owns an immutable transfer buffer
   --  while its non-reused completion token is outstanding.
   --  The shared geometry/layout packages admit richer layouts, but this copy
   --  renderer must not silently claim support for rotation or resampling.
   subtype Output_Index is DSP.Output_Number range 0 .. 1;
   type Output_Presentation is record
      Enabled, Leased, Granted : Boolean := False;
      Geometry : DG.Output := (Width => 1, Height => 1, others => <>);
      Pitch : Natural := 0;
      Buffer : System.Address := System.Null_Address;
      Grant : MG.Grant_Reference;
      Session, Token, Started : Unsigned_64 := 0;
      Phase : Transfer_Phase := Available;
      Damage : Rect;
   end record;
   presentations : array (Output_Index) of Output_Presentation;
   primaryOutput : Output_Index := 0;
   --  Desktop preference, not a property of the display/GPU service. Config
   --  publication and named-monitor matching will replace this initial value.
   preferredPrimary : constant DL.Named_Display_ID := 1;

   function primaryBounds return Rect is
     (if presentations (primaryOutput).Enabled then
        (Natural (presentations (primaryOutput).Geometry.X), 0,
         Natural (presentations (primaryOutput).Geometry.Width),
         Natural (presentations (primaryOutput).Geometry.Height))
      else (0, 0, fbWidth, fbHeight));

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

   type App_Kind is (APP_CLIENT, APP_DOOM, APP_SETTINGS);
   for App_Kind use
     (APP_CLIENT => 0, APP_DOOM => 1, APP_SETTINGS => 2);
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
      title : DP.Inline_Title;
      bufferAttached : Boolean := False;
      bufferGrant    : MG.Grant_Reference;
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
      deadline    : Unsigned_64 := 0;
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
      authorityTag      : Unsigned_64 := 0;
      device     : CuBit.Input.Device_Class := CuBit.Input.KEYBOARD;
      generation : CuBit.Input.Source_Generation := 0;
      sequence   : CuBit.Input.Source_Sequence := 0;
      buttons    : Unsigned_64 := 0;
   end record;
   type InputSourceTable is array (InputSourceIndex) of InputSourceState;
   inputSources : InputSourceTable := (others => (others => <>));

   pointerSurfaceId : Unsigned_64 := 0;
   launchMenuOpen : Boolean := False;
   masterAudio : CuBit.Audio_Control.State;
   audioPopupOpen : Boolean := False;
   audioPointerCapture : Boolean := False;
   audioSliderDragging : Boolean := False;
   clockText : String (1 .. 5) := "--:--";
   statusDueMs : Unsigned_64 := 0;
   desktopExtendedPrefix : Boolean := False;
   desktopShiftDown : Boolean := False;
   desktopCtrlDown  : Boolean := False;
   desktopAltDown   : Boolean := False;
   desktopCapsLockOn : Boolean := False;

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
      16#1A# => Character'Pos ('['),
      16#1B# => Character'Pos (']'),
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
      16#27# => Character'Pos (';'),
      16#28# => Character'Pos ('''),
      16#29# => Character'Pos ('`'),
      16#2B# => Character'Pos ('\'),
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
      16#1A# => Character'Pos ('{'),
      16#1B# => Character'Pos ('}'),
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
      16#27# => Character'Pos (':'),
      16#28# => Character'Pos ('"'),
      16#29# => Character'Pos ('~'),
      16#2B# => Character'Pos ('|'),
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
     (LAUNCH_NONE, LAUNCH_WORKBENCH, LAUNCH_DOOM,
      LAUNCH_DEVICES, LAUNCH_BROWSER, LAUNCH_FILES, LAUNCH_SAMEBOY, LAUNCH_SETTINGS, LAUNCH_POWER);
   for Launch_Action use
     (LAUNCH_NONE => 0, LAUNCH_WORKBENCH => 1, LAUNCH_DOOM => 2,
      LAUNCH_DEVICES => 3, LAUNCH_BROWSER => 4, LAUNCH_FILES => 5,
      LAUNCH_SAMEBOY => 6, LAUNCH_SETTINGS => 7, LAUNCH_POWER => 8);
   for Launch_Action'Size use 8;

   launchMenuSelection : Launch_Action := LAUNCH_WORKBENCH;

   function nextLaunchSelection
      (current : Launch_Action;
       upward  : Boolean) return Launch_Action
   is
   begin
      if upward then
         case current is
            when LAUNCH_WORKBENCH => return LAUNCH_SETTINGS;
            when LAUNCH_DOOM      => return LAUNCH_WORKBENCH;
            when LAUNCH_DEVICES   => return LAUNCH_DOOM;
            when LAUNCH_BROWSER   => return LAUNCH_DEVICES;
            when LAUNCH_FILES     => return LAUNCH_BROWSER;
            when LAUNCH_SAMEBOY   => return LAUNCH_FILES;
            when LAUNCH_SETTINGS  => return LAUNCH_SAMEBOY;
            when others           => return LAUNCH_WORKBENCH;
         end case;
      else
         case current is
            when LAUNCH_WORKBENCH => return LAUNCH_DOOM;
            when LAUNCH_DOOM      => return LAUNCH_DEVICES;
            when LAUNCH_DEVICES   => return LAUNCH_BROWSER;
            when LAUNCH_BROWSER   => return LAUNCH_FILES;
            when LAUNCH_FILES     => return LAUNCH_SAMEBOY;
            when LAUNCH_SAMEBOY   => return LAUNCH_SETTINGS;
            when LAUNCH_SETTINGS  => return LAUNCH_WORKBENCH;
            when others           => return LAUNCH_WORKBENCH;
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
   titleClicks : CuBit.Click_Sequences.State;
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
   MENU_H       : constant Natural := 320;
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

   appearance : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
   settingsView : Desktop_Settings.State;
   themeRevision : Unsigned_64 := 1;

   procedure Load_Theme is
      Value : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      Loaded : CuBit.UI.Theme_CCL.Result;
      use type CuBit.Config.ConfigStatus;
      use type CuBit.Appearance.Color_Scheme;
   begin
      for Scheme in CuBit.Appearance.Color_Scheme loop
      declare
         Fallback : constant CuBit.UI.Theme := CuBit.UI.Default_Palette (Scheme);
      begin
      CuBit.UI.Install_Palette (Scheme, Fallback);
      CuBit.Config.get
        ((if Scheme = CuBit.Appearance.Alloy_Light then
           "desktop.appearance.theme.light" else "desktop.appearance.theme.dark"),
         Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. CuBit.UI.Theme_CCL.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            CuBit.UI.Theme_CCL.Load (Source, Fallback, Loaded);
         end;
         if Loaded.Success then
            CuBit.UI.Install_Palette (Scheme, Loaded.Value);
            debugPrint ("desktop: CCL theme loaded" & LF);
         else
            debugPrint ("desktop: invalid CCL theme; using built-in Alloy" & LF);
         end if;
      elsif Status /= CuBit.Config.NotFound then
         debugPrint ("desktop: theme unavailable or oversized; using built-in Alloy" & LF);
      end if;
      end;
      end loop;
      CuBit.UI.Set_Theme (CuBit.UI.Palette (appearance.Scheme));
   end Load_Theme;

   procedure Read_Appearance is
      Value : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.get (CuBit.Appearance.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Length = 3 and then Value /= System.Null_Address then
         declare
            Text : String (1 .. 3) with Import, Address => Value;
         begin
            if CuBit.Appearance.Valid (Text) then appearance := CuBit.Appearance.Decode (Text); end if;
         end;
      end if;
      Load_Theme;
   end Read_Appearance;
   function C_BG return Unsigned_32 is (CuBit.UI.Current_Theme.desktop);
   function C_PANEL return Unsigned_32 is (CuBit.UI.Current_Theme.panel);
   function C_TEXT return Unsigned_32 is (CuBit.UI.Current_Theme.text);
   function C_MUTED return Unsigned_32 is (CuBit.UI.Current_Theme.muted);
   function C_ACCENT return Unsigned_32 is (CuBit.UI.Current_Theme.selection);
   C_WHITE  : constant Unsigned_32 := CuBit.Theme.White;
   C_BLACK  : constant Unsigned_32 := CuBit.Theme.Black;
   function C_DESK return Unsigned_32 is (CuBit.UI.Current_Theme.desktop);
   function C_BAR return Unsigned_32 is (CuBit.UI.Current_Theme.panel);
   function C_BLUE return Unsigned_32 is (CuBit.UI.Current_Theme.selection);
   function C_WIN return Unsigned_32 is (CuBit.UI.Current_Theme.face);
   function C_EDGE return Unsigned_32 is (CuBit.UI.Current_Theme.edge);
   function C_SHADOW return Unsigned_32 is (CuBit.UI.Current_Theme.shadow);

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
   statsCompletionMs : Unsigned_64 := 0;
   statsDamagePixels : Unsigned_64 := 0;
   statsSourceGaps   : Unsigned_64 := 0;
   statsSourceRejects : Unsigned_64 := 0;
   lastEventDrops    : Unsigned_64 := 0;
   lastInputQueueOverflows : Unsigned_64 := 0;
   inputTraceBudget  : Natural := 64;

   function Decimal (Value : Unsigned_64) return String is
      Text : constant String := Value'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Decimal;

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
         -- One write, not a scheduling opportunity between every field.
         -- This fixes same-CPU service interleaving; the debug console is not
         -- a cross-CPU structured or bounded-latency logging transport.
         debugPrint
           ("desktop: stats ev=" & Decimal (statsEvents) &
            " key=" & Decimal (statsKeyboardEvents) &
            " mouse=" & Decimal (statsMouseEvents) &
            " button=" & Decimal (statsButtonTransitions) &
            " wheel=" & Decimal (statsWheelEvents) &
            " event_drop=" & Decimal (eventDropsThisPeriod) &
            " input_resync=" & Decimal (inputOverflowsThisPeriod) &
            " source_gap=" & Decimal (statsSourceGaps) &
            " source_reject=" & Decimal (statsSourceRejects) &
            " req=" & Decimal (statsRequests) &
            " frames=" & Decimal (statsFrames) &
            " fast=" & Decimal (statsFastFrames) &
            " full=" & Decimal (statsFullFrames) &
            " present_req=" & Decimal (statsPresentReq) &
            " input_req=" & Decimal (statsInputReq) &
            " other_req=" & Decimal (statsOtherReq) &
            " draw_ms=" & Decimal (statsDrawMs) &
            " submit=" & Decimal (statsPresentOps) &
            " completion_ms=" & Decimal (statsCompletionMs) &
            " px=" & Decimal (statsDamagePixels) &
            " cursor_x=" & Decimal (Unsigned_64 (cursorX)) &
            " cursor_y=" & Decimal (Unsigned_64 (cursorY)) & LF);
      end if;

      statsStartMs := now;
      statsEvents := 0;
      CuBit.Graphics_Metrics_IO.Publish
        (GM.Desktop_Staging, stagingCopies, stagingReporter);
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
      statsCompletionMs := 0;
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
      debugPrint ("desktop: ptr " & label & " " & Decimal (a) &
        " " & Decimal (b) & " " & Decimal (c) & LF);
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
      Bounds : constant Rect := primaryBounds;
   begin
      if Bounds.h > TASKBAR_H then
         return Bounds.y + Bounds.h - TASKBAR_H;
      else
         return Bounds.y;
      end if;
   end taskbarY;

   function taskbarRect return Rect is
     (primaryBounds.x, taskbarY, primaryBounds.w, TASKBAR_H);

   function launchButtonRect return Rect is
   begin
      return clampRect ((x => primaryBounds.x + 6, y => taskbarY + 6,
                         w => LAUNCH_W, h => LAUNCH_H));
   end launchButtonRect;

   function launchMenuRect return Rect is
      y : Natural := 0;
   begin
      if taskbarY > MENU_H then
         y := taskbarY - MENU_H;
      end if;

      return clampRect ((x => primaryBounds.x + 6, y => y,
                         w => MENU_W, h => MENU_H));
   end launchMenuRect;

   function launchItemRect (action : Launch_Action) return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural;
   begin
      if isEmpty (menu) or else action = LAUNCH_NONE or else menu.w <= 16 then
         return (others => 0);
      end if;

      case action is
         when LAUNCH_WORKBENCH =>
            y := menu.y + 42;
         when LAUNCH_DOOM =>
            y := menu.y + 76;
         when LAUNCH_DEVICES =>
            y := menu.y + 110;
         when LAUNCH_BROWSER =>
            y := menu.y + 144;
         when LAUNCH_FILES =>
            y := menu.y + 178;
         when LAUNCH_SAMEBOY =>
            y := menu.y + 212;
         when LAUNCH_POWER =>
            y := menu.y + 286;
         when LAUNCH_SETTINGS =>
            y := menu.y + 246;
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
      y := menu.y + 278;
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
      x       : Natural := primaryBounds.x + 104 +
        ordinal * (TASK_BUTTON_W + TASK_BUTTON_GAP);
      right   : constant Natural := primaryBounds.x + primaryBounds.w;
      maxW    : Natural := TASK_BUTTON_W;
   begin
      if primaryBounds.w < 160 or else x >= right - 160 then
         return (others => 0);
      end if;

      if x + maxW + 6 > right - 160 then
         maxW := right - 160 - x;
      end if;

      return clampRect ((x => x, y => taskbarY + 6,
                         w => maxW, h => TASK_BUTTON_H));
   end taskButtonRect;

   function statusRect return Rect is
     (clampRect ((x => primaryBounds.x +
                    (if primaryBounds.w >= 160 then primaryBounds.w - 160 else 0),
                  y => taskbarY + 4, w => 154, h => 28)));

   function speakerRect return Rect is
     (clampRect ((x => statusRect.x + 4, y => taskbarY + 6,
                  w => 56, h => 24)));

   function audioPopupRect return Rect is
     (clampRect ((x => primaryBounds.x +
                    (if primaryBounds.w >= 250 then primaryBounds.w - 250 else 0),
                  y => (if taskbarY >= 124 then taskbarY - 124 else 0),
                  w => 244, h => 118)));

   function volumeTrackRect return Rect is
     (clampRect ((x => audioPopupRect.x + 18,
                  y => audioPopupRect.y + 46, w => 208, h => 24)));

   function muteButtonRect return Rect is
     (clampRect ((x => audioPopupRect.x + 18,
                  y => audioPopupRect.y + 80, w => 92, h => 26)));

   function pointInRect (x, y : Natural; r : Rect) return Boolean is
   begin
      return not isEmpty (r) and then
         x >= r.x and then y >= r.y and then
         x < r.x + r.w and then y < r.y + r.h;
   end pointInRect;

   function hitLaunchItem (x, y : Natural) return Launch_Action is
   begin
      if pointInRect (x, y, launchItemRect (LAUNCH_WORKBENCH)) then
         return LAUNCH_WORKBENCH;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_DOOM)) then
         return LAUNCH_DOOM;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_DEVICES)) then
         return LAUNCH_DEVICES;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_BROWSER)) then
         return LAUNCH_BROWSER;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_FILES)) then
         return LAUNCH_FILES;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_SAMEBOY)) then
         return LAUNCH_SAMEBOY;
      elsif pointInRect (x, y, launchItemRect (LAUNCH_SETTINGS)) then
         return LAUNCH_SETTINGS;
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

   function localDamage (Output : Output_Index; Area : Rect) return Rect is
      R : constant DG.Physical_Rectangle := DG.Damage
        (presentations (Output).Geometry,
         (DG.Logical_Coordinate (Area.x), DG.Logical_Coordinate (Area.y),
          DG.Logical_Coordinate (Area.x + Area.w),
          DG.Logical_Coordinate (Area.y + Area.h)));
   begin
      return (Natural (R.Left), Natural (R.Top),
              Natural (R.Right - R.Left), Natural (R.Bottom - R.Top));
   end localDamage;

   function windowWorkArea (Bounds : Rect) return Rect is
      Winner : Output_Index := primaryOutput;
      Largest : Unsigned_64 := 0;
      Result : Rect;
   begin
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            declare
               R : constant Rect := localDamage (Output, Bounds);
               Area : constant Unsigned_64 :=
                 Unsigned_64 (R.w) * Unsigned_64 (R.h);
            begin
               if Area > Largest or else
                 (Area = Largest and then Output = primaryOutput)
               then
                  Winner := Output;
                  Largest := Area;
               end if;
            end;
         end if;
      end loop;
      Result := (Natural (presentations (Winner).Geometry.X),
                 Natural (presentations (Winner).Geometry.Y),
                 Natural (presentations (Winner).Geometry.Width),
                 Natural (presentations (Winner).Geometry.Height));
      if Winner = primaryOutput and then Result.h > TASKBAR_H then
         Result.h := Result.h - TASKBAR_H;
      end if;
      return Result;
   end windowWorkArea;

   procedure flushBackBufferRect (dirty : Rect) is
      r : constant Rect := clampRect (dirty);
   begin
      if backBufferReady and then fbBpp = 32 and then not isEmpty (r) then
         for Output in Output_Index loop
            if presentations (Output).Enabled then
               presentations (Output).Damage := unionRect
                 (presentations (Output).Damage, localDamage (Output, r));
            end if;
         end loop;
      end if;
   end flushBackBufferRect;

   procedure quarantinePresentations is
   begin
      for P of presentations loop
         P.Phase := Quarantined;
      end loop;
   end quarantinePresentations;

   procedure collectPresentations is
      completion : CompletionEntry;
      count : Unsigned_64;
      matched : Boolean;
      use type DSP.Frame_Outcome, DSP.Buffer_Disposition;
   begin
      loop
         completion := NULL_COMPLETION;
         count := Poll_Completion (completion'Address);
         exit when count = 0;
         if count /= 1 then
            quarantinePresentations;
            debugPrint ("desktop: completion queue unavailable" & LF);
            exit;
         end if;
         if completion.token > retiredThrough then
            matched := False;
            for P of presentations loop
               if P.Enabled and then completion.token = P.Token then
                  matched := True;
                  declare
                     result : constant DSP.Frame_Result_Decoding :=
                       DSP.Decode_Frame_Result
                         (CuBit.Desktop_Messages.To_Wire (completion.msg));
                  begin
                     -- The non-reused kernel completion token selects the
                     -- output. Payload session/frame values only validate it.
                     if not completion.valid or else
                       completion.status /= COMPLETION_OK or else
                       P.Phase /= In_Flight or else not result.Valid or else
                       result.Value.Session /= P.Session or else
                       result.Value.Frame /= P.Token or else
                       result.Value.Outcome /= DSP.Published or else
                       result.Value.Buffer_State /= DSP.Released
                     then
                        P.Phase := Quarantined;
                        debugPrint ("desktop: asynchronous transfer quarantined" & LF);
                     else
                        P.Phase := Available;
                        statsCompletionMs := statsCompletionMs + nowMs - P.Started;
                        if not releaseAnnounced then
                           releaseAnnounced := True;
                           debugPrint ("desktop: asynchronous frame released" & LF);
                        end if;
                     end if;
                  end;
                  exit;
               end if;
            end loop;
            if not matched then
               -- No buffer may be reused on an unrecognized completion.
               quarantinePresentations;
               debugPrint ("desktop: unknown presentation completion" & LF);
            end if;
         end if;
      end loop;
   end collectPresentations;

   procedure pumpOutput (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      r : constant Rect := P.Damage;
      ignored : System.Address;
      request : Message;
   begin
      if not P.Enabled or else P.Phase /= Available or else isEmpty (r) then
         return;
      end if;
      if frameSequence >= Unsigned_64'Last - 1 then
         quarantinePresentations;
         debugPrint ("desktop: frame identifiers exhausted" & LF);
         return;
      end if;
      -- Source is desktop-local, destination is output-local. This initial
      -- copy path admits only unrotated unit scale; no resampling is implied.
      for row in r.y .. r.y + r.h - 1 loop
         ignored := memcpy
           (P.Buffer + Storage_Offset (row * P.Pitch + r.x * 4),
            backBufferAddr + Storage_Offset
              ((row + Natural (P.Geometry.Y)) * fbPitch +
               (r.x + Natural (P.Geometry.X)) * 4),
            Storage_Count (r.w * 4));
      end loop;
      GM.Add (stagingCopies, Unsigned_64 (r.w) * Unsigned_64 (r.h) * 4);
      frameSequence := frameSequence + 1;
      P.Token := frameSequence;
      request := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
        (DSP.Encode_Frame
           ((P.Session, P.Token,
             (DP.Pixel_Coordinate (r.x), DP.Pixel_Coordinate (r.y),
              DP.Pixel_Extent (r.w), DP.Pixel_Extent (r.h)))), Output));
      P.Phase := In_Flight;
      P.Started := nowMs;
      if capSubmit (CAP_SLOT_DISPLAY, request, P.Token) then
         P.Damage := (others => 0);
         statsPresentOps := statsPresentOps + 1;
         if not asyncAnnounced then
            asyncAnnounced := True;
            debugPrint ("desktop: asynchronous presentation active" & LF);
         end if;
      else
         P.Phase := Quarantined;
         debugPrint ("desktop: asynchronous submission unavailable" & LF);
      end if;
   end pumpOutput;

   procedure pumpPresentation is
   begin
      if backBufferReady then
         for Output in Output_Index loop
            pumpOutput (Output);
         end loop;
      end if;
   end pumpPresentation;

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

   procedure drawWallpaper is
      Area : Rect := (0, 0, fbWidth, fbHeight);
   begin
      if clipEnabled then Area := clipRect; end if;
      if backBufferAddr = System.Null_Address or else isEmpty (Area) then
         return;
      end if;
      -- Aspect-fill each monitor independently. The backing scene has a
      -- wider row pitch, but this call touches only its clipped local pixels.
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            declare
               P : Output_Presentation renames presentations (Output);
               R : constant Rect := localDamage (Output, Area);
            begin
               if not isEmpty (R) then
                  Desktop_Wallpaper.Paint
                    (backBufferAddr + Storage_Offset
                       (Natural (P.Geometry.Y) * fbPitch +
                        Natural (P.Geometry.X) * 4),
                     Natural (P.Geometry.Width), Natural (P.Geometry.Height),
                     fbPitch, R.x, R.y, R.w, R.h, appearance);
               end if;
            end;
         end if;
      end loop;
   end drawWallpaper;

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
   begin
      for i in s'Range loop
         width := width + CuBit.Fonts.Width (CuBit.Fonts.Sans, s (i));
      end loop;
      return width;
   end uiTextWidth;

   procedure drawUIGlyph
      (x, y : Natural;
       ch   : Character;
       fg   : Unsigned_32;
       bg   : Unsigned_32;
       transparent : Boolean := False)
   is
      glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Sans, ch);
      width : Natural;
      alpha : Natural;
   begin
      width := Natural (glyph.Advance);
      if not transparent then
         fillRect (x, y, width, CuBit.Fonts.Line_Height, bg);
      end if;

      for yy in 0 .. CuBit.Fonts.Line_Height - 1 loop
         for xx in 0 .. width - 1 loop
            alpha := Natural (glyph.Alpha (yy, xx));
            if alpha = 255 then
               putPixel (x + xx, y + yy, fg);
            elsif alpha /= 0 then
               putPixel (x + xx, y + yy, blendPixel
                 (fg, (if transparent then readBackPixel (x + xx, y + yy)
                       else bg), alpha));
            end if;
         end loop;
      end loop;
   end drawUIGlyph;

   procedure drawUIText
      (x, y : Natural;
       s    : String;
       fg   : Unsigned_32;
       bg   : Unsigned_32;
       transparent : Boolean := False)
   is
      cx : Natural := x;
   begin
      for i in s'Range loop
         drawUIGlyph (cx, y, s (i), fg, bg, transparent);
         cx := cx + uiTextWidth (s (i .. i));
      end loop;
   end drawUIText;

   procedure drawSurfaceTitle
      (s       : Surface;
       x, y    : Natural;
       fg, bg  : Unsigned_32;
       transparent : Boolean := False)
   is
   begin
      case s.appKind is
         when APP_DOOM =>
            drawUIText (x, y, "DOOM", fg, bg, transparent);
         when APP_SETTINGS =>
            drawUIText (x, y, "Settings", fg, bg, transparent);
         when others =>
            if s.title.Length > 0 then
               drawUIText (x, y, s.title.Text, fg, bg, transparent);
            else
               drawUIText (x, y, "Application", fg, bg, transparent);
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

      --  Scanout also needs the restored pixels, even outside scene damage
      --  and the new pointer position. All repaint paths share this rule.
      flushBackBufferRect (cursorSaveRect);
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
      flushBackBufferRect (r);
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
      flushBackBufferRect (damage);
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
      if isEmpty (r) or else launchMenuOpen or else audioPopupOpen or else
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
                  declare
                     savedClip : constant Rect := clipRect;
                     savedEnabled : constant Boolean := clipEnabled;
                  begin
                     --  Keep the private scene and scanout consistent outside
                     --  a partial present, including saved cursor backgrounds.
                     clipRect := r;
                     clipEnabled := True;
                     drawClientBuffer (surfaces (i), c.x, c.y, c.w, c.h);
                     clipRect := savedClip;
                     clipEnabled := savedEnabled;
                  end;
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
      theme : constant CuBit.UI.Theme := CuBit.UI.Current_Theme;
      titleTop : constant Unsigned_32 :=
        (if active then theme.activeTitleTop else theme.inactiveTitleTop);
      titleBottom : constant Unsigned_32 :=
        (if active then theme.activeTitleBottom else theme.inactiveTitleBottom);
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
         titleText := C_WHITE;
      end if;

      frame.x := x;
      frame.y := y;
      frame.w := w;
      frame.h := h;

      drawDappledShadow (x, y, w, h);
      fillRect (x, y, w, h, C_WIN);
      strokeRect (x, y, w, h, C_EDGE, C_SHADOW);
      for row in 0 .. titleH - 1 loop
         fillRect (x + 3, y + 3 + row, w - 6, 1,
                   blendPixel (titleBottom, titleTop, row * 255 / (titleH - 1)));
      end loop;
      drawSurfaceTitle (s, x + 10, y + 7, titleText, titleTop, True);
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
         when APP_SETTINGS =>
            declare
               bounds : constant Rect := clientRect (frame);
            begin
               Desktop_Settings.Draw
                 (settingsView,
                  CuBit.UI.With_Clip
                  ((addr => backBufferAddr, width => fbWidth, height => fbHeight,
                   pitch => fbPitch, clipEnabled => True,
                   clip => (if clipEnabled then (clipRect.x, clipRect.y, clipRect.w, clipRect.h)
                            else (0, 0, fbWidth, fbHeight))),
                   (bounds.x, bounds.y, bounds.w, bounds.h)),
                  (bounds.x, bounds.y, bounds.w, bounds.h));
            end;
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
        (LAUNCH_WORKBENCH, Desktop_Icons.UILab, "CCL Workbench", C_TEXT);
      drawLaunchItem (LAUNCH_DOOM, Desktop_Icons.Doom, "DOOM", C_TEXT);
      drawLaunchItem
        (LAUNCH_DEVICES, Desktop_Icons.Files, "Devices", C_TEXT);
      drawLaunchItem
        (LAUNCH_BROWSER, Desktop_Icons.Files, "NetSurf", C_TEXT);
      drawLaunchItem
        (LAUNCH_FILES, Desktop_Icons.Files, "Files", C_TEXT);
      drawLaunchItem
        (LAUNCH_SAMEBOY, Desktop_Icons.Doom, "SameBoy", C_TEXT);
      drawLaunchItem
        (LAUNCH_SETTINGS, Desktop_Icons.UILab, "Settings", C_TEXT);
      declare
         sep : constant Rect := launchSeparatorRect;
      begin
         fillRect (sep.x, sep.y, sep.w, sep.h, C_EDGE);
      end;
      drawLaunchItem
        (LAUNCH_POWER, Desktop_Icons.Power, "Power", C_MUTED);
   end drawLaunchMenu;

   procedure drawStatus is
      r : constant Rect := statusRect;
      speaker : constant Rect := speakerRect;
      ink : constant Unsigned_32 :=
        (if masterAudio.Available then C_TEXT else C_MUTED);
      iconX : constant Natural := speaker.x + 4;
      iconY : constant Natural := speaker.y + 5;
   begin
      fillRect (r.x, r.y, r.w, r.h, C_BAR);
      strokeRect (r.x, r.y, r.w, r.h, C_SHADOW, C_EDGE);
      if audioPopupOpen then
         strokeRect (speaker.x, speaker.y, speaker.w, speaker.h,
                     C_SHADOW, C_EDGE);
      end if;
      --  Original compact speaker geometry; no borrowed platform artwork.
      fillRect (iconX, iconY + 4, 4, 6, ink);
      for column in 0 .. 4 loop
         fillRect (iconX + 4 + column, iconY + 4 - column,
                   1, 6 + column * 2, ink);
      end loop;
      if masterAudio.Muted then
         for step in 0 .. 4 loop
            putPixel (iconX + 11 + step, iconY + 4 + step, C_ACCENT);
            putPixel (iconX + 11 + step, iconY + 8 - step, C_ACCENT);
         end loop;
      else
         fillRect (iconX + 11, iconY + 4, 1, 6, ink);
         fillRect (iconX + 14, iconY + 2, 1, 10, ink);
      end if;
      drawUIText (speaker.x + 24, speaker.y + 3,
                  masterAudio.Level'Image, ink, C_BAR);
      drawUIText (r.x + 84, r.y + 5, clockText, C_TEXT, C_BAR);
   end drawStatus;

   procedure drawAudioPopup is
      r : constant Rect := audioPopupRect;
      track : constant Rect := volumeTrackRect;
      mute : constant Rect := muteButtonRect;
      thumbX : constant Natural := track.x + masterAudio.Level * 196 / 100;
   begin
      if not audioPopupOpen then return; end if;
      fillRect (r.x, r.y, r.w, r.h, C_PANEL);
      strokeRect (r.x, r.y, r.w, r.h, C_EDGE, C_SHADOW);
      drawUIText (r.x + 18, r.y + 12,
                  "Master volume" & masterAudio.Level'Image & "%", C_TEXT, C_PANEL);
      fillRect (track.x + 6, track.y + 10, 196, 4, C_SHADOW);
      fillRect (track.x + 6, track.y + 10, masterAudio.Level * 196 / 100, 4, C_ACCENT);
      fillRect (thumbX, track.y + 2, 12, 20, C_BAR);
      strokeRect (thumbX, track.y + 2, 12, 20, C_EDGE, C_SHADOW);
      fillRect (mute.x, mute.y, mute.w, mute.h, C_BAR);
      strokeRect (mute.x, mute.y, mute.w, mute.h,
                  (if masterAudio.Muted then C_SHADOW else C_EDGE),
                  (if masterAudio.Muted then C_EDGE else C_SHADOW));
      drawUIText (mute.x + 12, mute.y + 4,
                  (if masterAudio.Muted then "Unmute" else "Mute"), C_TEXT, C_BAR);
   end drawAudioPopup;

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
         (if launch.h > CuBit.Fonts.Line_Height
          then (launch.h - CuBit.Fonts.Line_Height) / 2
          else 0);
   begin
      if fbWidth = 0 or else fbHeight = 0 then
         return;
      end if;

      --  First shell renderer: deliberately Win95-simple. The compositor owns
      --  pixels for now; the shell owns policy and talks through the protocol.
      --  Shared client buffers can replace this drawing path later without
      --  changing the surface/session shape.
      drawWallpaper;
      fillRect (primaryBounds.x, barY, primaryBounds.w, TASKBAR_H, C_BAR);
      strokeRect (primaryBounds.x, barY, primaryBounds.w, TASKBAR_H, C_EDGE, C_SHADOW);
      fillRect (primaryBounds.x, barY, primaryBounds.w, 2, C_ACCENT);

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
      drawStatus;
      drawAudioPopup;
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
         drawWallpaper;
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
      --  The retained layer can outlive a minute boundary or media-key press.
      --  Composite current taskbar state over it, within this damage clip.
      drawStatus;
      drawAudioPopup;
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

      procedure Present_Regions is
      begin
         for i in Damage_Index'First .. Damage_Index (count) loop
            flushBackBufferRect (regions (i));
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
      workArea  : constant Rect := windowWorkArea (oldBounds);
      nextW     : Natural;
      nextH     : Natural;
   begin
      if not hasWindowFlag (surfaces (idx), WINDOW_FLAG_MAXIMIZABLE) or else
         hasWindowFlag (surfaces (idx), WINDOW_FLAG_FIXED_SIZE)
      then
         return;
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
         nextW := workArea.w;
         nextH := workArea.h;
         clampSurfaceSize (surfaces (idx), nextW, nextH);
         surfaces (idx).x := workArea.x;
         surfaces (idx).y := workArea.y;
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
        (damage, inflateRect (taskbarRect, 2));
   end toggleMaximizeSurface;

   procedure releaseSurfaceBuffer (s : in out Surface) is
      returned : Boolean;
   begin
      if s.bufferAttached then
         -- Single compositor event loop: no blit retains this address after
         -- the handler returns. This is lifetime, not pixel immutability.
         s.bufferAttached := False;
         s.bufferAddr := System.Null_Address;
         MG.Return_Acquisition (s.bufferGrant, returned);
         if not returned then
            debugPrint ("TEST: FAIL desktop buffer acquisition return" & LF);
         end if;
      end if;
   end releaseSurfaceBuffer;

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

      releaseSurfaceBuffer (surfaces (idx));
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
            -- The acquisition keeps mappings alive across owner exit. Reaping
            -- releases it; processAlive is no longer the memory-safety fence.
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
            if surfaces (i).bufferAttached then
               releaseSurfaceBuffer (surfaces (i));
               debugPrint ("desktop: dead client buffer acquisition released" & LF);
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
           (damage, inflateRect (taskbarRect, 2));
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
         when APP_SETTINGS =>
            winW := 610;
            winH := 420;
            Desktop_Settings.Open (settingsView, appearance);
         when others =>
            null;
      end case;

      if winX + winW > primaryBounds.w then
         winW := Natural'Max (MIN_WIN_W, primaryBounds.w - winX);
      end if;
      if winY + winH > primaryBounds.h then
         winH := Natural'Max (MIN_WIN_H, primaryBounds.h - winY);
      end if;
      winX := winX + primaryBounds.x;
      winY := winY + primaryBounds.y;

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
            inflateRect (taskbarRect, 2));
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
         windowFlags => (if appKind = APP_SETTINGS then
           WINDOW_FLAG_DECORATED or WINDOW_FLAG_MINIMIZABLE or
           WINDOW_FLAG_CLOSEABLE or WINDOW_FLAG_FIXED_SIZE
           else WINDOW_FLAGS_DEFAULT),
         title => (0, ""),
         bufferAttached => False,
         bufferGrant => <>,
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
            reserved => 0);
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

   function nextInputDeadline return Unsigned_64 is
      deadline : Unsigned_64 := 0;
   begin
      for channel of inputChannels loop
         if channel.waiter.active and then channel.waiter.deadline /= 0 and then
           (deadline = 0 or else channel.waiter.deadline < deadline)
         then
            deadline := channel.waiter.deadline;
         end if;
      end loop;
      return deadline;
   end nextInputDeadline;

   procedure expireInputWaiters is
      now : constant Unsigned_64 := nowMs;
      response : Message;
      ignore : Unsigned_64;
   begin
      for channel of inputChannels loop
         if channel.waiter.active and then channel.waiter.deadline /= 0 and then
           now >= channel.waiter.deadline
         then
            -- Input publication completes its waiter immediately. An expiry
            -- consumes the same one-use reply, without inventing an input
            -- serial or clearing queued events.
            response := NULL_MESSAGE;
            response.tag := (OP_INPUT_WAIT, 4, 0, 0);
            response.words (0) := INPUT_NONE;
            response.words (1) := channel.waiter.afterSerial;
            declare
               slot : constant CapabilitySlot := channel.waiter.replySlot;
            begin
               channel.waiter := (replySlot => slot, others => <>);
               ignore := replyCap (slot, response);
            end;
         end if;
      end loop;
   end expireInputWaiters;

   --  A source-stream discontinuity invalidates transient ownership and every
   --  queued transition derived from the old stream history. Publish one
   --  authoritative state record per surface instead of attempting to guess
   --  which lost press/release events should be replayed.
   procedure forceInputResynchronization is
      mods : Unsigned_64 := 0;
   begin
      CuBit.Click_Sequences.Reset (titleClicks);
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
           (label => OP_INPUT_WAIT, length => 4, flags => 0, reserved => 0);
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
       w3    : Unsigned_64 := 0;
       Output : DSP.Output_Number := 0) return Message;

   procedure setupDisplayBuffer (ok : out Boolean);
   procedure releaseDisplayBuffer;
   procedure activateInternalSession (ok : out Boolean);

   procedure handleRequest (from : ProcessID; request : Message) is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
      replyNow : Boolean := True;
      creation : DP.Create_Decoding;
      present : DP.Present_Decoding;
      resize : DP.Resize_Decoding;
      limits : DP.Limits_Decoding;
      destruction : DP.Destroy_Decoding;
      cursorRequest : DP.Cursor_Decoding;
      titleRequest : DP.Title_Decoding;
      greeting : DP.Hello_Decoding;
      inputRequest : DP.Input_Request_Decoding;
      use type DP.Protocol_Revision;
      use type DP.Status_Code;
      use type DP.Operation;
   begin
      if request.tag.label = DP.Code (DP.Get_Appearance) then
         --  Read-only observation over the existing desktop endpoint. There
         --  is deliberately no client-supplied appearance mutation request.
         if request.tag.length = 1 and then request.tag.flags = 0 and then
           request.tag.reserved = 0 and then request.words (0) <= 3 and then
           request.words (1) = 0 and then request.words (2) = 0 and then request.words (3) = 0
         then
            declare
               Data : constant CuBit.UI.Theme_Data.Color_Chunk :=
                 CuBit.UI.Theme_Data.Chunk (CuBit.UI.Theme_Data.Colors (CuBit.UI.Current_Theme),
                   CuBit.UI.Theme_Data.Chunk_Index (request.words (0)));
            begin
               replyMsg.tag := (DP.Code (DP.Get_Appearance), 4, 0, 0);
               replyMsg.words := [themeRevision, Data (1), Data (2), Data (3)];
            end;
         else
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Get_Appearance, DP.Invalid_Request));
         end if;
         ignore := reply (from, replyMsg);
         return;
      end if;
      -- Snapshot the small inline payload once. The pure decoder establishes
      -- geometry bounds before any narrowing conversion or scene mutation.
      if request.tag.label = OP_SURFACE_CREATE then
         creation := DP.Decode_Create (CuBit.Desktop_Messages.To_Wire (request));
         if not creation.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Creation_Result ((Status => DP.Invalid_Request)));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_SURFACE_PRESENT then
         present := DP.Decode_Present (CuBit.Desktop_Messages.To_Wire (request));
         if not present.Valid then
            replyMsg.tag := (OP_SURFACE_PRESENT, 1, 0, 0);
            replyMsg.words (0) := DP.Status_Code'Enum_Rep (DP.Invalid_Request);
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_SURFACE_RESIZE then
         resize := DP.Decode_Resize (CuBit.Desktop_Messages.To_Wire (request));
         if not resize.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Resize_Result ((Status => DP.Invalid_Request)));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_WINDOW_SET_LIMITS then
         limits := DP.Decode_Limits (CuBit.Desktop_Messages.To_Wire (request));
         if not limits.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Limits_Result ((Status => DP.Invalid_Request)));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_SURFACE_DESTROY then
         destruction := DP.Decode_Destroy
           (CuBit.Desktop_Messages.To_Wire (request));
         if not destruction.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Destroy_Surface, DP.Invalid_Request));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_SURFACE_SET_POINTER_CURSOR then
         cursorRequest := DP.Decode_Cursor
           (CuBit.Desktop_Messages.To_Wire (request));
         if not cursorRequest.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Set_Pointer_Cursor, DP.Invalid_Request));
            ignore := reply (from, replyMsg);
            return;
         end if;
      end if;
      if request.tag.label = OP_WINDOW_SET_TITLE then
         titleRequest := DP.Decode_Title
           (CuBit.Desktop_Messages.To_Wire (request));
         if not titleRequest.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Set_Window_Title, DP.Invalid_Request));
            ignore := reply (from, replyMsg);
            return;
         end if;
      end if;
      if request.tag.label = OP_DESKTOP_HELLO then
         greeting := DP.Decode_Hello (CuBit.Desktop_Messages.To_Wire (request));
         if not greeting.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Hello_Result ((Status => DP.Invalid_Request)));
            ignore := reply (from, replyMsg);
            return;
         elsif greeting.Revision /= DP.Current_Revision then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Hello_Result ((Status => DP.Unsupported)));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_DESKTOP_GET_INFO then
         if not DP.Valid_Empty_Request
           (CuBit.Desktop_Messages.To_Wire (request), DP.Get_Information)
         then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Information_Result ((Status => DP.Invalid_Request)));
            ignore := reply (from, replyMsg);
            return;
         end if;
      elsif request.tag.label = OP_DESKTOP_BYE then
         if not DP.Valid_Empty_Request
           (CuBit.Desktop_Messages.To_Wire (request), DP.Goodbye)
         then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Goodbye, DP.Invalid_Request));
            ignore := reply (from, replyMsg);
            return;
         end if;
      end if;
      if request.tag.label = OP_INPUT_POLL or else request.tag.label = OP_INPUT_WAIT then
         inputRequest := DP.Decode_Input_Request
           (CuBit.Desktop_Messages.To_Wire (request));
         if not inputRequest.Valid then
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status
                 ((if request.tag.label = OP_INPUT_POLL then DP.Poll_Input else DP.Wait_Input),
                  DP.Invalid_Request));
            ignore := reply (from, replyMsg);
            return;
         end if;
         declare
            idx : constant Integer := findSurface (Unsigned_64 (inputRequest.Value.Surface));
            accessStatus : constant DP.Status_Code := DP.Surface_Access
              (idx >= 0,
               (if idx >= 0 then Unsigned_64 (surfaces (SurfaceIndex (idx)).owner) else 0),
               Unsigned_64 (from));
         begin
            if accessStatus /= DP.Success then
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Status (inputRequest.Value.Kind, accessStatus));
               ignore := reply (from, replyMsg);
               return;
            end if;
         end;
      end if;
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
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Hello_Result ((DP.Success, 1,
                                        DP.Surface_Capacity (MAX_SURFACES))));

         when OP_DESKTOP_GET_INFO =>
            if primaryBounds.w not in 1 .. Natural (DP.Positive_Extent'Last) or else
              primaryBounds.h not in 1 .. Natural (DP.Positive_Extent'Last)
            then
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Information_Result ((Status => DP.Bad_State)));
            else
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Information_Result
                    ((DP.Success, DP.Positive_Extent (primaryBounds.w),
                      DP.Positive_Extent (primaryBounds.h), DP.BGRA_8888, DP.Unit_Scale)));
            end if;

         when OP_SURFACE_CREATE =>
            declare
               slot : Integer := -1;
               reqW : Natural := Natural (creation.Value.Width);
               reqH : Natural := Natural (creation.Value.Height);
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
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result ((Status => DP.Bad_State)));
               elsif slot < 0 or else nextSurfaceId = Unsigned_64'Last then
                  -- Never wrap the name allocator and alias a live object.
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result ((Status => DP.Resources_Exhausted)));
               else
                  if reqW = 0 or else reqW > primaryBounds.w then
                     reqW := primaryBounds.w;
                  end if;
                  if reqH = 0 or else reqH > primaryBounds.h then
                     reqH := primaryBounds.h;
                  end if;

                  if (request.words (2) and SURFACE_FLAG_WINDOW) /= 0 then
                     surfX := 80 + Natural (slot) * 18;
                     surfY := 64 + Natural (slot) * 18;
                     if reqW = primaryBounds.w or else reqW < 220 then
                        reqW := 360;
                     end if;
                     if reqH = primaryBounds.h or else reqH < 140 then
                        reqH := 220;
                     end if;
                     if surfX + reqW > primaryBounds.w then
                        reqW := primaryBounds.w - surfX;
                     end if;
                     if surfY + reqH > primaryBounds.h then
                        reqH := primaryBounds.h - surfY;
                     end if;
                  end if;

                  surfX := surfX + primaryBounds.x;
                  surfY := surfY + primaryBounds.y;
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
                     title => (0, ""),
                     bufferAttached => False,
                     bufferGrant => <>,
                     bufferAddr => System.Null_Address,
                     bufferW => 0,
                     bufferH => 0,
                     bufferPitch => 0,
                     bufferFormat => 0,
                     pointerCursor => POINTER_DEFAULT);
                  focusSurface := nextSurfaceId;

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result
                       ((DP.Success, DP.Live_Surface_Name (nextSurfaceId),
                         DP.Positive_Extent (reqW), DP.Positive_Extent (reqH), 1)));

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
               idx : constant Integer :=
                 findSurface (Unsigned_64 (resize.Value.Surface));
               newW : Natural := Natural (resize.Value.Width);
               newH : Natural := Natural (resize.Value.Height);
               oldBounds : Rect;
               newBounds : Rect;
            begin
               if idx < 0 then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Resize_Result ((Status => DP.Bad_Object)));
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Resize_Result ((Status => DP.Denied)));
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

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Resize_Result
                       ((DP.Success, DP.Pixel_Extent (newW),
                         DP.Pixel_Extent (newH),
                         surfaces (SurfaceIndex (idx)).serial)));

                  queueConfigure (request.words (0),
                                  Unsigned_64 (newW),
                                  Unsigned_64 (newH));
                  scheduleRedrawRect
                    (inflateRect (unionRect (oldBounds, newBounds), 4));
               end if;
            end;

         when OP_SURFACE_SET_POINTER_CURSOR =>
            declare
               idx : constant Integer :=
                 findSurface (Unsigned_64 (cursorRequest.Value.Surface));
               nextStyle : Pointer_Cursor_Style;
               result : DP.Status_Code;
            begin
               if idx < 0 then
                  result := DP.Bad_Object;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  surfaces (SurfaceIndex (idx)).pointerCursor :=
                    (case cursorRequest.Value.Style is
                       when DP.Default_Cursor => Pointer_Default,
                       when DP.Text_Cursor => Pointer_Text,
                       when DP.Horizontal_Resize_Cursor => Pointer_Resize_Horizontal,
                       when DP.Vertical_Resize_Cursor => Pointer_Resize_Vertical,
                       when DP.Diagonal_Resize_Cursor => Pointer_Resize_Diagonal);
                  nextStyle := cursorStyleAtPointer;
                  if nextStyle /= cursorStyle then
                     cursorStyle := nextStyle;
                     scheduleCursorPresent;
                  end if;
                  result := DP.Success;
               end if;
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Status (DP.Set_Pointer_Cursor, result));
            end;

         when OP_WINDOW_SET_LIMITS =>
            declare
               idx : constant Integer :=
                 findSurface (Unsigned_64 (limits.Value.Surface));
               minW : Natural := Natural (limits.Value.Bounds.Minimum_Width);
               minH : Natural := Natural (limits.Value.Bounds.Minimum_Height);
               maxW : Natural := Natural (limits.Value.Bounds.Maximum_Width);
               maxH : Natural := Natural (limits.Value.Bounds.Maximum_Height);
               winFlags : constant Unsigned_64 :=
                 DP.Feature_Bits (limits.Value.Features);
               oldBounds : Rect;
               newBounds : Rect;
               nextW : Natural;
               nextH : Natural;
            begin
               if idx < 0 then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Limits_Result ((Status => DP.Bad_Object)));
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Limits_Result ((Status => DP.Denied)));
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

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Limits_Result
                       ((DP.Success,
                         (DP.Pixel_Extent (minW), DP.Pixel_Extent (minH),
                          DP.Pixel_Extent (maxW), DP.Pixel_Extent (maxH)),
                         surfaces (SurfaceIndex (idx)).serial)));

                  scheduleRedrawRect
                    (inflateRect (unionRect (oldBounds, newBounds), 4));
               end if;
            end;

         when OP_WINDOW_SET_TITLE =>
            declare
               idx : constant Integer :=
                 findSurface (Unsigned_64 (titleRequest.Value.Surface));
               result : DP.Status_Code;
            begin
               if idx < 0 then
                  result := DP.Bad_Object;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  surfaces (SurfaceIndex (idx)).title := titleRequest.Value.Title;
                  surfaces (SurfaceIndex (idx)).dirty := True;
                  result := DP.Success;
                  scheduleRedrawRect
                    (inflateRect
                       (surfaceRect (surfaces (SurfaceIndex (idx))), 2));
               end if;
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Status (DP.Set_Window_Title, result));
            end;

         when OP_SURFACE_ATTACH_BUFFER =>
            declare
               decoded : constant DP.Attachment_Decoding :=
                 DP.Decode_Attachment (CuBit.Desktop_Messages.To_Wire (request));
               idx : constant Integer := findSurface (request.words (0));
               mapped : System.Address;
               acquired : Boolean;
            begin
               replyMsg.tag := (OP_SURFACE_ATTACH_BUFFER, 1, 0, 0);
               if not decoded.Valid then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (DP.Invalid_Request);
               elsif idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif surfaces (SurfaceIndex (idx)).serial = Unsigned_64'Last then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               else
                  -- Acquire the new buffer BEFORE releasing the old one.
                  -- Failed validation/acquisition leaves the old attachment intact.
                  MG.Acquire
                    (decoded.Value.Grant, from, 0,
                     DP.Byte_Length (decoded.Value.Layout), MG.Read_Access,
                     mapped, acquired);
                  if not acquired then
                     replyMsg.words (0) := UI_ERR_DENIED;
                  else
                     releaseSurfaceBuffer (surfaces (SurfaceIndex (idx)));
                     surfaces (SurfaceIndex (idx)).bufferGrant := decoded.Value.Grant;
                     surfaces (SurfaceIndex (idx)).bufferAddr := mapped;
                     surfaces (SurfaceIndex (idx)).bufferW :=
                       Natural (decoded.Value.Layout.Width);
                     surfaces (SurfaceIndex (idx)).bufferH :=
                       Natural (decoded.Value.Layout.Height);
                     surfaces (SurfaceIndex (idx)).bufferPitch := decoded.Value.Layout.Pitch;
                     surfaces (SurfaceIndex (idx)).bufferFormat := PIXEL_FORMAT_BGRA8888;
                     surfaces (SurfaceIndex (idx)).bufferAttached := True;
                     surfaces (SurfaceIndex (idx)).dirty := True;
                     surfaces (SurfaceIndex (idx)).serial :=
                       surfaces (SurfaceIndex (idx)).serial + 1;
                     replyMsg.words (0) := UI_OK;
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
                             reserved  => 0);
            replyMsg.words (0) := UI_OK;
            declare
               idx : constant Integer := findSurface (request.words (0));
               accessStatus : constant DP.Status_Code := DP.Surface_Access
                 (idx >= 0,
                  (if idx >= 0 then Unsigned_64 (surfaces (SurfaceIndex (idx)).owner) else 0),
                  Unsigned_64 (from));
            begin
               if accessStatus /= DP.Success then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (accessStatus);
               elsif
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
                     damage : constant DP.Rectangle :=
                       DP.Clip (present.Value.Area, client.w, client.h);
                  begin
                     if DP.Mode (present.Value) = DP.Whole_Surface then
                        scheduleRedrawRect (client);
                     else
                        scheduleRedrawRect
                          ((x => client.x + Natural (damage.X),
                            y => client.y + Natural (damage.Y),
                            w => Natural (damage.Width), h => Natural (damage.Height)));
                     end if;
                  end;
               else
                  scheduleRedraw;
               end if;
            end;

         when OP_SURFACE_DESTROY =>
            declare
               target : constant Unsigned_64 :=
                 Unsigned_64 (destruction.Value.Surface);
               idx : constant Integer := findSurface (target);
               result : DP.Status_Code;
            begin
               if idx < 0 then
                  result := DP.Bad_Object;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  if pointerSurfaceId = target then
                     pointerSurfaceId := 0;
                  end if;
                  if dragSurfaceId = target then
                     dragSurfaceId := 0;
                     dragMode := DRAG_NONE;
                     dragPreviewValid := False;
                     dragPresentedValid := False;
                  end if;
                  clearInputForTarget (target);
                  releaseSurfaceBuffer (surfaces (SurfaceIndex (idx)));
                  surfaces (SurfaceIndex (idx)) := (others => <>);
                  if focusSurface = target then
                     focusSurface := 0;
                  end if;
                  result := DP.Success;
                  if anySurfaceUsed then
                     scheduleRedraw;
                  else
                     releaseDisplayBuffer;
                  end if;
               end if;
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (DP.Encode_Status (DP.Destroy_Surface, result));
            end;

         when OP_INPUT_POLL | OP_INPUT_WAIT =>
            replyMsg.tag := (label  => request.tag.label,
                             length => 4,
                             flags  => 0,
                             reserved  => 0);
            declare
               found : Boolean;
               event : PendingInput;
               target : constant Unsigned_64 := Unsigned_64 (inputRequest.Value.Surface);
               afterSerial : constant Unsigned_64 := inputRequest.Value.After_Serial;
               deadline : constant Unsigned_64 :=
                 (if inputRequest.Value.Kind = DP.Wait_Input then inputRequest.Value.Deadline else 0);
               channelSlot : constant Integer := findInputChannel (target);
            begin
               --  Admission checked the complete request and authenticated
               --  owner before any dequeue or saved-reply state mutation.
               dequeueInput (target, afterSerial, found, event);
               if found then
                  if hasInputAfter (target, event.serial) then
                     replyMsg.tag.flags := INPUT_REPLY_MORE_PENDING;
                  end if;
                  replyMsg.words (0) := event.kind;
                  replyMsg.words (1) := event.serial;
                  replyMsg.words (2) := event.payload0;
                  replyMsg.words (3) := event.payload1;
               elsif request.tag.label = OP_INPUT_WAIT and then
                 (deadline = 0 or else deadline > nowMs) and then
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
                           target      => target,
                           afterSerial => afterSerial,
                           deadline    => deadline,
                           replySlot   => slot);
                        replyNow := False;
                     else
                        replyMsg.words (0) := INPUT_RESYNC;
                        replyMsg.words (1) := afterSerial;
                     end if;
                  end;
               else
                  replyMsg.words (0) := INPUT_NONE;
                  replyMsg.words (1) := afterSerial;
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
                  releaseSurfaceBuffer (surfaces (i));
                  surfaces (i) := (others => <>);
               end if;
            end loop;
            if focusSurface /= 0 and then findSurface (focusSurface) < 0 then
               focusSurface := 0;
            end if;
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (DP.Encode_Status (DP.Goodbye, DP.Success));
            if anySurfaceUsed then
               scheduleRedraw;
            else
               releaseDisplayBuffer;
            end if;

         when others =>
            replyMsg.tag := (label  => request.tag.label,
                             length => 1,
                             flags  => 0,
                             reserved  => 0);
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
      --  Modifiers are now tracked. Plain Tab belongs to widget navigation.
      return (not release) and then desktopAltDown and then code = 16#0F#;
   end shouldCycleFocusKey;

   function updateDesktopModifierKey (raw : Unsigned_8) return Boolean is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
   begin
      --  Keep compositor-side modifier state for internal desktop surfaces.
      --  Client surfaces still receive the raw key events; this state is only
      --  used by desktop shortcuts and compositor-owned Settings controls.
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
         debugPrint ("desktop: spawn buffer allocation failed" & LF);
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
         debugPrint ("desktop: spawn grant to procmgr failed" & LF);
      end if;
   end ensureSpawnGrant;

   procedure trySpawnApplication (name : String; ok : out Boolean) is
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
                  reserved  => 0);
      msg.words (0) := spawnGrantId;
      msg.words (1) := 5;
      msg.words (2) := 0;
      msg.words (3) := 0;
      tag := capCall (CAP_SLOT_PROCMGR, msg);

      if tag.label = REPLY_OK then
         lastSpawnedPid := ProcessID (msg.words (0) and 16#FFFF#);
         ok := True;
      end if;
   end trySpawnApplication;

   procedure Apply_Appearance (damage : in out Rect) is
      Text : constant CuBit.Appearance.Encoding := CuBit.Appearance.Encode (settingsView.Pending);
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      if themeRevision = Unsigned_64'Last then return; end if;
      appearance := settingsView.Pending;
      Load_Theme;
      themeRevision := themeRevision + 1;
      CuBit.Config.set (CuBit.Appearance.Config_Key, Text'Address, Text'Length, Status);
      settingsView.Applied := appearance;
      settingsView.Status := (if Status = CuBit.Config.OK then Desktop_Settings.Saved_In_Config
                              else Desktop_Settings.Session_Only);
      dragBaseReady := False;
      damage := (0, 0, fbWidth, fbHeight);
      for S of surfaces loop
         if S.used and then S.owner /= NO_PROCESS then
            queueConfigure (S.id, Unsigned_64 (S.w), Unsigned_64 (S.h));
         end if;
      end loop;
      debugPrint ("desktop: appearance applied" & LF);
   end Apply_Appearance;

   procedure Settings_Pointer (Index : SurfaceIndex; Down, Pressed, Released : Boolean;
                               damage : in out Rect) is
      Bounds : constant Rect := clientRect (surfaces (Index));
      Before : constant Desktop_Settings.State := settingsView;
      Apply : Boolean;
      use type Desktop_Settings.State;
   begin
      Desktop_Settings.Pointer (settingsView, (Bounds.x, Bounds.y, Bounds.w, Bounds.h),
        (cursorX, cursorY, Down, Pressed, Released, True), Apply);
      if Apply then Apply_Appearance (damage);
      elsif Before /= settingsView then damage := unionRect (damage, Bounds);
      end if;
   end Settings_Pointer;

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
         when APP_SETTINGS =>
            if raw < 128 then
               declare Apply : Boolean; begin
                  Desktop_Settings.Key (settingsView, Natural (raw), desktopShiftDown, Apply);
                  if Apply then Apply_Appearance (damage);
                  else damage := unionRect (damage, surfaceRect (surfaces (SurfaceIndex (idx)))); end if;
               end;
            end if;
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
         when LAUNCH_WORKBENCH =>
            trySpawnApplication ("ccl-workbench.app", ok);
         when LAUNCH_SETTINGS =>
            openInternalApp (APP_SETTINGS, damage);
         when LAUNCH_DOOM =>
            if doomPid /= NO_PROCESS and then processAlive (doomPid) then
               debugPrint ("desktop: DOOM is already running" & LF);
            else
               trySpawnApplication ("doom.elf", ok);
               if ok then
                  doomPid := lastSpawnedPid;
               end if;
            end if;
         when LAUNCH_DEVICES =>
            trySpawnApplication ("devices.app", ok);
         when LAUNCH_BROWSER =>
            trySpawnApplication ("netsurf.app", ok);
         when LAUNCH_FILES =>
            trySpawnApplication ("files.app", ok);
         when LAUNCH_SAMEBOY =>
            trySpawnApplication ("sameboy.app", ok);
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

   procedure refreshStatus is
      stamp : CuBit.Clocks.Snapshot;
      audio : CuBit.Audio_Control.State;
      ok : Boolean;
      nextText : String (1 .. 5) := "--:--";
      now : constant Unsigned_64 := nowMs;
      use type CuBit.Clocks.Time_Quality;
      use type CuBit.Audio_Control.State;
      function digit (v : Natural) return Character is
        (Character'Val (Character'Pos ('0') + v));
   begin
      if now < statusDueMs then return; end if;
      statusDueMs := now + 10_000;
      CuBit.Clocks.Read (stamp, ok);
      if ok and then stamp.Quality = CuBit.Clocks.RTC_Only then
         nextText := [digit (stamp.Hour / 10), digit (stamp.Hour mod 10), ':',
                      digit (stamp.Minute / 10), digit (stamp.Minute mod 10)];
         statusDueMs := now + Unsigned_64'Min
           (10_000, Unsigned_64 (60 - stamp.Second) * 1000);
      elsif ok and then stamp.Quality = CuBit.Clocks.Invalid_Zone then
         nextText := " TZ? ";
      end if;
      if clockText /= nextText then
         clockText := nextText;
         scheduleRedrawRect (statusRect);
      end if;
      CuBit.Audio_Control.Read (audio, ok);
      if ok and then audio /= masterAudio then
         masterAudio := audio;
         scheduleRedrawRect (statusRect);
         if audioPopupOpen then scheduleRedrawRect (audioPopupRect); end if;
      end if;
   end refreshStatus;

   procedure setMasterAudio (level : CuBit.Audio_Control.Percent; muted : Boolean) is
      value : CuBit.Audio_Control.State;
      ok : Boolean;
   begin
      if not masterAudio.Available or else
        (level = masterAudio.Level and then muted = masterAudio.Muted)
      then return; end if;
      CuBit.Audio_Control.Set (level, muted, value, ok);
      if ok then
         masterAudio := value;
         scheduleRedrawRect (statusRect);
         if audioPopupOpen then scheduleRedrawRect (audioPopupRect); end if;
      end if;
   end setMasterAudio;

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
      titlePress : Boolean := False;
      clickKind : CuBit.Click_Sequences.Press_Kind :=
        CuBit.Click_Sequences.Single_Press;
      use type CuBit.Click_Sequences.Press_Kind;
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

      --  Popup input belongs to the desktop, including the release outside
      --  its bounds. Never deliver half a gesture to the focused application.
      if shellSurfaceVisible and then pointerSurfaceId = 0 and then
        dragMode = DRAG_NONE and then
        (audioPointerCapture or else audioPopupOpen or else
         pointInRect (cursorX, cursorY, speakerRect))
      then
         if leftDown and then not leftWasDown then
            audioPointerCapture := True;
            if pointInRect (cursorX, cursorY, speakerRect) then
               audioPopupOpen := not audioPopupOpen and then masterAudio.Available;
               if launchMenuOpen then
                  launchMenuOpen := False;
                  scheduleRedrawRect (inflateRect (launchMenuRect, 4));
               end if;
            elsif audioPopupOpen and then
              pointInRect (cursorX, cursorY, muteButtonRect)
            then
               setMasterAudio (masterAudio.Level, not masterAudio.Muted);
            elsif audioPopupOpen and then
              pointInRect (cursorX, cursorY, volumeTrackRect)
            then audioSliderDragging := True;
            elsif not pointInRect (cursorX, cursorY, audioPopupRect) then
               audioPopupOpen := False;
            end if;
            scheduleRedrawRect (audioPopupRect);
            scheduleRedrawRect (statusRect);
         end if;
         if audioSliderDragging and then leftDown then
            declare
               relative : constant Integer :=
                 Integer (cursorX) - Integer (volumeTrackRect.x) - 6;
               level : constant Natural :=
                 Natural (Integer'Max (0, Integer'Min (196, relative))) * 100 / 196;
            begin setMasterAudio (level, masterAudio.Muted); end;
         end if;
         if not leftDown then
            audioSliderDragging := False;
            audioPointerCapture := False;
         end if;
         lastButtons := buttons;
         cursorStyle := POINTER_DEFAULT;
         scheduleCursorPresent;
         return;
      end if;

      --  Moving away, using another button or scrolling breaks the sequence,
      --  even if the pointer later returns to the first click's position.
      CuBit.Click_Sequences.Motion (titleClicks, (cursorX, cursorY));
      if dz /= 0 or else (buttons and not Unsigned_64'(1)) /= 0 then
         CuBit.Click_Sequences.Reset (titleClicks);
      end if;

      if pointerSurfaceId /= 0 then
         idx := findSurface (pointerSurfaceId);
         if idx >= 0 and then surfaces (SurfaceIndex (idx)).appKind = APP_SETTINGS then
            declare Before : constant Rect := damage; begin
               Settings_Pointer (SurfaceIndex (idx), leftDown, False, not leftDown and leftWasDown, damage);
               sceneDamage := sceneDamage or damage /= Before;
            end;
         end if;
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
               launchMenuSelection := LAUNCH_WORKBENCH;
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
            titlePress :=
              cursorY < surfaces (SurfaceIndex (idx)).y + TITLE_HEIGHT and then
              dragMode in DRAG_NONE | DRAG_MOVE;
            if titlePress and then buttons = 1 then
               CuBit.Click_Sequences.Press
                 (titleClicks, CuBit.Click_Sequences.Target_ID (clickedId),
                  (cursorX, cursorY), syscall (SYSCALL_GETTIME), clickKind);
            else
               CuBit.Click_Sequences.Reset (titleClicks);
            end if;
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
                  if dragMode = HIT_MAXIMIZE or else
                    (titlePress and then
                     clickKind = CuBit.Click_Sequences.Double_Press)
                  then
                     toggleMaximizeSurface (SurfaceIndex (idx), damage);
                     if titlePress then
                        tracePointer
                          ("title-double", clickedId,
                           Boolean'Pos (surfaces (SurfaceIndex (idx)).maximized),
                           0);
                     end if;
                     dragMode := DRAG_NONE;
                  elsif titlePress and then surfaces (SurfaceIndex (idx)).maximized then
                     --  Maximized captions are still chrome, not client area.
                     --  Single presses do not start a move; a pair restores.
                     null;
                  elsif dragMode = DRAG_NONE then
                     pointerSurfaceId := clickedId;
                     if surfaces (SurfaceIndex (idx)).appKind = APP_SETTINGS then
                        Settings_Pointer (SurfaceIndex (idx), True, True, False, damage);
                     end if;
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
         if not titlePress then
            CuBit.Click_Sequences.Reset (titleClicks);
         end if;
      elsif not leftDown and then leftWasDown
      then
         if CuBit.Click_Sequences.Needs_Release (titleClicks) then
            CuBit.Click_Sequences.Release
              (titleClicks, (cursorX, cursorY), syscall (SYSCALL_GETTIME));
         end if;
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
            inputSources (i).authorityTag = report.sourceAuthorityTag and then
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
         source.authorityTag := report.sourceAuthorityTag;
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

               if extended and then code in 16#20# | 16#2E# | 16#30# then
                  --  Set-1 multimedia keys: mute, volume down, volume up.
                  if not release then
                     if code = 16#20# then
                        setMasterAudio (masterAudio.Level, not masterAudio.Muted);
                     else
                        setMasterAudio
                          (Natural (Integer'Max (0, Integer'Min (100,
                             Integer (masterAudio.Level) +
                             (if code = 16#30# then 5 else -5)))), masterAudio.Muted);
                     end if;
                  end if;
               elsif audioPopupOpen then
                  if not release and then code = KEY_ESCAPE then
                     audioPopupOpen := False;
                     scheduleRedrawRect (audioPopupRect);
                     scheduleRedrawRect (statusRect);
                  end if;
               elsif extended and then
                  (code = KEY_LEFT_SUPER or else code = KEY_RIGHT_SUPER)
               then
                  if not release and then shellSurfaceVisible then
                     launchMenuOpen := not launchMenuOpen;
                     launchMenuSelection := LAUNCH_WORKBENCH;
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
       w3    : Unsigned_64 := 0;
       Output : DSP.Output_Number := 0) return Message
   is
      msg : Message :=
        (tag      => (label => label, length => 4, flags => 0,
                     reserved => Unsigned_16 (Output)),
         authorityTag => 0,
         words    => (w0, w1, w2, w3));
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_DISPLAY, msg);
      msg.tag := tag;
      return msg;
   end callDisplay;

   procedure closeOutput (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      released : Message;
      revoked : Boolean;
   begin
      if P.Leased then
         released := callDisplay (OP_DISPLAY_RELEASE, Output => Output);
         if released.tag.length /= 1 or else released.words (0) /= 0 then
            debugPrint ("desktop: display release failed" & LF);
         end if;
      end if;
      if P.Granted then
         MG.Revoke (P.Grant, revoked);
         if not revoked then
            debugPrint ("desktop: display grant revoke failed" & LF);
         end if;
      end if;
      -- Old pages are never recycled here. A failed release can retain pins,
      -- but cannot expose this storage to a later session's writes.
      P := (others => <>);
   end closeOutput;

   procedure releaseDisplayBuffer is
   begin
      if not backBufferReady then return; end if;
      restoreCursorOverlay;
      for Output in Output_Index loop
         closeOutput (Output);
      end loop;
      backBufferReady := False;
      retiredThrough := frameSequence;
      drawingBackBuffer := False;
      cursorSaveValid := False;
      framePending := False;
      frameDamage := (others => 0);
      frameDueMs := 0;
      clearInputQueue;
   end releaseDisplayBuffer;

   function validDisplayInfo (Info : Message) return Boolean is
   begin
      if Info.tag.label /= OP_DISPLAY_GET_INFO or else Info.tag.length /= 4 or else
        Info.tag.flags /= 0 or else Info.tag.reserved /= 0 or else
        Info.words (0) not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Info.words (1) not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Info.words (2) > DP.Maximum_Buffer_Bytes or else Info.words (3) /= 32
      then
         return False;
      end if;
      return DP.Valid_Layout
        ((DP.Positive_Extent (Info.words (0)),
          DP.Positive_Extent (Info.words (1)), DP.Buffer_Pitch (Info.words (2))));
   end validDisplayInfo;

   procedure prepareOutput
     (Output : Output_Index; Info : Message; Ok : out Boolean)
   is
      P : Output_Presentation renames presentations (Output);
      Response : Message;
      Layout : DP.Buffer_Layout;
      Pages, Raw : Unsigned_64;
      Ignored : Unsigned_64;
      Granted : Boolean;
   begin
      Ok := False;
      if not validDisplayInfo (Info) then return; end if;
      -- Output zero retains the bounded shell-to-Desktop handoff retry.
      -- An optional output must not delay startup behind an existing owner.
      for Attempt in 1 .. (if Output = 0 then 100 else 1) loop
         Response := callDisplay (OP_DISPLAY_ACQUIRE, Output => Output);
         exit when Response.tag.label = OP_DISPLAY_ACQUIRE and then
           Response.tag.length = 1 and then Response.words (0) = 0;
         Ignored := syscall (SYSCALL_SLEEP, 2);
      end loop;
      if Response.tag.label /= OP_DISPLAY_ACQUIRE or else
        Response.tag.length /= 1 or else Response.words (0) /= 0
      then
         return;
      end if;
      P.Leased := True;
      Layout := (DP.Positive_Extent (Info.words (0)),
                 DP.Positive_Extent (Info.words (1)),
                 DP.Buffer_Pitch (Info.words (2)));
      Pages := (DP.Byte_Length (Layout) + 4095) / 4096;
      Raw := syscall (SYSCALL_SBRK, Pages * 4096 + 4096);
      if Raw = Unsigned_64'Last then
         closeOutput (Output);
         return;
      end if;
      P.Buffer := To_Address (Integer_Address (alignUpPage (Raw)));
      P.Pitch := Natural (Layout.Pitch);
      P.Geometry := (Width => DG.Physical_Extent (Layout.Width),
                     Height => DG.Physical_Extent (Layout.Height), others => <>);
      MG.Create_Via_Capability
        (CAP_SLOT_DISPLAY, P.Buffer, Natural (Pages), False, P.Grant, Granted);
      P.Granted := Granted;
      if not Granted then
         closeOutput (Output);
         return;
      end if;
      Response := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
        (DSP.Encode_Attachment ((P.Grant, Layout)), Output));
      Response.tag := capCall (CAP_SLOT_DISPLAY, Response);
      if Response.tag.label /= OP_DISPLAY_ATTACH_BUFFER or else
        Response.tag.length /= 1 or else Response.words (0) /= 0
      then
         closeOutput (Output);
         return;
      end if;
      Response := CuBit.Desktop_Messages.From_Wire
        (DSP.With_Output (DSP.Encode_Open_Session, Output));
      Response.tag := capCall (CAP_SLOT_DISPLAY, Response);
      if Response.tag.label /= DSP.Code (DSP.Open_Presentation_Session) or else
        Response.tag.length /= 4 or else Response.tag.flags /= 0 or else
        Response.tag.reserved /= 0 or else Response.words (0) /= 0 or else
        Response.words (1) = 0 or else Response.words (2) /= 0 or else
        Response.words (3) /= 0
      then
         closeOutput (Output);
         return;
      end if;
      P.Session := Response.words (1);
      P.Enabled := True;
      Ok := True;
   end prepareOutput;

   procedure setupDisplayBuffer (ok : out Boolean) is
      First_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
      Second_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO, Output => 1);
      Prepared : Boolean;
      Layout : DP.Buffer_Layout;
      Candidate : DL.Layout;
      Choice : DL.Primary_Selection;
      Raw, Pages, Drag_Raw : Unsigned_64;
      Total_Width : Natural;
   begin
      ok := False;
      prepareOutput (0, First_Info, Prepared);
      if not Prepared then
         debugPrint ("desktop: display setup failed" & LF);
         return;
      end if;
      Total_Width := Natural (First_Info.words (0));
      if validDisplayInfo (Second_Info) and then
        Second_Info.words (0) = First_Info.words (0) and then
        Second_Info.words (1) = First_Info.words (1) and then
        Total_Width <= Natural (DP.Positive_Extent'Last) / 2
      then
         -- Admit the combined private scene before acquiring optional storage.
         Layout := (DP.Positive_Extent (Total_Width * 2),
                    DP.Positive_Extent (First_Info.words (1)),
                    DP.Buffer_Pitch (Total_Width * 8));
         if DP.Valid_Layout (Layout) then
            prepareOutput (1, Second_Info, Prepared);
            if Prepared then
               presentations (1).Geometry.X := DG.Output_Origin (Total_Width);
               Total_Width := Total_Width * 2;
            end if;
         end if;
      end if;
      Candidate.Count := (if presentations (1).Enabled then 2 else 1);
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            Candidate.Items (Natural (Output) + 1) :=
              (DL.Named_Display_ID (Natural (Output) + 1),
               presentations (Output).Geometry);
         end if;
      end loop;
      if DL.Validate (Candidate).Status /= DL.Accepted then
         for Output in Output_Index loop closeOutput (Output); end loop;
         debugPrint ("desktop: disconnected layout rejected" & LF);
         return;
      end if;
      Choice := DL.Select_Primary (Candidate, preferredPrimary);
      if not Choice.Available then
         for Output in Output_Index loop closeOutput (Output); end loop;
         return;
      end if;
      primaryOutput := Output_Index (Choice.Index - 1);
      Layout := (DP.Positive_Extent (Total_Width),
                 DP.Positive_Extent (First_Info.words (1)),
                 DP.Buffer_Pitch (Total_Width * 4));
      if not DP.Valid_Layout (Layout) then
         for Output in Output_Index loop closeOutput (Output); end loop;
         return;
      end if;
      Pages := (DP.Byte_Length (Layout) + 4095) / 4096;
      Raw := syscall (SYSCALL_SBRK, Pages * 4096 + 4096);
      if Raw = Unsigned_64'Last then
         for Output in Output_Index loop closeOutput (Output); end loop;
         debugPrint ("desktop: scene allocation failed" & LF);
         return;
      end if;
      backBufferAddr := To_Address (Integer_Address (alignUpPage (Raw)));
      fbWidth := Total_Width;
      fbHeight := Natural (Layout.Height);
      fbPitch := Natural (Layout.Pitch);
      fbBpp := 32;
      dragBaseReady := False;
      dragBaseBufferAddr := System.Null_Address;
      Drag_Raw := syscall (SYSCALL_SBRK, Pages * 4096 + 4096);
      if Drag_Raw /= Unsigned_64'Last then
         dragBaseBufferAddr := To_Address (Integer_Address (alignUpPage (Drag_Raw)));
      else
         debugPrint ("desktop: retained drag layer unavailable" & LF);
      end if;
      backBufferReady := True;
      debugPrint ("desktop: wallpaper uses retained scene layers" & LF);
      debugPrint ("desktop: active outputs=" & Candidate.Count'Image &
        " primary=" & primaryOutput'Image & LF);
      ok := True;
   end setupDisplayBuffer;

   procedure queryDisplayInfo (ok : out Boolean) is
      Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
   begin
      ok := validDisplayInfo (Info);
      if ok then
         fbWidth := Natural (Info.words (0));
         fbHeight := Natural (Info.words (1));
         fbPitch := Natural (Info.words (2));
         fbBpp := 32;
      else
         debugPrint ("desktop: display info unsupported" & LF);
      end if;
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
   Read_Appearance;

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

   --  Validate the initial output before acquiring presentation buffers and
   --  starting the desktop session. Applications launch through Apps.
   queryDisplayInfo (displayInfoOk);
   if not displayInfoOk or else fbWidth not in 1 .. Natural (DP.Pixel_Extent'Last) or else
     fbHeight not in 1 .. Natural (DP.Pixel_Extent'Last)
   then
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
         activity : Activity_Result;
         eventsRemaining : Natural range 0 .. EVENT_BUDGET := EVENT_BUDGET;
         procedure Drain_Events is
         begin
            while eventsRemaining > 0 and then running loop
               eventFound := Poll_Event (eventMsg);
               exit when not eventFound;
               eventsRemaining := eventsRemaining - 1;
               if Presentation_Test_Policy.Enabled and then
                 presentations (primaryOutput).Phase = In_Flight and then
                 inputWhileHeldFrame /= presentations (primaryOutput).Token
               then
                  inputWhileHeldFrame := presentations (primaryOutput).Token;
                  debugPrint ("desktop: input during frame" & inputWhileHeldFrame'Image & LF);
               end if;
               handleEvent (eventMsg, running);
            end loop;
         end Drain_Events;
      begin
         collectPresentations;
         Drain_Events;

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

         --  A reply can hand execution to an app that publishes input before
         --  we resume. Dispatch those arrivals before painting, not one full
         --  repaint later. Both drains share a finite budget: input floods
         --  must still permit requests and rendering to make progress.
         Drain_Events;
         refreshStatus;
         flushFrame;
         flushCursorPresent;
         pumpPresentation;
         maybePrintStats;
         expireInputWaiters;

         if not eventFound and then not found then
            if not framePending and then not cursorPresentPending and then
              nextInputDeadline = 0 and then statusDueMs = 0
            then
               --  Input, requests and frame completions all wake the same
               --  non-consuming wait; typed dispatch remains above.
               activity := Wait_For_Activity_Until (Unsigned_64'Last);
               if activity = Unavailable then running := False; end if;
            else
               declare
                  now : constant Unsigned_64 := nowMs;
                  nextDueMs : Unsigned_64 := nextInputDeadline;
                  mayWait : Boolean := now /= Unsigned_64'Last;
               begin
                  if statusDueMs /= 0 and then
                    (nextDueMs = 0 or else statusDueMs < nextDueMs)
                  then nextDueMs := statusDueMs; end if;
                  if framePending and then now /= Unsigned_64'Last and then
                     frameDueMs /= 0 and then now < frameDueMs
                  then
                     if nextDueMs = 0 or else frameDueMs < nextDueMs then
                        nextDueMs := frameDueMs;
                     end if;
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
                     activity := Wait_For_Activity_Until (nextDueMs);
                     if activity = Unavailable then running := False; end if;
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

   releaseDisplayBuffer;

   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
