with Compositor_Transfer_Delta;
with Desktop_Renderer_Startup;
with Desktop_Input_Overlay;
with Compositor_Frame_Replacement;
with Desktop_Input_Diagnostic;
with Desktop_Breadcrumbs;
with Compositor_Backend_Selection;
with Compositor_Focus;
with Compositor_Source_Damage;
with Compositor_Source_Content;
with Vulkan_Submission;
with Compositor_Stall_Watch;
with Compositor_Surface_State;
with Compositor_Source_Loans;
with Compositor_Density;
with Compositor_Density_Selection;
with CuBit.Desktop_Protocol.Publication;
with Compositor_Gradient;
with Compositor_Workspace;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop compositor/session service prototype
------------------------------------------------------------------------------
with Desktop_Timing_Policy;
with Desktop_Metrics;
with Compositor_Trace_Wire;
with Desktop_Logs;
with Compositor_Metric_Batch_Policy;
with Desktop_Storage_Policy;
with Compositor_Text;
with Compositor_Sampling;
with Compositor_Row_Copy;
with Compositor_Elapsed;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;
with Compositor_Frame_Trace;
with Compositor_Source_Trace;
with Compositor_Render_Trace;
with Compositor_Input_Trace;
with Compositor_Input_Queue;
with Compositor_Input_Batches;
with Compositor_Input_Batch_Wire;
with Compositor_Input_Protocol;
with Compositor_Input_Acknowledgment;
with Compositor_Input_Delivery_Pool;
with Desktop_Input_Transfer;
with Compositor_Close_Request;
with Compositor_Dispatch_Budget;
with CuBit.Monotonic;
with CuBit.Process_List;
with CuBit.Timing_Histograms;
with Compositor_Requests;
with Compositor_Presentation;
with Compositor_Pool;
with Compositor_Repaint;
with Compositor_Output_Retirement;
with Compositor_Lease_Request;
with Compositor_Layout_Restore;
with Compositor_Storage;
with CuBit.Display_Pool_Protocol;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Display_Protocol;
with CuBit.Display_Layouts;
with CuBit.Display_Arrangement;
with CuBit.Desktop_Messages;
with CuBit.Input;
with CuBit.Audio_Control;
with CuBit.Clocks;
with CuBit.Click_Sequences;
with CuBit.Theme;
with Desktop_Cursors;
with Desktop_Pointer_Plane;
with CuBit.Display_Plane_Protocol;
with CuBit.Display_Planes;
with Compositor_Cursor;
with Desktop_Icons;
with Desktop_Icon_Pixels;
with CuBit.Fonts;
with Desktop_Window_Icons;
with Desktop_Wallpaper;
with Desktop_Wallpaper_Assets;
with Desktop_Wallpaper_Layers;
with Desktop_Settings;
with Desktop_Launch;
with Desktop_Launch_Refresh;
with Desktop_Status_Refresh;
with Desktop_Launch_Menus;
with Client_Popup_Layout;
with CCL.Interfaces.Desktop_Launch;
with CuBit.Appearance;
with CuBit.Config;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with CuBit.UI.Theme_Data;
with CuBit.Desktop_Protocol;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with Presentation_Test_Policy;
with Desktop_Composition;
with Compositor_Damage;
with Compositor_Transition;
with Desktop_Compositor;

procedure main is
   procedure debugPrint (Text : String) renames Desktop_Logs.Write;
   package DM renames Desktop_Metrics;
   package Close_Policy renames Compositor_Close_Request;
   package DSP renames CuBit.Display_Protocol;
   package DL renames CuBit.Display_Layouts;
   package DG renames DL.G;
   use type DSP.Output_Number, DL.Admission_Status;
   use type DG.Pixel_Edge;
   use type DG.Logical_Coordinate;
   use type Desktop_Settings.Page;
   package MG renames CuBit.Memory_Grants;
   package GM renames CuBit.Graphics_Metrics;
   stagingCopies : GM.Counter;
   stagingReporter : CuBit.Graphics_Metrics_IO.Reporter;
   use ASCII;
   package DP renames CuBit.Desktop_Protocol;
   package Publication renames CuBit.Desktop_Protocol.Publication;
   package Surface_Policy is new Compositor_Surface_State;
   use type Publication.Configuration;
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

   -- Current logical drawing canvas: an owned output slot for direct rendering,
   -- otherwise the compatibility scene. Output-local storage lives below.
   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   backBufferAddr : System.Address := System.Null_Address;
   privateSceneAddr : System.Address := System.Null_Address;
   directOutput : Boolean := False;
   repairingTarget : Boolean := False;
   sceneCapacityBytes : Natural := 0;
   package CR renames Compositor_Requests;
   package CP renames Compositor_Presentation;
   package BP renames Compositor_Pool;
   package RP renames Compositor_Repaint;
   package PS is new Compositor_Storage;
   use type PS.Ticket, PS.Phase;
   -- Two outputs with three targets each, one scene reserve and one drag layer.
   pixelStorage : PS.State := PS.Open
     (Positive'Min (Desktop_Storage_Policy.Maximum_Bytes,
       8 * Positive (DP.Maximum_Buffer_Bytes)));
   type Pixel_Allocation is record
      Ticket : PS.Ticket := PS.No_Ticket;
      Address : Unsigned_64 := 0;
   end record;
   pixelAllocations : array (PS.Slot) of Pixel_Allocation;
   sceneAllocation, dragAllocation : PS.Ticket := PS.No_Ticket;
   package PW renames CuBit.Display_Pool_Protocol;
   use type BP.Ticket;
   use type CP.Phase;
   requestSequence : Unsigned_64 := 0;
   metricsStoppedAnnounced : Boolean := False;
   asyncAnnounced, releaseAnnounced : Boolean := False;
   sparseCopyAnnounced : Boolean := False;
   retiredThrough : Unsigned_64 := 0;
   dragBaseBufferAddr : System.Address := System.Null_Address;
   dragBaseReady : Boolean := False;
   dragCacheAnnounced : Boolean := False;
   compositionExcludedSurface : Unsigned_64 := 0;
   spawnGrantAddr : System.Address := System.Null_Address;
   spawnGrant     : CuBit.Memory_Grants.Grant_Reference;
   spawnGrantReady : Boolean := False;
   launchRequest : CR.State;
   launchName : String (1 .. 255) := (others => ' ');
   launchNameLength : Natural range 0 .. 255 := 0;
   launchInputAnnounced : Boolean := False;
   doomPid : Process_ID := No_Process;
   psBufAddr : System.Address := System.Null_Address;

   -- The current canvas is always privately writable. Completed output slots
   -- remain immutable until Display confirms the matching frame acquisition ended.
   -- One software-rendering line per process, carrying its actual cause.
   mesaAnnounced, softwareAnnounced : Boolean := False;
   mesaTextAnnounced, mesaTextFallbackAnnounced, retainedTextAnnounced : Boolean := False;
   textSceneRetry : Boolean := False;
   physicalClientAnnounced : Boolean := False;
   procedure exitCompositor (Status : Integer)
     with Import, Convention => C, External_Name => "_exit", No_Return;
   Bootstrap_CPU : Boolean := False;
   Diagnostic_Loops, Diagnostic_Keys, Diagnostic_Mouse,
     Diagnostic_Requests, Diagnostic_Frames, Diagnostic_Buttons : Unsigned_64 := 0;
   package DI renames Desktop_Input_Diagnostic;
   Input_Diagnostic : DI.State;
   Diagnostic_Last_Us : Unsigned_64 := Unsigned_64'Last;
   Diagnostic_Pipeline_Valid : Boolean := False;
   Diagnostic_Pipeline_Stage, Diagnostic_Pipeline_Index : Unsigned_32 := 0;
   Diagnostic_Pipeline_Result : Integer_32 := 0;
   backBufferReady : Boolean := False;
   outputDrainRequested, outputReopenPending, shutdownRequested : Boolean := False;
   drawingBackBuffer : Boolean := False;

   type Rect is record
      x : Natural := 0;
      y : Natural := 0;
      w : Natural := 0;
      h : Natural := 0;
   end record;
   function damageRectangle (B : Compositor_Damage.Box) return Rect is
     (if Compositor_Damage.Valid (B) then
        (B.Left, B.Top, B.Right - B.Left, B.Bottom - B.Top)
      else (others => 0));

   procedure addOutputDamage (S : in out Compositor_Damage.State; R : Rect) is
   begin
      if R.w > 0 and then R.h > 0 then
         Compositor_Damage.Add (S, (R.x, R.y, R.x + R.w, R.y + R.h));
      end if;
   end addOutputDamage;

   --  Initial native arrangement: adjacent, mixed-size, unit-scale outputs.
   --  Each output registers three root-owned targets and tracks one held frame.
   --  A pool writer stays distinct from the immutable Display-held slot.
   --  The Mesa selection renders scene state directly at each output density;
   --  the legacy backend retains its logical-canvas copy path. Native-density
   --  client allocations, Settings toolkit drawing and rotation remain work.
   subtype Output_Index is DSP.Output_Number range 0 .. 1;
   type Target_Storage is record
      Allocation : PS.Ticket := PS.No_Ticket;
      Address : System.Address := System.Null_Address;
      Grant : MG.Grant_Reference;
      Granted : Boolean := False;
   end record;
   type Target_Array is array (BP.Live_Slot) of Target_Storage;
   type Output_Presentation is record
      Trace_Output : Output_Index := 0;
      Enabled, Leased : Boolean := False;
      Geometry : DG.Output := (Width => 1, Height => 1, others => <>);
      Pitch : Natural := 0;
      Buffer : System.Address := System.Null_Address;
      Targets : Target_Array;
      Pool : BP.State;
      Repaint : RP.State;
      Started : Unsigned_64 := 0;
      Started_Us : Unsigned_64 := Compositor_Elapsed.Unavailable;
      Transfer : CP.State;
      Damage, Frame_Damage : Compositor_Damage.State;
   end record;
   presentations : array (Output_Index) of Output_Presentation;
   -- Bounded process-lifetime diagnostics, independent of output reconfiguration.
   type GPU_Progress is record
      Completed, Published : Boolean := False;
      Submitted : BP.Ticket := BP.None;
   end record;
   gpuProgress : array (Output_Index) of GPU_Progress;
   Renderer_Recovery : Boolean := False;
   Renderer_Recovery_Key : Compositor_Backend_Selection.Recovery_Key;
   -- GPU stall judgement by time and progress, never by frame counts: the
   -- renderer is stalled only when no GPU frame has completed and no upload
   -- has been accepted for this long while repaints keep being retried.
   -- Cold uploads (serialized, one writer) are progress, not a stall.
   GPU_Stall_Deadline_Ms : constant := 2_000;
   gpuStallWatch : Compositor_Stall_Watch.State;
   gpuStallCause : Desktop_Compositor.Retry_Cause := Desktop_Compositor.No_Retry;
   -- Evidence for the stats line: retried captures by cause this period and
   -- the backing allocation/free counters at the last report.
   type Retry_Counts is array (Desktop_Compositor.Retry_Cause) of Unsigned_64;
   statsRetries : Retry_Counts := (others => 0);
   type Backing_Counts is array (Vulkan_Submission.Source_Class, Boolean) of Unsigned_64;
   reportedBackings : Backing_Counts := (others => (others => 0));
   reportedPlaceholders : Natural := 0;
   package Output_Retirement is new Compositor_Output_Retirement
     (Positive (BP.Live_Slot'Last));
   use type Output_Retirement.Phase, Output_Retirement.Grant_Phase;
   outputRetirement : array (Output_Index) of Output_Retirement.State;
   package LR renames Compositor_Lease_Request;
   use type LR.Phase;
   outputLeaseRequests : array (Output_Index) of LR.State;
   primaryOutput : Output_Index := 0;
   -- Scene layout stays logical. Only this synchronous drawing scope selects
   -- a physical output; it never changes input/window coordinates.
   nativeOutputPass : Boolean := False;
   activeOutput : Output_Index := 0;
   outputDamage : DG.Physical_Rectangle := (others => 0);
   function nativeScene return Boolean is (Desktop_Compositor.Selected);
   procedure renderOutput (Output : Output_Index; Area : Rect);
   procedure Paint_Diagnostic (Output : Output_Index);
   function physicalClip (Area : Rect) return DG.Physical_Rectangle is
     (Compositor_Text.Clip
       (presentations (activeOutput).Geometry,
        (DG.Logical_Coordinate (Area.x), DG.Logical_Coordinate (Area.y),
         DG.Logical_Coordinate (Area.x + Area.w),
         DG.Logical_Coordinate (Area.y + Area.h)), outputDamage));

   desktopLayout : DL.Layout;
   --  Desktop preference, not a property of the display/GPU service. Config
   --  publication and named-monitor matching will replace this initial value.
   preferredPrimary : DL.Named_Display_ID := 1;

   function logicalBounds (Geometry : DG.Output) return Rect is
      B : constant DG.Logical_Rectangle := DG.Bounds (Geometry);
   begin
      return (Natural (B.Left), Natural (B.Top),
              Natural (B.Right - B.Left), Natural (B.Bottom - B.Top));
   end logicalBounds;

   function primaryBounds return Rect is
     (if presentations (primaryOutput).Enabled then
        logicalBounds (presentations (primaryOutput).Geometry)
      else (0, 0, fbWidth, fbHeight));

   --  Damage clipping for compositor redraws. A full scene redraw with a clip
   --  rectangle lets existing drawing code repaint correct background/window
   --  ordering while touching only the region that changed.
   clipEnabled : Boolean := False;
   clipRect    : Rect;
   framePending : Boolean := False;
   frameDamage  : Rect;
   --  Coalesce pointer motion during each bounded event-loop pass. Painting
   --  happens after input dispatch, with no timer delay and no per-report
   --  display IPC. Output retirement still governs transfer-buffer writes.
   cursorPresentPending : Boolean := False;

   --  Priority for applications launched from Apps (see trySpawnApplication).
   APP_PRIORITY : constant Unsigned_64 := 3;

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

   MAX_SURFACES : constant Natural := 8;
   package Source_Loans is new Compositor_Source_Loans
     (MAX_SURFACES * (Surface_Policy.Slot'Last + 1));
   use type Source_Loans.Phase;
   use type Source_Loans.Ticket;
   type Source_Loan is record
      Grant : MG.Grant_Reference;
      Address : System.Address := System.Null_Address;
   end record;
   type Source_Loan_Table is array (Source_Loans.Slot) of Source_Loan;
   sourceLoanPolicy : Source_Loans.State;
   sourceLoans : Source_Loan_Table;
   use type Surface_Policy.Phase;
   use type MG.Grant_Reference;
   type Publication_Buffer is record
      Acquired : Boolean := False;
      Loan : Source_Loans.Ticket := Source_Loans.No_Ticket;
      Grant : MG.Grant_Reference;
      Address : System.Address := System.Null_Address;
      Configuration : Publication.Configuration;
      Retired : Publication.Receipt;
   end record;
   type Publication_Buffers is array (Surface_Policy.Slot) of Publication_Buffer;

   type Surface is record
      used      : Boolean := False;
      owner     : Process_ID := No_Process;
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
      -- Configuration is independent of the currently visible source. Merely
      -- querying it never attaches or withdraws a buffer.
      publicationMode : Boolean := False;
      publicationInputAfter : Unsigned_64 := 0;
      publicationBuffers : Publication_Buffers;
      publicationPolicy : Surface_Policy.State;
      publicationConfiguration : Publication.Configuration_Result;
      bufferAttached : Boolean := False;
      bufferLoan : Source_Loans.Ticket := Source_Loans.No_Ticket;
      bufferGrant    : MG.Grant_Reference;
      bufferAddr     : System.Address := System.Null_Address;
      bufferLogicalW : Natural := 0;
      bufferLogicalH : Natural := 0;
      bufferW        : Natural := 0;
      bufferH        : Natural := 0;
      bufferPitch    : Natural := 0;
      bufferFormat   : Unsigned_64 := 0;
      -- Version of the pixels behind bufferAddr; the surface id is the
      -- renderer's persistent source key. Bumped with every content change
      -- together with Note_Source_Change.
      contentVersion : Compositor_Source_Content.Content_Version :=
        Compositor_Source_Content.No_Version;
      pointerCursor  : Pointer_Cursor_Style := POINTER_DEFAULT;
   end record;

   package Focus_Policy is new Compositor_Focus (MAX_SURFACES);
   subtype SurfaceIndex is Natural range 0 .. MAX_SURFACES - 1;
   type SurfaceTable is array (SurfaceIndex) of Surface;

   surfaces : SurfaceTable;

   MAX_STREAM_ANNOUNCEMENTS : constant Natural := 16;
   subtype StreamAnnouncementIndex is Natural
      range 0 .. MAX_STREAM_ANNOUNCEMENTS - 1;
   type StreamAnnouncement is record
      used : Boolean := False;
      pid  : Process_ID := No_Process;
      mask : Unsigned_64 := 0;
   end record;
   type StreamAnnouncementTable is array (StreamAnnouncementIndex) of
      StreamAnnouncement;

   streamAnnouncements : StreamAnnouncementTable;

   nextSurfaceId : Unsigned_64 := 1;
   focusSurface  : Unsigned_64 := 0;
   internalShellSurface : Unsigned_64 := 0;
   inputOwned : Boolean := False;

   package IQ renames Compositor_Input_Queue;
   subtype PendingInput is IQ.Event;
   subtype PendingInputQueue is IQ.Queue;

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
      owner       : Process_ID := No_Process;
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
      -- Any possibly exposed event is immutable until acknowledged or resynced.
      exposedThrough : Unsigned_64 := 0;
      pendingClose : Unsigned_64 := 0;
      events   : PendingInputQueue := [others => (others => <>)];
      snapshot : InputSnapshot;
      waiter   : InputWaiter;
   end record;
   type SurfaceInputChannelTable is
     array (SurfaceIndex) of SurfaceInputChannel;
   inputChannels : SurfaceInputChannelTable;
   -- These states survive channel clearing and surface destruction. Each
   -- maintenance turn retries one exact retained acquisition at most once.
   package Input_Transfers is new Compositor_Input_Delivery_Pool
     (MAX_SURFACES, Desktop_Input_Transfer.Engine);
   inputTransfers : Input_Transfers.State;
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
   inputSources : InputSourceTable := [others => (others => <>)];

   pointerSurfaceId : Unsigned_64 := 0;
   launchMenuOpen : Boolean := False;
   masterAudio : CuBit.Audio_Control.State;
   audioPopupOpen : Boolean := False;
   audioPointerCapture : Boolean := False;
   audioSliderDragging : Boolean := False;
   clockText : String (1 .. 5) := "--:--";
   statusDueMs : Unsigned_64 := 0;
   --  Status area clock and volume refresh period (and the latest next read).
   Status_Interval_Ms : constant := 10_000;
   desktopExtendedPrefix : Boolean := False;
   desktopShiftDown : Boolean := False;
   desktopCtrlDown  : Boolean := False;
   desktopAltDown   : Boolean := False;
   desktopCapsLockOn : Boolean := False;

   type ScanTable is array (Unsigned_8 range 0 .. 16#39#) of Unsigned_8;
   scancodeNormal : constant ScanTable :=
     [16#02# => Character'Pos ('1'),
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
      others => 0];

   scancodeShifted : constant ScanTable :=
     [16#02# => Character'Pos ('!'),
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
      others => 0];

   --  The Apps menu: entries from Config (desktop.launch.*; Desktop_Launch),
   --  then Power, which is always last.
   launchMenu : Desktop_Launch.Menu := Desktop_Launch.Defaults;
   LAUNCH_NONE  : constant := 0;
   LAUNCH_POWER : constant := Desktop_Launch.Maximum_Entries + 1;
   subtype Launch_Action is Natural range LAUNCH_NONE .. LAUNCH_POWER;

   type Launch_PID_Array is array (1 .. Desktop_Launch.Maximum_Entries) of Process_ID;
   launchPids : Launch_PID_Array :=
     [others => No_Process];

   --  The menu's two levels (UI-013): category rows, then Power; a
   --  category's submenu lists its entries (Desktop_Launch_Menus).
   launchMenuState : Desktop_Launch_Menus.Menu_State;
   package LM renames Desktop_Launch_Menus;
   function launchRows return Natural is (LM.Rows (launchMenu));
   function launchPowerRow return Natural is (LM.Power_Row (launchMenu));

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
   titleClockFromSource : Boolean := False;
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
   --  Menu layout: category rows every 34 pixels from 42, a separator,
   --  then Power; a submenu beside the open category, rows every 34.
   LAUNCH_FIRST_Y : constant Natural := 42;
   LAUNCH_STEP    : constant Natural := 34;
   SUBMENU_PAD    : constant Natural := 6;

   function MENU_H return Natural is
     (LAUNCH_FIRST_Y + launchRows * LAUNCH_STEP - 2 + 8 + LAUNCH_STEP);
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
   cursorSave      : CursorSaveBuffer := [others => 0];
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

   --  The Apps menu entries from Config: every desktop.launch.* setting, in
   --  key order. Invalid entries are skipped and reported; without a valid
   --  one the built-in list stays.
   procedure Load_Launch_Menu is
      use type CuBit.Config.ConfigStatus;
      Keys   : System.Address;
      Count  : Natural;
      Status : CuBit.Config.ConfigStatus;
      Loaded : Desktop_Launch.Menu;
      MAX_KEY : constant := 64;
      type Key_Text is record
         Text   : String (1 .. MAX_KEY) := [others => ' '];
         Length : Natural range 0 .. MAX_KEY := 0;
      end record;
      Names : array (1 .. Desktop_Launch.Maximum_Entries) of Key_Text;
      Named : Natural := 0;
   begin
      CuBit.Config.list ("desktop.launch.", Keys, Count, Status);
      if Status /= CuBit.Config.OK or else Keys = System.Null_Address or else Count = 0 then
         return;
      end if;
      --  Copy the NUL-separated names before the next Config call reuses
      --  the buffer; keep at most Maximum_Entries.
      declare
         Buffer : String (1 .. 4096) with Import, Address => Keys;
         Pos    : Positive := 1;
      begin
         for I in 1 .. Count loop
            exit when Pos > Buffer'Last;
            declare
               First : constant Positive := Pos;
            begin
               while Pos <= Buffer'Last and then Buffer (Pos) /= ASCII.NUL loop
                  Pos := Pos + 1;
               end loop;
               if Named < Names'Last and then Pos - First in 1 .. MAX_KEY then
                  Named := Named + 1;
                  Names (Named).Text (1 .. Pos - First) := Buffer (First .. Pos - 1);
                  Names (Named).Length := Pos - First;
               end if;
               Pos := Pos + 1;
            end;
         end loop;
      end;
      --  Key order (insertion sort; at most Maximum_Entries names).
      for I in 2 .. Named loop
         declare
            Item : constant Key_Text := Names (I);
            J    : Natural := I - 1;
         begin
            while J >= 1 and then
              Names (J).Text (1 .. Names (J).Length) > Item.Text (1 .. Item.Length)
            loop
               Names (J + 1) := Names (J);
               J := J - 1;
            end loop;
            Names (J + 1) := Item;
         end;
      end loop;
      for I in 1 .. Named loop
         declare
            Value  : System.Address;
            Length : Natural;
            Item   : Desktop_Launch.Entry_Info;
            OK     : Boolean := False;
         begin
            CuBit.Config.get (Names (I).Text (1 .. Names (I).Length), Value, Length, Status);
            if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
              Length in 1 .. 512
            then
               declare
                  Source : String (1 .. Length) with Import, Address => Value;
               begin
                  Desktop_Launch.Decode (Source, Item, OK);
               end;
            end if;
            if OK then
               Desktop_Launch.Append (Loaded, Item);
            else
               debugPrint ("desktop: invalid launch entry " &
                           Names (I).Text (1 .. Names (I).Length) & LF);
            end if;
         end;
      end loop;
      if Loaded.Count > 0 then
         launchMenu := Loaded;
      end if;
   end Load_Launch_Menu;
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
   statsRepairPixels : Unsigned_64 := 0;
   statsScenePixels  : Unsigned_64 := 0;
   statsSourceGaps   : Unsigned_64 := 0;
   statsSourceRejects : Unsigned_64 := 0;
   -- Timing builds only (microseconds): the longest output pass that
   -- published a frame, and the longest wait from the first unpresented
   -- pointer motion until the next accepted frame submission.
   statsMaxFrameUs   : Unsigned_64 := 0;
   statsMaxMotionUs  : Unsigned_64 := 0;
   --  Pointer source age at intake: driver acquisition (GETTIME ms, carried
   --  in the report snapshot) to desktop dispatch. Includes driver retention
   --  and kernel queueing; timing builds only.
   statsSourceAgeMaxMs : Unsigned_64 := 0;
   statsSourceAgeSumMs : Unsigned_64 := 0;
   statsSourceAgeCount : Unsigned_64 := 0;
   pendingMotionUs   : Unsigned_64 := Compositor_Elapsed.Unavailable;
   --  Intake of the oldest input not yet shown by a submitted frame
   --  (Input_To_Present metric); Unavailable when none is waiting.
   pendingInputUs    : Unsigned_64 := Compositor_Elapsed.Unavailable;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;
   lastEventDrops    : Unsigned_64 := 0;
   lastInputQueueOverflows : Unsigned_64 := 0;
   inputTraceBudget  : Natural := 64;

   function Decimal (Value : Unsigned_64) return String is
      Text : constant String := Value'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Decimal;

   package FT renames Compositor_Frame_Trace;
   frameTrace : FT.State;
   package ST renames Compositor_Source_Trace;
   sourceTrace : ST.State;
   package IT renames Compositor_Input_Trace;
   inputDequeueTrace : IT.State;
   package RT renames Compositor_Render_Trace;
   renderTrace : RT.State;
   package TW renames Compositor_Trace_Wire;
   Trace_Metrics_Enabled : Boolean := False;
   function Trace_Enabled return Boolean is
     (Desktop_Timing_Policy.Enabled or else Trace_Metrics_Enabled);

   procedure Read_Trace_Configuration is
      Value : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      Trace_Metrics_Enabled := False;
      if not DM.Enabled then return; end if;
      CuBit.Config.get ("desktop.metrics.trace", Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then Length = 4 then
         declare Text : String (1 .. 4) with Import, Address => Value;
         begin Trace_Metrics_Enabled := Text = "true"; end;
      end if;
      if Trace_Metrics_Enabled then
         debugPrint ("desktop: detailed trace metrics enabled" & LF);
      end if;
   end Read_Trace_Configuration;

   procedure recordFrameTrace (Value : FT.Record_Value) is
   begin
      if Desktop_Timing_Policy.Enabled then FT.Add (frameTrace, Value); end if;
      if Trace_Metrics_Enabled then DM.Record_Trace ((TW.Frame_Event, 0, Value)); end if;
   end recordFrameTrace;
   procedure recordSourceTrace (Value : ST.Record_Value) is
   begin
      if Desktop_Timing_Policy.Enabled then ST.Add (sourceTrace, Value); end if;
      if Trace_Metrics_Enabled then DM.Record_Trace ((TW.Source_Event, 0, Value)); end if;
   end recordSourceTrace;
   procedure recordInputTrace (Value : IT.Record_Value) is
   begin
      if Desktop_Timing_Policy.Enabled then IT.Add (inputDequeueTrace, Value); end if;
      if Trace_Metrics_Enabled then DM.Record_Trace ((TW.Input_Event, 0, Value)); end if;
   end recordInputTrace;
   procedure recordRenderTrace (Value : RT.Record_Value) is
   begin
      if Desktop_Timing_Policy.Enabled then RT.Add (renderTrace, Value); end if;
      if Trace_Metrics_Enabled then DM.Record_Trace ((TW.Render_Event, 0, Value)); end if;
   end recordRenderTrace;
   procedure recordUnsupportedTrace is
   begin
      if Desktop_Timing_Policy.Enabled then RT.Note_Unsupported (renderTrace); end if;
      if Trace_Metrics_Enabled then DM.Record_Unsupported_Trace; end if;
   end recordUnsupportedTrace;

   package TH renames CuBit.Timing_Histograms;
   package DB renames Compositor_Dispatch_Budget;
   type Timing_Stage is (Input_Dispatch, Request_Dispatch, Scene_Draw,
                        Submit_Call, Submit_To_Completion);
   type Stage_Histograms is array (Timing_Stage) of TH.Histogram;
   type Stage_Counts is array (Timing_Stage) of Unsigned_64;
   Timing : Stage_Histograms := (others => TH.Empty);
   Timing_Invalid, Timing_Dropped : Stage_Counts := (others => 0);
   function dispatchNow return Unsigned_64 is
      R : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Diagnostic_Last_Us := (if R.Available then R.Microseconds else DB.Unavailable);
      return Diagnostic_Last_Us;
   end dispatchNow;

   function timingNow return Unsigned_64 is
   begin
      if Desktop_Timing_Policy.Enabled or else (DM.Enabled and then not DM.Disabled) then
         declare R : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
         begin
            if R.Available then return R.Microseconds; end if;
         end;
      end if;
      return Compositor_Elapsed.Unavailable;
   end timingNow;
   Last_Transfer_Work : Desktop_Compositor.Transfer_Counters;
   Transfer_Metrics_Invalid : Boolean := False;
   procedure recordTransferWork is
      Current : Desktop_Compositor.Transfer_Counters;
      Work : Compositor_Transfer_Delta.Sample;
      use type Compositor_Transfer_Delta.Action;
      Stamp : Unsigned_64;
   begin
      if not DM.Enabled or else DM.Disabled or else Transfer_Metrics_Invalid then return; end if;
      Current := Desktop_Compositor.Readback_Work;
      Work := Compositor_Transfer_Delta.Prepare
        (Last_Transfer_Work.GPU_Submitted, Last_Transfer_Work.CPU_Copied,
         Current.GPU_Submitted, Current.CPU_Copied, Current.Saturated);
      if Work.Kind = Compositor_Transfer_Delta.Invalid_Counters then
         Transfer_Metrics_Invalid := True;
         debugPrint ("desktop: transfer metrics unavailable; counter saturated or reset" & LF);
         return;
      elsif Work.Kind = Compositor_Transfer_Delta.No_Work then return;
      end if;
      Stamp := timingNow;
      if Stamp = Compositor_Elapsed.Unavailable then return; end if;
      if Work.GPU > 0 then
         DM.Record_Work (Compositor_Work_Metrics.GPU_Readback_Bytes,
           Work.GPU, Stamp);
      end if;
      if Work.CPU > 0 then
         DM.Record_Work (Compositor_Work_Metrics.CPU_Copy_Bytes,
           Work.CPU, Stamp);
      end if;
      -- Publisher refusal is represented by its existing bounded loss counters.
      -- Never retry a sample that may already have been accepted.
      Last_Transfer_Work := Current;
   end recordTransferWork;
   function presentationNow return Unsigned_64 is
     (if DM.Enabled then dispatchNow else timingNow);
   procedure noteTiming (Stage : Timing_Stage; First : Unsigned_64) is
      Last : Unsigned_64;
   begin
      if not Desktop_Timing_Policy.Enabled and then
        (Stage = Submit_To_Completion or else not DM.Enabled or else DM.Disabled)
      then return; end if;
      Last := timingNow;
      if DM.Enabled and then not DM.Disabled then
         case Stage is
            when Input_Dispatch => DM.Record_Stage (Compositor_Stage_Metrics.Input_Dispatch, First, Last);
            when Request_Dispatch => DM.Record_Stage (Compositor_Stage_Metrics.Request_Dispatch, First, Last);
            when Scene_Draw => DM.Record_Stage (Compositor_Stage_Metrics.Scene_Draw, First, Last);
            when Submit_Call => DM.Record_Stage (Compositor_Stage_Metrics.Submit_Call, First, Last);
            when Submit_To_Completion => null; -- Per-output release spans already published.
         end case;
      end if;
      if Desktop_Timing_Policy.Enabled then
         declare V : constant Compositor_Elapsed.Sample :=
           Compositor_Elapsed.Measure (First, Last);
         begin
            if not V.Valid then
               if Timing_Invalid (Stage) /= Unsigned_64'Last then
                  Timing_Invalid (Stage) := Timing_Invalid (Stage) + 1;
               end if;
            elsif TH.Count (Timing (Stage)) = TH.Maximum_Samples then
               if Timing_Dropped (Stage) /= Unsigned_64'Last then
                  Timing_Dropped (Stage) := Timing_Dropped (Stage) + 1;
               end if;
            else TH.Add (Timing (Stage), V.Microseconds);
            end if;
         end;
      end if;
   end noteTiming;
   ---------------------------------------------------------------------------
   -- Periodic report. The period boundary (maybePrintStats) only captures
   -- it: counter values and copies of the bounded trace rings, then resets
   -- them. Its text is housekeeping: formatted and written one line at a
   -- time by runHousekeeping, only while no input, request or completion is
   -- pending, for at most Housekeeping_Slice_Us per loop turn. A report still
   -- unwritten at the next boundary is finished there first (counted in
   -- report_forced=), so no line is lost. Serial and log text never runs
   -- between taking input and presenting it.
   ---------------------------------------------------------------------------
   Housekeeping_Slice_Us : constant := 250;
   type Period_Counters is record
      Events, Keyboard, Mouse, Buttons, Wheel, Event_Busy, Input_Resync,
      Source_Gaps, Source_Rejects, Age_Max_Ms, Age_Avg_Ms, Requests, Frames,
      Fast_Frames, Full_Frames, Present_Req, Input_Req, Other_Req, Draw_Ms,
      Present_Ops, Completion_Ms, Damage_Px, Repair_Px, Scene_Px,
      Cursor_X, Cursor_Y, Max_Frame_Us, Max_Motion_Us, Forced : Unsigned_64 := 0;
   end record;
   type Report_Step is
     (Stats_Line, Frames_Line, GPU_Sources_Line, Stage_Lines,
      Frame_Records, Frame_Summary, Source_Records, Source_Summary,
      Input_Records, Input_Summary, Render_Records, Render_Summary,
      Graphics_Line, Report_Done);
   type Period_Report is record
      Counters : Period_Counters;
      Stages : Stage_Histograms;
      Stage_Invalid, Stage_Dropped : Stage_Counts;
      Frames : FT.State;
      Sources : ST.State;
      Inputs : IT.State;
      Renders : RT.State;
      GPU_Changed : Boolean := False;
      Backings : Backing_Counts := (others => (others => 0));
      Retries : Retry_Counts := (others => 0);
      Resident, Uploads, Peak_Layers, Placeholders : Unsigned_64 := 0;
      Staging : GM.Counter;
   end record;
   report : Period_Report;
   reportStep : Report_Step := Report_Done;
   reportItem : Natural := 0;
   reportsForced : Unsigned_64 := 0;

   function Stage_Name (Stage : Timing_Stage) return String is
     (case Stage is
        when Input_Dispatch => "input_dispatch",
        when Request_Dispatch => "request_dispatch",
        when Scene_Draw => "scene_draw",
        when Submit_Call => "submit_call",
        when Submit_To_Completion => "submit_to_completion");

   --  Write the report's next line, skipping steps with nothing to say.
   --  Opt-in trace batches belong to the serial test transport (they stay
   --  out of logsvc); the frames and GPU source lines also go to the log.
   procedure writeReportLine is
      C : Period_Counters renames report.Counters;
      Written : Boolean := False;
      procedure Next (Step : Report_Step) is
      begin
         reportStep := Step;
         reportItem := 0;
      end Next;
   begin
      while not Written and then reportStep /= Report_Done loop
         case reportStep is
            when Stats_Line =>
               if Desktop_Timing_Policy.Enabled and then (C.Frames > 0 or else C.Events > 0) then
                  CuBit.Messages.debugPrint
                    ("desktop: stats ev=" & Decimal (C.Events) &
                     " key=" & Decimal (C.Keyboard) &
                     " mouse=" & Decimal (C.Mouse) &
                     " button=" & Decimal (C.Buttons) &
                     " wheel=" & Decimal (C.Wheel) &
                     " event_busy=" & Decimal (C.Event_Busy) &
                     " input_resync=" & Decimal (C.Input_Resync) &
                     " source_gap=" & Decimal (C.Source_Gaps) &
                     " source_reject=" & Decimal (C.Source_Rejects) &
                     " src_age_max_ms=" & Decimal (C.Age_Max_Ms) &
                     " src_age_avg_ms=" & Decimal (C.Age_Avg_Ms) &
                     " req=" & Decimal (C.Requests) &
                     " frames=" & Decimal (C.Frames) &
                     " fast=" & Decimal (C.Fast_Frames) &
                     " full=" & Decimal (C.Full_Frames) &
                     " present_req=" & Decimal (C.Present_Req) &
                     " input_req=" & Decimal (C.Input_Req) &
                     " other_req=" & Decimal (C.Other_Req) &
                     " draw_ms=" & Decimal (C.Draw_Ms) &
                     " submit=" & Decimal (C.Present_Ops) &
                     " completion_ms=" & Decimal (C.Completion_Ms) &
                     " px=" & Decimal (C.Damage_Px) &
                     " repair_px=" & Decimal (C.Repair_Px) &
                     " scene_px=" & Decimal (C.Scene_Px) &
                     " cursor_x=" & Decimal (C.Cursor_X) &
                     " cursor_y=" & Decimal (C.Cursor_Y) &
                     " report_forced=" & Decimal (C.Forced) & LF);
                  Written := True;
               end if;
               Next (Frames_Line);
            when Frames_Line =>
               -- Hardware evidence for the on-screen Logs viewer: at most one
               -- record per period, and only while frames are published.
               if Desktop_Timing_Policy.Enabled and then C.Present_Ops > 0 then
                  debugPrint
                    ("desktop: frames=" & Decimal (C.Present_Ops) &
                     " draw_ms=" & Decimal (C.Draw_Ms) &
                     " present_ms=" & Decimal (C.Completion_Ms) &
                     " max_frame_ms=" & Decimal (C.Max_Frame_Us / MICROSECONDS_PER_MILLISECOND) &
                     " max_motion_ms=" & Decimal (C.Max_Motion_Us / MICROSECONDS_PER_MILLISECOND) &
                     " motion_events=" & Decimal (C.Mouse) &
                     " scene_px=" & Decimal (C.Scene_Px) & LF);
                  Written := True;
               end if;
               Next (GPU_Sources_Line);
            when GPU_Sources_Line =>
               -- Printed only when backings were allocated or freed, or
               -- captures retried, that period (steady state is silent).
               if report.GPU_Changed then
                  declare
                     package VS renames Vulkan_Submission;
                     B : Backing_Counts renames report.Backings;
                     R : Retry_Counts renames report.Retries;
                  begin
                     debugPrint
                       ("desktop: gpu sources alloc glyph=" & Decimal (B (VS.Glyph_Cell, False)) &
                        " client=" & Decimal (B (VS.Client_Image, False)) &
                        " atlas=" & Decimal (B (VS.Icon_Atlas, False)) &
                        " backdrop=" & Decimal (B (VS.Backdrop_Image, False)) &
                        " free glyph=" & Decimal (B (VS.Glyph_Cell, True)) &
                        " client=" & Decimal (B (VS.Client_Image, True)) &
                        " atlas=" & Decimal (B (VS.Icon_Atlas, True)) &
                        " backdrop=" & Decimal (B (VS.Backdrop_Image, True)) &
                        " resident=" & Decimal (report.Resident) &
                        " uploads=" & Decimal (report.Uploads) &
                        " retry cold=" & Decimal (R (Desktop_Compositor.Cold_Upload)) &
                        " layers=" & Decimal (R (Desktop_Compositor.Layer_Limit)) &
                        " glyphs=" & Decimal (R (Desktop_Compositor.Glyph_Limit)) &
                        " images=" & Decimal (R (Desktop_Compositor.Image_Limit)) &
                        " draw=" & Decimal (R (Desktop_Compositor.Rejected_Draw)) &
                        " readback=" & Decimal (R (Desktop_Compositor.Readback_Failed)) &
                        " peak_layers=" & Decimal (report.Peak_Layers) &
                        " placeholders=" & Decimal (report.Placeholders) & LF);
                  end;
                  Written := True;
               end if;
               Next (Stage_Lines);
            when Stage_Lines =>
               if not Desktop_Timing_Policy.Enabled or else
                 reportItem > Timing_Stage'Pos (Timing_Stage'Last)
               then
                  Next (Frame_Records);
               else
                  declare
                     Stage : constant Timing_Stage := Timing_Stage'Val (reportItem);
                  begin
                     reportItem := reportItem + 1;
                     if TH.Count (report.Stages (Stage)) > 0 or else
                       report.Stage_Invalid (Stage) > 0 or else report.Stage_Dropped (Stage) > 0
                     then
                        CuBit.Messages.debugPrint
                          ("COMPOSITOR-TIMING: stage=" & Stage_Name (Stage) &
                           " count=" & Decimal (Unsigned_64 (TH.Count (report.Stages (Stage)))) &
                           " min_us=" & Decimal (TH.Minimum (report.Stages (Stage))) &
                           " max_us=" & Decimal (TH.Maximum (report.Stages (Stage))) &
                           " p50_upper_us=" & Decimal (TH.Quantile_Upper (report.Stages (Stage), 50)) &
                           " p99_upper_us=" & Decimal (TH.Quantile_Upper (report.Stages (Stage), 99)) &
                           " invalid=" & Decimal (report.Stage_Invalid (Stage)) &
                           " dropped=" & Decimal (report.Stage_Dropped (Stage)) & LF);
                        Written := True;
                     end if;
                  end;
               end if;
            when Frame_Records =>
               if reportItem >= FT.Count (report.Frames) then
                  Next (Frame_Summary);
               else
                  reportItem := reportItem + 1;
                  declare V : constant FT.Record_Value := FT.Item (report.Frames, reportItem);
                  begin
                     CuBit.Messages.debugPrint
                       ("COMPOSITOR-FRAME: output=" & Decimal (Unsigned_64 (V.Output_ID)) &
                        " session=" & Decimal (V.Session) & " frame=" & Decimal (V.Frame) &
                        " submit_us=" & Decimal (V.Submitted) & " complete_us=" & Decimal (V.Completed) & LF);
                  end;
                  Written := True;
               end if;
            when Frame_Summary =>
               if FT.Count (report.Frames) > 0 or else FT.Invalid (report.Frames) > 0 or else
                 FT.Lost (report.Frames) > 0
               then
                  CuBit.Messages.debugPrint
                    ("COMPOSITOR-FRAME-STATS: count=" & Decimal (Unsigned_64 (FT.Count (report.Frames))) &
                     " invalid=" & Decimal (Unsigned_64 (FT.Invalid (report.Frames))) &
                     " dropped=" & Decimal (Unsigned_64 (FT.Lost (report.Frames))) & LF);
                  Written := True;
               end if;
               Next (Source_Records);
            when Source_Records =>
               if reportItem >= ST.Count (report.Sources) then
                  Next (Source_Summary);
               else
                  reportItem := reportItem + 1;
                  declare V : constant ST.Record_Value := ST.Item (report.Sources, reportItem);
                  begin
                     CuBit.Messages.debugPrint
                       ("COMPOSITOR-SOURCE: surface=" & Decimal (V.Surface) &
                        " epoch=" & Decimal (V.Epoch) & " ticket=" & Decimal (V.Ticket) &
                        " input_after=" & Decimal (V.Input_After) &
                        " accepted_us=" & Decimal (V.Accepted) & LF);
                  end;
                  Written := True;
               end if;
            when Source_Summary =>
               if ST.Count (report.Sources) > 0 or else ST.Invalid (report.Sources) > 0 or else
                 ST.Lost (report.Sources) > 0
               then
                  CuBit.Messages.debugPrint
                    ("COMPOSITOR-SOURCE-STATS: count=" & Decimal (Unsigned_64 (ST.Count (report.Sources))) &
                     " invalid=" & Decimal (Unsigned_64 (ST.Invalid (report.Sources))) &
                     " dropped=" & Decimal (Unsigned_64 (ST.Lost (report.Sources))) & LF);
                  Written := True;
               end if;
               Next (Input_Records);
            when Input_Records =>
               if reportItem >= IT.Count (report.Inputs) then
                  Next (Input_Summary);
               else
                  reportItem := reportItem + 1;
                  declare V : constant IT.Record_Value := IT.Item (report.Inputs, reportItem);
                  begin
                     CuBit.Messages.debugPrint
                       ("COMPOSITOR-INPUT: surface=" & Decimal (V.Surface) &
                        " serial=" & Decimal (V.Serial) & " kind=" & Decimal (V.Kind) &
                        " dequeued_us=" & Decimal (V.Dequeued) & LF);
                  end;
                  Written := True;
               end if;
            when Input_Summary =>
               if IT.Count (report.Inputs) > 0 or else IT.Invalid (report.Inputs) > 0 or else
                 IT.Lost (report.Inputs) > 0
               then
                  CuBit.Messages.debugPrint
                    ("COMPOSITOR-INPUT-STATS: count=" & Decimal (Unsigned_64 (IT.Count (report.Inputs))) &
                     " invalid=" & Decimal (Unsigned_64 (IT.Invalid (report.Inputs))) &
                     " dropped=" & Decimal (Unsigned_64 (IT.Lost (report.Inputs))) & LF);
                  Written := True;
               end if;
               Next (Render_Records);
            when Render_Records =>
               if reportItem >= RT.Count (report.Renders) then
                  Next (Render_Summary);
               else
                  reportItem := reportItem + 1;
                  declare V : constant RT.Record_Value := RT.Item (report.Renders, reportItem);
                  begin
                     CuBit.Messages.debugPrint
                       ("COMPOSITOR-RENDER: kind=" & Decimal (RT.Phase'Pos (V.Kind) + 1) &
                        " output=" & Decimal (Unsigned_64 (V.Output_ID)) &
                        " buffer=" & Decimal (V.Buffer) & " writer_epoch=" & Decimal (V.Writer_Epoch) &
                        " writer_serial=" & Decimal (V.Writer_Serial) & " surface=" & Decimal (V.Surface) &
                        " source_epoch=" & Decimal (V.Source_Epoch) & " source_ticket=" & Decimal (V.Source_Ticket) &
                        " session=" & Decimal (V.Session) & " frame=" & Decimal (V.Frame) &
                        " observed_us=" & Decimal (V.Observed) & LF);
                  end;
                  Written := True;
               end if;
            when Render_Summary =>
               if RT.Count (report.Renders) > 0 or else RT.Invalid (report.Renders) > 0 or else
                 RT.Lost (report.Renders) > 0 or else RT.Unsupported (report.Renders) > 0
               then
                  CuBit.Messages.debugPrint
                    ("COMPOSITOR-RENDER-STATS: count=" & Decimal (Unsigned_64 (RT.Count (report.Renders))) &
                     " invalid=" & Decimal (Unsigned_64 (RT.Invalid (report.Renders))) &
                     " dropped=" & Decimal (Unsigned_64 (RT.Lost (report.Renders))) &
                     " unsupported=" & Decimal (Unsigned_64 (RT.Unsupported (report.Renders))) & LF);
                  Written := True;
               end if;
               Next (Graphics_Line);
            when Graphics_Line =>
               CuBit.Graphics_Metrics_IO.Publish
                 (GM.Desktop_Staging, report.Staging, stagingReporter);
               Next (Report_Done);
            when Report_Done =>
               null;
         end case;
      end loop;
   end writeReportLine;

   --  Housekeeping slice, at the end of a loop turn: report lines while no
   --  work is pending (a nonblocking activity check before each line), for
   --  at most Housekeeping_Slice_Us.
   procedure runHousekeeping is
      Started : Unsigned_64;
      Wrote : Boolean := False;
   begin
      if reportStep = Report_Done then return; end if;
      Started := dispatchNow;
      while reportStep /= Report_Done loop
         exit when Wait_For_Activity_Until (0) /= Deadline_Reached;
         writeReportLine;
         Wrote := True;
         declare Spent : constant Compositor_Elapsed.Sample :=
           Compositor_Elapsed.Measure (Started, dispatchNow);
         begin
            exit when not Spent.Valid or else Spent.Microseconds >= Housekeeping_Slice_Us;
         end;
      end loop;
      --  As before, Diagnostic_Output measures the timing builds' text.
      if Wrote and then Desktop_Timing_Policy.Enabled and then DM.Enabled and then not DM.Disabled then
         DM.Record_Stage (Compositor_Stage_Metrics.Diagnostic_Output, Started, dispatchNow);
      end if;
   end runHousekeeping;

   --  The period boundary: capture the report and reset the period's
   --  counters and traces. Cheap (copies of bounded records, no text).
   procedure capturePeriod (Event_Busy, Input_Resync : Unsigned_64) is
   begin
      if reportStep /= Report_Done then
         --  Housekeeping never found a quiet moment for the whole period.
         while reportStep /= Report_Done loop writeReportLine; end loop;
         if reportsForced < Unsigned_64'Last then reportsForced := reportsForced + 1; end if;
      end if;
      report.Counters :=
        (Events => statsEvents, Keyboard => statsKeyboardEvents, Mouse => statsMouseEvents,
         Buttons => statsButtonTransitions, Wheel => statsWheelEvents,
         Event_Busy => Event_Busy, Input_Resync => Input_Resync,
         Source_Gaps => statsSourceGaps, Source_Rejects => statsSourceRejects,
         Age_Max_Ms => statsSourceAgeMaxMs,
         Age_Avg_Ms => (if statsSourceAgeCount = 0 then 0 else statsSourceAgeSumMs / statsSourceAgeCount),
         Requests => statsRequests, Frames => statsFrames, Fast_Frames => statsFastFrames,
         Full_Frames => statsFullFrames, Present_Req => statsPresentReq, Input_Req => statsInputReq,
         Other_Req => statsOtherReq, Draw_Ms => statsDrawMs, Present_Ops => statsPresentOps,
         Completion_Ms => statsCompletionMs, Damage_Px => statsDamagePixels,
         Repair_Px => statsRepairPixels, Scene_Px => statsScenePixels,
         Cursor_X => Unsigned_64 (cursorX), Cursor_Y => Unsigned_64 (cursorY),
         Max_Frame_Us => statsMaxFrameUs, Max_Motion_Us => statsMaxMotionUs,
         Forced => reportsForced);
      report.Stages := Timing;
      report.Stage_Invalid := Timing_Invalid;
      report.Stage_Dropped := Timing_Dropped;
      Timing := (others => TH.Empty);
      Timing_Invalid := (others => 0);
      Timing_Dropped := (others => 0);
      report.Frames := frameTrace; FT.Reset (frameTrace);
      report.Sources := sourceTrace; ST.Reset (sourceTrace);
      report.Inputs := inputDequeueTrace; IT.Reset (inputDequeueTrace);
      report.Renders := renderTrace; RT.Reset (renderTrace);
      report.GPU_Changed := False;
      for Class in Vulkan_Submission.Source_Class loop
         for Freed in Boolean loop
            report.Backings (Class, Freed) := Desktop_Compositor.Backing_Events (Class, Freed);
            if report.Backings (Class, Freed) /= reportedBackings (Class, Freed) then
               report.GPU_Changed := True;
               reportedBackings (Class, Freed) := report.Backings (Class, Freed);
            end if;
         end loop;
      end loop;
      for Cause in Desktop_Compositor.Retry_Cause loop
         report.GPU_Changed := report.GPU_Changed or else statsRetries (Cause) /= 0;
      end loop;
      report.Retries := statsRetries;
      statsRetries := (others => 0);
      report.Resident := Unsigned_64 (Desktop_Compositor.Resident_Sources);
      report.Uploads := Desktop_Compositor.Upload_Progress;
      report.Peak_Layers := Unsigned_64 (Desktop_Compositor.Peak_Scene_Layers);
      report.Placeholders := Unsigned_64 (Desktop_Compositor.Placeholder_Draws);
      if Desktop_Compositor.Placeholder_Draws /= reportedPlaceholders then
         -- Placeholders only follow a refused allocation after every idle
         -- source was evicted; say so once, then count it in the line.
         if reportedPlaceholders = 0 then
            debugPrint ("desktop: GPU source memory exhausted; surface drawn as placeholder" & LF);
         end if;
         reportedPlaceholders := Desktop_Compositor.Placeholder_Draws;
         report.GPU_Changed := True;
      end if;
      report.Staging := stagingCopies;
      reportStep := Stats_Line;
      reportItem := 0;
   end capturePeriod;

   type Turn_Work is record
      Events, Requests, Presents : Unsigned_64 := 0;
   end record;
   turnStartedUs : Unsigned_64 := 0;
   turnWork : Turn_Work;

   procedure maybePrintStats is
      now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      eventDrops : Unsigned_64;
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
      eventDrops := getInfo (SYSINFO_EVENT_DROPS_SELF);
      DI.Observe_Drops (Input_Diagnostic, eventDrops);

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

      capturePeriod (eventDropsThisPeriod, inputOverflowsThisPeriod);

      -- Reuse the existing reporting clock. The store sums Counter deltas;
      -- submit each interval once, before resetting the local work counters.
      if DM.Enabled and then not DM.Disabled and then
        now < Unsigned_64'Last / 1000 and then
        (statsScenePixels > 0 or else statsRepairPixels > 0)
      then
         DM.Record_Work (Compositor_Work_Metrics.Scene_Pixels, statsScenePixels, now * 1000);
         DM.Record_Work (Compositor_Work_Metrics.Repair_Pixels, statsRepairPixels, now * 1000);
      end if;
      if Trace_Metrics_Enabled and then not DM.Disabled and then
        now < Unsigned_64'Last / 1000
      then DM.Record_Trace_Status (now * 1000); end if;
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
      statsCompletionMs := 0;
      statsDamagePixels := 0;
      statsRepairPixels := 0;
      statsScenePixels := 0;
      statsSourceGaps := 0;
      statsSourceRejects := 0;
      statsMaxFrameUs := 0;
      statsMaxMotionUs := 0;
      statsSourceAgeMaxMs := 0;
      statsSourceAgeSumMs := 0;
      statsSourceAgeCount := 0;
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

   procedure fillRect (x, y, w, h : Natural; color : Unsigned_32;
                       Use_Compositor : Boolean := True);

   procedure putPixel (x, y : Natural; color : Unsigned_32) is
      offset : Storage_Offset;
   begin
      if nativeOutputPass then
         -- Deferred output must capture pixels; readback overwrites CPU writes.
         -- Keep synchronous software pixel drawing local.
         fillRect (x, y, 1, 1, color,
                   Use_Compositor => Desktop_Compositor.Full_Output);
         return;
      end if;
      if x < fbWidth and then y < fbHeight then
         offset := Storage_Offset (y * fbPitch + x * 4);
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
      offset : Storage_Offset;
   begin
      if backBufferAddr = System.Null_Address or else
         x >= fbWidth or else y >= fbHeight
      then
         return 0;
      end if;

      offset := Storage_Offset (y * fbPitch + x * 4);
      declare
         pixel : Unsigned_32 with
            Import, Address => backBufferAddr + offset;
      begin
         return pixel;
      end;
   end readBackPixel;

   procedure writeBackPixel (x, y : Natural; color : Unsigned_32) is
      offset : Storage_Offset;
   begin
      if backBufferAddr = System.Null_Address or else
         x >= fbWidth or else y >= fbHeight
      then
         return;
      end if;

      offset := Storage_Offset (y * fbPitch + x * 4);
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

   function cursorShape return DG.Logical_Rectangle is
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (cursorAsset);
      Shape : constant Compositor_Cursor.Plan := Compositor_Cursor.Build
        ((DG.Logical_Coordinate (cursorX), DG.Logical_Coordinate (cursorY)),
         M.Width, M.Height, M.Hotspot_X, M.Hotspot_Y);
   begin
      if not Shape.Valid then exitCompositor (1); end if;
      return Shape.Surface;
   end cursorShape;

   function cursorOriginX return Integer is (Integer (cursorShape.Left));
   function cursorOriginY return Integer is (Integer (cursorShape.Top));

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

   --  A top row: a category (1 .. launchRows) or Power (launchPowerRow).
   function launchItemRect (row : Natural) return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural;
   begin
      if isEmpty (menu) or else row = 0 or else menu.w <= 16 then
         return (others => 0);
      end if;

      if row = launchPowerRow then
         y := menu.y + LAUNCH_FIRST_Y + launchRows * LAUNCH_STEP - 2 + 8;
      elsif row <= launchRows then
         y := menu.y + LAUNCH_FIRST_Y + (row - 1) * LAUNCH_STEP;
      else
         return (others => 0);
      end if;

      return clampRect ((x => menu.x + 8, y => y,
                         w => menu.w - 16, h => 30));
   end launchItemRect;

   --  The open category's submenu, beside its row and kept on the output
   --  above the taskbar (Client_Popup_Layout).
   function launchSubmenuRect return Rect is
      package PL renames Client_Popup_Layout;
      menu : constant Rect := launchMenuRect;
      open : constant Natural := launchMenuState.Open;
      items : constant Natural := LM.Items (launchMenu, open);
      row : constant Rect := launchItemRect (open);
      areaHeight : constant Natural :=
        (if taskbarY > primaryBounds.y then taskbarY - primaryBounds.y else primaryBounds.h);
      placed : PL.Box;
   begin
      if not launchMenuOpen or else open = 0 or else items = 0 or else isEmpty (menu) or else isEmpty (row) then
         return (others => 0);
      end if;
      placed := PL.Place_Beside
        ((Natural'Min (menu.x, PL.MAXIMUM_COORDINATE), Natural'Min (menu.y, PL.MAXIMUM_COORDINATE),
          Natural'Min (menu.w, PL.MAXIMUM_COORDINATE), Natural'Min (menu.h, PL.MAXIMUM_COORDINATE)),
         Natural'Min ((if row.y > SUBMENU_PAD then row.y - SUBMENU_PAD else row.y), PL.MAXIMUM_COORDINATE),
         MENU_W, 2 * SUBMENU_PAD + items * LAUNCH_STEP - 4,
         (Natural'Min (primaryBounds.x, PL.MAXIMUM_COORDINATE), Natural'Min (primaryBounds.y, PL.MAXIMUM_COORDINATE),
          Natural'Min (primaryBounds.w, PL.MAXIMUM_COORDINATE), Natural'Min (areaHeight, PL.MAXIMUM_COORDINATE)));
      return clampRect ((x => placed.X, y => placed.Y, w => placed.W, h => placed.H));
   end launchSubmenuRect;

   function launchSubItemRect (index : Natural) return Rect is
      sub : constant Rect := launchSubmenuRect;
   begin
      if isEmpty (sub) or else index = 0 or else sub.w <= 16 then
         return (others => 0);
      end if;
      return clampRect ((x => sub.x + 8, y => sub.y + SUBMENU_PAD + (index - 1) * LAUNCH_STEP,
                         w => sub.w - 16, h => 30));
   end launchSubItemRect;

   --  Everything the open menu covers (for damage).
   function launchMenuArea return Rect is
      menu : constant Rect := launchMenuRect;
      sub : constant Rect := launchSubmenuRect;
      left, top, right, bottom : Natural;
   begin
      if isEmpty (sub) then
         return menu;
      elsif isEmpty (menu) then
         return sub;
      end if;
      left := Natural'Min (menu.x, sub.x);
      top := Natural'Min (menu.y, sub.y);
      right := Natural'Max (menu.x + menu.w, sub.x + sub.w);
      bottom := Natural'Max (menu.y + menu.h, sub.y + sub.h);
      return (x => left, y => top, w => right - left, h => bottom - top);
   end launchMenuArea;

   function launchSeparatorRect return Rect is
      menu : constant Rect := launchMenuRect;
      y    : Natural := 0;
   begin
      if isEmpty (menu) or else menu.w <= 24 then
         return (others => 0);
      end if;
      y := menu.y + LAUNCH_FIRST_Y + launchRows * LAUNCH_STEP - 2;
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

   --  The top row under (x, y), 0 for none.
   function hitLaunchItem (x, y : Natural) return Natural is
   begin
      for row in 1 .. launchPowerRow loop
         if pointInRect (x, y, launchItemRect (row)) then
            return row;
         end if;
      end loop;
      return 0;
   end hitLaunchItem;

   --  The open submenu's entry under (x, y), 0 for none.
   function hitLaunchSubItem (x, y : Natural) return Natural is
   begin
      for index in 1 .. LM.Items (launchMenu, launchMenuState.Open) loop
         if pointInRect (x, y, launchSubItemRect (index)) then
            return index;
         end if;
      end loop;
      return 0;
   end hitLaunchSubItem;

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

   function transitionDamage
     (Old_Bounds, New_Bounds, Presented : Rect;
      Has_Presented : Boolean) return Rect
   is
      function Box_Of (R : Rect) return Compositor_Damage.Box is
        (R.x, R.y, R.x + R.w, R.y + R.h);
   begin
      -- The pure policy covers all three footprints. In particular, a small
      -- resize outline never substitutes for the old, still-visible window.
      return windowVisualRect (damageRectangle
        (Compositor_Transition.Cover
           (Box_Of (Old_Bounds), Box_Of (New_Bounds),
            Box_Of (Presented), Has_Presented)));
   end transitionDamage;

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

   function processAlive (pid : Process_ID) return Boolean is
      count : Unsigned_64;
   begin
      if pid = No_Process then
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
         if CuBit.Process_List.Get (psBufAddr, Natural (i)).Identity = pid then
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

   function localLogicalDamage (Output : Output_Index; Area : Rect) return Rect is
      B : constant Rect := logicalBounds (presentations (Output).Geometry);
      Left : constant Natural := Natural'Max (Area.x, B.x);
      Top : constant Natural := Natural'Max (Area.y, B.y);
      Right : constant Natural := Natural'Min (Area.x + Area.w, B.x + B.w);
      Bottom : constant Natural := Natural'Min (Area.y + Area.h, B.y + B.h);
   begin
      if Left >= Right or Top >= Bottom then return (others => 0); end if;
      return (Left - B.x, Top - B.y, Right - Left, Bottom - Top);
   end localLogicalDamage;

   function windowWorkArea (Bounds : Rect) return Rect is
      Winner : Output_Index := primaryOutput;
      Largest : Unsigned_64 := 0;
      Result : Rect;
   begin
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            declare
               R : constant Rect := localLogicalDamage (Output, Bounds);
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
      Result := logicalBounds (presentations (Winner).Geometry);
      if Winner = primaryOutput and then Result.h > TASKBAR_H then
         Result.h := Result.h - TASKBAR_H;
      end if;
      return Result;
   end windowWorkArea;

   procedure flushBackBufferRect (dirty : Rect) is
      r : constant Rect := clampRect (dirty);
   begin
      if not repairingTarget and then backBufferReady and then fbBpp = 32 and then not isEmpty (r) then
         for Output in Output_Index loop
            if presentations (Output).Enabled then
               declare
                  P : Output_Presentation renames presentations (Output);
                  Area : constant Rect := localDamage (Output, r);
                  Ignored : Compositor_Damage.State;
               begin
                  addOutputDamage (P.Damage, Area);
                  if not isEmpty (Area) then
                     RP.Invalidate (P.Repaint, (Area.x, Area.y, Area.x + Area.w, Area.y + Area.h));
                     if directOutput then
                        -- The current writer has already painted these pixels.
                        RP.Take (P.Repaint, BP.Writer (P.Pool).Buffer, Ignored);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end if;
   end flushBackBufferRect;

   procedure quarantinePresentations is
   begin
      for P of presentations loop
         CP.Quarantine (P.Transfer);
      end loop;
      -- No buffer whose release is uncertain may become a future writer.
      exitCompositor (1);
   end quarantinePresentations;

   procedure collectLaunch (C : CompletionEntry) is
      M : Message renames C.msg;
      Envelope_OK : constant Boolean := C.valid and C.status = COMPLETION_OK and
        M.tag.length = 1 and M.tag.flags = 0 and M.tag.reserved = 0 and
        M.words (1) = 0 and M.words (2) = 0 and M.words (3) = 0;
      --  The reply names the new process by its full identity (KERN-003).
      Success : constant Boolean := Envelope_OK and M.tag.label = REPLY_OK and
        Is_Process (From_Word (M.words (0)));
      Rejected : constant Boolean := Envelope_OK and M.tag.label = 16#F001# and M.words (0) = 0;
   begin
      CR.Complete (launchRequest, C.token, Success or Rejected);
      if not CR.Available (launchRequest) then
         debugPrint ("desktop: launch completion uncertain; buffer retained" & LF);
         return;
      end if;
      if Success then
         -- Menu refresh may reorder entries while the request is pending.
         -- Match the captured program name, not the old menu index.
         for I in 1 .. launchMenu.Count loop
            if Desktop_Launch.Program_Of (launchMenu.Entries (I)) = launchName (1 .. launchNameLength) then
               launchPids (I) := From_Word (M.words (0));
            end if;
         end loop;
         if launchName (1 .. launchNameLength) = "doom.elf" then doomPid := From_Word (M.words (0)); end if;
         debugPrint ("desktop: launch complete token=" & Decimal (C.token) &
           " pid=" & Decimal (M.words (0)) & LF);
      else
         debugPrint ("desktop: launch rejected token=" & Decimal (C.token) & LF);
      end if;
   end collectLaunch;

   procedure collectPresentations is
      completion : CompletionEntry;
      count : Unsigned_64;
      matched : Boolean;
      Started : constant Unsigned_64 := dispatchNow;
      Batch : DB.Completion_Batch := DB.New_Completions (Started);
      Non_Metric_Work : Boolean := False;
      use type DSP.Frame_Outcome, DSP.Buffer_Disposition;
   begin
      while DB.Can_Complete (Batch, dispatchNow) loop
         completion := NULL_COMPLETION;
         count := Poll_Completion (completion'Address);
         exit when count = 0;
         if count /= 1 then
            CR.Quarantine (launchRequest);
            Desktop_Launch_Refresh.Quarantine;
            debugPrint ("desktop: completion queue unavailable" & LF);
            quarantinePresentations;
            return;
         end if;
         DB.Charge_Completion (Batch);
         if not (DM.Enabled and then DM.Matches (completion.token)) then
            Non_Metric_Work := True;
         end if;
         if DM.Enabled and then DM.Matches (completion.token) then
            DM.Collect (completion);
         elsif CR.Token (launchRequest) /= 0 and then completion.token = CR.Token (launchRequest) then
            collectLaunch (completion);
         elsif Desktop_Launch_Refresh.Token /= 0 and then
           completion.token = Desktop_Launch_Refresh.Token
         then
            Desktop_Launch_Refresh.Collect (completion);
         elsif Desktop_Pointer_Plane.Matches (completion.token) then
            Desktop_Pointer_Plane.Collect (completion, requestSequence);
         elsif Desktop_Status_Refresh.Matches (completion.token) then
            Desktop_Status_Refresh.Collect (completion, requestSequence);
         elsif completion.token > retiredThrough then
            matched := False;
            for Output in Output_Index loop
               if LR.Token (outputLeaseRequests (Output)) /= 0 and then
                 completion.token = LR.Token (outputLeaseRequests (Output))
               then
                  matched := True;
                  LR.Complete (outputLeaseRequests (Output), completion.token,
                    completion.valid and then completion.status = COMPLETION_OK and then
                    completion.msg.tag.label = OP_DISPLAY_RELEASE and then
                    completion.msg.tag.length = 1 and then completion.msg.tag.flags = 0 and then
                    completion.msg.tag.reserved = 0 and then
                    (for all Word of completion.msg.words => Word = 0));
                  if LR.Status (outputLeaseRequests (Output)) /= LR.Released then
                     debugPrint ("desktop: output lease completion uncertain" & LF);
                     quarantinePresentations;
                  end if;
                  exit;
               end if;
            end loop;
            if not matched then
            for P of presentations loop
               if P.Enabled and then completion.token = CP.Token (P.Transfer) then
                  matched := True;
                  -- Continue validating completions while draining. Display
                  -- rejects lease release until its active frame retires;
                  -- the drain guard still prohibits new rendering/submission.
                  declare
                     GPU_Frame : constant BP.Ticket := BP.Displayed (P.Pool);
                     result : constant PW.Completion_Decoding :=
                       PW.Decode_Completion
                         (CuBit.Desktop_Messages.To_Wire (completion.msg));
                  begin
                     -- The non-reused kernel completion token selects the
                     -- output. Payload session/frame values only validate it.
                     CP.Complete
                       (P.Transfer,
                        (Kernel_Valid => completion.valid,
                         Kernel_OK => completion.status = COMPLETION_OK,
                         Payload_Valid => result.Valid and then result.Value.Buffer = BP.Displayed (P.Pool).Buffer,
                         Kernel_Token => completion.token,
                         Payload_Session => (if result.Valid then result.Value.Result.Session else 0),
                         Payload_Frame => (if result.Valid then result.Value.Result.Frame else 0),
                         Published => result.Valid and then result.Value.Result.Outcome = DSP.Published,
                         Released => result.Valid and then result.Value.Result.Buffer_State = DSP.Released));
                     BP.Retire_Display (P.Pool, BP.Displayed (P.Pool), CP.Writable (P.Transfer));
                     if not CP.Writable (P.Transfer) or else BP.Faulted (P.Pool) then
                        debugPrint ("desktop: asynchronous transfer quarantined" & LF);
                        exitCompositor (1);
                     else
                        if not gpuProgress (P.Trace_Output).Published and then
                          GPU_Frame /= BP.None and then
                          GPU_Frame = gpuProgress (P.Trace_Output).Submitted
                        then
                           gpuProgress (P.Trace_Output).Published := True;
                           if not Bootstrap_CPU then Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Released); end if;
                        debugPrint ("DESKTOP-VULKAN: frame=PUBLISHED output=" & P.Trace_Output'Image &
                             " epoch=" & GPU_Frame.Epoch'Image &
                             " frame=" & GPU_Frame.Serial'Image &
                             " buffer=" & GPU_Frame.Buffer'Image &
                             " session=" & CP.Session (P.Transfer)'Image &
                             " token=" & completion.token'Image & LF);
                        end if;
                        if Desktop_Timing_Policy.Enabled or DM.Enabled then
                           declare Finished_Us : constant Unsigned_64 := presentationNow;
                           begin
                              if Trace_Enabled then
                                 recordFrameTrace (
                                   (Natural (P.Trace_Output), CP.Session (P.Transfer),
                                    CP.Token (P.Transfer), P.Started_Us, Finished_Us));
                              end if;
                              if DM.Enabled then
                                 DM.Record_Completion
                                   ((Natural (P.Trace_Output), CP.Session (P.Transfer),
                                     CP.Token (P.Transfer), P.Started_Us, Finished_Us));
                              end if;
                           end;
                        end if;
                        noteTiming (Submit_To_Completion, P.Started_Us);
                        declare Finished : constant Unsigned_64 := nowMs;
                        begin
                           if Finished /= Unsigned_64'Last and P.Started /= Unsigned_64'Last and
                             Finished >= P.Started
                           then statsCompletionMs := statsCompletionMs + Finished - P.Started;
                           end if;
                        end;
                        if not releaseAnnounced then
                           releaseAnnounced := True;
                           debugPrint ("desktop: asynchronous frame released" & LF);
                        end if;
                     end if;
                  end;
                  exit;
               end if;
            end loop;
            end if;
            if not matched then
               -- No buffer may be reused on an unrecognized completion.
               debugPrint ("desktop: unknown presentation completion" & LF);
               quarantinePresentations;
            end if;
         end if;
      end loop;
      if Non_Metric_Work and then DM.Enabled and then not DM.Disabled then
         DM.Record_Stage (Compositor_Stage_Metrics.Completion_Dispatch,
           Started, dispatchNow);
      end if;
   end collectPresentations;

   procedure submitPreparedOutput (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      Held : constant BP.Ticket := BP.Displayed (P.Pool);
      Attempt_Time : constant Unsigned_64 := nowMs;
      r : constant Rect := damageRectangle (Compositor_Damage.Bounds (P.Frame_Damage));
      request : Message;
      Accepted : Boolean;
   begin
      if not CP.Can_Attempt (P.Transfer, Attempt_Time) then return; end if;
      if Held = BP.None or else isEmpty (r) then exitCompositor (1); end if;
      declare
         use type CuBit.Display_Planes.Plan_Epoch;
         Item : constant PW.Frame :=
           (Held.Buffer, (CP.Session (P.Transfer), CP.Token (P.Transfer),
             (DP.Pixel_Coordinate (r.x), DP.Pixel_Coordinate (r.y),
              DP.Pixel_Extent (r.w), DP.Pixel_Extent (r.h))));
         Epoch : constant CuBit.Display_Planes.Plan_Epoch :=
           Desktop_Pointer_Plane.Frame_Epoch;
      begin
         --  Tag the frame with the plane plan whose pointer composition it
         --  shows; display swaps plane and composite with this frame.
         request := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
           ((if Epoch = CuBit.Display_Planes.No_Epoch then PW.Encode (Item)
             else CuBit.Display_Plane_Protocol.Encode
               (CuBit.Display_Plane_Protocol.Plan_Frame'(Item, Epoch))), Output));
      end;
      P.Started := Attempt_Time;
      P.Started_Us := presentationNow;
      if not Bootstrap_CPU then Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Submit); end if;
      Accepted := capSubmit (CAP_SLOT_DISPLAY, request, CP.Token (P.Transfer));
      CP.Submitted (P.Transfer, Accepted, Attempt_Time);
      if Accepted then
         if Desktop_Compositor.Full_Output and then not gpuProgress (Output).Published then
            gpuProgress (Output).Submitted := Held;
         end if;
         if Trace_Enabled then
            recordRenderTrace (
              (RT.Submit, Natural (Output), Unsigned_64 (Held.Buffer), Held.Epoch, Held.Serial,
               0, 0, 0, CP.Session (P.Transfer), CP.Token (P.Transfer), P.Started_Us));
         end if;
         noteTiming (Submit_Call, P.Started_Us);
         if pendingInputUs /= Compositor_Elapsed.Unavailable then
            if DM.Enabled and then not DM.Disabled then
               DM.Record_Stage (Compositor_Stage_Metrics.Input_To_Present, pendingInputUs, dispatchNow);
            end if;
            pendingInputUs := Compositor_Elapsed.Unavailable;
         end if;
         declare Waited : constant Compositor_Elapsed.Sample :=
           Compositor_Elapsed.Measure (pendingMotionUs, timingNow);
         begin
            if Waited.Valid then
               statsMaxMotionUs := Unsigned_64'Max (statsMaxMotionUs, Waited.Microseconds);
            end if;
            pendingMotionUs := Compositor_Elapsed.Unavailable;
         end;
         Compositor_Damage.Clear (P.Frame_Damage);
         statsPresentOps := statsPresentOps + 1;
         if not asyncAnnounced then
            asyncAnnounced := True;
            debugPrint ("desktop: asynchronous presentation active" & LF);
         end if;
      else
         -- False confirms non-publication; retain the immutable frame and
         -- captured damage until a later attempt admitted by the deadline.
         noteTiming (Submit_Call, P.Started_Us);
      end if;
   end submitPreparedOutput;

   procedure pumpOutput (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      r : Rect := (others => 0);
      Completion : Desktop_Compositor.Render_Completion;
      Captured : Boolean;
      use type Desktop_Compositor.Render_Completion;
      Repair, Capture_Repair, Writer_Repair : Compositor_Damage.State;
      Held, Next_Writer : BP.Ticket;
      copiedBytes : Unsigned_64 := 0;
      envelopeBytes : Unsigned_64 := 0;
      Frame_Started : constant Unsigned_64 := timingNow;
      use type DG.Scale_Component;
      procedure copyRegion (r : Rect) is
         ignored : System.Address;
      begin
         if P.Geometry.Scale.Numerator = P.Geometry.Scale.Denominator then
            -- Preserve the bulk-copy fast path for unscaled outputs.
            for row in r.y .. r.y + r.h - 1 loop
            ignored := memcpy
              (P.Buffer + Storage_Offset (row * P.Pitch + r.x * 4),
               backBufferAddr + Storage_Offset
                 ((row + Natural (P.Geometry.Y)) * fbPitch +
                  (r.x + Natural (P.Geometry.X)) * 4),
               Storage_Count (r.w * 4));
            end loop;
         else
            -- Compatibility sampling of logical-pixel surfaces. Map each column
            -- once per damage rectangle, not once per pixel. This writes directly
            -- into the existing transfer buffer; no extra intermediate image.
            declare
               Columns : array (r.x .. r.x + r.w - 1) of Natural;
               Source : array (0 .. sceneCapacityBytes / 4 - 1) of Unsigned_32
                 with Import, Address => backBufferAddr;
            begin
               for X in Columns'Range loop
                  declare M : constant DG.Point_Mapping := DG.To_Desktop
                    (P.Geometry, (DG.Pixel_Index (X), 0));
                  begin
                     if not M.Valid then quarantinePresentations; return; end if;
                     Columns (X) := Natural (M.Value.X);
                  end;
               end loop;
               for Y in r.y .. r.y + r.h - 1 loop
                  declare
                     M : constant DG.Point_Mapping := DG.To_Desktop
                       (P.Geometry, (0, DG.Pixel_Index (Y)));
                     Target_Row : array (0 .. Natural (P.Geometry.Width) - 1) of Unsigned_32
                       with Import, Address => P.Buffer + Storage_Offset (Y * P.Pitch),
                       Alignment => 1;
                     Row : Natural;
                  begin
                     if not M.Valid then quarantinePresentations; return; end if;
                     Row := Natural (M.Value.Y) * (fbPitch / 4);
                     for X in Columns'Range loop
                        Target_Row (X) := Source (Row + Columns (X));
                     end loop;
                  end;
               end loop;
            end;
         end if;
         copiedBytes := copiedBytes + Unsigned_64 (r.w) * Unsigned_64 (r.h) * 4;
         GM.Add (stagingCopies, Unsigned_64 (r.w) * Unsigned_64 (r.h) * 4);
      end copyRegion;

   begin
      if P.Enabled and then CP.Current (P.Transfer) = CP.Prepared then
         declare Replaced : Boolean; begin
            Compositor_Frame_Replacement.Replace
              (P.Transfer, P.Pool, P.Damage, P.Frame_Damage, nowMs, Replaced);
            if not Replaced then
               submitPreparedOutput (Output);
               return;
            end if;
         end;
         -- Preserve all undelivered damage, then render the latest scene into
         -- the already separate writer. No copying or in-flight cancellation.
      end if;
      if not P.Enabled or else not CP.Writable (P.Transfer) then
         return;
      end if;
      if requestSequence >= Unsigned_64'Last - 1 then
         debugPrint ("desktop: frame identifiers exhausted" & LF);
         quarantinePresentations;
      end if;
      if BP.Rendering (P.Pool) then
         -- Pending GPU work owns this writer; only poll, never draw or submit
         -- it again. New scene/input damage remains in the separate live set.
         Desktop_Compositor.Complete_Output (P.Buffer, BP.Writer (P.Pool), Output = 1, True, Completion);
      else
         if Compositor_Damage.Count (P.Damage) = 0 then return; end if;
         if not BP.Writable (P.Pool, BP.Writer (P.Pool)) then exitCompositor (1); end if;
         -- Optional diagnostics piggyback on damage; production adds no HUD area.
         if Desktop_Input_Overlay.Enabled and then not Bootstrap_CPU then
            Compositor_Damage.Add (P.Damage,
              (0, 0, Natural'Min (900, Natural (P.Geometry.Width)),
               Natural'Min (128, Natural (P.Geometry.Height))));
         end if;
         Capture_Repair := P.Damage;
         -- Retain the output writer's history independently of GPU target repair.
         Writer_Repair := RP.Pending (P.Repaint, BP.Writer (P.Pool).Buffer);
         if nativeScene and then not Bootstrap_CPU then
            declare
               Started : Desktop_Compositor.Output_Start;
            begin
               Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Begin_Before);
               Desktop_Compositor.Begin_Output
                 ((P.Buffer, Unsigned_32 (P.Geometry.Width), Unsigned_32 (P.Geometry.Height),
                   Unsigned_32 (P.Pitch), 1), Unsigned_64 (P.Pitch) * Unsigned_64 (P.Geometry.Height),
                  BP.Writer (P.Pool), P.Geometry, Output = 1, Started, Capture_Repair, Writer_Repair);
               Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Begin_After);
               case Started is
                  when Desktop_Compositor.Deferred =>
                     Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Deferred); return;
                  when Desktop_Compositor.Start_Unsafe =>
                     Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Unsafe);
                     debugPrint ("desktop: renderer begin uncertain; writer retained" & LF);
                     exitCompositor (1);
                  when Desktop_Compositor.Started => null;
               end case;
            end;
         end if;
         Compositor_Damage.Capture (P.Damage, P.Frame_Damage, Captured);
         if not Captured then exitCompositor (1); end if;
         if Bootstrap_CPU then
            renderOutput (Output, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
         elsif not directOutput then
            RP.Take (P.Repaint, BP.Writer (P.Pool).Buffer, Repair);
            -- Render both histories into the command snapshot. GPU replay
            -- scissors to its own target repair; CPU paths retain their writer
            -- repair. Historical repair must not dirty other targets again.
            if Desktop_Compositor.Full_Output then
               for I in 1 .. Compositor_Damage.Count (Capture_Repair) loop
                  Compositor_Damage.Add (Repair, Compositor_Damage.Item (Capture_Repair, I));
               end loop;
            end if;
            for I in 1 .. Compositor_Damage.Count (Repair) loop
               if nativeScene then
                  renderOutput (Output, damageRectangle (Compositor_Damage.Item (Repair, I)));
               else
                  copyRegion (damageRectangle (Compositor_Damage.Item (Repair, I)));
               end if;
            end loop;
         end if;
         BP.Start_Render (P.Pool, BP.Writer (P.Pool));
         if Bootstrap_CPU then
            Completion := Desktop_Compositor.Complete;
         else
            Desktop_Compositor.Complete_Output (P.Buffer, BP.Writer (P.Pool), Output = 1, False, Completion);
         end if;
      end if;
      case Completion is
         when Desktop_Compositor.Pending =>
            Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Pending);
            if not nativeScene then exitCompositor (1); end if;
            return;
         when Desktop_Compositor.Unsafe =>
            Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Unsafe);
            debugPrint ("desktop: renderer completion uncertain; writer retained" & LF);
            exitCompositor (1);
         when Desktop_Compositor.Retry =>
            -- The renderer has retired every reader/writer, but has no frame
            -- to publish (for example, a cold glyph upload needs recapture).
            -- Preserve fresh damage and mark this possibly partial target for
            -- full repair before reusing it; never send it to Display.
            if not nativeScene then exitCompositor (1); end if;
            Held := BP.Writer (P.Pool);
            if Held = BP.None then exitCompositor (1); end if;
            declare
               Cause : constant Desktop_Compositor.Retry_Cause := Desktop_Compositor.Last_Retry_Cause;
               Stalled : Boolean;
            begin
               if statsRetries (Cause) < Unsigned_64'Last then
                  statsRetries (Cause) := statsRetries (Cause) + 1;
               end if;
               Compositor_Stall_Watch.Retried (gpuStallWatch, nowMs,
                 Desktop_Compositor.Upload_Progress, GPU_Stall_Deadline_Ms, Stalled);
               if Stalled and then Desktop_Compositor.Full_Output then
                  gpuStallCause := Cause;
                  Renderer_Recovery := True;
                  Renderer_Recovery_Key := (Unsigned_64 (Output), Held.Epoch, Held.Serial, Unsigned_64 (Held.Buffer));
               end if;
            end;
            BP.Finish_Render (P.Pool, Held, BP.Failed_Quiescent);
            if BP.Faulted (P.Pool) then exitCompositor (1); end if;
            RP.Failed_Render (P.Repaint, Held.Buffer);
            Compositor_Damage.Restore (P.Damage, P.Frame_Damage);
            BP.Acquire (P.Pool, Next_Writer);
            if Next_Writer = BP.None then exitCompositor (1); end if;
            P.Buffer := P.Targets (Next_Writer.Buffer).Address;
            if Renderer_Recovery then
               RP.Invalidate (P.Repaint, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
               Compositor_Damage.Add (P.Damage, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
            end if;
            return;
         when Desktop_Compositor.Complete =>
            if not Bootstrap_CPU then Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Complete); end if;
            Compositor_Stall_Watch.Completed (gpuStallWatch);
      end case;
      if Desktop_Compositor.Full_Output and then not gpuProgress (Output).Completed then
         gpuProgress (Output).Completed := True;
         debugPrint ("DESKTOP-VULKAN: frame=COMPLETE output=" & Output'Image &
           " epoch=" & BP.Writer (P.Pool).Epoch'Image &
           " frame=" & BP.Writer (P.Pool).Serial'Image &
           " buffer=" & BP.Writer (P.Pool).Buffer'Image & LF);
      end if;
      -- Complete includes retirement of GPU readback before these CPU writes.
      -- Pool writer is still owned here; BP.Present below hands it to Display.
      if Desktop_Input_Overlay.Enabled and then not Bootstrap_CPU then
         Paint_Diagnostic (Output);
      end if;
      r := damageRectangle (Compositor_Damage.Bounds (P.Frame_Damage));
      if isEmpty (r) then exitCompositor (1); end if;
      envelopeBytes := Unsigned_64 (r.w) * Unsigned_64 (r.h) * 4;
      BP.Finish_Render (P.Pool, BP.Writer (P.Pool), BP.Completed);
      BP.Present (P.Pool, Held);
      if Held = BP.None then exitCompositor (1); end if;
      declare Frame : constant Compositor_Elapsed.Sample :=
        Compositor_Elapsed.Measure (Frame_Started, timingNow);
      begin
         if Frame.Valid then
            statsMaxFrameUs := Unsigned_64'Max (statsMaxFrameUs, Frame.Microseconds);
         end if;
      end;
      if not nativeScene and then not directOutput and then not sparseCopyAnnounced and then
        copiedBytes < envelopeBytes
      then
         sparseCopyAnnounced := True;
         debugPrint ("desktop: sparse output copy bytes=" & copiedBytes'Image &
           " envelope=" & envelopeBytes'Image & LF);
      end if;
      declare
         Started : Boolean;
         New_Token : Unsigned_64;
      begin
         CR.Allocate (requestSequence, New_Token);
         CP.Prepare (P.Transfer, New_Token, Started);
         if not Started then
            debugPrint ("desktop: invalid presentation transition" & LF);
            exitCompositor (1);
         end if;
      end;
      BP.Acquire (P.Pool, Next_Writer);
      if Next_Writer = BP.None then exitCompositor (1); end if;
      P.Buffer := P.Targets (Next_Writer.Buffer).Address;
      if directOutput then
         -- Switch write authority now, but leave missing pixels queued until
         -- visible work exists. In particular, input-time cursor restoration
         -- must never use the underlay belonging to the submitted slot.
         backBufferAddr := P.Buffer;
         cursorSaveValid := False;
      end if;
      -- A refused IPC must not leave input drawing into the held target.
      submitPreparedOutput (Output);
   end pumpOutput;

   function renderingPending return Boolean is
     -- Pending begin work also needs a bounded retry: it has not entered
     -- BP.Rendering yet, and no completion event is guaranteed to wake us.
     (for some P of presentations => P.Enabled and then
        (CP.Current (P.Transfer) = CP.Prepared or else BP.Rendering (P.Pool) or else
         (nativeScene and then CP.Writable (P.Transfer) and then
          Compositor_Damage.Count (P.Damage) > 0)));

   procedure pumpPresentation is
   begin
      if Renderer_Recovery then
         declare
            P : Output_Presentation renames presentations (Output_Index (Renderer_Recovery_Key.Output));
            Result : Desktop_Compositor.Recovery_Result;
            use type Desktop_Compositor.Recovery_Result;
            Writer_Retired : constant Boolean := not BP.Faulted (P.Pool) and then
              not BP.Rendering (P.Pool) and then CP.Writable (P.Transfer) and then
              BP.Epoch (P.Pool) = Renderer_Recovery_Key.Epoch and then
              BP.Writer (P.Pool).Serial > Renderer_Recovery_Key.Frame;
         begin
            if BP.Epoch (P.Pool) /= Renderer_Recovery_Key.Epoch then
               debugPrint ("desktop: recovery output epoch changed; backing retained" & LF);
               exitCompositor (1);
               return;
            end if;
            -- Establish actual full repaint coverage; its bounding box alone
            -- cannot establish that disjoint damage covers every pixel.
            RP.Invalidate (P.Repaint,
              (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
            Compositor_Damage.Add (P.Damage,
              (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
            Desktop_Compositor.Recover_Renderer
              (Renderer_Recovery_Key, Writer_Retired, True, Result);
            case Result is
               when Desktop_Compositor.Recovery_Complete =>
                  Renderer_Recovery := False;
                  softwareAnnounced := True;
                  debugPrint ("desktop: software rendering (GPU scene stalled" & Natural'Image (GPU_Stall_Deadline_Ms) &
                    " ms, cause=" & Desktop_Compositor.Retry_Cause'Image (gpuStallCause) &
                    " peak_layers=" & Natural'Image (Desktop_Compositor.Peak_Scene_Layers) &
                    "; switched at runtime)" & LF);
               when Desktop_Compositor.Recovery_Pending => null;
               when Desktop_Compositor.Recovery_Unsafe =>
                  debugPrint ("desktop: renderer recovery uncertain; backing retained" & LF);
                  exitCompositor (1);
            end case;
         end;
         return;
      end if;
      if backBufferReady and then not outputDrainRequested and then not shutdownRequested then
         for Output in Output_Index loop
            pumpOutput (Output);
         end loop;
      end if;
   end pumpPresentation;

   procedure fillRect (x, y, w, h : Natural; color : Unsigned_32;
                       Use_Compositor : Boolean := True) is
      minX : Natural := x;
      minY : Natural := y;
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      pairColor : constant Unsigned_64 :=
         Shift_Left (Unsigned_64 (color), 32) or Unsigned_64 (color);
      startX : Natural;
      endX : Natural;
      offset : Storage_Offset;
      Target : System.Address := backBufferAddr;
      Pitch : Natural := fbPitch;
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

      if nativeOutputPass then
         declare
            R : constant DG.Physical_Rectangle := physicalClip
              ((minX, minY, maxX - minX, maxY - minY));
            O : Output_Presentation renames presentations (activeOutput);
            Drawn, Must_Restart : Boolean;
         begin
            if Use_Compositor and then not Bootstrap_CPU then
               Desktop_Compositor.Draw_Fill
                 ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
                  Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height), R, color,
                  activeOutput = 1, Drawn, Must_Restart);
               if Must_Restart then exitCompositor (1); end if;
               if Drawn then return; end if;
            end if;
            minX := Natural (R.Left); minY := Natural (R.Top);
            maxX := Natural (R.Right); maxY := Natural (R.Bottom);
            Target := presentations (activeOutput).Buffer;
            Pitch := presentations (activeOutput).Pitch;
         end;
         if minX >= maxX or else minY >= maxY then return; end if;
      end if;
      if Target = System.Null_Address then
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
                  Storage_Offset (yy * Pitch + startX * 4);
               pixel : Unsigned_32 with
                  Import, Address => Target + offset;
            begin
               pixel := color;
            end;
            startX := startX + 1;
         end if;

         while startX + 1 < endX loop
            offset := Storage_Offset (yy * Pitch + startX * 4);
            declare
               pixels : Unsigned_64 with
                  Import, Address => Target + offset;
            begin
               pixels := pairColor;
            end;
            startX := startX + 2;
         end loop;

         if startX < endX then
            offset := Storage_Offset (yy * Pitch + startX * 4);
            declare
               pixel : Unsigned_32 with
                  Import, Address => Target + offset;
            begin
               pixel := color;
            end;
         end if;
      end loop;
   end fillRect;

   wallpaperCullAnnounced : Boolean := False;
   procedure drawWallpaper (Cull_Windows : Boolean := False) is
      use type DG.Physical_Rectangle;
      Area : Rect := (0, 0, fbWidth, fbHeight);
      --  The appearance's backdrop, or its flat colour while its image file
      --  is unavailable (loaded on first use; docs/assets.md).
      shown : CuBit.Appearance.Preferences;
   begin
      Desktop_Wallpaper_Assets.Resolve (appearance, shown);
      if nativeOutputPass then
         -- The shell subsequently fills every visible window body opaquely.
         -- Only skip a complete damage rectangle: no fragmentation, allocation
         -- or assumption about client opacity. Match fillRect's logical clamp
         -- and its proved physical clip, including fractional DPI/rotation.
         if Cull_Windows and then not clipEnabled then
            for I in surfaces'Range loop
               if surfaces (I).used and then not surfaces (I).minimized and then
                 surfaces (I).id /= compositionExcludedSurface and then
                 (surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0
               then
                  declare
                     Body_Area : constant Rect := clampRect
                       ((surfaces (I).x, surfaces (I).y, surfaces (I).w, surfaces (I).h));
                  begin
                     if not isEmpty (Body_Area) and then physicalClip (Body_Area) = outputDamage then
                        if not wallpaperCullAnnounced then
                           debugPrint ("desktop: fully covered wallpaper skipped" & LF);
                           wallpaperCullAnnounced := True;
                        end if;
                        return;
                     end if;
                  end;
               end if;
            end loop;
         end if;
         declare P : Output_Presentation renames presentations (activeOutput); begin
            declare Drawn, Must_Restart : Boolean; begin
               Desktop_Compositor.Draw_Backdrop
                 ((P.Buffer, Unsigned_32 (P.Geometry.Width),
                   Unsigned_32 (P.Geometry.Height), Unsigned_32 (P.Pitch), 1),
                  Unsigned_64 (P.Pitch) * Unsigned_64 (P.Geometry.Height),
                  outputDamage, shown, activeOutput = 1, Drawn, Must_Restart);
               if Must_Restart then exitCompositor (1); end if;
               if Drawn then return; end if;
            end;
            Desktop_Wallpaper_Layers.Paint
              (Natural (activeOutput),
               P.Buffer, Natural (P.Geometry.Width), Natural (P.Geometry.Height),
               P.Pitch, Natural (outputDamage.Left), Natural (outputDamage.Top),
               Natural (outputDamage.Right - outputDamage.Left),
               Natural (outputDamage.Bottom - outputDamage.Top), shown);
         end;
         return;
      end if;
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
               R : constant Rect := localLogicalDamage (Output, Area);
               B : constant Rect := logicalBounds (P.Geometry);
            begin
               if not isEmpty (R) then
                  Desktop_Wallpaper_Layers.Paint
                    (Natural (Output), backBufferAddr + Storage_Offset
                       (Natural (P.Geometry.Y) * fbPitch +
                        Natural (P.Geometry.X) * 4),
                     B.w, B.h,
                     fbPitch, R.x, R.y, R.w, R.h, shown);
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

      if nativeOutputPass and then Desktop_Compositor.Full_Output then
         declare
            O : Output_Presentation renames presentations (activeOutput);
            Drawn, Must_Restart : Boolean;
         begin
            Desktop_Compositor.Draw_Shadow
              ((O.Buffer, Unsigned_32 (O.Geometry.Width),
                Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
               Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height),
               (DG.Logical_Coordinate (x), DG.Logical_Coordinate (y),
                DG.Logical_Coordinate (x + w), DG.Logical_Coordinate (y + h)),
               physicalClip ((x, y, w + DROP_SHADOW_DEPTH, h + DROP_SHADOW_DEPTH)),
               C_BLACK, activeOutput = 1, Drawn, Must_Restart);
            if Must_Restart then exitCompositor (1); end if;
            if Drawn then return; end if;
         end;
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

   function drawNativeIcon
     (Item : Desktop_Icon_Pixels.Asset; X, Y : Natural) return Boolean
   is
      Drawn, Must_Restart : Boolean;
      Size : constant Positive := Desktop_Icon_Pixels.Size (Item);
   begin
      if not nativeOutputPass then return False; end if;
      declare
         O : Output_Presentation renames presentations (activeOutput);
         Damage : constant DG.Physical_Rectangle := physicalClip ((X, Y, Size, Size));
      begin
         if Damage.Left >= Damage.Right or else Damage.Top >= Damage.Bottom then return True; end if;
         -- Embedded icons use straight alpha from the shared immutable icon
         -- atlas; no per-icon GPU image or premultiplied shadow copy.
         Desktop_Compositor.Draw_Icon
           ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
            Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height),
            O.Geometry, Item, (DG.Logical_Coordinate (X), DG.Logical_Coordinate (Y),
              DG.Logical_Coordinate (X + Size), DG.Logical_Coordinate (Y + Size)),
            Damage, activeOutput = 1, Drawn, Must_Restart);
         if Must_Restart then exitCompositor (1); end if;
         return Drawn;
      end;
   end drawNativeIcon;

   procedure drawIcon
      (id : Desktop_Icons.Icon_ID;
       x, y : Natural;
       bg   : Unsigned_32)
   is
      pixel : Unsigned_32;
   begin
      if drawNativeIcon ((Desktop_Icon_Pixels.Application, id), x, y) then return; end if;
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
      if drawNativeIcon ((Desktop_Icon_Pixels.Window_Control, id), x, y) then return; end if;
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

      if nativeOutputPass then
         declare
            O : Output_Presentation renames presentations (activeOutput);
            Damage : constant DG.Physical_Rectangle := physicalClip
              ((x, y, width, CuBit.Fonts.Line_Height));
            Cell : constant DG.Logical_Rectangle :=
              (DG.Logical_Coordinate (x), DG.Logical_Coordinate (y),
               DG.Logical_Coordinate (x + width),
               DG.Logical_Coordinate (y + CuBit.Fonts.Line_Height));
         begin
            if Damage.Left >= Damage.Right or else Damage.Top >= Damage.Bottom then return; end if;
            for Y in Natural (Damage.Top) .. Natural (Damage.Bottom) - 1 loop
               for X in Natural (Damage.Left) .. Natural (Damage.Right) - 1 loop
                  declare
                     M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                       (O.Geometry, (DG.Pixel_Index (X), DG.Pixel_Index (Y)), Cell,
                        DG.Physical_Extent (width), DG.Physical_Extent (CuBit.Fonts.Line_Height));
                  begin
                     if M.Valid then
                        declare
                           Target : Unsigned_32 with Import, Address => O.Buffer + Storage_Offset (Y * O.Pitch + X * 4);
                           Coverage : constant Natural := Natural (glyph.Alpha (Natural (M.Y), Natural (M.X)));
                        begin
                           if Coverage /= 0 then
                              Target := blendPixel (fg, (if transparent then Target else bg), Coverage);
                           end if;
                        end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         return;
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

   procedure Paint_Diagnostic (Output : Output_Index) is
      O : Output_Presentation renames presentations (Output);
      W : constant Natural := Natural'Min (900, Natural (O.Geometry.Width));
      H : constant Natural := Natural'Min (128, Natural (O.Geometry.Height));
      procedure Pixel (X, Y : Natural; Color : Unsigned_32) is
      begin
         if X >= W or else Y >= H then return; end if;
         declare Target : Unsigned_32 with Import,
           Address => O.Buffer + Storage_Offset (Y * O.Pitch + X * 4), Alignment => 1;
         begin Target := Color; end;
      end Pixel;
      procedure Line (Y : Natural; Text : String) is
         X : Natural := 8;
      begin
         for C of Text loop
            declare Glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Sans, C);
               Advance : constant Natural := Natural (Glyph.Advance);
            begin
               exit when X >= W;
               if Advance > 0 then
                  for YY in 0 .. CuBit.Fonts.Line_Height - 1 loop
                     for XX in 0 .. Advance - 1 loop
                        if Glyph.Alpha (YY, XX) >= 128 then Pixel (X + XX, Y + YY, 16#00FFFFFF#); end if;
                     end loop;
                  end loop;
               end if;
               X := X + Advance;
            end;
         end loop;
      end Line;
   begin
      if not BP.Rendering (O.Pool) or else not CP.Writable (O.Transfer) or else
        BP.Writer (O.Pool) = BP.None or else O.Buffer = System.Null_Address
      then exitCompositor (1); end if;
      if Diagnostic_Frames < Unsigned_64'Last then Diagnostic_Frames := Diagnostic_Frames + 1; end if;
      for Y in 0 .. H - 1 loop
         for X in 0 .. W - 1 loop Pixel (X, Y, 16#00203648#); end loop;
      end loop;
      Line (2, "IN1 selected " & (if Desktop_Compositor.Full_Output then "GPU" else "CPU") &
        " loop=" & Decimal (Diagnostic_Loops) & " frame=" & Decimal (Diagnostic_Frames) &
        " pipe=" & (if Diagnostic_Pipeline_Valid then
          Diagnostic_Pipeline_Stage'Image & "/" & Diagnostic_Pipeline_Index'Image & "/" & Diagnostic_Pipeline_Result'Image
          else "unavailable"));
      Line (22, "raw key/mouse=" & Decimal (Input_Diagnostic.Raw_Key) & "/" & Decimal (Input_Diagnostic.Raw_Pointer) &
        " legacy key/mouse=" & Decimal (Input_Diagnostic.Legacy_Key) & "/" & Decimal (Input_Diagnostic.Legacy_Pointer));
      Line (42, "key=" & Decimal (Diagnostic_Keys) & " mouse=" & Decimal (Diagnostic_Mouse) &
        " button=" & Decimal (Diagnostic_Buttons) & " request=" & Decimal (Diagnostic_Requests));
      Line (62, "reject wire/delivery/duplicate/full=" & Decimal (Input_Diagnostic.Invalid_Wire) & "/" & Decimal (Input_Diagnostic.Bad_Delivery) &
        "/" & Decimal (Input_Diagnostic.Duplicate) & "/" & Decimal (Input_Diagnostic.Full) & " sources=" & Decimal (Input_Diagnostic.Sources));
      Line (82, "raw bits payload/snapshot=" & Decimal (Input_Diagnostic.Payload_Buttons) & "/" & Decimal (Input_Diagnostic.Snapshot_Buttons) &
        " seat=" & Decimal (Input_Diagnostic.Seat_Buttons) & " gaps=" & Decimal (Input_Diagnostic.Gaps) & " drops=" &
        (if Input_Diagnostic.Drops_Valid then Decimal (Input_Diagnostic.Drops) else "unavailable"));
      Line (102, "pointer age ms last/max=" & Decimal (Input_Diagnostic.Last_Age) & "/" & Decimal (Input_Diagnostic.Max_Age) &
        " " & (case Input_Diagnostic.Age_Status is
          when DI.No_Time => "NO_TIME", when DI.Backward => "BACKWARD", when DI.OK => "OK"));
   end Paint_Diagnostic;

   procedure drawUIText
      (x, y : Natural;
       s    : String;
       fg   : Unsigned_32;
       bg   : Unsigned_32;
       transparent : Boolean := False)
   is
      package T renames Compositor_Text;
      cx : Natural := x;
      Cursor : Integer := s'First;
      Batch : T.Glyphs;
      Count : T.Count;
      Chunk_First, Chunk_Last : Integer;
      Chunk_X, Width : Natural;
      Drawn, Repaint, Must_Restart : Boolean;
      Area : Rect;
      Target : System.Address := backBufferAddr;
      Target_W : Natural := fbWidth;
      Target_H : Natural := fbHeight;
      Target_Pitch : Natural := fbPitch;
      Screen : DG.Output := (DG.Physical_Extent (fbWidth),
        DG.Physical_Extent (fbHeight), DG.Unrotated, (1, 1), 0, 0);
      Damage : DG.Physical_Rectangle;
   begin
      if x >= fbWidth or else y >= fbHeight then return; end if;
      -- Bootstrap must not create a Mesa owner before renderer selection.
      if Bootstrap_CPU or else not Desktop_Compositor.Selected or else textSceneRetry or else
        (not nativeOutputPass and then backBufferAddr = System.Null_Address) or else fbBpp /= 32
      then
         for I in s'Range loop
            drawUIGlyph (cx, y, s (I), fg, bg, transparent);
            cx := cx + uiTextWidth (s (I .. I));
         end loop;
         return;
      end if;
      Area := (if clipEnabled then clampRect (clipRect) else (0, 0, fbWidth, fbHeight));
      if isEmpty (Area) then return; end if;
      if nativeOutputPass then
         Screen := presentations (activeOutput).Geometry;
         Target := presentations (activeOutput).Buffer;
         Target_W := Natural (Screen.Width); Target_H := Natural (Screen.Height);
         Target_Pitch := presentations (activeOutput).Pitch;
         Damage := physicalClip (Area);
      else
         Damage := (DG.Pixel_Edge (Area.x), DG.Pixel_Edge (Area.y),
                    DG.Pixel_Edge (Area.x + Area.w), DG.Pixel_Edge (Area.y + Area.h));
      end if;
      while Cursor <= s'Last and cx < fbWidth loop
         Count := 0; Chunk_First := Cursor; Chunk_X := cx;
         while Cursor <= s'Last and Count < T.Count'Last and cx < fbWidth loop
            Width := uiTextWidth (s (Cursor .. Cursor));
            Count := Count + 1;
            Batch (Count) :=
              (Code => (if Character'Pos (s (Cursor)) in 32 .. 126 then Character'Pos (s (Cursor)) else 63),
               Cell => (T.G.Logical_Coordinate (cx), T.G.Logical_Coordinate (y),
                        T.G.Logical_Coordinate (cx + Width), T.G.Logical_Coordinate (y + CuBit.Fonts.Line_Height)));
            cx := cx + Width; Cursor := Cursor + 1;
         end loop;
         Chunk_Last := Cursor - 1;
         if not transparent then
            if T.Can_Join (Batch, Count) then
               declare R : constant T.G.Logical_Rectangle := T.Background (Batch, Count); begin
                  fillRect (Natural (R.Left), Natural (R.Top),
                    Natural (R.Right) - Natural (R.Left),
                    Natural (R.Bottom) - Natural (R.Top), bg);
               end;
            else
               for I in 1 .. Count loop
                  declare R : constant T.G.Logical_Rectangle := Batch (I).Cell; begin
                     fillRect (Natural (R.Left), Natural (R.Top),
                       Natural (R.Right) - Natural (R.Left),
                       Natural (R.Bottom) - Natural (R.Top), bg);
                  end;
               end loop;
            end if;
         end if;
         Desktop_Compositor.Draw_Text
           ((Target, Unsigned_32 (Target_W), Unsigned_32 (Target_H), Unsigned_32 (Target_Pitch), 1),
            Unsigned_64 (Target_Pitch) * Unsigned_64 (Target_H), Screen,
            Batch, Count, Damage, fg or 16#FF00_0000#,
            (if nativeOutputPass then activeOutput = 1
             else backBufferAddr = dragBaseBufferAddr),
            Drawn, Repaint, Must_Restart);
         if Must_Restart then
            debugPrint ("desktop: text completion uncertain; restarting" & LF);
            exitCompositor (1);
         end if;
         if Repaint then
            textSceneRetry := True;
            debugPrint ("desktop: text batch failed; repainting scene in software" & LF);
            return;
         elsif not Drawn then
            if not mesaTextFallbackAnnounced then
               debugPrint ("desktop: CPU text fallback active" & LF); mesaTextFallbackAnnounced := True;
            end if;
            declare Fallback_X : Natural := Chunk_X; begin
               for I in Chunk_First .. Chunk_Last loop
                  drawUIGlyph (Fallback_X, y, s (I), fg, bg, transparent);
                  Fallback_X := Fallback_X + uiTextWidth (s (I .. I));
               end loop;
            end;
         elsif Desktop_Compositor.Software_Text then
            if not retainedTextAnnounced then
               debugPrint ("desktop: retained software text active" & LF); retainedTextAnnounced := True;
            end if;
         elsif not mesaTextAnnounced then
            debugPrint ("desktop: Mesa retained-mask text active" & LF); mesaTextAnnounced := True;
         end if;
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

   function streamMaskForPID (pid : Process_ID) return Unsigned_64 is
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

   procedure rememberStreams (pid : Process_ID; mask : Unsigned_64) is
      firstFree : Integer := -1;
   begin
      if pid = No_Process then
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

   procedure noteClientDraw (S : Surface) is
      Output : Output_Index;
      use type Surface_Policy.Phase;
   begin
      if not Trace_Enabled then return; end if;
      if not S.publicationMode then
         recordUnsupportedTrace; return;
      end if;
      if nativeOutputPass then
         Output := activeOutput;
      elsif directOutput and then backBufferAddr = presentations (primaryOutput).Buffer then
         Output := primaryOutput;
      else
         -- The logical compatibility/retained-scene path needs a separate
         -- copy-region bridge. Do not attribute it to an arbitrary writer.
         recordUnsupportedTrace;
         return;
      end if;
      declare
         P : Output_Presentation renames presentations (Output);
         Writer : constant BP.Ticket := BP.Writer (P.Pool);
      begin
         if not BP.Writable (P.Pool, Writer) then
            recordRenderTrace ( (others => <>)); return;
         end if;
         for I in Surface_Policy.Slot loop
            if S.publicationPolicy.Buffers (I).Status = Surface_Policy.Visible and then
              S.publicationBuffers (I).Acquired and then
              S.publicationBuffers (I).Address = S.bufferAddr
            then
               recordRenderTrace (
                 (RT.Draw, Natural (Output), Unsigned_64 (Writer.Buffer), Writer.Epoch, Writer.Serial,
                  S.id, Unsigned_64 (S.publicationPolicy.Buffers (I).Epoch),
                  Unsigned_64 (S.publicationPolicy.Buffers (I).Ticket), 0, 0, timingNow));
               return;
            end if;
         end loop;
         -- A draw with no matching acquired publication identity is not
         -- trustworthy trace evidence, even if the rasterizer returned.
         recordRenderTrace ( (others => <>));
      end;
   end noteClientDraw;

   procedure drawClientBuffer (s : Surface; x, y, w, h : Natural) is
      use type DG.Orientation;
      P : Desktop_Composition.Blit_Plan;
      Logical_W : constant Natural :=
        (if S.publicationMode then S.bufferLogicalW else S.bufferW);
      Logical_H : constant Natural :=
        (if S.publicationMode then S.bufferLogicalH else S.bufferH);
      drawn, mustRestart : Boolean;
      Sampled : Boolean := False;
   begin
      if not s.bufferAttached or else
         s.bufferAddr = System.Null_Address or else
         (not nativeOutputPass and then backBufferAddr = System.Null_Address) or else
         s.bufferFormat /= PIXEL_FORMAT_BGRA8888 or else
         s.bufferPitch < s.bufferW * 4
      then
         return;
      end if;

      P := Desktop_Composition.Plan
        (fbWidth, fbHeight, Logical_W, Logical_H,
         (x, y, w, h), clipEnabled,
         (clipRect.x, clipRect.y, clipRect.w, clipRect.h));
      if P.Width = 0 or else P.Height = 0 then
         return;
      end if;

      if nativeOutputPass then
         declare
            O : Output_Presentation renames presentations (activeOutput);
            Surface_Bounds : constant DG.Logical_Rectangle :=
              (DG.Logical_Coordinate (x), DG.Logical_Coordinate (y),
               DG.Logical_Coordinate (x + Logical_W), DG.Logical_Coordinate (y + Logical_H));
            Damage : constant DG.Physical_Rectangle := physicalClip
              ((P.Target_X, P.Target_Y, P.Width, P.Height));
         begin
            if Damage.Left >= Damage.Right or else Damage.Top >= Damage.Bottom then return; end if;
            drawn := False; mustRestart := False;
            Desktop_Compositor.Draw_Output
              ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
               (s.bufferAddr, Unsigned_32 (s.bufferW), Unsigned_32 (s.bufferH), Unsigned_32 (s.bufferPitch), 0),
               Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height),
               Unsigned_64 (s.bufferPitch) * Unsigned_64 (s.bufferH),
               O.Geometry, Surface_Bounds, Damage,
               Compositor_Source_Content.Source_Key (s.id), s.contentVersion,
               activeOutput = 1, drawn, mustRestart);
            if mustRestart then exitCompositor (1); end if;
            if drawn then
               noteClientDraw (S);
               if not physicalClientAnnounced then
                  debugPrint ("desktop: physical output client drawing active" & LF);
                  physicalClientAnnounced := True;
               end if;
               if not mesaAnnounced then
                  debugPrint ("desktop: Mesa imported-surface compositor active" & LF);
                  mesaAnnounced := True;
               end if;
               return;
            end if;
            if not softwareAnnounced then
               -- Startup and runtime selections announce their own cause.
               debugPrint ("desktop: software rendering (renderer declined client surface)" & LF);
               softwareAnnounced := True;
            end if;
            -- Opaque 1:1 client pixels need no per-pixel division. The SPARK
            -- planner clips both mappings; grants and nonaliasing remain the
            -- same trusted mapping boundary as the general sampler below.
            declare
               Rows : constant Compositor_Row_Copy.Region := Compositor_Row_Copy.Plan
                 (O.Geometry, Surface_Bounds, DG.Physical_Extent (s.bufferW),
                  DG.Physical_Extent (s.bufferH), Damage);
               Ignore : System.Address;
            begin
               if Rows.Width > 0 then
                  for Row in 0 .. Rows.Height - 1 loop
                     Ignore := memcpy
                       (O.Buffer + Storage_Offset
                          ((Rows.Target_Y + Row) * O.Pitch + Rows.Target_X * 4),
                        s.bufferAddr + Storage_Offset
                          ((Rows.Source_Y + Row) * s.bufferPitch + Rows.Source_X * 4),
                        Storage_Count (Rows.Width * 4));
                  end loop;
                  noteClientDraw (S);
                  return;
               end if;
            end;
            -- On unrotated outputs the two sampling axes are independent.
            -- Hoist Y out of the pixel loop; keep the same proved centre-based
            -- Axis operation for fractional DPI and density-aware clients.
            if O.Geometry.Rotation = DG.Unrotated then
               for Y in Natural (Damage.Top) .. Natural (Damage.Bottom) - 1 loop
                  declare
                     MY : constant Compositor_Sampling.Axis_Result := Compositor_Sampling.Axis
                       (DG.Pixel_Index (Y), O.Geometry.Scale, O.Geometry.Y,
                        Surface_Bounds.Top, Compositor_Sampling.Logical_Size (Logical_H),
                        DG.Physical_Extent (s.bufferH));
                  begin
                     if MY.Valid then
                        for X in Natural (Damage.Left) .. Natural (Damage.Right) - 1 loop
                           declare
                              MX : constant Compositor_Sampling.Axis_Result := Compositor_Sampling.Axis
                                (DG.Pixel_Index (X), O.Geometry.Scale, O.Geometry.X,
                                 Surface_Bounds.Left, Compositor_Sampling.Logical_Size (Logical_W),
                                 DG.Physical_Extent (s.bufferW));
                           begin
                              if MX.Valid then
                                 if Desktop_Timing_Policy.Enabled then Sampled := True; end if;
                                 declare
                                    Source : Unsigned_32 with Import, Address => s.bufferAddr +
                                      Storage_Offset (Natural (MY.Index) * s.bufferPitch + Natural (MX.Index) * 4);
                                    Target : Unsigned_32 with Import, Address => O.Buffer +
                                      Storage_Offset (Y * O.Pitch + X * 4);
                                 begin Target := Source; end;
                              end if;
                           end;
                        end loop;
                     end if;
                  end;
               end loop;
               if Sampled then noteClientDraw (S); end if;
               return;
            end if;
            -- Synchronous memory bridge. The proved sampler bounds each source
            -- coordinate; mapped source/target authority remains an FFI assumption.
            for Y in Natural (Damage.Top) .. Natural (Damage.Bottom) - 1 loop
               for X in Natural (Damage.Left) .. Natural (Damage.Right) - 1 loop
                  declare
                     M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                       (O.Geometry, (DG.Pixel_Index (X), DG.Pixel_Index (Y)), Surface_Bounds,
                        DG.Physical_Extent (s.bufferW), DG.Physical_Extent (s.bufferH));
                  begin
                     if M.Valid then
                        if Desktop_Timing_Policy.Enabled then Sampled := True; end if;
                        declare
                           Source : Unsigned_32 with Import, Address => s.bufferAddr +
                             Storage_Offset (Natural (M.Y) * s.bufferPitch + Natural (M.X) * 4);
                           Target : Unsigned_32 with Import, Address => O.Buffer +
                             Storage_Offset (Y * O.Pitch + X * 4);
                        begin Target := Source; end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         if Sampled then noteClientDraw (S); end if;
         return;
      end if;

      if Logical_W /= S.bufferW or else Logical_H /= S.bufferH then
         --  Also preserve density-correct software drawing if a retained
         --  publication reaches the legacy unit-scale target.
         declare
            Screen : constant DG.Output :=
              (DG.Physical_Extent (fbWidth), DG.Physical_Extent (fbHeight),
               DG.Unrotated, (1, 1), 0, 0);
            Bounds : constant DG.Logical_Rectangle :=
              (DG.Logical_Coordinate (X), DG.Logical_Coordinate (Y),
               DG.Logical_Coordinate (X + Logical_W), DG.Logical_Coordinate (Y + Logical_H));
         begin
            for Row in P.Target_Y .. P.Target_Y + P.Height - 1 loop
               for Column in P.Target_X .. P.Target_X + P.Width - 1 loop
                  declare
                     M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                       (Screen, (DG.Pixel_Index (Column), DG.Pixel_Index (Row)), Bounds,
                        DG.Physical_Extent (S.bufferW), DG.Physical_Extent (S.bufferH));
                  begin
                     if M.Valid then
                        if Desktop_Timing_Policy.Enabled then Sampled := True; end if;
                        declare
                           Source : Unsigned_32 with Import, Address => S.bufferAddr +
                             Storage_Offset (Natural (M.Y) * S.bufferPitch + Natural (M.X) * 4);
                           Target : Unsigned_32 with Import, Address => backBufferAddr +
                             Storage_Offset (Row * fbPitch + Column * 4);
                        begin Target := Source; end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         if Sampled then noteClientDraw (S); end if;
         return;
      end if;

      --  Preserve the existing opaque BGRA row-copy renderer. The SPARK plan
      --  bounds both images and clips while preserving source coordinates.
      for Row in 0 .. P.Height - 1 loop
         declare
            ignore : System.Address;
         begin
            ignore := memcpy
              (backBufferAddr + Storage_Offset
                 ((P.Target_Y + Row) * fbPitch + P.Target_X * 4),
               s.bufferAddr + Storage_Offset
                 ((P.Source_Y + Row) * s.bufferPitch + P.Source_X * 4),
               Storage_Count (P.Width * 4));
         end;
      end loop;
      noteClientDraw (S);
   end drawClientBuffer;

   procedure restoreCursorOverlay is
   begin
      if outputDrainRequested or else outputReopenPending then cursorSaveValid := False; return; end if;
      if not cursorSaveValid then
         return;
      end if;
      if nativeScene then
         flushBackBufferRect (cursorSaveRect);
         cursorSaveValid := False;
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
      if outputDrainRequested or else outputReopenPending or else isEmpty (r) then
         return;
      end if;
      --  A hardware cursor plane shows the pointer: frames never contain it.
      if Desktop_Pointer_Plane.Composed_Hardware then
         return;
      end if;

      if nativeOutputPass then
         declare
            O : Output_Presentation renames presentations (activeOutput);
            Damage : constant DG.Physical_Rectangle := physicalClip (r);
            Shape : constant DG.Logical_Rectangle := cursorShape;
            Drawn, Must_Restart : Boolean;
         begin
            if Damage.Left >= Damage.Right or else Damage.Top >= Damage.Bottom then return; end if;
            -- Cursor styles share one immutable atlas image that outlives every
            -- frame. Keep the cursor in scene order through the renderer
            -- facade; the CPU loop remains the known-quiescent software path.
            Desktop_Compositor.Draw_Cursor
              ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
               Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height),
               O.Geometry, asset, Shape, Damage, activeOutput = 1, Drawn, Must_Restart);
            if Must_Restart then exitCompositor (1); end if;
            if Drawn then return; end if;
            for Y in Natural (Damage.Top) .. Natural (Damage.Bottom) - 1 loop
               for X in Natural (Damage.Left) .. Natural (Damage.Right) - 1 loop
                  declare
                     M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                       (O.Geometry, (DG.Pixel_Index (X), DG.Pixel_Index (Y)), Shape,
                        DG.Physical_Extent (metadata.Width), DG.Physical_Extent (metadata.Height));
                  begin
                     if M.Valid then
                        declare
                           Target : Unsigned_32 with Import, Address => O.Buffer + Storage_Offset (Y * O.Pitch + X * 4);
                           Source : constant Unsigned_32 := Desktop_Cursors.Pixels
                             (metadata.Offset + Natural (M.Y) * metadata.Width + Natural (M.X));
                        begin Target := premultipliedPixelOver (Source, Target); end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         return;
      elsif nativeScene then
         cursorSaveRect := r;
         cursorSaveValid := True;
         flushBackBufferRect (r);
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
   begin
      cursorPresentPending := False;
   end noteCursorPresented;

   procedure presentCursorOverlay is
      Started : constant Unsigned_64 := timingNow;
   begin
      if outputDrainRequested or else outputReopenPending then return; end if;
      -- Both helpers queue their exact written footprints. Keep them separate
      -- so a pointer jump does not copy the unchanged rectangle between them.
      --  Switch between plane and composite here, where both pointer
      --  footprints join the frame damage, so the tagged frame shows it.
      Desktop_Pointer_Plane.Note_Composed;
      restoreCursorOverlay;
      drawCursorOverlay;
      noteCursorPresented;
      noteTiming (Scene_Draw, Started);
   end presentCursorOverlay;

   uploadedCursor : Desktop_Cursors.Cursor_ID := Desktop_Cursors.Arrow;
   --  The first shape has been handed to the plane (it submits it, and any
   --  later one, without waiting).
   pointerShapeSent : Boolean := False;

   --  Hand the current shape and position to the pointer plane request.
   procedure updatePointerPlane is
      use type Desktop_Cursors.Cursor_ID;
      Asset : constant Desktop_Cursors.Cursor_ID := cursorAsset;
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Asset);
   begin
      if not Desktop_Pointer_Plane.Active then return; end if;
      if not pointerShapeSent or else Asset /= uploadedCursor then
         Desktop_Pointer_Plane.Set_Shape
           (Desktop_Cursors.Pixels (M.Offset)'Address, M.Width, M.Height,
            M.Hotspot_X, M.Hotspot_Y, requestSequence);
         uploadedCursor := Asset;
         pointerShapeSent := True;
      end if;
      Desktop_Pointer_Plane.Move (cursorX, cursorY, requestSequence);
   end updatePointerPlane;

   --  Declare every output's place in pointer space (logical coordinates;
   --  only unscaled, unrotated outputs can show a hardware pointer).
   procedure syncPointerPlane is
      use type DG.Scale_Component, DG.Orientation;
   begin
      Desktop_Pointer_Plane.Start;
      for I in 1 .. desktopLayout.Count loop
         declare
            G : DG.Output renames desktopLayout.Items (I).Geometry;
         begin
            Desktop_Pointer_Plane.Place
              (I - 1, Integer (G.X), Integer (G.Y),
               G.Scale.Numerator = G.Scale.Denominator and then
               G.Rotation = DG.Unrotated);
         end;
      end loop;
      updatePointerPlane;
   end syncPointerPlane;

   procedure scheduleCursorPresent is
   begin
      updatePointerPlane;
      --  A hardware pointer needs no frame unless a composited one must be
      --  erased (presentCursorOverlay restores only a drawn footprint).
      cursorPresentPending := True;
   end scheduleCursorPresent;

   procedure flushCursorPresent is
   begin
      if Desktop_Pointer_Plane.Take_Change then
         cursorPresentPending := True;
      end if;
      if cursorPresentPending and then not outputReopenPending and then not shutdownRequested then
         presentCursorOverlay;
      end if;
   end flushCursorPresent;

   function tryFastClientRedraw (dirty : Rect) return Boolean is
      r : constant Rect := clampRect (dirty);
      c : Rect;
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
            surfaces (i).bufferAddr /= System.Null_Address and then
            surfaces (i).bufferFormat = PIXEL_FORMAT_BGRA8888 and then
            surfaces (i).bufferW > 0 and then surfaces (i).bufferH > 0 and then
            surfaces (i).bufferW <= surfaces (i).bufferPitch / 4 and then
            (surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            c := clientRect (surfaces (i));
            -- A partial/smaller source cannot replace all client pixels;
            -- route it through drawWindow's complete background fill instead.
            if rectContains (c, r) and then
              (if surfaces (i).publicationMode then surfaces (i).bufferLogicalW else surfaces (i).bufferW) >= c.w and then
              (if surfaces (i).publicationMode then surfaces (i).bufferLogicalH else surfaces (i).bufferH) >= c.h
            then
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
                  declare Draw_Started : constant Unsigned_64 := timingNow;
                  begin
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
                  noteCursorPresented;

                  flushBackBufferRect (r);
                  noteTiming (Scene_Draw, Draw_Started);
                  return True;
                  end;
               end if;
            end if;
         end if;
      end loop;

      return False;
   end tryFastClientRedraw;

   -- Settings uses logical layout and clip coordinates; all control pixels
   -- are emitted into the currently owned physical output writer.
   procedure settingsFill (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Fill : CuBit.UI.Color) is
      P : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, R);
   begin
      fillRect (P.x, P.y, P.w, P.h, Fill);
   end settingsFill;

   procedure settingsStroke
     (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; Light, Dark : CuBit.UI.Color) is
   begin
      if R.w < 2 or else R.h < 2 then return; end if;
      settingsFill (C, (R.x, R.y, R.w, 1), Light);
      settingsFill (C, (R.x, R.y, 1, R.h), Light);
      settingsFill (C, (R.x, R.y + R.h - 1, R.w, 1), Dark);
      settingsFill (C, (R.x + R.w - 1, R.y, 1, R.h), Dark);
   end settingsStroke;

   procedure settingsGradient
     (C : CuBit.UI.Canvas; R : CuBit.UI.Rect; TopColor, BottomColor : CuBit.UI.Color)
   is
      P : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, R);
   begin
      if P.w = 0 or else P.h = 0 then return; end if;
      if R.h = 1 then settingsFill (C, R, TopColor); return; end if;
      -- Preserve the original gradient origin. Equal-color rows form one
      -- fill, bounding a complete gradient to at most 256 backend operations.
      declare
         Y : Natural := P.y;
         Last : Natural;
      begin
         while Y < P.y + P.h loop
            Last := Natural'Min (P.y + P.h - 1,
              R.y + Compositor_Gradient.Color_Run_Last
                (TopColor, BottomColor, Y - R.y, R.h));
            settingsFill (C, (P.x, Y, P.w, Last - Y + 1),
              Compositor_Gradient.At_Row (TopColor, BottomColor, Y - R.y, R.h));
            Y := Last + 1;
         end loop;
      end;
   end settingsGradient;

   procedure settingsText
     (C : CuBit.UI.Canvas; X, Y : Natural; Text : String; FG, BG : CuBit.UI.Color)
   is
      Saved_Enabled : constant Boolean := clipEnabled;
      Saved_Clip : constant Rect := clipRect;
      P : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, (0, 0, C.width, C.height));
   begin
      clipEnabled := True;
      clipRect := (P.x, P.y, P.w, P.h);
      drawUIText (X, Y, Text, FG, BG);
      clipEnabled := Saved_Enabled;
      clipRect := Saved_Clip;
   end settingsText;

   package Settings_Controls is new CuBit.UI.Control_Renderer
     (settingsFill, settingsStroke, settingsText);

   procedure settingsWallpaper
     (C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect; Style : CuBit.Appearance.Preferences)
   is
      P : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, Bounds);
      O : Output_Presentation renames presentations (activeOutput);
      Drawn, Must_Restart : Boolean;
      Shown : CuBit.Appearance.Preferences;
   begin
      if P.w = 0 or else P.h = 0 then return; end if;
      Desktop_Wallpaper_Assets.Resolve (Style, Shown);
      if Desktop_Compositor.Full_Output then
         Desktop_Compositor.Draw_Preview
           ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
            Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height),
            (DG.Logical_Coordinate (Bounds.x), DG.Logical_Coordinate (Bounds.y),
             DG.Logical_Coordinate (Bounds.x + Bounds.w), DG.Logical_Coordinate (Bounds.y + Bounds.h)),
            physicalClip ((P.x, P.y, P.w, P.h)), Shown, activeOutput = 1, Drawn, Must_Restart);
         if Must_Restart then exitCompositor (1); end if;
         if Drawn then return; end if;
      end if;
      Desktop_Wallpaper.Paint_Output
        (O.Buffer, O.Pitch, O.Geometry,
         (DG.Logical_Coordinate (Bounds.x), DG.Logical_Coordinate (Bounds.y),
          DG.Logical_Coordinate (Bounds.x + Bounds.w), DG.Logical_Coordinate (Bounds.y + Bounds.h)),
         physicalClip ((P.x, P.y, P.w, P.h)), Shown);
   end settingsWallpaper;

   procedure renderSettings is new Desktop_Settings.Render
     (settingsFill, settingsStroke, settingsGradient, settingsText,
      Settings_Controls.Draw_Button, Settings_Controls.Draw_Tab, settingsWallpaper);

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
               C : constant CuBit.UI.Canvas := CuBit.UI.With_Clip
                 ((addr => (if nativeOutputPass then System.Null_Address else backBufferAddr),
                   width => fbWidth, height => fbHeight, pitch => fbPitch,
                   clipEnabled => True,
                   clip => (if clipEnabled then (clipRect.x, clipRect.y, clipRect.w, clipRect.h)
                            else (0, 0, fbWidth, fbHeight)), others => <>),
                  (bounds.x, bounds.y, bounds.w, bounds.h));
            begin
               if nativeOutputPass then
                  renderSettings (settingsView, C, (bounds.x, bounds.y, bounds.w, bounds.h));
               else
                  Desktop_Wallpaper_Assets.Prepare (settingsView.Pending.Backdrop);
                  Desktop_Settings.Draw (settingsView, C, (bounds.x, bounds.y, bounds.w, bounds.h));
               end if;
            end;
         when others =>
            if s.bufferAttached then
               declare
                  c : constant Rect := clientRect (frame);
               begin
                  if not isEmpty (c) then
                     if (if S.publicationMode then S.bufferLogicalW else S.bufferW) < C.w or else
                       (if S.publicationMode then S.bufferLogicalH else S.bufferH) < C.h
                     then
                        fillRect (c.x, c.y, c.w, c.h, C_WIN);
                     end if;
                     drawClientBuffer (s, c.x, c.y, c.w, c.h);
                  end if;
               end;
            -- Before the first client buffer, retain the neutral window fill.
            end if;
      end case;
   end drawWindow;

   --  Where a taskbar button's label goes: its line box centred in the
   --  button, the same for the Apps button and every window button, so the
   --  baselines line up.
   function taskbarTextY (r : Rect) return Natural is
     (r.y + (if r.h > CuBit.Fonts.Line_Height then (r.h - CuBit.Fonts.Line_Height) / 2 else 0));

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
               drawSurfaceTitle (surfaces (i), r.x + 10, taskbarTextY (r),
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

   --  A category's icon in the Apps menu.
   function categoryIcon (group : Desktop_Launch.Category) return Desktop_Icons.Icon_ID is
     (case group is
         when CCL.Interfaces.Desktop_Launch.System => Desktop_Icons.Category_System,
         when CCL.Interfaces.Desktop_Launch.Development => Desktop_Icons.Category_Development,
         when CCL.Interfaces.Desktop_Launch.Web => Desktop_Icons.Category_Web,
         when CCL.Interfaces.Desktop_Launch.Games => Desktop_Icons.Category_Games,
         when CCL.Interfaces.Desktop_Launch.Media => Desktop_Icons.Category_Media,
         when CCL.Interfaces.Desktop_Launch.Tools => Desktop_Icons.Category_Tools);

   procedure drawLaunchMenu is
      r : constant Rect := launchMenuRect;

      procedure drawRow
         (item     : Rect;
          selected : Boolean;
          icon     : Desktop_Icons.Icon_ID;
          label    : String;
          fg       : Unsigned_32;
          arrow    : Boolean)
      is
         bg : constant Unsigned_32 :=
           (if selected then C_ACCENT else C_PANEL);
         textColor : constant Unsigned_32 :=
           (if selected then C_WHITE else fg);
      begin
         if isEmpty (item) then
            return;
         end if;
         if selected then
            fillRect (item.x, item.y, item.w, item.h, bg);
         end if;
         drawIcon (icon, item.x + 8, item.y + 3, bg);
         drawUIText (item.x + 40, item.y + 7, label, textColor, bg);
         if arrow and then item.w > 24 then
            drawUIText (item.x + item.w - 18, item.y + 7, ">", textColor, bg);
         end if;
      end drawRow;
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
      for row in 1 .. launchRows loop
         drawRow
           (launchItemRect (row),
            launchMenuState.Row = row and then not launchMenuState.In_Submenu,
            categoryIcon (LM.Category_Of (launchMenu, row)),
            LM.Category_Name (LM.Category_Of (launchMenu, row)),
            (if launchMenuState.Open = row then C_ACCENT else C_TEXT), True);
      end loop;
      declare
         sep : constant Rect := launchSeparatorRect;
      begin
         fillRect (sep.x, sep.y, sep.w, sep.h, C_EDGE);
      end;
      drawRow (launchItemRect (launchPowerRow), False, Desktop_Icons.Power, "Power", C_MUTED, False);

      --  The open category's submenu, in the same look.
      declare
         sub : constant Rect := launchSubmenuRect;
      begin
         if not isEmpty (sub) then
            drawDappledShadow (sub.x, sub.y, sub.w, sub.h);
            fillRect (sub.x, sub.y, sub.w, sub.h, C_PANEL);
            strokeRect (sub.x, sub.y, sub.w, sub.h, C_EDGE, C_SHADOW);
            for index in 1 .. LM.Items (launchMenu, launchMenuState.Open) loop
               declare
                  entryIndex : constant Natural := LM.Entry_Of (launchMenu, launchMenuState.Open, index);
               begin
                  if entryIndex in 1 .. launchMenu.Count then
                     drawRow
                       (launchSubItemRect (index),
                        launchMenuState.In_Submenu and then launchMenuState.Item = index,
                        launchMenu.Entries (entryIndex).Icon,
                        Desktop_Launch.Label_Of (launchMenu.Entries (entryIndex)), C_TEXT, False);
                  end if;
               end;
            end loop;
         end if;
      end;
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
      launchTextY : constant Natural := taskbarTextY (launch);
   begin
      if fbWidth = 0 or else fbHeight = 0 then
         return;
      end if;

      --  First shell renderer: deliberately Win95-simple. The compositor owns
      --  pixels for now; the shell owns policy and talks through the protocol.
      --  Shared client buffers can replace this drawing path later without
      --  changing the surface/session shape.
      drawWallpaper (Cull_Windows => True);
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

      -- A quiescent source-over failure can leave partial text in this
      -- clip. Reconstruct it from scene state before any cursor/presentation.
      -- The backend disables itself on that failure, so the second pass uses
      -- the software fallback and cannot request another text retry.
      for Pass in Compositor_Text.Attempt loop
         textSceneRetry := False;
         if shellVisible then drawDesktopShell; else drawWallpaper; end if;
         drawDragOutline;
         case Compositor_Text.Finish (Pass, textSceneRetry) is
            when Compositor_Text.Complete => exit;
            when Compositor_Text.Replay => null;
            when Compositor_Text.Restart => exitCompositor (1);
         end case;
      end loop;
   end drawCurrentScene;

   procedure renderOutput (Output : Output_Index; Area : Rect) is
      Saved_Clip : constant Rect := clipRect;
      Saved_Enabled : constant Boolean := clipEnabled;
      Saved_Excluded : constant Unsigned_64 := compositionExcludedSurface;
      Started : constant Unsigned_64 := timingNow;
      Start_Ms : constant Unsigned_64 := nowMs;
      Pixels : constant Unsigned_64 := Unsigned_64 (Area.w) * Unsigned_64 (Area.h);
   begin
      if isEmpty (Area) then return; end if;
      activeOutput := Output;
      outputDamage := (DG.Pixel_Edge (Area.x), DG.Pixel_Edge (Area.y),
        DG.Pixel_Edge (Area.x + Area.w), DG.Pixel_Edge (Area.y + Area.h));
      nativeOutputPass := True;
      clipEnabled := False;
      compositionExcludedSurface := 0;
      if Bootstrap_CPU then
         fillRect (0, 0, fbWidth, fbHeight, 16#00203648#, False);
         drawUIText (24, 32, "CuBit Desktop reached", 16#00FFFFFF#, 16#00203648#);
         drawUIText (24, 68, "Preparing renderer startup", 16#00FFFFFF#, 16#00203648#);
         drawUIText (24, 104, "If this remains visible, photograph this screen.", 16#00FFFFFF#, 16#00203648#);
      else
         drawCurrentScene;
         drawCursorOverlay;
      end if;
      nativeOutputPass := False;
      clipRect := Saved_Clip; clipEnabled := Saved_Enabled;
      compositionExcludedSurface := Saved_Excluded;
      statsScenePixels := Unsigned_64'Min (statsScenePixels, Unsigned_64'Last - Pixels) + Pixels;
      declare Finished : constant Unsigned_64 := nowMs; begin
         if Start_Ms /= Unsigned_64'Last and then Finished /= Unsigned_64'Last and then Finished >= Start_Ms then
            statsDrawMs := statsDrawMs + (Finished - Start_Ms);
         end if;
      end;
      noteTiming (Scene_Draw, Started);
   end renderOutput;

   procedure repairDirectWriter is
      P : Output_Presentation renames presentations (primaryOutput);
      Areas : Compositor_Damage.State;
      C : constant Rect := cursorRect;
      Upcoming : constant Rect := clampRect (frameDamage);
      Guaranteed_Draw : constant Boolean := framePending and then
        not dragPresentedValid and then not dragPreviewValid;
      Started : constant Unsigned_64 := timingNow;
   begin
      if not BP.Writable (P.Pool, BP.Writer (P.Pool)) then exitCompositor (1); end if;
      backBufferAddr := P.Buffer;
      -- Repairing the reused target also removes its old cursor, but Display
      -- updates only submitted damage. Preserve the previous overlay footprint
      -- before drawCursorOverlay replaces it, even when its saved underlay
      -- belongs to the previously submitted slot and is no longer writable.
      addOutputDamage (P.Damage, cursorSaveRect);
      if not isEmpty (cursorSaveRect) then
         RP.Invalidate (P.Repaint,
           (cursorSaveRect.x, cursorSaveRect.y,
            cursorSaveRect.x + cursorSaveRect.w,
            cursorSaveRect.y + cursorSaveRect.h));
      end if;
      cursorSaveValid := False;
      RP.Take (P.Repaint, BP.Writer (P.Pool).Buffer, Areas);
      -- Retained slots may contain a prior cursor. Always repaint the current
      -- cursor footprint before saving its clean underlay and blending once.
      addOutputDamage (Areas, C);
      repairingTarget := True;
      drawingBackBuffer := True;
      for I in 1 .. Compositor_Damage.Count (Areas) loop
         -- flushFrame follows without input dispatch or submission between.
         -- It redraws Upcoming completely except in the excluded drag paths.
         -- Keep the current cursor footprint clean before saving its underlay.
         for Region of RP.Before_Draw
           (Compositor_Damage.Item (Areas, I),
            (Upcoming.x, Upcoming.y, Upcoming.x + Upcoming.w, Upcoming.y + Upcoming.h),
            (C.x, C.y, C.x + C.w, C.y + C.h), Guaranteed_Draw)
         loop
            clipRect := damageRectangle (Region);
            if not isEmpty (clipRect) then
               declare Pixels : constant Unsigned_64 := Unsigned_64 (clipRect.w) * Unsigned_64 (clipRect.h);
               begin
                  statsRepairPixels := Unsigned_64'Min (statsRepairPixels, Unsigned_64'Last - Pixels) + Pixels;
               end;
               clipEnabled := True;
               drawCurrentScene;
            end if;
         end loop;
      end loop;
      clipEnabled := False;
      drawCursorOverlay;
      drawingBackBuffer := False;
      repairingTarget := False;
      noteTiming (Scene_Draw, Started);
   end repairDirectWriter;

   procedure redraw is
   begin
      if fbBpp /= 32 then
         return;
      end if;

      if nativeScene then
         restoreCursorOverlay;
         drawCursorOverlay;
         flushBackBufferRect ((0, 0, fbWidth, fbHeight));
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

      if nativeScene then
         restoreCursorOverlay;
         drawCursorOverlay;
         flushBackBufferRect (r);
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
         flushBackBufferRect (r);
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
      if nativeScene then return; end if;
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
      textSceneRetry := False;
      drawWindow (surfaces (SurfaceIndex (idx)));
      --  The retained layer can outlive a minute boundary or media-key press.
      --  Composite current taskbar state over it, within this damage clip.
      drawStatus;
      drawAudioPopup;
      if textSceneRetry then drawCurrentScene; end if;
      clipEnabled := False;
      drawCursorOverlay;
      drawingBackBuffer := False;
      flushBackBufferRect (r);

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
      if dragMode = DRAG_MOVE and then dragPresentedValid and then
        dragPresentedRect /= dragPreviewRect
      then
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
         if dragMode /= DRAG_MOVE and then dragPresentedValid then
            Add_Outline (dragPresentedRect);
         end if;
         if dragPreviewValid then
            Add_Outline (dragPreviewRect);
         end if;
      end if;

      if fbBpp /= 32 or else count = 0 then
         return;
      end if;

      if nativeScene then
         restoreCursorOverlay;
         drawCursorOverlay;
         Present_Regions;
         for I in 1 .. count loop
            pixelCount := pixelCount + Unsigned_64 (regions (I).w) * Unsigned_64 (regions (I).h);
         end loop;
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
   begin
      framePending := True;
      frameDamage := (x => 0, y => 0, w => fbWidth, h => fbHeight);
   end scheduleRedraw;

   procedure scheduleRedrawRect (dirty : Rect) is
      r : constant Rect := clampRect (dirty);
   begin
      if isEmpty (r) then
         return;
      end if;

      if framePending then
         frameDamage := unionRect (frameDamage, r);
      else
         frameDamage := r;
         framePending := True;
      end if;
   end scheduleRedrawRect;

   --  Every way a move or resize ends (release, cancel, resynchronization,
   --  surface destruction) goes through here. The outline or window that the
   --  last frame showed, and any newer preview, is scene damage: forgetting
   --  the drag state first would leave that outline on screen for good.
   procedure endDrag is
   begin
      if dragPresentedValid then
         scheduleRedrawRect (windowVisualRect (dragPresentedRect));
      end if;
      if dragPreviewValid then
         scheduleRedrawRect (windowVisualRect (dragPreviewRect));
      end if;
      dragPreviewValid := False;
      dragPresentedValid := False;
      dragBaseReady := False;
      dragMode := DRAG_NONE;
      dragSurfaceId := 0;
   end endDrag;

   procedure clearInputForTarget (target : Unsigned_64);

   procedure reapDeadClientSurfaces (damage : in out Rect);

   procedure flushFrame is
      damage : Rect := frameDamage;
      t0 : Unsigned_64;
      t1 : Unsigned_64;
      Draw_Started : Unsigned_64;
      full : Boolean;
      damagePixels : Unsigned_64 := 0;
   begin
      if shutdownRequested or else outputDrainRequested or else outputReopenPending or else not framePending then
         return;
      end if;
      framePending := False;
      frameDamage := (others => 0);

      reapDeadClientSurfaces (damage);
      full :=
         damage.x = 0 and then damage.y = 0 and then
         damage.w = fbWidth and then damage.h = fbHeight;

      Draw_Started := timingNow;
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

      noteTiming (Scene_Draw, Draw_Started);
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
      Eligible : Focus_Policy.Candidates;
      Chosen : Focus_Policy.Selection;
   begin
      for I in surfaces'Range loop
         Eligible (I) := surfaces (I).used and then not surfaces (I).minimized and then
           (surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0;
      end loop;
      Chosen := Focus_Policy.Topmost (Eligible);
      if Chosen.Found then
         focusSurface := surfaces (Chosen.Slot).id;
         damage := unionRect (damage, inflateRect (surfaceRect (surfaces (Chosen.Slot)), 4));
         damage := unionRect (damage, inflateRect (taskButtonRect (Chosen.Slot), 4));
      else
         focusSurface := 0;
      end if;
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

   procedure damagePreviousFocus (New_Focus : Unsigned_64; Damage : in out Rect) is
      Previous : constant Integer := findSurface (focusSurface);
   begin
      if focusSurface = New_Focus or else Previous < 0 then return; end if;
      declare
         S : Surface renames surfaces (SurfaceIndex (Previous));
      begin
         if S.minimized then return; end if;
         -- Focus changes both titlebars, even when the windows do not overlap.
         -- Include the complete title and frame, not just the clicked region.
         Damage := unionRect (Damage, inflateRect
           ((S.x, S.y, S.w, Natural'Min (S.h, CLIENT_INSET_TOP)), 4));
         Damage := unionRect
           (Damage, inflateRect (taskButtonRect (SurfaceIndex (Previous)), 4));
      end;
   end damagePreviousFocus;

   procedure focusAndRaiseSurface
      (idx    : SurfaceIndex;
       damage : in out Rect)
   is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      oldTask   : constant Rect := taskButtonRect (idx);
      id        : constant Unsigned_64 := surfaces (idx).id;
      raisedIdx : Integer;
      Already_Top : Boolean := True;
   begin
      if idx < surfaces'Last then
         for I in idx + 1 .. surfaces'Last loop
            if surfaces (I).used and then
              (surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0
            then Already_Top := False; exit; end if;
         end loop;
      end if;
      -- A press on the already-focused top window changes no visible state.
      -- Do not put a whole-window repaint between physical click edges.
      if focusSurface = id and then Already_Top then return; end if;
      damagePreviousFocus (id, damage);
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
      damagePreviousFocus (surfaces (idx).id, damage);
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
      damagePreviousFocus (surfaces (idx).id, damage);
      focusSurface := surfaces (idx).id;
      newBounds := surfaceRect (surfaces (idx));

      if surfaces (idx).owner /= No_Process then
         queueConfigure (surfaces (idx).id,
                         Unsigned_64 (newBounds.w),
                         Unsigned_64 (newBounds.h));
      end if;

      damage := unionRect
        (damage, inflateRect (unionRect (oldBounds, newBounds), 4));
      damage := unionRect
        (damage, inflateRect (taskbarRect, 2));
   end toggleMaximizeSurface;

   -- Actual acquisitions outlive surface records. Reserve bounded metadata
   -- before entering the kernel; never lose a loan because a window closes.
   procedure acquireSourceLoan
     (Grant : MG.Grant_Reference; Owner : Process_ID; Bytes : Unsigned_64;
      Address : out System.Address; Loan : out Source_Loans.Ticket;
      Result : out DP.Status_Code)
   is
      Acquired : Boolean;
   begin
      Address := System.Null_Address;
      Source_Loans.Reserve (sourceLoanPolicy, Loan);
      if Loan = Source_Loans.No_Ticket then
         Result := DP.Resources_Exhausted;
         return;
      end if;
      MG.Acquire (Grant, Owner, 0, Bytes, MG.Read_Access, Address, Acquired);
      if not Acquired then
         Source_Loans.Cancel (sourceLoanPolicy, Loan);
         Loan := Source_Loans.No_Ticket;
         Result := DP.Denied;
         return;
      end if;
      sourceLoans (Source_Loans.Index (Loan)) := (Grant, Address);
      Source_Loans.Activate (sourceLoanPolicy, Loan);
      Result := DP.Success;
   end acquireSourceLoan;

   procedure pumpSourceLoan (Loan : Source_Loans.Ticket) is
      Renderer : Desktop_Compositor.Source_Release;
      Confirmed : Boolean;
   begin
      if Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Renderer_Pending then
         Desktop_Compositor.Forget_Source
           (sourceLoans (Source_Loans.Index (Loan)).Address, Renderer);
         Source_Loans.Observe_Renderer
           (sourceLoanPolicy, Loan,
            (case Renderer is
               when Desktop_Compositor.Source_Retired => Source_Loans.Retired,
               when Desktop_Compositor.Source_Busy => Source_Loans.Busy,
               when Desktop_Compositor.Source_Unsafe => Source_Loans.Uncertain));
      end if;
      if Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Grant_Pending then
         MG.Return_Acquisition (sourceLoans (Source_Loans.Index (Loan)).Grant, Confirmed);
         Source_Loans.Observe_Grant (sourceLoanPolicy, Loan, Confirmed);
         if Confirmed then
            sourceLoans (Source_Loans.Index (Loan)) := (others => <>);
         end if;
      end if;
      if Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Quarantined then
         debugPrint ("desktop: source retirement uncertain; restart required" & LF);
         exitCompositor (1);
      end if;
   end pumpSourceLoan;

   procedure retireSourceLoan (Loan : Source_Loans.Ticket) is
   begin
      if Source_Loans.Current (sourceLoanPolicy, Loan) in
        Source_Loans.Attached | Source_Loans.Renderer_Pending | Source_Loans.Grant_Pending
      then
         Source_Loans.Retire (sourceLoanPolicy, Loan);
         pumpSourceLoan (Loan);
      end if;
   end retireSourceLoan;

   procedure pumpSourceRetirements is
   begin
      for I in Source_Loans.Slot loop
         pumpSourceLoan (Source_Loans.At_Slot (sourceLoanPolicy, I));
      end loop;
   end pumpSourceRetirements;

   function sourceRetirementPending return Boolean is
   begin
      for I in Source_Loans.Slot loop
         if Source_Loans.Current (sourceLoanPolicy, Source_Loans.At_Slot (sourceLoanPolicy, I)) in
           Source_Loans.Renderer_Pending | Source_Loans.Grant_Pending
         then return True; end if;
      end loop;
      return False;
   end sourceRetirementPending;

   procedure retirePublicationBuffer
     (S : in out Surface; I : Surface_Policy.Slot)
   is
      Epoch : constant Surface_Policy.Generation := S.publicationPolicy.Buffers (I).Epoch;
      Ticket : constant Natural := S.publicationPolicy.Buffers (I).Ticket;
   begin
      if S.publicationPolicy.Buffers (I).Status /= Surface_Policy.Retiring then
         return;
      end if;
      if S.publicationBuffers (I).Acquired then
         retireSourceLoan (S.publicationBuffers (I).Loan);
         if Source_Loans.Current (sourceLoanPolicy, S.publicationBuffers (I).Loan) /=
           Source_Loans.Released
         then return; end if;
      end if;
      Surface_Policy.Retire (S.publicationPolicy, I, Ticket, True);
      S.publicationBuffers (I).Acquired := False;
      S.publicationBuffers (I).Loan := Source_Loans.No_Ticket;
      S.publicationBuffers (I).Address := System.Null_Address;
      S.publicationBuffers (I).Retired :=
        (DP.Success, Publication.Identity (Epoch), Publication.Identity (Ticket));
   end retirePublicationBuffer;

   -- New pixels behind S (any buffer/attachment change or client present).
   -- Rows are source rows changed since the previous version; the renderer
   -- copies only those into its persistent image of this surface.
   procedure noteContentChange
     (S : in out Surface; Rows : Compositor_Source_Content.Row_Band)
   is
      use type Compositor_Source_Content.Content_Version;
   begin
      if S.contentVersion < Compositor_Source_Content.Content_Version'Last then
         S.contentVersion := S.contentVersion + 1;
      else
         -- Versions never wrap; 2**64 changes of one surface cannot occur.
         return;
      end if;
      Desktop_Compositor.Note_Source_Change (Compositor_Source_Content.Source_Key (S.id), Rows);
   end noteContentChange;

   function bufferRows (Height : Natural) return Compositor_Source_Content.Row_Band is
     (Compositor_Source_Content.Whole
        (Natural'Min (Height, Compositor_Source_Content.Maximum_Rows)));

   -- The surface record is about to be destroyed: its persistent renderer
   -- image is released once no frame reads it.
   procedure retireSurfaceSource (S : Surface) is
   begin
      if S.id /= 0 then
         Desktop_Compositor.Retire_Source (Compositor_Source_Content.Source_Key (S.id));
      end if;
   end retireSurfaceSource;

   procedure releaseSurfaceBuffer (S : in out Surface) is
   begin
      if S.publicationMode then
         Surface_Policy.Close (S.publicationPolicy);
         for I in Surface_Policy.Slot loop
            retirePublicationBuffer (S, I);
         end loop;
      elsif S.bufferAttached then
         retireSourceLoan (S.bufferLoan);
      end if;
      -- Only visible aliases are cleared. The global table owns every pending
      -- acquisition even when this entire Surface record is destroyed/reused.
      S.bufferAttached := False;
      S.bufferLoan := Source_Loans.No_Ticket;
      S.bufferAddr := System.Null_Address;
      S.bufferW := 0; S.bufferH := 0; S.bufferPitch := 0; S.bufferFormat := 0;
      S.bufferLogicalW := 0; S.bufferLogicalH := 0;
   end releaseSurfaceBuffer;

   procedure requestClose (target : Unsigned_64);

   procedure closeSurface (idx : SurfaceIndex; damage : in out Rect) is
      oldBounds : constant Rect := surfaceRect (surfaces (idx));
      oldId     : constant Unsigned_64 := surfaces (idx).id;
      oldOwner  : constant Process_ID := surfaces (idx).owner;
      button    : constant Rect := taskButtonRect (idx);
      ignore    : Unsigned_64;
   begin
      if oldOwner /= No_Process and then
        (surfaces (idx).windowFlags and DP.Window_Feature'Enum_Rep (DP.Graceful_Close)) /= 0
      then
         -- The client owns the decision and closes only this surface through
         -- Destroy_Surface. Keep its buffer and siblings alive until then.
         requestClose (oldId);
         return;
      end if;
      -- Internal windows close immediately; non-opted-in clients retain their
      -- existing process-termination behavior.
      if pointerSurfaceId = oldId then
         pointerSurfaceId := 0;
      end if;
      if dragSurfaceId = oldId then
         endDrag;
      end if;
      clearInputForTarget (oldId);

      releaseSurfaceBuffer (surfaces (idx));
      retireSurfaceSource (surfaces (idx));
      surfaces (idx) := (others => <>);

      if focusSurface = oldId then
         focusSurface := 0;
      end if;

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));

      if focusSurface = 0 then
         focusTopmostVisibleWindow (damage);
      end if;

      if oldOwner /= No_Process and then processAlive (oldOwner) then
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
            surfaces (i).owner /= No_Process and then
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
               endDrag;
            end if;
            clearInputForTarget (surfaces (i).id);
            if focusSurface = surfaces (i).id then
               focusSurface := 0;
            end if;
            if surfaces (i).bufferAttached or else surfaces (i).publicationMode then
               releaseSurfaceBuffer (surfaces (i));
               debugPrint ("desktop: dead client buffer retirement requested" & LF);
            end if;
            retireSurfaceSource (surfaces (i));
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
      Work_Area : constant Rect := windowWorkArea (primaryBounds);
   begin
      for i in surfaces'Range loop
         if surfaces (i).used and then
            surfaces (i).owner = No_Process and then
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
            winW := 742;
            winH := 420;
            Desktop_Settings.Open (settingsView, appearance, desktopLayout,
              desktopLayout.Items (Natural (primaryOutput) + 1).Display);
         when others =>
            null;
      end case;

      if winW <= primaryBounds.w then
         winX := Natural'Min (winX, primaryBounds.w - winW);
      elsif winX + winW > primaryBounds.w then
         winW := Natural'Max (MIN_WIN_W, primaryBounds.w - winX);
      end if;
      if winH <= Work_Area.h then
         winY := Natural'Min (winY, Work_Area.h - winH);
      elsif winY + winH > Work_Area.h then
         winH := Natural'Max (MIN_WIN_H, Work_Area.h - Natural'Min (winY, Work_Area.h));
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
         owner  => No_Process,
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
         bufferAttached => False, bufferLoan => Source_Loans.No_Ticket,
         bufferGrant => <>,
         bufferAddr => System.Null_Address,
         bufferLogicalW => 0, bufferLogicalH => 0,
         bufferW => 0,
         bufferH => 0,
         bufferPitch => 0,
         bufferFormat => 0,
         contentVersion => Compositor_Source_Content.No_Version,
         pointerCursor => POINTER_DEFAULT,
         publicationPolicy => <>, publicationConfiguration => <>,
         publicationMode => False, publicationBuffers => <>,
         publicationInputAfter => 0);
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

   procedure requestClose (target : Unsigned_64) is
      Slot : Integer;
   begin
      ensureInputChannel (target, Slot);
      if Slot < 0 then return; end if;
      declare
         Channel : SurfaceInputChannel renames inputChannels (SurfaceIndex (Slot));
      begin
         Close_Policy.Request (Channel.pendingClose, Channel.nextSerial);
         if Channel.pendingClose = 0 then
            debugPrint ("desktop: input serials exhausted" & LF);
            exitCompositor (1);
            return;
         end if;
      end;
      completeInputWaiter (target);
   end requestClose;

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
      Result : IQ.Outcome;
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

         IQ.Push
           (queue, inputChannels (idx).nextSerial,
            (True, 0, kind, target, payload0, payload1),
            (True, 0, INPUT_RESYNC, target, snapshot.pointerPosition,
             (snapshot.buttons and 16#FFFF_FFFF#) or
               Shift_Left (snapshot.modifiers and 16#FFFF_FFFF#, 32)),
            (if Close_Policy.May_Coalesce
               (inputChannels (idx).pendingClose,
                (if IQ.Newest (queue) = -1 then 0
                 else queue (IQ.Newest (queue)).Serial)) and then
               (IQ.Newest (queue) = -1 or else
                queue (IQ.Newest (queue)).Serial > inputChannels (idx).exposedThrough)
             then INPUT_POINTER_MOVE else INPUT_NONE), Result);
         case Result is
            when IQ.Resynchronized =>
               -- Preserve explicit recovery on bounded overflow; never
               -- silently replace a key, button, wheel or configure event.
               if snapshot.generation < Unsigned_64'Last then
                  snapshot.generation := snapshot.generation + 1;
               end if;
               if inputQueueOverflows < Unsigned_64'Last then
                  inputQueueOverflows := inputQueueOverflows + 1;
               end if;
            when IQ.Exhausted =>
               -- A wrapped serial could alias an already consumed event.
               debugPrint ("desktop: input serials exhausted" & LF);
               exitCompositor (1);
            when IQ.Appended | IQ.Motion_Replaced => null;
         end case;
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
      Selected : IQ.Selection;
   begin
      found := False;
      event := (others => <>);
      if channelSlot < 0 then return; end if;
      declare
         Channel : SurfaceInputChannel renames inputChannels (SurfaceIndex (channelSlot));
         Queued_Serial : Unsigned_64 := 0;
      begin
         Close_Policy.Acknowledge (Channel.pendingClose, afterSerial);
         Selected := IQ.Oldest_After (Channel.events, afterSerial);
         if Selected /= -1 then Queued_Serial := Channel.events (Selected).Serial; end if;
         if Close_Policy.Select_Close (Channel.pendingClose, afterSerial, Queued_Serial) then
            event := (True, Channel.pendingClose,
              DP.Input_Event_Kind'Enum_Rep (DP.Close_Requested), target, 0, 0);
            found := True;
         else
            IQ.Pop (Channel.events, afterSerial, Selected, event);
            found := Selected /= -1;
         end if;
      end;
      if found and then Trace_Enabled then
         -- Removal from the queue precedes the IPC reply. This timestamp is
         -- neither hardware arrival nor proof the application handled input.
         recordInputTrace (
           (event.target, event.serial, event.kind, timingNow));
      end if;
   end dequeueInput;

   function hasInputAfter
     (target : Unsigned_64; afterSerial : Unsigned_64) return Boolean
   is
      channelSlot : constant Integer := findInputChannel (target);
   begin
      return channelSlot >= 0 and then
        (Close_Policy.Has_After
           (inputChannels (SurfaceIndex (channelSlot)).pendingClose, afterSerial) or else
         IQ.Has_After (inputChannels (SurfaceIndex (channelSlot)).events, afterSerial));
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
      Input_Transfers.Poll (inputTransfers);
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
      endDrag;
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
               Accepted : Boolean;
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

               inputChannels (i).snapshot.pointerPosition :=
                 packU32Pair (localX, localY);
               inputChannels (i).snapshot.buttons := lastButtons;
               inputChannels (i).snapshot.modifiers := mods;
               if inputChannels (i).snapshot.generation < Unsigned_64'Last then
                  inputChannels (i).snapshot.generation :=
                    inputChannels (i).snapshot.generation + 1;
               end if;
               IQ.Recover
                 (inputChannels (i).events, inputChannels (i).nextSerial,
                  (True, 0, INPUT_RESYNC, inputChannels (i).target,
                   inputChannels (i).snapshot.pointerPosition,
                   (lastButtons and 16#FFFF_FFFF#) or
                     Shift_Left (mods and 16#FFFF_FFFF#, 32)), Accepted);
               if not Accepted then
                  debugPrint ("desktop: input serials exhausted" & LF);
                  exitCompositor (1);
               end if;
               completeInputWaiter (inputChannels (i).target);
            end;
         end if;
      end loop;
   end forceInputResynchronization;

   procedure clearInputQueue is
   begin
      inputChannels := [others => (others => <>)];
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
         IQ.Reserve (inputChannels (SurfaceIndex (channelSlot)).nextSerial,
           response.words (1));
         if response.words (1) = 0 then
            debugPrint ("desktop: input serials exhausted" & LF);
            exitCompositor (1);
         end if;
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
      if idx < 0 or else surfaces (SurfaceIndex (idx)).owner = No_Process then
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
         surfaces (SurfaceIndex (idx)).owner /= No_Process and then
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
      if idx < 0 or else surfaces (SurfaceIndex (idx)).owner = No_Process then
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

   procedure setupDisplayBuffer (ok : out Boolean; Wait_For_Owner : Boolean := True);
   procedure releaseDisplayBuffer;
   procedure activateInternalSession (ok : out Boolean);

   function currentPublicationConfiguration (S : in out Surface)
      return Publication.Configuration_Result
   is
      package Density renames Compositor_Density;
      package Selection renames Compositor_Density_Selection;
      use type Density.Admission;
      W : Natural := S.w;
      H : Natural := S.h;
      X : Natural := S.x;
      Y : Natural := S.y;
      Scale : DG.UI_Scale := (1, 1);
      Screens : Selection.Outputs :=
        (others => (Width => 1, Height => 1, others => <>));
      Count : Selection.Output_Index := 1;
      Chosen : Selection.Output_Index;
      Accepted : Boolean;
   begin
      if (S.flags and SURFACE_FLAG_WINDOW) /= 0 then
         if W <= CLIENT_INSET_X * 2 or else
           H <= CLIENT_INSET_TOP + CLIENT_INSET_BOTTOM
         then
            return (Status => DP.Bad_State);
         end if;
         W := W - CLIENT_INSET_X * 2;
         H := H - CLIENT_INSET_TOP - CLIENT_INSET_BOTTOM;
         X := X + CLIENT_INSET_X;
         Y := Y + CLIENT_INSET_TOP;
      end if;
      if W not in 1 .. 65_535 or else H not in 1 .. 65_535 then
         return (Status => DP.Resources_Exhausted);
      end if;
      if nativeScene then
         Screens (1) := presentations (primaryOutput).Geometry;
         for I in presentations'Range loop
            if I /= primaryOutput and then presentations (I).Enabled then
               Count := Count + 1;
               Screens (Count) := presentations (I).Geometry;
            end if;
         end loop;
         Chosen := Selection.Choose
           (Screens, Count, 1,
            (DG.Logical_Coordinate (X), DG.Logical_Coordinate (Y),
             DG.Logical_Coordinate (X + W), DG.Logical_Coordinate (Y + H)));
         Scale := Screens (Chosen).Scale;
      end if;
      declare
         Layout : constant Density.Layout := Density.Plan
           (W, H, (Positive (Scale.Numerator), Positive (Scale.Denominator)),
            DP.Maximum_Buffer_Bytes);
      begin
         if Layout.Status /= Density.Accepted then
            return (Status => DP.Resources_Exhausted);
         end if;
         declare
            Had_Configuration : constant Boolean :=
              S.publicationConfiguration.Status = DP.Success;
            Candidate : Publication.Configuration :=
              (Publication.Identity (Natural'Max (1, S.publicationPolicy.Requested)),
               DP.Positive_Extent (W), DP.Positive_Extent (H),
               Positive (Scale.Numerator), Positive (Scale.Denominator),
               (DP.Positive_Extent (Layout.Width), DP.Positive_Extent (Layout.Height),
                Layout.Pitch));
         begin
            if S.publicationConfiguration.Status /= DP.Success or else
              S.publicationConfiguration.Value /= Candidate
            then
               Surface_Policy.Configure (S.publicationPolicy, Accepted);
               if not Accepted then
                  return (Status => DP.Resources_Exhausted);
               end if;
               Candidate.Epoch := Publication.Identity (S.publicationPolicy.Requested);
               S.publicationConfiguration := (DP.Success, Candidate);
               if Had_Configuration and then S.owner /= No_Process then
                  --  A density change can keep the same logical extent. Wake
                  --  an idle producer so it queries the new configuration;
                  --  its old immutable publication remains owned meanwhile.
                  queueConfigure (S.id, Unsigned_64 (S.w), Unsigned_64 (S.h));
               end if;
            end if;
         end;
      end;
      return S.publicationConfiguration;
   end currentPublicationConfiguration;

   procedure refreshPublicationConfigurations is
   begin
      --  Run only for pending scene work, after geometry mutations settle.
      --  This also covers moves and output arrangements that do not resize a
      --  window. Unchanged configurations neither advance epochs nor enqueue
      --  input. The bounded surface table adds no allocation or idle timer.
      for S of surfaces loop
         if S.used and then S.publicationMode and then S.owner /= No_Process then
            declare
               Configuration : constant Publication.Configuration_Result :=
                 currentPublicationConfiguration (S);
               pragma Unreferenced (Configuration);
            begin
               null;
            end;
         end if;
      end loop;
   end refreshPublicationConfigurations;

   procedure handleRequest (from : Process_ID; request : Message) is
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
      if request.tag.label = Compositor_Input_Protocol.Label then
         declare
            package Batch_Protocol renames Compositor_Input_Protocol;
            package Batch_Wire renames Compositor_Input_Batch_Wire;
            package Batches renames Compositor_Input_Batches;
            package Transfer renames Desktop_Input_Transfer.Engine;
            use type Transfer.Outcome;
            Decoded : constant Batch_Protocol.Request_Decoding :=
              Batch_Protocol.Decode (CuBit.Desktop_Messages.To_Wire (request));
            Result : Batch_Protocol.Receipt := (Status => DP.Invalid_Request);
            Index, Channel_Slot : Integer;
         begin
            statsRequests := statsRequests + 1;
            statsInputReq := statsInputReq + 1;
            if Decoded.Accepted then
               Index := findSurface (Decoded.Value.Surface);
               if Index < 0 then Result := (Status => DP.Bad_Object);
               elsif from = No_Process or else surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  ensureInputChannel (Decoded.Value.Surface, Channel_Slot);
                  if Channel_Slot < 0 then Result := (Status => DP.Resources_Exhausted);
                  elsif inputChannels (SurfaceIndex (Channel_Slot)).waiter.active then
                     Result := (Status => DP.Bad_State);
                  else
                     declare
                        Channel : SurfaceInputChannel renames inputChannels (SurfaceIndex (Channel_Slot));
                        Close : constant IQ.Event :=
                          (Channel.pendingClose /= 0, Channel.pendingClose,
                           DP.Input_Event_Kind'Enum_Rep (DP.Close_Requested),
                           Decoded.Value.Surface, 0, 0);
                        Value : constant Batches.Batch := Batches.Snapshot
                          (Channel.events, Close, Decoded.Value.After);
                        Delivered : Transfer.Outcome;
                     begin
                        if not Batch_Wire.Valid (Value, Decoded.Value.Surface,
                          Decoded.Value.After, Batches.Capacity)
                        then Result := (Status => DP.Bad_State);
                        else
                           -- Freeze before handing bytes to the grant writer:
                           -- a failed return or partial write can still expose
                           -- a serial. Freezing on an acquisition failure is
                           -- conservative; it neither acknowledges nor drops it.
                           if Value.Length > 0 then
                              Channel.exposedThrough := Unsigned_64'Max
                                (Channel.exposedThrough, Value.Through);
                           end if;
                           Input_Transfers.Publish
                             (inputTransfers, To_Word (from), Decoded.Value.Surface,
                              Decoded.Value.Grant,
                              Batch_Wire.Encode (Value, Decoded.Value.Surface,
                                Decoded.Value.Identity, Decoded.Value.After), Delivered);
                           if Delivered = Transfer.Published then
                              -- Only the client's previous acknowledgment is
                              -- consumed. This newly published batch remains
                              -- retryable until a later successful request.
                              Compositor_Input_Acknowledgment.Apply
                                (Channel.events, Channel.pendingClose, Decoded.Value.After);
                              Result := (DP.Success, Decoded.Value.Identity,
                                Value.Length, Value.Through, Value.More);
                           elsif Delivered = Transfer.Acquisition_Failed then
                              Result := (Status => DP.Denied);
                           else Result := (Status => DP.Resources_Exhausted);
                           end if;
                        end if;
                     end;
                  end if;
               end if;
            end if;
            replyMsg := CuBit.Desktop_Messages.From_Wire (Batch_Protocol.Encode (Result));
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      if request.tag.label = Publication.Publish_Label then
         declare
            Decoded : constant Publication.Publish_Decoding :=
              Publication.Decode_Publish (CuBit.Desktop_Messages.To_Wire (request));
            Result : Publication.Receipt := (Status => DP.Invalid_Request);
            Index : Integer;
         begin
            if Decoded.Valid then
               Index := findSurface (Unsigned_64 (Decoded.Value.Surface));
               if Index < 0 then
                  Result := (Status => DP.Bad_Object);
               elsif surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  declare
                     S : Surface renames surfaces (SurfaceIndex (Index));
                     Config : constant Publication.Configuration_Result :=
                       currentPublicationConfiguration (S);
                     Accepted : Boolean;
                  begin
                     Result := (Status => DP.Bad_State);
                     if S.publicationMode and then Config.Status = DP.Success and then
                       Decoded.Value.Epoch = Config.Value.Epoch and then S.serial < Unsigned_64'Last
                     then
                        for I in Surface_Policy.Slot loop
                           if S.publicationBuffers (I).Acquired and then
                             Unsigned_64 (S.publicationPolicy.Buffers (I).Ticket) = Decoded.Value.Ticket
                           then
                              declare
                                 B : Publication_Buffer renames S.publicationBuffers (I);
                                 Whole_Change : constant Boolean :=
                                   Natural (Decoded.Value.Area.Width) = 0 or else not S.bufferAttached or else
                                   S.bufferW /= Natural (B.Configuration.Layout.Width) or else
                                   S.bufferH /= Natural (B.Configuration.Layout.Height) or else
                                   S.bufferLogicalW /= Natural (B.Configuration.Width) or else
                                   S.bufferLogicalH /= Natural (B.Configuration.Height);
                                 Damage : constant Compositor_Source_Damage.Box :=
                                   Compositor_Source_Damage.Map
                                     (Natural (B.Configuration.Layout.Width), Natural (B.Configuration.Layout.Height),
                                      Natural (B.Configuration.Width), Natural (B.Configuration.Height),
                                      (Natural (Decoded.Value.Area.X), Natural (Decoded.Value.Area.Y),
                                       Natural (Decoded.Value.Area.Width), Natural (Decoded.Value.Area.Height)),
                                      Full => Whole_Change);
                                 -- Source rows for the persistent renderer image.
                                 Source_Rows : constant Compositor_Source_Content.Row_Band :=
                                   (if Whole_Change or else Natural (Decoded.Value.Area.Height) = 0
                                    then bufferRows (Natural (B.Configuration.Layout.Height))
                                    else Compositor_Source_Content.Clip
                                      (Compositor_Source_Content.Band
                                         (Natural (Decoded.Value.Area.Y),
                                          Natural'Min (Compositor_Source_Content.Maximum_Rows,
                                            Natural (Decoded.Value.Area.Y) + Natural (Decoded.Value.Area.Height))),
                                       Natural (B.Configuration.Layout.Height)));
                                 Origin_X : constant Natural := S.x +
                                   (if (S.flags and SURFACE_FLAG_WINDOW) /= 0 then CLIENT_INSET_X else 0);
                                 Origin_Y : constant Natural := S.y +
                                   (if (S.flags and SURFACE_FLAG_WINDOW) /= 0 then CLIENT_INSET_TOP else 0);
                              begin
                                 Surface_Policy.Present (S.publicationPolicy, I,
                                   Surface_Policy.Generation (Decoded.Value.Epoch),
                                   Natural (Decoded.Value.Ticket), Accepted);
                                 if Accepted then
                                    S.bufferGrant := S.publicationBuffers (I).Grant;
                                    S.bufferAddr := S.publicationBuffers (I).Address;
                                    S.bufferW := Natural (S.publicationBuffers (I).Configuration.Layout.Width);
                                    S.bufferH := Natural (S.publicationBuffers (I).Configuration.Layout.Height);
                                    S.bufferPitch := S.publicationBuffers (I).Configuration.Layout.Pitch;
                                    S.bufferLogicalW := Natural (S.publicationBuffers (I).Configuration.Width);
                                    S.bufferLogicalH := Natural (S.publicationBuffers (I).Configuration.Height);
                                    S.bufferFormat := PIXEL_FORMAT_BGRA8888;
                                    S.bufferAttached := True;
                                    S.dirty := True;
                                    S.serial := S.serial + 1;
                                    noteContentChange (S, Source_Rows);
                                    S.publicationInputAfter := Decoded.Value.Input_After;
                                    if Trace_Enabled then
                                       -- Capture the accepted publication identity; both
                                       -- exporters perform bounded appends without IPC.
                                       recordSourceTrace (
                                         (S.id, Decoded.Value.Epoch, Decoded.Value.Ticket,
                                          S.publicationInputAfter, timingNow));
                                    end if;
                                    scheduleRedrawRect
                                      ((Origin_X + Damage.Left, Origin_Y + Damage.Top,
                                        Damage.Right - Damage.Left, Damage.Bottom - Damage.Top));
                                    Result := (DP.Success, Decoded.Value.Epoch, Decoded.Value.Ticket);
                                 end if;
                              end;
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;
            end if;
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (Publication.Encode_Receipt (Result, Publication.Publish_Label));
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      if request.tag.label = Publication.Stage_Label or else
        request.tag.label = Publication.Retirement_Label
      then
         declare
            Is_Stage : constant Boolean := request.tag.label = Publication.Stage_Label;
            Staged : constant Publication.Stage_Decoding :=
              Publication.Decode_Stage (CuBit.Desktop_Messages.To_Wire (request));
            Query : constant Publication.Query_Decoding :=
              Publication.Decode_Query (CuBit.Desktop_Messages.To_Wire (request), True);
            Result : Publication.Receipt := (Status => DP.Invalid_Request);
            Index : Integer;
         begin
            if (Is_Stage and then Staged.Valid) or else
              (not Is_Stage and then Query.Valid)
            then
               Index := findSurface
                 (Unsigned_64 (if Is_Stage then Staged.Value.Surface else Query.Value.Surface));
               if Index < 0 then
                  Result := (Status => DP.Bad_Object);
               elsif surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  declare
                     S : Surface renames surfaces (SurfaceIndex (Index));
                  begin
                     Result := (Status => DP.Bad_State);
                     if Is_Stage then
                        if S.publicationMode or else not S.bufferAttached then
                           declare
                              Config : constant Publication.Configuration_Result :=
                                currentPublicationConfiguration (S);
                              Accepted : Boolean;
                              Acquisition : DP.Status_Code;
                              Loan : Source_Loans.Ticket;
                              Address : System.Address;
                              Duplicate : Boolean := False;
                           begin
                              for I in Surface_Policy.Slot loop
                                 Duplicate := Duplicate or else
                                   (S.publicationBuffers (I).Acquired and then
                                    S.publicationBuffers (I).Grant = Staged.Value.Grant);
                              end loop;
                              if Config.Status /= DP.Success then
                                 Result := (Status => Publication.Failure_Status (Config.Status));
                              elsif not Duplicate and then Staged.Value.Epoch = Config.Value.Epoch then
                                 for I in Surface_Policy.Slot loop
                                    if S.publicationPolicy.Buffers (I).Status = Surface_Policy.Empty then
                                       Surface_Policy.Stage
                                         (S.publicationPolicy, I,
                                          Surface_Policy.Generation (Staged.Value.Epoch), Accepted);
                                       if Accepted then
                                          acquireSourceLoan (Staged.Value.Grant, from,
                                            DP.Byte_Length (Config.Value.Layout), Address, Loan, Acquisition);
                                          if Acquisition = DP.Success then
                                             S.publicationBuffers (I).Loan := Loan;
                                             S.publicationMode := True;
                                             S.publicationBuffers (I).Acquired := True;
                                             S.publicationBuffers (I).Grant := Staged.Value.Grant;
                                             S.publicationBuffers (I).Address := Address;
                                             S.publicationBuffers (I).Configuration := Config.Value;
                                             Result := (DP.Success, Staged.Value.Epoch,
                                               Publication.Identity (S.publicationPolicy.Buffers (I).Ticket));
                                          else
                                             Surface_Policy.Discard (S.publicationPolicy, I,
                                               S.publicationPolicy.Buffers (I).Ticket);
                                             Surface_Policy.Retire (S.publicationPolicy, I,
                                               S.publicationPolicy.Buffers (I).Ticket, True);
                                             Result := (Status => Publication.Failure_Status (Acquisition));
                                          end if;
                                       end if;
                                       exit;
                                    end if;
                                 end loop;
                              end if;
                           end;
                        end if;
                     elsif S.publicationMode then
                        for I in Surface_Policy.Slot loop
                           if Unsigned_64 (S.publicationPolicy.Buffers (I).Ticket) = Query.Value.Ticket then
                              retirePublicationBuffer (S, I);
                           end if;
                           if S.publicationBuffers (I).Retired.Status = DP.Success and then
                             S.publicationBuffers (I).Retired.Ticket = Query.Value.Ticket
                           then
                              Result := S.publicationBuffers (I).Retired;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;
            end if;
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (Publication.Encode_Receipt (Result, request.tag.label));
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      if request.tag.label = Publication.Configuration_Label then
         declare
            Decoded : constant Publication.Query_Decoding :=
              Publication.Decode_Query
                (CuBit.Desktop_Messages.To_Wire (request), Retirement => False);
            Result : Publication.Configuration_Result;
            Index : Integer;
         begin
            if not Decoded.Valid then
               Result := (Status => DP.Invalid_Request);
            else
               Index := findSurface (Unsigned_64 (Decoded.Value.Surface));
               if Index < 0 then
                  Result := (Status => DP.Bad_Object);
               elsif surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  Result := currentPublicationConfiguration
                    (surfaces (SurfaceIndex (Index)));
               end if;
            end if;
            replyMsg := CuBit.Desktop_Messages.From_Wire
              (Publication.Encode_Configuration (Result));
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
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
               (if idx >= 0 then To_Word (surfaces (SurfaceIndex (idx)).owner) else 0),
               To_Word (from));
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
               if not backBufferReady and then not outputReopenPending then
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
                       (if from = doomPid and then doomPid /= No_Process
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
                     bufferAttached => False, bufferLoan => Source_Loans.No_Ticket,
                     bufferGrant => <>,
                     bufferAddr => System.Null_Address,
                     bufferLogicalW => 0, bufferLogicalH => 0,
                     bufferW => 0,
                     bufferH => 0,
                     bufferPitch => 0,
                     bufferFormat => 0,
                     contentVersion => Compositor_Source_Content.No_Version,
                     pointerCursor => POINTER_DEFAULT,
         publicationPolicy => <>, publicationConfiguration => <>,
         publicationMode => False, publicationBuffers => <>,
         publicationInputAfter => 0);
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
                  --  The taskbar button shows the same title.
                  scheduleRedrawRect (taskButtonRect (SurfaceIndex (idx)));
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
               Acquisition : DP.Status_Code;
               Loan : Source_Loans.Ticket;
            begin
               replyMsg.tag := (OP_SURFACE_ATTACH_BUFFER, 1, 0, 0);
               if not decoded.Valid then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (DP.Invalid_Request);
               elsif idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif surfaces (SurfaceIndex (idx)).publicationMode then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               elsif surfaces (SurfaceIndex (idx)).serial = Unsigned_64'Last then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               else
                  -- Acquire the new buffer BEFORE releasing the old one.
                  -- Failed validation/acquisition leaves the old attachment intact.
                  acquireSourceLoan
                    (decoded.Value.Grant, from, DP.Byte_Length (decoded.Value.Layout),
                     mapped, Loan, Acquisition);
                  if Acquisition /= DP.Success then
                     replyMsg.words (0) := DP.Status_Code'Enum_Rep (Acquisition);
                  else
                     releaseSurfaceBuffer (surfaces (SurfaceIndex (idx)));
                     surfaces (SurfaceIndex (idx)).bufferLoan := Loan;
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
                     noteContentChange (surfaces (SurfaceIndex (idx)),
                       bufferRows (surfaces (SurfaceIndex (idx)).bufferH));
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
                  (if idx >= 0 then To_Word (surfaces (SurfaceIndex (idx)).owner) else 0),
                  To_Word (from));
            begin
               if accessStatus /= DP.Success then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (accessStatus);
               elsif surfaces (SurfaceIndex (idx)).publicationMode then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
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
                     bufferH : constant Natural := surfaces (SurfaceIndex (idx)).bufferH;
                  begin
                     -- Client-local damage rows are buffer rows only at 1:1.
                     noteContentChange (surfaces (SurfaceIndex (idx)),
                       (if DP.Mode (present.Value) = DP.Whole_Surface or else bufferH /= client.h
                        then bufferRows (bufferH)
                        else Compositor_Source_Content.Clip
                          (Compositor_Source_Content.Band
                             (Natural'Min (Natural (damage.Y), Compositor_Source_Content.Maximum_Rows),
                              Natural'Min (Natural (damage.Y) + Natural (damage.Height),
                                Compositor_Source_Content.Maximum_Rows)),
                           Natural'Min (bufferH, Compositor_Source_Content.Maximum_Rows))));
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
                  noteContentChange (surfaces (SurfaceIndex (idx)),
                    bufferRows (surfaces (SurfaceIndex (idx)).bufferH));
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
                     endDrag;
                  end if;
                  clearInputForTarget (target);
                  releaseSurfaceBuffer (surfaces (SurfaceIndex (idx)));
                  retireSurfaceSource (surfaces (SurfaceIndex (idx)));
                  surfaces (SurfaceIndex (idx)) := (others => <>);
                  if focusSurface = target then
                     -- Select after removal, before accepting more input.
                     -- The full redraw below includes old/new titlebar state.
                     declare Focus_Damage : Rect := (others => 0); begin
                        focusTopmostVisibleWindow (Focus_Damage);
                     end;
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
                     endDrag;
                  end if;
                  clearInputForTarget (surfaces (i).id);
                  releaseSurfaceBuffer (surfaces (i));
                  retireSurfaceSource (surfaces (i));
                  surfaces (i) := (others => <>);
               end if;
            end loop;
            if focusSurface /= 0 and then findSurface (focusSurface) < 0 then
               declare Focus_Damage : Rect := (others => 0); begin
                  focusTopmostVisibleWindow (Focus_Damage);
               end;
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
      CuBit.Memory_Grants.Create_Via_Capability
        (slot      => CAP_SLOT_PROCMGR,
         localAddr => spawnGrantAddr,
         numPages  => 1,
         readWrite => True,
         reference => spawnGrant,
         success   => grantOk);

      if grantOk then
         spawnGrantReady := True;
      else
         debugPrint ("desktop: spawn grant to procmgr failed" & LF);
      end if;
   end ensureSpawnGrant;

   procedure trySpawnApplication (name : String) is
      msg : Message := NULL_MESSAGE;
      New_Token : Unsigned_64;
      Accepted : Boolean;
      len : Natural := name'Length;
   begin
      if not CR.Available (launchRequest) then
         debugPrint ("desktop: launch busy or quarantined" & LF);
         return;
      end if;
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
      msg.words (0) := CuBit.Grant_References.Encode (spawnGrant);
      --  Scheduling priority: higher runs first. Launched applications run
      --  below desktop.svc (4) so a busy app cannot starve the compositor,
      --  which also draws the software cursor.
      msg.words (1) := APP_PRIORITY;
      msg.words (2) := 0;
      msg.words (3) := 0;
      CR.Allocate (requestSequence, New_Token);
      CR.Begin_Request (launchRequest, New_Token, Accepted);
      if not Accepted then
         debugPrint ("desktop: launch identifiers exhausted" & LF);
         return;
      end if;
      launchName (1 .. len) := name;
      launchNameLength := len;
      launchInputAnnounced := False;
      if capSubmit (CAP_SLOT_PROCMGR, msg, New_Token) then
         debugPrint ("desktop: launch submitted token=" & Decimal (New_Token) & LF);
      else
         CR.Quarantine (launchRequest);
         debugPrint ("desktop: launch submission uncertain; buffer retained" & LF);
      end if;
   end trySpawnApplication;

   procedure Apply_Arrangement (damage : in out Rect) is
      Candidate : constant DL.Layout := settingsView.Pending_Layout;
      Width, Height : Natural := 0;
      Layout : DP.Buffer_Layout;
      Pointer : DL.Pointer_Position;
      Choice : DL.Primary_Selection;
      use type DG.UI_Scale, DG.Scale_Component, DG.Orientation, DL.Named_Display_ID;
      function Translate (Value : Natural; Old_Origin, New_Origin : DG.Output_Origin;
                          Extent, Object_Extent : Natural) return Natural is
         Low : constant Integer := Integer (New_Origin);
         High : constant Integer := Low + Integer'Max (0, Integer (Extent) - Integer (Object_Extent));
      begin
         return Natural (Integer'Max (Low, Integer'Min (High,
           Integer (Value) - Integer (Old_Origin) + Low)));
      end Translate;
   begin
      settingsView.Layout_Status := Desktop_Settings.Rejected;
      -- Logical layout changes do not write or resize the retained targets.
      -- Keep accepting them while draining; reopening restores this layout.
      if not backBufferReady or else Candidate.Count /= desktopLayout.Count or else
        Candidate.Count = 0 or else DL.Validate (Candidate).Status /= DL.Accepted
      then return; end if;
      Choice := DL.Select_Primary
        (Candidate, settingsView.Pending_Primary, Policy => DL.Apply_Primary_Preference);
      if not Choice.Available or else Choice.Display /= settingsView.Pending_Primary then
         return;
      end if;
      -- Mode/identity/rotation are unchanged. Scale affects logical composition,
      -- never scanout mode, transfer-buffer size or driver ownership.
      for I in 1 .. Candidate.Count loop
         declare
            Old : DG.Output renames desktopLayout.Items (I).Geometry;
            New_G : DG.Output renames Candidate.Items (I).Geometry;
            B : constant DG.Logical_Rectangle := DG.Bounds (New_G);
         begin
            if Candidate.Items (I).Display /= desktopLayout.Items (I).Display or
              Old.Width /= New_G.Width or Old.Height /= New_G.Height or
              Old.Rotation /= New_G.Rotation or
              New_G.Scale.Numerator < New_G.Scale.Denominator or
              New_G.X < 0 or New_G.Y < 0
            then return; end if;
            if Old.Scale /= New_G.Scale and then
              (B.Right - B.Left < CuBit.Display_Arrangement.Minimum_Width or
               B.Bottom - B.Top < CuBit.Display_Arrangement.Minimum_Height)
            then return; end if;
            Width := Natural'Max (Width, Natural (B.Right));
            Height := Natural'Max (Height, Natural (B.Bottom));
         end;
      end loop;
      if Width not in 1 .. Natural (DP.Positive_Extent'Last) or
         Height not in 1 .. Natural (DP.Positive_Extent'Last)
      then return; end if;
      Layout := (DP.Positive_Extent (Width), DP.Positive_Extent (Height), Width * 4);
      if nativeScene then
         if not Compositor_Workspace.Valid (Width, Height) then return; end if;
      elsif not DP.Valid_Layout (Layout) or else DP.Byte_Length (Layout) > Unsigned_64 (sceneCapacityBytes) then
         return;
      end if;

      restoreCursorOverlay;
      Pointer := DL.Confine (desktopLayout,
        (DG.Logical_Coordinate (cursorX), DG.Logical_Coordinate (cursorY)));
      -- Keep each title bar (and minimized/maximized restore position) on its
      -- existing monitor. Scale changes may shrink their logical work area;
      -- use the usual configure path when an existing client needs resizing.
      for S of surfaces loop
         if S.used and then (S.flags and SURFACE_FLAG_WINDOW) /= 0 then
            declare
               Owner : constant DL.Pointer_Position := DL.Confine (desktopLayout,
                 (DG.Logical_Coordinate (S.x + Natural'Min (S.w / 2, 64)),
                  DG.Logical_Coordinate (S.y + Natural'Min (S.h / 2, 12))));
               Old : DG.Output renames desktopLayout.Items (Owner.Screen).Geometry;
               New_G : DG.Output renames Candidate.Items (Owner.Screen).Geometry;
               B : constant Rect := logicalBounds (New_G);
               Work_Height : constant Natural := B.h -
                 (if Owner.Screen = Choice.Index then
                     Natural'Min (B.h, TASKBAR_H) else 0);
            begin
               if Old.Scale /= New_G.Scale and not S.maximized then
                  declare
                     W : Natural := Natural'Min (S.w, B.w);
                     H : Natural := Natural'Min (S.h, Work_Height);
                  begin
                     clampSurfaceSize (S, W, H);
                     if W /= S.w or H /= S.h then
                        S.w := W; S.h := H; S.serial := S.serial + 1;
                        if S.owner /= No_Process then
                           queueConfigure (S.id, Unsigned_64 (W), Unsigned_64 (H));
                        end if;
                     end if;
                  end;
               end if;
               S.x := Translate (S.x, Old.X, New_G.X, B.w, S.w);
               S.y := Translate (S.y, Old.Y, New_G.Y, Work_Height, Natural'Min (S.h, Work_Height));
               S.restoreX := Translate (S.restoreX, Old.X, New_G.X, B.w, S.restoreW);
               S.restoreY := Translate (S.restoreY, Old.Y, New_G.Y, Work_Height, Natural'Min (S.restoreH, TITLE_HEIGHT));
               S.dirty := True;
            end;
         elsif S.used and then (S.flags and SURFACE_FLAG_SHELL) /= 0 then
            S.x := 0; S.y := 0; S.w := Width; S.h := Height;
         end if;
      end loop;
      cursorX := Translate (cursorX, desktopLayout.Items (Pointer.Screen).Geometry.X,
        Candidate.Items (Pointer.Screen).Geometry.X, logicalBounds (Candidate.Items (Pointer.Screen).Geometry).w, 1);
      cursorY := Translate (cursorY, desktopLayout.Items (Pointer.Screen).Geometry.Y,
        Candidate.Items (Pointer.Screen).Geometry.Y, logicalBounds (Candidate.Items (Pointer.Screen).Geometry).h, 1);
      directOutput := False;
      backBufferAddr := privateSceneAddr;
      fbWidth := Width; fbHeight := Height; fbPitch := Compositor_Workspace.Pitch (Width);
      desktopLayout := Candidate;
      syncPointerPlane;
      primaryOutput := Output_Index (Choice.Index - 1);
      preferredPrimary := Choice.Display;
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            presentations (Output).Geometry := Candidate.Items (Natural (Output) + 1).Geometry;
         end if;
      end loop;
      -- A primary change changes work areas, not client ownership or restore
      -- placement. Resize maximized windows in place without unmaximizing them.
      for S of surfaces loop
         if S.used and then (S.flags and SURFACE_FLAG_WINDOW) /= 0 and then S.maximized then
            declare
               Area : constant Rect := windowWorkArea (surfaceRect (S));
               W : Natural := Area.w;
               H : Natural := Area.h;
            begin
               clampSurfaceSize (S, W, H);
               S.x := Area.x; S.y := Area.y;
               if S.w /= W or S.h /= H then
                  S.w := W; S.h := H;
                  S.serial := S.serial + 1;
                  if S.owner /= No_Process then
                     queueConfigure (S.id, Unsigned_64 (W), Unsigned_64 (H));
                  end if;
               end if;
            end;
         end if;
      end loop;
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            -- In-flight transfers remain immutable. Queue a full replacement
            -- in output-local coordinates for after their matching completion.
            presentations (Output).Repaint := RP.Open ((0, 0,
              Natural (presentations (Output).Geometry.Width), Natural (presentations (Output).Geometry.Height)));
            Compositor_Damage.Clear (presentations (Output).Damage);
            addOutputDamage (presentations (Output).Damage, (0, 0,
              Natural (presentations (Output).Geometry.Width), Natural (presentations (Output).Geometry.Height)));
         end if;
      end loop;
      dragBaseReady := False;
      cursorSaveValid := False;
      cursorPresentPending := False;
      launchMenuOpen := False;
      audioPopupOpen := False;
      settingsView.Applied_Layout := Candidate;
      settingsView.Applied_Primary := Choice.Display;
      settingsView.Dragging := False;
      settingsView.Layout_Status := Desktop_Settings.Session_Only;
      damage := (0, 0, fbWidth, fbHeight);
      scheduleRedraw;
      debugPrint ("desktop: arrangement applied" & LF);
      debugPrint ("desktop: primary display" & Choice.Display'Image & LF);
      for I in 1 .. Candidate.Count loop
         debugPrint ("desktop: display" & I'Image & " origin" &
           Candidate.Items (I).Geometry.X'Image & "," & Candidate.Items (I).Geometry.Y'Image & LF);
         debugPrint ("desktop: display" & I'Image & " scale" &
           Candidate.Items (I).Geometry.Scale.Numerator'Image & "/" &
           Candidate.Items (I).Geometry.Scale.Denominator'Image & LF);
      end loop;
   end Apply_Arrangement;

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
         if S.used and then S.owner /= No_Process then
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
      if Apply then
         if settingsView.Current_Page = Desktop_Settings.Displays then Apply_Arrangement (damage);
         else Apply_Appearance (damage); end if;
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

      if surfaces (SurfaceIndex (idx)).owner /= No_Process then
         return False;
      end if;

      case surfaces (SurfaceIndex (idx)).appKind is
         when APP_SETTINGS =>
            if raw < 128 then
               declare Apply : Boolean; begin
                  Desktop_Settings.Key (settingsView, Natural (raw), desktopShiftDown, Apply);
                  if Apply then
                     if settingsView.Current_Page = Desktop_Settings.Displays then Apply_Arrangement (damage);
                     else Apply_Appearance (damage); end if;
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
   begin
      if action not in 1 .. launchMenu.Count then
         return;
      end if;
      declare
         item : Desktop_Launch.Entry_Info renames launchMenu.Entries (action);
         name : constant String := Desktop_Launch.Program_Of (item);
      begin
         case item.Kind is
            when Desktop_Launch.Internal_Settings =>
               openInternalApp (APP_SETTINGS, damage);
            when Desktop_Launch.Launch_Program =>
               if item.Single_Instance and then launchPids (action) /= No_Process
                 and then processAlive (launchPids (action))
               then
                  debugPrint ("desktop: " & Desktop_Launch.Label_Of (item) &
                              " is already running" & LF);
               else
                  trySpawnApplication (name);
               end if;
         end case;
      end;
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

   --  The clock and volume arrive as completions (Desktop_Status_Refresh):
   --  this only applies fresh replies and submits the next reads when due.
   procedure refreshStatus is
      stamp : CuBit.Clocks.Snapshot;
      audio : CuBit.Audio_Control.State;
      fresh : Boolean;
      nextText : String (1 .. 5) := "--:--";
      now : constant Unsigned_64 := nowMs;
      use type CuBit.Clocks.Time_Quality;
      use type CuBit.Audio_Control.State;
      function digit (v : Natural) return Character is
        (Character'Val (Character'Pos ('0') + v));
   begin
      Desktop_Status_Refresh.Take_Clock (stamp, fresh);
      if fresh then
         if CuBit.Clocks.Is_Valid_Wall_Time (stamp.Quality) then
            nextText := [digit (stamp.Hour / 10), digit (stamp.Hour mod 10), ':',
                         digit (stamp.Minute / 10), digit (stamp.Minute mod 10)];
            --  Next read at the next minute, within the regular interval.
            if now < Unsigned_64'Last - Status_Interval_Ms then
               statusDueMs := Unsigned_64'Min
                 (statusDueMs, now + Unsigned_64 (60 - stamp.Second) * 1000);
            end if;
         elsif stamp.Quality = CuBit.Clocks.Invalid_Zone then
            nextText := " TZ? ";
         end if;
         if clockText /= nextText then
            clockText := nextText;
            scheduleRedrawRect (statusRect);
         end if;
      end if;
      Desktop_Status_Refresh.Take_Audio (audio, fresh);
      if fresh and then audio /= masterAudio then
         masterAudio := audio;
         scheduleRedrawRect (statusRect);
         if audioPopupOpen then scheduleRedrawRect (audioPopupRect); end if;
      end if;
      if now = Unsigned_64'Last or else now < statusDueMs then return; end if;
      statusDueMs := (if now < Unsigned_64'Last - Status_Interval_Ms
                      then now + Status_Interval_Ms else Unsigned_64'Last - 1);
      Desktop_Status_Refresh.Request_Clock (requestSequence);
      Desktop_Status_Refresh.Request_Audio (requestSequence);
   end refreshStatus;

   procedure setMasterAudio (level : CuBit.Audio_Control.Percent; muted : Boolean) is
   begin
      if not masterAudio.Available or else
        (level = masterAudio.Level and then muted = masterAudio.Muted)
      then return; end if;
      --  The mixer's reply (its resulting state) redraws the status area.
      Desktop_Status_Refresh.Set_Audio (level, muted, requestSequence);
   end setMasterAudio;

   procedure handleMouseMotion
      (buttons : Unsigned_64;
       dx      : Integer;
       dy      : Integer;
       dz      : Integer;
       Observed_Ms : Unsigned_64)
   is
      Source_Clock : constant Boolean := Observed_Ms /= Unsigned_64'Last;
      Click_Ms : constant Unsigned_64 := (if Source_Clock then Observed_Ms else syscall (SYSCALL_GETTIME));
      oldCursor : constant Rect := cursorRect;
      oldCursorStyle : constant Pointer_Cursor_Style := cursorStyle;
      oldBounds : Rect := (others => 0);
      newBounds : Rect := (others => 0);
      --  Scene damage only. The pointer footprints are presented separately
      --  (scheduleCursorPresent); any area recorded here schedules a scene
      --  pass, so no handler can compute damage that is then dropped.
      damage    : Rect := (others => 0);
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
      -- Never pair source-acquisition time with legacy processing time.
      if Source_Clock /= titleClockFromSource then
         CuBit.Click_Sequences.Reset (titleClicks);
         titleClockFromSource := Source_Clock;
      end if;
      if leftTransition then
         statsButtonTransitions := statsButtonTransitions + 1;
         if Diagnostic_Buttons < Unsigned_64'Last then Diagnostic_Buttons := Diagnostic_Buttons + 1; end if;
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
      if desktopLayout.Count > 0 then
         declare
            Visible : constant DL.Pointer_Position := DL.Confine
              (desktopLayout, (DG.Logical_Coordinate (cursorX), DG.Logical_Coordinate (cursorY)));
         begin
            cursorX := Natural (Visible.Point.X);
            cursorY := Natural (Visible.Point.Y);
         end;
      end if;

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
                  scheduleRedrawRect (inflateRect (launchMenuArea, 4));
                  launchMenuOpen := False;
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

      --  The Apps menu follows the pointer: a resting category opens
      --  (Desktop_Launch_Menus.Tick), a submenu entry is selected.
      if launchMenuOpen and then not leftDown then
         declare
            before : constant LM.Menu_State := launchMenuState;
            area : constant Rect := launchMenuArea;
            subItem : constant Natural := hitLaunchSubItem (cursorX, cursorY);
         begin
            if subItem > 0 then
               LM.Hover_Item (launchMenuState, subItem);
            elsif pointInRect (cursorX, cursorY, launchMenuRect) then
               LM.Hover_Row (launchMenuState, launchMenu, hitLaunchItem (cursorX, cursorY));
            end if;
            if not Desktop_Launch_Menus."=" (launchMenuState, before) then
               damage := unionRect (damage, inflateRect (unionRect (area, launchMenuArea), 4));
            end if;
         end;
      end if;

      if leftDown and then not leftWasDown then
         if shellSurfaceVisible and then
            pointInRect (cursorX, cursorY, launchButtonRect)
         then
            damage := unionRect (damage, inflateRect (launchMenuArea, 4));
            launchMenuOpen := not launchMenuOpen;
            if launchMenuOpen then
               Desktop_Launch_Refresh.Request;
               LM.Reset (launchMenuState);
            end if;
            damage := unionRect
              (damage,
               inflateRect (unionRect (launchButtonRect, launchMenuArea), 4));
            handledChromeClick := True;
         elsif launchMenuOpen and then
            pointInRect (cursorX, cursorY, launchSubmenuRect)
         then
            declare
               index : constant Natural := hitLaunchSubItem (cursorX, cursorY);
               entryIndex : constant Natural :=
                 (if index > 0 then LM.Entry_Of (launchMenu, launchMenuState.Open, index) else 0);
            begin
               damage := unionRect (damage, inflateRect (launchMenuArea, 4));
               if entryIndex > 0 then
                  launchMenuOpen := False;
                  performLaunchAction (entryIndex, damage);
               end if;
            end;
            handledChromeClick := True;
         elsif launchMenuOpen and then
            pointInRect (cursorX, cursorY, launchMenuRect)
         then
            declare
               result : LM.Choice;
               use type LM.Choice;
            begin
               damage := unionRect (damage, inflateRect (launchMenuArea, 4));
               LM.Click_Row (launchMenuState, launchMenu, hitLaunchItem (cursorX, cursorY), result);
               if result = LM.Power then
                  launchMenuOpen := False;
               end if;
               damage := unionRect (damage, inflateRect (launchMenuArea, 4));
            end;
            handledChromeClick := True;
         elsif launchMenuOpen then
            damage := unionRect (damage, inflateRect (launchMenuArea, 4));
            launchMenuOpen := False;
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
                  (cursorX, cursorY), Click_Ms, clickKind);
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
                        -- No focus/z-order/menu damage and no pointer motion:
                        -- this press only arms a possible drag/double click.
                        if nativeScene and then not pointerMoved and then
                          isEmpty (damage)
                        then sceneDamage := False; end if;
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
              (titleClicks, (cursorX, cursorY), Click_Ms);
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
               -- The release packet may carry the final pointer movement.
               -- Commit that position even when no held-button motion packet
               -- updated the preview before release.
               newBounds := previewRectFromPointer (surfaces (SurfaceIndex (idx)));

               surfaces (SurfaceIndex (idx)).x := newBounds.x;
               surfaces (SurfaceIndex (idx)).y := newBounds.y;
               surfaces (SurfaceIndex (idx)).w := newBounds.w;
               surfaces (SurfaceIndex (idx)).h := newBounds.h;
               surfaces (SurfaceIndex (idx)).serial :=
                  surfaces (SurfaceIndex (idx)).serial + 1;
               if dragMode /= DRAG_MOVE and then
                 surfaces (SurfaceIndex (idx)).owner /= No_Process
               then
                  queueConfigure (surfaces (SurfaceIndex (idx)).id,
                                  Unsigned_64 (newBounds.w),
                                  Unsigned_64 (newBounds.h));
               end if;

               if nativeScene and then dragMode = DRAG_MOVE and then
                 dragPresentedValid and then dragPresentedRect = newBounds and then
                 oldBounds = newBounds and then not pointerMoved and then isEmpty (damage)
               then sceneDamage := False; end if;

               --  Ensure a final position that arrived just before release is
               --  presented even if this pass has not painted the drag yet.
               if dragMode /= DRAG_MOVE or else not dragPresentedValid or else
                 dragPresentedRect /= newBounds
               then
               damage := unionRect
                 (damage,
                  transitionDamage
                    (oldBounds, newBounds, dragPresentedRect,
                     dragPresentedValid));
               end if;
               tracePointer
                 ("drag-up", dragSurfaceId,
                  Unsigned_64 (newBounds.x), Unsigned_64 (newBounds.y));
            end if;
         end if;

         endDrag;
      elsif not leftDown then
         endDrag;
      end if;

      if leftDown and then dragMode /= DRAG_NONE and then dragSurfaceId /= 0 then
         idx := findSurface (dragSurfaceId);
         if idx >= 0 then
            declare
               Previous : constant Rect := dragPreviewRect;
            begin
               dragPreviewRect := previewRectFromPointer (surfaces (SurfaceIndex (idx)));
               if dragPreviewRect /= Previous then sceneDamage := True; end if;
            end;
            if dragMode = DRAG_MOVE then
               surfaces (SurfaceIndex (idx)).x := dragPreviewRect.x;
               surfaces (SurfaceIndex (idx)).y := dragPreviewRect.y;
            end if;
            --  The compositor frame compares the last drag geometry actually
            --  presented with this latest position. Intermediate mouse
            --  positions drained in the same pass require no repaint at all.
         end if;
      end if;

      cursorStyle := cursorStyleAtPointer;
      lastButtons := buttons;
      if sceneDamage or else not isEmpty (damage) then
         scheduleRedrawRect
           (inflateRect (unionRect (damage, unionRect (oldCursor, cursorRect)), 2));
      elsif not nativeScene or else oldCursor /= cursorRect or else oldCursorStyle /= cursorStyle then
         scheduleCursorPresent;
      end if;
      if pointerMoved and then pendingMotionUs = Compositor_Elapsed.Unavailable then
         pendingMotionUs := timingNow;
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
         if slot < 0 then DI.Reject (Input_Diagnostic, DI.Table_Full);
         else DI.Reject (Input_Diagnostic, DI.Delivery); end if;
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
            DI.Reject (Input_Diagnostic, DI.Replayed);
            return;
         end if;

         discontinuity := report.flags (CuBit.Input.RESYNCHRONIZE) or else
           (source.used and then
             (source.generation /= report.generation or else
              not CuBit.Input.Is_Immediate_Successor
                (source.sequence, report.sequence)));

         if not source.used then DI.New_Source (Input_Diagnostic); end if;
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

      DI.Accepted (Input_Diagnostic, seatButtons, discontinuity);
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
      DI.Observe_Raw (Input_Diagnostic, eventMsg.tag.label = CuBit.Input.OP_SOURCE_REPORT, eventMsg.tag.label,
        eventMsg.words (1), eventMsg.words (2), eventMsg.words (3), Diagnostic_Last_Us);

      if eventMsg.tag.label = CuBit.Input.OP_SOURCE_REPORT then
         CuBit.Input.Decode (eventMsg, sourceReport, sourceValid);
         if sourceValid then
            acceptSourceReport
              (sourceReport, sourceAccepted, sourceDiscontinuity,
               seatButtons);
         else
            statsSourceRejects := statsSourceRejects + 1;
            DI.Reject (Input_Diagnostic, DI.Malformed);
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
         if Diagnostic_Keys < Unsigned_64'Last then Diagnostic_Keys := Diagnostic_Keys + 1; end if;
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
                     damage := unionRect (damage, inflateRect (launchMenuArea, 4));
                     launchMenuOpen := not launchMenuOpen;
                     if launchMenuOpen then
                        Desktop_Launch_Refresh.Request;
                     end if;
                     LM.Reset (launchMenuState);
                     damage := unionRect
                       (damage,
                        inflateRect
                          (unionRect (launchButtonRect, launchMenuArea), 4));
                     scheduleRedrawRect (inflateRect (damage, 2));
                  end if;
               elsif launchMenuOpen then
                  --  The compositor owns keyboard input while its Apps menu
                  --  is open. Swallow both presses and releases so the focused
                  --  client cannot observe half of a key transition.
                  if not release then
                     declare
                        --  Set-1 arrow scancodes (sent after the extended prefix).
                        KEY_LEFT_ARROW : constant Unsigned_8 := 16#4B#;
                        KEY_RIGHT_ARROW : constant Unsigned_8 := 16#4D#;
                        result : LM.Choice := LM.Nothing;
                        entryIndex : Natural := 0;
                        use type LM.Choice;
                     begin
                        damage := unionRect (damage, inflateRect (launchMenuArea, 4));
                        if code = KEY_UP then
                           LM.Press (launchMenuState, launchMenu, LM.Up, result, entryIndex);
                        elsif code = KEY_DOWN then
                           LM.Press (launchMenuState, launchMenu, LM.Down, result, entryIndex);
                        elsif code = KEY_LEFT_ARROW then
                           LM.Press (launchMenuState, launchMenu, LM.Left, result, entryIndex);
                        elsif code = KEY_RIGHT_ARROW then
                           LM.Press (launchMenuState, launchMenu, LM.Right, result, entryIndex);
                        elsif code = KEY_ENTER then
                           LM.Press (launchMenuState, launchMenu, LM.Enter, result, entryIndex);
                        elsif code = KEY_ESCAPE then
                           LM.Press (launchMenuState, launchMenu, LM.Escape, result, entryIndex);
                        end if;
                        if result = LM.Launch then
                           launchMenuOpen := False;
                           performLaunchAction (entryIndex, damage);
                        elsif result in LM.Close | LM.Power then
                           launchMenuOpen := False;
                        end if;
                     end;

                     damage := unionRect
                       (damage,
                        inflateRect
                          (unionRect (launchButtonRect, launchMenuArea), 4));
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
            -- Recovery discards old stream state, but this accepted byte
            -- already begins the new stream. Preserve its extended prefix
            -- so the following suffix cannot become an ordinary key.
            desktopExtendedPrefix := raw = KEY_EXTENDED_PREFIX;
         end if;
      elsif eventMsg.tag.label = EVENT_MOUSE or else
         (sourceAccepted and then
          sourceReport.device = CuBit.Input.RELATIVE_POINTER)
      then
         statsMouseEvents := statsMouseEvents + 1;
         if Diagnostic_Mouse < Unsigned_64'Last then Diagnostic_Mouse := Diagnostic_Mouse + 1; end if;
         packed :=
           (if sourceAccepted then sourceReport.payload
            else eventMsg.words (0));
         if (Desktop_Timing_Policy.Enabled or else (DM.Enabled and then not DM.Disabled))
           and then sourceAccepted and then
           CuBit.Input.Pointer_Time (sourceReport.snapshot) /= Unsigned_64'Last
         then
            declare
               Observed : constant Unsigned_64 :=
                 CuBit.Input.Pointer_Time (sourceReport.snapshot);
               Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
            begin
               if DM.Enabled and then not DM.Disabled and then Now /= Unsigned_64'Last and then
                 Now >= Observed and then Now < Unsigned_64'Last / MICROSECONDS_PER_MILLISECOND
               then
                  DM.Record_Stage (Compositor_Stage_Metrics.Input_Source_Age,
                    Observed * MICROSECONDS_PER_MILLISECOND, Now * MICROSECONDS_PER_MILLISECOND);
               end if;
               if Now /= Unsigned_64'Last and then Now >= Observed and then
                 statsSourceAgeSumMs < Unsigned_64'Last - (Now - Observed)
               then
                  statsSourceAgeMaxMs :=
                    Unsigned_64'Max (statsSourceAgeMaxMs, Now - Observed);
                  statsSourceAgeSumMs := statsSourceAgeSumMs + (Now - Observed);
                  statsSourceAgeCount := statsSourceAgeCount + 1;
               end if;
            end;
         end if;
         if signed8 (Shift_Right (packed, 32)) /= 0 then
            statsWheelEvents := statsWheelEvents + 1;
         end if;
         handleMouseMotion
           (buttons =>
              (if sourceAccepted then seatButtons
               else packed and 16#FF#),
            dx      => signed12 (Shift_Right (packed, 8)),
            dy      => signed12 (Shift_Right (packed, 20)),
            dz      => signed8 (Shift_Right (packed, 32)),
            Observed_Ms => (if sourceAccepted then CuBit.Input.Pointer_Time (sourceReport.snapshot)
                            else Unsigned_64'Last));

         if sourceAccepted and then sourceDiscontinuity then
            forceInputResynchronization;
         end if;
      elsif eventMsg.tag.label = OP_STREAM_AVAILABLE then
         rememberStreams
           (From_Word (eventMsg.words (0)),
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
         words    => [w0, w1, w2, w3]);
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_DISPLAY, msg, CuBit.Messages.Wait_Forever);
      msg.tag := tag;
      return msg;
   end callDisplay;

   procedure allocatePixelStorage
     (Pages : Unsigned_64; Raw : out Unsigned_64; Allocation : out PS.Ticket)
   is
      Ticket : PS.Ticket;
   begin
      Raw := Unsigned_64'Last;
      Allocation := PS.No_Ticket;
      if Pages not in 1 .. DP.Maximum_Buffer_Bytes / 4096 then return; end if;
      declare Bytes : constant Positive := Positive (Pages * 4096);
      begin
         PS.Reserve (pixelStorage, Bytes, Ticket);
         if Ticket = PS.No_Ticket then
            debugPrint ("desktop: pixel storage budget exhausted" & LF);
            return;
         end if;
         Raw := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Unsigned_64 (Bytes));
         -- Zero may conceal a quarantined allocation prefix. Retain its charge
         -- and descriptor; never infer rollback from the address sentinel.
         PS.Allocated (pixelStorage, Ticket, Raw /= 0 and Raw mod 4096 = 0);
         pixelAllocations (PS.Index (Ticket)) := (Ticket, Raw);
         if Raw = 0 then
            Raw := Unsigned_64'Last;
            debugPrint ("desktop: pixel allocation failed; charge retained" & LF);
            return;
         elsif Raw mod 4096 /= 0 then
            debugPrint ("desktop: pixel allocation malformed" & LF);
            exitCompositor (1);
         end if;
         Allocation := Ticket;
         debugPrint ("desktop: pixel storage request=" & Bytes'Image &
           " charged=" & PS.Charged (pixelStorage)'Image &
           " limit=" & PS.Limit (pixelStorage)'Image & LF);
      end;
   end allocatePixelStorage;

   -- Call only after Mesa imports and every external grant reader retire.
   procedure releasePixelStorage (Allocation : in out PS.Ticket) is
      Result : Unsigned_64;
      Bytes : Positive;
   begin
      if Allocation = PS.No_Ticket then return; end if;
      if not PS.Current (pixelStorage, Allocation) or else
        PS.Status (pixelStorage, Allocation) /= PS.Live or else
        pixelAllocations (PS.Index (Allocation)).Ticket /= Allocation
      then
         debugPrint ("desktop: pixel storage identity mismatch" & LF);
         exitCompositor (1);
      end if;
      Bytes := PS.Bytes (pixelStorage, Allocation);
      PS.Begin_Release (pixelStorage, Allocation, Readers_Retired => True);
      Result := syscall (SYSCALL_RELEASE_OWNED_MEMORY,
        pixelAllocations (PS.Index (Allocation)).Address, Unsigned_64 (Bytes));
      PS.Released (pixelStorage, Allocation, Confirmed => Result = 0);
      if Result /= 0 then
         debugPrint ("desktop: pixel storage release uncertain" & LF);
         exitCompositor (1);
      end if;
      pixelAllocations (PS.Index (Allocation)) := (others => <>);
      Allocation := PS.No_Ticket;
      debugPrint ("desktop: pixel storage released=" & Bytes'Image &
        " charged=" & PS.Charged (pixelStorage)'Image & LF);
   end releasePixelStorage;

   procedure closeOutput (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      R : Output_Retirement.State renames outputRetirement (Output);
      Grants : Output_Retirement.Grant_Set;
      Renderer : Desktop_Compositor.Target_Release;
      Confirmed : Boolean;
   begin
      Desktop_Wallpaper_Layers.Release (Natural (Output));
      if CP.Current (P.Transfer) = CP.Prepared then
         -- No successful publication occurred. Retire only this local hold;
         -- renderer and grant retirement remain independently required.
         CP.Cancel (P.Transfer);
         BP.Retire_Display (P.Pool, BP.Displayed (P.Pool), True);
         if BP.Faulted (P.Pool) then exitCompositor (1); end if;
         Compositor_Damage.Restore (P.Damage, P.Frame_Damage);
      end if;
      if Output_Retirement.Status (R) = Output_Retirement.Idle then
         for B in BP.Live_Slot loop Grants (Positive (B)) := P.Targets (B).Granted; end loop;
         R := Output_Retirement.Start (P.Leased, Grants);
         declare Empty : LR.State; begin outputLeaseRequests (Output) := Empty; end;
      end if;
      if Output_Retirement.Status (R) = Output_Retirement.Renderer_Pending then
         Desktop_Compositor.Forget_Targets (Renderer);
         Output_Retirement.Observe_Renderer
           (R, (case Renderer is
              when Desktop_Compositor.Targets_Retired => Output_Retirement.Retired,
              when Desktop_Compositor.Targets_Busy => Output_Retirement.Busy,
              when Desktop_Compositor.Targets_Unsafe => Output_Retirement.Uncertain));
      end if;
      if Output_Retirement.Can_Release_Lease
        (R, Presentation_Retired => not P.Enabled or else CP.Writable (P.Transfer))
      then
         if LR.Status (outputLeaseRequests (Output)) = LR.Ready then
            declare
               Token : Unsigned_64;
               Prepared : Boolean;
               Request : constant Message := CuBit.Desktop_Messages.From_Wire
                 (DSP.With_Output (DSP.Encode_Lease_Request (DSP.Release_Display), Output));
            begin
               CR.Allocate (requestSequence, Token);
               LR.Prepare (outputLeaseRequests (Output), Token, Prepared);
               if not Prepared then
                  debugPrint ("desktop: output lease identifiers exhausted" & LF);
                  quarantinePresentations;
               end if;
               -- A false capSubmit result confirms no request was published.
               -- Retain the lease and retry once on a later bounded loop pass.
               LR.Submitted (outputLeaseRequests (Output),
                 capSubmit (CAP_SLOT_DISPLAY, Request, Token));
            end;
         end if;
         if LR.Status (outputLeaseRequests (Output)) = LR.Released then
            Output_Retirement.Observe_Lease (R, True);
         elsif LR.Status (outputLeaseRequests (Output)) = LR.Quarantined then
            Output_Retirement.Observe_Lease (R, False);
         end if;
      end if;
      if Output_Retirement.Status (R) = Output_Retirement.Grants_Pending then
         for B in BP.Live_Slot loop
            if Output_Retirement.Grant_Status (R, Positive (B)) = Output_Retirement.Revoke_Required then
               MG.Revoke (P.Targets (B).Grant, Confirmed);
               Output_Retirement.Observe_Revoke (R, Positive (B), Confirmed);
               exit when Output_Retirement.Status (R) = Output_Retirement.Quarantined;
            end if;
            if Output_Retirement.Grant_Status (R, Positive (B)) = Output_Retirement.Confirmation_Pending then
               Output_Retirement.Observe_Grant
                 (R, Positive (B), MG.Retirement_Confirmed (P.Targets (B).Grant));
            end if;
         end loop;
      end if;
      if Output_Retirement.Status (R) = Output_Retirement.Quarantined then
         debugPrint ("desktop: output retirement uncertain" & LF);
         exitCompositor (1);
      end if;
      if Output_Retirement.Status (R) = Output_Retirement.Storage_Ready then
         for Target of P.Targets loop releasePixelStorage (Target.Allocation); end loop;
         Output_Retirement.Observe_Storage (R, True);
         debugPrint ("desktop: output readers retired=" & Output_Index'Image (Output) & LF);
      end if;
      if Output_Retirement.Status (R) /= Output_Retirement.Released then
         -- Also covers a partial setup before backBufferReady. A delayed
         -- cleanup suspends all outputs because target imports are global.
         outputDrainRequested := True;
         cursorSaveValid := False;
      elsif not outputDrainRequested then
         -- Synchronous partial-setup rollback has no submitted frames.
         P := (others => <>);
      end if;
   end closeOutput;

   procedure pumpOutputRetirements is
   begin
      if not outputDrainRequested then return; end if;
      for Output in Output_Index loop closeOutput (Output); end loop;
      if (for some R of outputRetirement =>
          Output_Retirement.Status (R) /= Output_Retirement.Released)
      then return; end if;
      -- Renderer, lease and grant confirmations have arrived for every output.
      -- Keep old token identities until this global commit; queued completions
      -- then fall below the retired watermark, never into a newly opened pool.
      releasePixelStorage (sceneAllocation);
      releasePixelStorage (dragAllocation);
      for Output in Output_Index loop presentations (Output) := (others => <>); end loop;
      retiredThrough := requestSequence;
      backBufferAddr := System.Null_Address;
      privateSceneAddr := System.Null_Address;
      dragBaseBufferAddr := System.Null_Address;
      dragBaseReady := False;
      sceneCapacityBytes := 0;
      directOutput := False;
      backBufferReady := False;
      drawingBackBuffer := False;
      cursorSaveValid := False;
      framePending := False;
      frameDamage := (others => 0);
      cursorPresentPending := False;
      outputDrainRequested := False;
      outputReopenPending := anySurfaceUsed and then not shutdownRequested;
      debugPrint ("desktop: pixel teardown charged=" & PS.Charged (pixelStorage)'Image & LF);
      -- Do not clear input queues here: newly created windows may have accepted
      -- input while old outputs were draining. Their records remain live.
   end pumpOutputRetirements;

   procedure releaseDisplayBuffer is
      Empty : Output_Retirement.State;
   begin
      if not outputDrainRequested then
         if not backBufferReady and then
           (for all P of presentations => not P.Leased and
              (for all T of P.Targets => T.Allocation = PS.No_Ticket))
         then return; end if;
         outputDrainRequested := True;
         outputReopenPending := False;
         cursorSaveValid := False;
         for Output in Output_Index loop
            if Output_Retirement.Status (outputRetirement (Output)) = Output_Retirement.Released then
               outputRetirement (Output) := Empty;
            end if;
         end loop;
      end if;
      pumpOutputRetirements;
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
     (Output : Output_Index; Info : Message; Ok : out Boolean; Wait_For_Owner : Boolean := True)
   is
      P : Output_Presentation renames presentations (Output);
      Response : Message;
      Layout : DP.Buffer_Layout;
      Pages, Raw : Unsigned_64;
      Ignored : Unsigned_64;
      Granted : Boolean;
   begin
      Ok := False;
      if outputDrainRequested then return; end if;
      if Output_Retirement.Status (outputRetirement (Output)) not in
        Output_Retirement.Idle | Output_Retirement.Released
      then return; end if;
      declare Empty : Output_Retirement.State; begin outputRetirement (Output) := Empty; end;
      if not validDisplayInfo (Info) then return; end if;
      -- Output zero retains the bounded shell-to-Desktop handoff retry.
      -- An optional output must not delay startup behind an existing owner.
      for Attempt in 1 .. (if Output = 0 and then Wait_For_Owner then 100 else 1) loop
         Response := callDisplay (OP_DISPLAY_ACQUIRE, Output => Output);
         exit when Response.tag.label = OP_DISPLAY_ACQUIRE and then
           Response.tag.length = 1 and then Response.words (0) = 0;
         if Output = 0 and then Wait_For_Owner and then Attempt < 100 then
            Ignored := syscall (SYSCALL_SLEEP, 2);
         end if;
      end loop;
      if Response.tag.label /= OP_DISPLAY_ACQUIRE or else
        Response.tag.length /= 1 or else Response.words (0) /= 0
      then
         return;
      end if;
      P.Leased := True;
      P.Trace_Output := Output;
      Layout := (DP.Positive_Extent (Info.words (0)),
                 DP.Positive_Extent (Info.words (1)),
                 DP.Buffer_Pitch (Info.words (2)));
      Pages := (DP.Byte_Length (Layout) + 4095) / 4096;
      P.Pitch := Natural (Layout.Pitch);
      P.Geometry := (Width => DG.Physical_Extent (Layout.Width),
                     Height => DG.Physical_Extent (Layout.Height), others => <>);
      for B in BP.Live_Slot loop
         allocatePixelStorage (Pages, Raw, P.Targets (B).Allocation);
         if Raw = Unsigned_64'Last then closeOutput (Output); return; end if;
         P.Targets (B).Address := To_Address (Integer_Address (Raw));
         MG.Create_Via_Capability
           (CAP_SLOT_DISPLAY, P.Targets (B).Address, Natural (Pages), False,
            P.Targets (B).Grant, Granted);
         P.Targets (B).Granted := Granted;
         if not Granted then closeOutput (Output); return; end if;
         Response := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
           (PW.Encode (PW.Attachment'(B, (P.Targets (B).Grant, Layout))), Output));
         Response.tag := capCall (CAP_SLOT_DISPLAY, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= PW.Attach_Buffer or else Response.tag.length /= 1 or else
           Response.tag.flags /= 0 or else Response.tag.reserved /= 0 or else Response.words (0) /= 0
         then closeOutput (Output); return; end if;
      end loop;
      Response := CuBit.Desktop_Messages.From_Wire (DSP.With_Output (PW.Encode_Open, Output));
      Response.tag := capCall (CAP_SLOT_DISPLAY, Response, CuBit.Messages.Wait_Forever);
      if Response.tag.label /= PW.Open_Session or else Response.tag.length /= 4 or else
        Response.tag.flags /= 0 or else Response.tag.reserved /= 0 or else
        Response.words (0) /= 0 or else Response.words (1) = 0 or else
        Response.words (2) /= 3 or else Response.words (3) /= 1
      then closeOutput (Output); return; end if;
      P.Transfer := CP.Open (Response.words (1));
      P.Pool := BP.Open (Response.words (1));
      P.Repaint := RP.Open ((0, 0, Natural (Layout.Width), Natural (Layout.Height)));
      declare Writer : BP.Ticket;
      begin
         BP.Acquire (P.Pool, Writer);
         if Writer = BP.None then closeOutput (Output); return; end if;
         P.Buffer := P.Targets (Writer.Buffer).Address;
      end;
      P.Enabled := True;
      Ok := True;
   end prepareOutput;

   procedure setupDisplayBuffer (ok : out Boolean; Wait_For_Owner : Boolean := True) is
      First_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
      Second_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO, Output => 1);
      Prepared : Boolean;
      Layout : DP.Buffer_Layout;
      Candidate : DL.Layout;
      Choice : DL.Primary_Selection;
      Raw, Pages, Drag_Raw : Unsigned_64;
      Total_Width, Total_Height : Natural;
   begin
      ok := False;
      if outputDrainRequested then return; end if;
      prepareOutput (0, First_Info, Prepared, Wait_For_Owner);
      if not Prepared then
         debugPrint ("desktop: display setup failed" & LF);
         return;
      end if;
      Total_Width := Natural (First_Info.words (0));
      Total_Height := Natural (First_Info.words (1));
      if validDisplayInfo (Second_Info) and then
        Natural (Second_Info.words (0)) <= Natural (DP.Positive_Extent'Last) - Total_Width
      then
         -- Admit logical bounds separately from physical target allocation.
         Layout := (DP.Positive_Extent (Total_Width + Natural (Second_Info.words (0))),
                    DP.Positive_Extent (Natural'Max (Total_Height, Natural (Second_Info.words (1)))),
                    DP.Buffer_Pitch ((Total_Width + Natural (Second_Info.words (0))) * 4));
         if (if nativeScene then Compositor_Workspace.Valid (Natural (Layout.Width), Natural (Layout.Height))
             else DP.Valid_Layout (Layout)) then
            prepareOutput (1, Second_Info, Prepared, False);
            if outputDrainRequested then return; end if;
            if Prepared then
               presentations (1).Geometry.X := DG.Output_Origin (Total_Width);
               Total_Width := Natural (Layout.Width);
               Total_Height := Natural (Layout.Height);
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
      if outputReopenPending then
         Candidate := Compositor_Layout_Restore.Choose (desktopLayout, Candidate);
         Total_Width := 0; Total_Height := 0;
         for I in 1 .. Candidate.Count loop
            declare B : constant DG.Logical_Rectangle := DG.Bounds (Candidate.Items (I).Geometry);
            begin
               if B.Right <= 0 or B.Bottom <= 0 then
                  for Output in Output_Index loop closeOutput (Output); end loop;
                  return;
               end if;
               Total_Width := Natural'Max (Total_Width, Natural (B.Right));
               Total_Height := Natural'Max (Total_Height, Natural (B.Bottom));
            end;
         end loop;
         for Output in Output_Index loop
            if presentations (Output).Enabled then
               presentations (Output).Geometry := Candidate.Items (Natural (Output) + 1).Geometry;
            end if;
         end loop;
      end if;
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
      if Total_Width not in 1 .. Natural (DP.Positive_Extent'Last) or else
        Total_Height not in 1 .. Natural (DP.Positive_Extent'Last)
      then
         for Output in Output_Index loop closeOutput (Output); end loop;
         return;
      end if;
      Layout := (DP.Positive_Extent (Total_Width),
                 DP.Positive_Extent (Total_Height),
                 DP.Buffer_Pitch (Total_Width * 4));
      if (if nativeScene then not Compositor_Workspace.Valid (Total_Width, Total_Height)
          else not DP.Valid_Layout (Layout)) then
         for Output in Output_Index loop closeOutput (Output); end loop;
         return;
      end if;
      Pages := 0;
      privateSceneAddr := System.Null_Address;
      backBufferAddr := System.Null_Address;
      sceneCapacityBytes := 0;
      if not nativeScene then
         Pages := (DP.Byte_Length (Layout) + 4095) / 4096;
         -- Reusable private scene capacity, bounded by the existing scene budget.
         -- Rearrangement changes stride/extents but never allocates on drag/Apply.
         -- Scanout buffers/grants keep their original sizes and lifetimes.
         if Candidate.Count = 2 then
            Pages := (Unsigned_64'Min (DP.Maximum_Buffer_Bytes,
              Unsigned_64 (Total_Width) * (First_Info.words (1) + Second_Info.words (1)) * 4) + 4095) / 4096;
         end if;
         allocatePixelStorage (Pages, Raw, sceneAllocation);
         if Raw = Unsigned_64'Last and then Pages > (DP.Byte_Length (Layout) + 4095) / 4096 then
            Pages := (DP.Byte_Length (Layout) + 4095) / 4096;
            allocatePixelStorage (Pages, Raw, sceneAllocation);
         end if;
         if Raw = Unsigned_64'Last then
            for Output in Output_Index loop closeOutput (Output); end loop;
            debugPrint ("desktop: scene allocation failed" & LF);
            return;
         end if;
         privateSceneAddr := To_Address (Integer_Address (Raw));
         backBufferAddr := privateSceneAddr;
         sceneCapacityBytes := Natural (Pages * 4096);
      else
         debugPrint ("desktop: native scene allocation bytes=0" & LF);
      end if;
      fbWidth := Total_Width;
      fbHeight := Natural (Layout.Height);
      fbPitch := Compositor_Workspace.Pitch (Total_Width);
      fbBpp := 32;
      dragBaseReady := False;
      dragBaseBufferAddr := System.Null_Address;
      Drag_Raw := Unsigned_64'Last;
      if not nativeScene then allocatePixelStorage (Pages, Drag_Raw, dragAllocation); end if;
      if Drag_Raw /= Unsigned_64'Last then
         dragBaseBufferAddr := To_Address (Integer_Address (Drag_Raw));
      elsif not nativeScene then
         debugPrint ("desktop: retained drag layer unavailable" & LF);
      end if;
      directOutput := not nativeScene and then Candidate.Count = 1 and then presentations (0).Pitch = fbPitch;
      if directOutput then
         backBufferAddr := presentations (0).Buffer;
         debugPrint ("desktop: direct pooled rendering active" & LF);
      end if;
      if nativeScene then debugPrint ("desktop: native output scene rendering active" & LF); end if;
      backBufferReady := True;
      debugPrint ("desktop: wallpaper uses retained scene layers" & LF);
      debugPrint ("desktop: active outputs=" & Candidate.Count'Image &
        " primary=" & primaryOutput'Image & LF);
      desktopLayout := Candidate;
      syncPointerPlane;
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

      if not backBufferReady and then not outputReopenPending then
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

   procedure Present_Bootstrap (Ready : out Boolean) is
      P : Output_Presentation renames presentations (primaryOutput);
      Started : constant Unsigned_64 := nowMs;
      Ignore : Unsigned_64;
   begin
      Ready := False;
      if not backBufferReady or else not P.Enabled or else
        not CP.Writable (P.Transfer) or else not BP.Writable (P.Pool, BP.Writer (P.Pool)) or else
        Started = Unsigned_64'Last
      then return; end if;
      -- This CPU frame has no renderer/GPU owner. Do not call Begin_Output:
      -- early renderer admission would irreversibly select software.
      Bootstrap_CPU := True;
      Compositor_Damage.Add (P.Damage, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
      pumpOutput (primaryOutput);
      if CP.Current (P.Transfer) not in CP.Prepared | CP.In_Flight then return; end if;
      for Attempt in 1 .. 2000 loop
         -- Bound attempts even if the clock freezes. Exhaustion never
         -- authorizes renderer startup or release of uncertain storage.
         if CP.Current (P.Transfer) = CP.Prepared then
            submitPreparedOutput (primaryOutput);
         end if;
         -- Sole existing CQ dispatcher authenticates exact token/session/frame,
         -- retires Display ownership and routes unrelated completions normally.
         collectPresentations;
         if CP.Writable (P.Transfer) and then BP.Displayed (P.Pool) = BP.None and then
           not BP.Faulted (P.Pool) and then not BP.Rendering (P.Pool)
         then
            Bootstrap_CPU := False;
            RP.Invalidate (P.Repaint, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
            Compositor_Damage.Add (P.Damage, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
            Ready := True;
            debugPrint ("DESKTOP-BOOTSTRAP: released; renderer may start" & LF);
            return;
         end if;
         declare Now : constant Unsigned_64 := nowMs; begin
            if Now = Unsigned_64'Last or else Now < Started or else Now - Started >= 2000 then return; end if;
         end;
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
   end Present_Bootstrap;

   procedure Start_Renderer (Allow_GPU : Boolean) is
      package Selection renames Compositor_Backend_Selection;
      Evidence : Selection.Readiness := (others => False);
      Diagnostic : Desktop_Renderer_Startup.Pipeline_Diagnostic := (others => <>);
      Accepted : Boolean;
   begin
      -- No GPU owner exists before Initialize. On an unconfirmed CPU bootstrap,
      -- select software without entering foreign initialization or cleanup.
      if Allow_GPU then
      Desktop_Renderer_Startup.Initialize
        (Configuration => backBufferReady and then desktopLayout.Count = 1 and then
           primaryOutput = 0 and then presentations (0).Enabled,
         Width => Unsigned_64 (presentations (0).Geometry.Width),
         Height => Unsigned_64 (presentations (0).Geometry.Height),
         Epoch => BP.Writer (presentations (0).Pool).Epoch,
         Evidence => Evidence, Diagnostic => Diagnostic);
      end if;
      if Diagnostic.Valid and then not Diagnostic_Pipeline_Valid then
         Diagnostic_Pipeline_Stage := Diagnostic.Stage;
         Diagnostic_Pipeline_Index := Diagnostic.Index;
         Diagnostic_Pipeline_Result := Diagnostic.Result;
         Diagnostic_Pipeline_Valid := True;
      end if;
      Desktop_Compositor.Configure_Renderer (Evidence, Accepted);
      if not Accepted then exitCompositor (1); return; end if;
      if Selection.Ready (Evidence) then
         debugPrint ("DESKTOP-VULKAN: startup=READY" & LF);
      else
         debugPrint ("DESKTOP-VULKAN: startup=SOFTWARE" & LF);
         if Allow_GPU then Desktop_Renderer_Startup.Stop; end if;
         if Desktop_Renderer_Startup.GPU_Capable then
            softwareAnnounced := True;
            debugPrint ((if Allow_GPU then "desktop: software rendering (GPU unavailable at startup)"
                         else "desktop: software rendering (GPU not started; bootstrap unconfirmed)") & LF);
         end if;
      end if;
   end Start_Renderer;

   ret      : Unsigned_64;
   from     : Process_ID;
   msg      : Message;
   found    : Boolean;
   running  : Boolean := True;
   displayInfoOk : Boolean := False;
begin
   debugPrint ("desktop: starting" & LF);
   Read_Appearance;
   Desktop_Wallpaper_Assets.Prepare (appearance.Backdrop);
   Read_Trace_Configuration;
   Load_Launch_Menu;
   Desktop_Launch_Refresh.Initialize;

   ret := setLatencyContract
      (LATENCY_INTERACTIVE,
       4_167,   --  Advisory 240 Hz target; presentation follows the output.
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

   Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Entered);
   declare Bootstrap_Ready : Boolean; begin
      Present_Bootstrap (Bootstrap_Ready);
      if not Bootstrap_Ready then
         debugPrint ("DESKTOP-BOOTSTRAP: unconfirmed; software event loop selected" & LF);
         -- Preserve Transfer/Pool ownership exactly. Normal bounded dispatch
         -- can handle input and later authenticated completion; the output
         -- pump still refuses writes to Display-owned storage. Never treat
         -- timeout as retirement, or initialize a GPU over uncertain storage.
         Bootstrap_CPU := False;
      end if;
      Start_Renderer (Allow_GPU => Bootstrap_Ready);
   end;
   Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Selected);

   while running or else not shutdownRequested or else outputDrainRequested or else sourceRetirementPending loop
      declare
         eventMsg   : Message;
         eventFound : Boolean := False;
         activity : Activity_Result;
         metricsDelayUs : Unsigned_64 := Unsigned_64'Last;
         inputBatch : DB.Input_Batch := DB.New_Input;
         procedure Drain_Events is
         begin
            DB.Begin_Input_Phase (inputBatch, dispatchNow);
            while running and then DB.Can_Input (inputBatch, dispatchNow) loop
               eventFound := Poll_Event (eventMsg);
               exit when not eventFound;
               DB.Charge_Input (inputBatch);
               if Desktop_Timing_Policy.Enabled and then CR.Busy (launchRequest) and then
                 not launchInputAnnounced
               then
                  launchInputAnnounced := True;
                  debugPrint ("desktop: input during launch token=" & Decimal (CR.Token (launchRequest)) & LF);
               end if;
               if Presentation_Test_Policy.Enabled and then
                 CP.Current (presentations (primaryOutput).Transfer) = CP.In_Flight
               then
                  -- Test-only: the first event may precede the reader's hold.
                  -- Record each bounded dispatch so later progress is visible.
                  debugPrint ("desktop: input during frame" &
                    CP.Token (presentations (primaryOutput).Transfer)'Image & LF);
               end if;
               declare Started : constant Unsigned_64 := timingNow;
               begin
                  if pendingInputUs = Compositor_Elapsed.Unavailable then
                     pendingInputUs := dispatchNow;
                  end if;
                  handleEvent (eventMsg, running);
                  noteTiming (Input_Dispatch, Started);
               end;
            end loop;
         end Drain_Events;
         procedure Drain_Requests is
            Batch : DB.Request_Batch := DB.New_Requests (dispatchNow);
         begin
            while running and then DB.Can_Request (Batch, dispatchNow, framePending) loop
               Poll_Service_Request (from, msg, found);
               exit when not found;
               declare Started : constant Unsigned_64 := timingNow;
               begin
                  if Diagnostic_Requests < Unsigned_64'Last then Diagnostic_Requests := Diagnostic_Requests + 1; end if;
                  handleRequest (from, msg);
                  noteTiming (Request_Dispatch, Started);
               end;
               DB.Charge_Request (Batch);
            end loop;
         end Drain_Requests;
      begin
         if Diagnostic_Loops < Unsigned_64'Last then Diagnostic_Loops := Diagnostic_Loops + 1; end if;
         turnStartedUs := dispatchNow;
         turnWork := (statsEvents, statsRequests, statsPresentOps);
         collectPresentations;
         if Desktop_Launch_Refresh.Can_Take (launchMenuOpen) then
            declare
               Fresh : Desktop_Launch.Menu;
               Updated : Boolean;
               Previous : constant Desktop_Launch.Menu := launchMenu;
               Old_Pids : constant Launch_PID_Array := launchPids;
            begin
               Desktop_Launch_Refresh.Take (launchMenuOpen, Fresh, Updated);
               if Updated then
                  launchPids := [others => No_Process];
                  for I in 1 .. Fresh.Count loop
                     for J in 1 .. Previous.Count loop
                        if Desktop_Launch.Program_Of (Fresh.Entries (I)) =
                          Desktop_Launch.Program_Of (Previous.Entries (J))
                        then launchPids (I) := Old_Pids (J); exit;
                        end if;
                     end loop;
                  end loop;
                  launchMenu := Fresh;
                  LM.Reset (launchMenuState);
               end if;
            end;
         end if;
         Drain_Events;
         Desktop_Launch_Refresh.Pump (requestSequence);

         -- Count and elapsed-time admission both yield a paint opportunity
         -- under a busy request stream. A handler already running can overrun
         -- its allowance; that execution cost is outside the admission proof.
         Drain_Requests;

         --  A reply can hand execution to an app that publishes input before
         --  we resume. Dispatch those arrivals before painting, not one full
         --  repaint later. Both drains share a finite budget: input floods
         --  must still permit requests and rendering to make progress.
         Drain_Events;
         if not running and then not shutdownRequested then
            shutdownRequested := True;
            outputReopenPending := False;
            for S of surfaces loop
               if S.used then releaseSurfaceBuffer (S); end if;
            end loop;
            releaseDisplayBuffer;
         end if;
         pumpOutputRetirements;
         if outputReopenPending and then not outputDrainRequested and then not shutdownRequested then
            declare Ready : Boolean;
            begin
               setupDisplayBuffer (Ready, False);
               outputReopenPending := not Ready;
               if Ready then scheduleRedraw; end if;
            end;
         end if;
         refreshStatus;
         if framePending then refreshPublicationConfigurations; end if;
         if not outputDrainRequested and then directOutput and then RP.Preparation_Required
           (presentations (primaryOutput).Repaint,
            BP.Writer (presentations (primaryOutput).Pool).Buffer,
            Work_Pending => framePending or cursorPresentPending,
            Full_Repaint => framePending and then frameDamage.x = 0 and then
              frameDamage.y = 0 and then frameDamage.w = fbWidth and then
              frameDamage.h = fbHeight)
         then
            -- Input and requests have settled the latest scene. Repair only
            -- when about to paint, never speculatively after presentation.
            repairDirectWriter;
         end if;
         flushFrame;
         flushCursorPresent;
         pumpPresentation;
         pumpSourceRetirements;
         recordTransferWork;
         if DM.Enabled and then DM.Pending then DM.Pump (requestSequence, dispatchNow); end if;
         if DM.Enabled and then DM.Disabled and then not metricsStoppedAnnounced then
            metricsStoppedAnnounced := True;
            debugPrint ("desktop: metrics quarantined dropped=" & Decimal (DM.Dropped) &
              " invalid=" & Decimal (DM.Invalid) & " rejected=" & Decimal (DM.Rejected) & LF);
         end if;
         expireInputWaiters;
         --  Input, requests and presentation are done for this turn;
         --  housekeeping (the period boundary, then report text) comes
         --  last, and only while nothing is pending. A turn that did work is
         --  one Loop_Turn sample, housekeeping included.
         if not framePending and then not cursorPresentPending then
            --  Nothing left to show: input that changed no pixels is not
            --  carried into a later, unrelated frame's latency.
            pendingInputUs := Compositor_Elapsed.Unavailable;
         end if;
         maybePrintStats;
         runHousekeeping;
         if DM.Enabled and then not DM.Disabled and then
           turnWork /= (statsEvents, statsRequests, statsPresentOps)
         then
            DM.Record_Stage (Compositor_Stage_Metrics.Loop_Turn, turnStartedUs, dispatchNow);
         end if;
         if DM.Enabled and then DM.Pending then metricsDelayUs := DM.Delay_Us (dispatchNow); end if;

         if not eventFound and then not found and then
           (running or else outputDrainRequested or else sourceRetirementPending) then
            if reportStep /= Report_Done then
               --  Housekeeping yielded to work or ran out of its slice:
               --  take the next turn without sleeping.
               null;
            elsif not framePending and then not cursorPresentPending and then
              nextInputDeadline = 0 and then statusDueMs = 0 and then
              metricsDelayUs = Unsigned_64'Last and then not renderingPending and then
              not sourceRetirementPending and then not outputDrainRequested and then not outputReopenPending
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
                  if metricsDelayUs = 0 then
                     mayWait := False;
                  elsif mayWait and then metricsDelayUs /= Unsigned_64'Last then
                     declare Deadline : constant Unsigned_64 :=
                       Compositor_Metric_Batch_Policy.Wake_At_Ms (now, metricsDelayUs);
                     begin
                        if nextDueMs = 0 or else Deadline < nextDueMs then nextDueMs := Deadline; end if;
                     end;
                  end if;
                  if renderingPending or else sourceRetirementPending or else outputDrainRequested or else outputReopenPending then
                     -- Bounded fallback until renderer completion has a kernel
                     -- activity event. Input can wake this wait immediately.
                     if now < Unsigned_64'Last - 1 then
                        if nextDueMs = 0 or else now + 1 < nextDueMs then nextDueMs := now + 1; end if;
                     else mayWait := False;
                     end if;
                  end if;
                  if not outputDrainRequested and then not outputReopenPending and then
                    (framePending or else cursorPresentPending) then
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

   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
