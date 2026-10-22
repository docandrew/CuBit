with Desktop_Log_IO;
with GNAT.Source_Info;
with CuBit.Boot_Milestone_Codes;
with Desktop_Bootstrap_Logs;
with Desktop_Bootstrap_Notes;
with Desktop_Startup_Clock;
with CuBit.Boot_Milestones;
with Compositor_Transfer_Delta;
with Desktop_Renderer_Startup;
with Desktop_Renderer_Worker;
with Desktop_Startup_Progress;
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
with Client_Glyph_Blend;
with Glyph_Gamma;
with Glyph_Pen;
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
with Compositor_Layout_Resize;
with CuBit.Output_Discovery;
with CuBit.Output_Modes;
with Compositor_Storage;
with CuBit.Display_Pool_Protocol;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.System_Facts;
with CuBit.Display_Protocol;
with CuBit.Display_Layouts;
with CuBit.Display_Arrangement;
with CuBit.Desktop_Messages;
with CuBit.Input;
with Absolute_Pointer;
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
with Text_Fit;
with CuBit.UI.Window_Chrome;
with Desktop_Wallpaper;
with Desktop_Wallpaper_Assets;
with Desktop_Icon_Assets;
with Icon_Edge_Choice;
with Desktop_Wallpaper_Layers;
with Desktop_Window_Placement;
with Desktop_Settings;
with Desktop_Time_Zone_Setting;
with Time_Zones;
with Desktop_Launch;
with Desktop_Launch_Notice;
with Desktop_Notifications;
with Desktop_Task_Groups;
with Desktop_Surface_Limits;
with Desktop_Heap_Objects;
with Desktop_Deskbar_Layout;
with Desktop_Deskbar_Style;
with Desktop_Status_Items;
with Desktop_Launch_Refresh;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with Desktop_Status_Refresh;
with Desktop_GPU_Wake;
with Desktop_Launch_Menus;
with Desktop_Clipboard;
with Clipboard_Store;
with Clipboard_Menu;
with Key_Repeat;
with Desktop_Key_Repeat_Settings;
with Desktop_Display_Scale_Settings;
with Desktop_Display_Modeset_Settings;
with Desktop_Display_Mode_Settings;
with Desktop_Mode_Trial;
with Desktop_Auto_Scale;
with Desktop_GPU_Power_Settings;
with Client_Popup_Layout;
with Client_Tooltip_Policy;
with Desktop_Tooltips;
with CCL.Interfaces.Desktop_Launch;
with CuBit.Appearance;
with CuBit.Config;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with CuBit.UI.Theme_Data;
with CuBit.Desktop_Protocol;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Startup;
with CuBit.Startup_Tables;
with CuBit.Startup_State;
with Startup;
with Desktop_Endpoints;
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
   INPUT_REPLY_MORE_PENDING : constant Protocol_Flags := 1;

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
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

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
   KEY_LEFT           : constant Unsigned_8 := 16#4B#;
   KEY_RIGHT          : constant Unsigned_8 := 16#4D#;
   KEY_TAB            : constant Unsigned_8 := 16#0F#;
   KEY_SPACE          : constant Unsigned_8 := 16#39#;
   KEY_DELETE         : constant Unsigned_8 := 16#53#;
   --  Super+N opens the Deskbar panel; Super+T puts keyboard focus on the
   --  taskbar.
   KEY_DESKBAR_N      : constant Unsigned_8 := 16#31#;
   KEY_TASKBAR_T      : constant Unsigned_8 := 16#14#;
   KEY_LEFT_SUPER     : constant Unsigned_8 := 16#5B#;
   KEY_RIGHT_SUPER    : constant Unsigned_8 := 16#5C#;
   --  Super+V opens the clipboard manager.
   KEY_CLIPBOARD_V    : constant Unsigned_8 := 16#2F#;
   KEY_EXTENDED_PREFIX : constant Unsigned_8 := 16#E0#;
   --  Set-1 multimedia keys (after the extended prefix).
   KEY_MUTE           : constant Unsigned_8 := 16#20#;
   KEY_VOLUME_DOWN    : constant Unsigned_8 := 16#2E#;
   KEY_VOLUME_UP      : constant Unsigned_8 := 16#30#;

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
     (Positive (Unsigned_64'Min (Unsigned_64 (Desktop_Storage_Policy.Maximum_Bytes),
       8 * DP.Maximum_Buffer_Bytes)));
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
   --  The menu label of the pending launch: what a launch notice names.
   launchLabel : Desktop_Launch_Notice.Name_Text := (others => ' ');
   launchLabelLength : Desktop_Launch_Notice.Name_Length := 0;
   --  The launched children not yet showing a window (Desktop_Launch_Notice);
   --  a failure to start is posted to the Deskbar's notification centre.
   launchWatch : Desktop_Launch_Notice.Watch_Table;
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
   procedure exitProcess (Status : Integer)
     with Import, Convention => C, External_Name => "_exit", No_Return;
   --  Every exit says why first (a Warning naming the exit's source
   --  location, flushed to logstore): Desktop never exits silently (NUC
   --  freeze 2026-10-12: a silent _exit left the screen on its last frame
   --  and nothing in Logs).
   procedure exitCompositor
     (Status : Integer;
      Where : String := GNAT.Source_Info.Source_Location;
      Unit : String := GNAT.Source_Info.Enclosing_Entity) with No_Return;
   procedure exitCompositor
     (Status : Integer;
      Where : String := GNAT.Source_Info.Source_Location;
      Unit : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Desktop_Logs.Warn ("desktop: exiting (code" & Status'Image & ") at " & Where & " in " & Unit);
      Desktop_Log_IO.Flush;
      exitProcess (Status);
   end exitCompositor;
   Bootstrap_CPU : Boolean := False;
   --  The bootstrap screen's stage line (drawBootstrapDiagnostics).
   Stage_Presenting : aliased constant String := "bootstrap frame (Display)";
   Waiting_Display  : aliased constant String := "Display to show this frame";
   Stage_Renderer   : aliased constant String := "renderer startup next";
   Waiting_GPU      : aliased constant String :=
     "the render service (intel-gpu) to create the Vulkan device; " &
     "if this stays, Desktop is blocked there";
   bootstrapStage   : access constant String := Stage_Presenting'Access;
   bootstrapWaiting : access constant String := Waiting_Display'Access;
   --  While the renderer worker runs (Wait_For_Renderer), the bootstrap
   --  screen is redrawn every Bootstrap_Tick_Ms with the step in progress,
   --  so a photo shows whether Desktop is alive and where startup stands.
   --  Renderer_Startup_Budget_Ms bounds the whole startup, whatever the GPU
   --  stack does (it may block outside IPC too). Each GPU-service call is
   --  bounded separately (5 s, Desktop_Renderer_Startup) and a stalled
   --  service fails the rest at once (Native_GPU_Calls), so a service that
   --  never answers ends startup by the bound; the budget is that plus 1 s.
   Bootstrap_Tick_Ms          : constant := 500;
   Renderer_Startup_Budget_Ms : constant := 6_000;
   rendererWorkerActive : Boolean := False;
   rendererStartedMs    : Unsigned_64 := 0;
   bootstrapFrames      : Unsigned_64 := 0;
   bootstrapReleased    : Boolean := False;
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
   --  DESKTOP-FIRST-SCENE was printed for the output.
   firstSceneReported : array (Output_Index) of Boolean := [others => False];
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
      --  Frames Display dropped in a row (CP.Drops); bounded below.
      Dropped_In_Row : Natural := 0;
   end record;
   presentations : array (Output_Index) of Output_Presentation;
   --  Consecutive dropped frames tolerated on one output before the
   --  transfer counts as broken; and all dropped frames, for the log.
   Max_Dropped_In_Row : constant := 8;
   droppedFrames : Unsigned_64 := 0;

   --  Output mode changes (a resized VM window; docs/vm-guide.md). Display
   --  answers the watch when its catalog changes; an output whose host
   --  preference changed is re-laid out: release every output, ask Display
   --  for the mode (Set_Mode), reopen, carry the scale over. The preference
   --  last acted on is remembered, so a mode the backend could only
   --  approximate (OM.Fit) is not requested again and again.
   package OM renames CuBit.Output_Modes;
   use type OM.Mode, OM.Mode_Count, OM.Rate_Choice, Desktop_Mode_Trial.Phase, Desktop_Mode_Trial.Time,
     Desktop_Settings.Monitor_Mode_List, Desktop_Settings.Mode_Status;
   MODE_CALL_DEADLINE_MS : constant := 15_000;
   outputRevisionSeen : OM.Revision := 1;
   outputWatchToken : Unsigned_64 := 0;
   --  The first pass runs at startup: an Intel output's best admitted mode
   --  is in Display's catalog from discovery, before this watch exists, so
   --  no change would ever announce it.
   outputChangeNoticed : Boolean := True;
   firstOutputPass : Boolean := True;
   type Wanted_Mode is record
      Pending : Boolean := False;
      Mode : OM.Mode_Request;
   end record;
   wantedModes : array (Output_Index) of Wanted_Mode;
   --  Settings > Monitor (DISP-016): a chosen mode on its way to the
   --  primary output (Switching: asked for, the outputs reopening), then on
   --  trial until the user keeps it or the question times out
   --  (Desktop_Mode_Trial). Revert_Switch: going back, no question.
   type Switch_Kind is (No_Switch, Trial_Switch, Revert_Switch);
   type Mode_Switch is record
      Kind : Switch_Kind := No_Switch;
      Output : Output_Index := 0;
      Previous, Trial : OM.Mode_Request;
      Auto : Boolean := False;
      Accepted : Boolean := False;
   end record;
   modeSwitch : Mode_Switch;
   modeTrial : Desktop_Mode_Trial.State;
   --  The countdown Settings shows (redrawn when it changes).
   trialSecondsShown : Natural := 0;
   --  The Monitor page needs re-reading once the outputs reopen.
   monitorRefreshPending : Boolean := False;
   actedPreference : array (Output_Index) of OM.Mode;
   modeChangePending : Boolean := False;
   --  False once Display (or its GPU) answered that it has no mode watch.
   outputWatchUsable : Boolean := True;
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

   --  The surface table starts at INITIAL_SURFACES and doubles whenever a
   --  new surface finds it full (growSurfaceTables), with memory from
   --  CuAlloc; the protocol's surface capacity type is the only bound.
   MAX_SURFACES : constant := Desktop_Surface_Limits.Maximum_Surfaces;
   pragma Compile_Time_Error
     (MAX_SURFACES /= Natural (DP.Surface_Capacity'Last), "the surface table's bound is the protocol's");
   INITIAL_SURFACES : constant := 8;
   --  Source loans per surface: its publication slots and one replacement
   --  acquisition. The loan table grows with the surface table.
   LOANS_PER_SURFACE : constant := Surface_Policy.Slot'Last + 1;
   package Source_Loans renames Compositor_Source_Loans;
   use type Source_Loans.Phase;
   use type Source_Loans.Ticket;
   type Source_Loan is record
      Grant : MG.Grant_Reference;
      Address : System.Address := System.Null_Address;
   end record;
   type Source_Loan_Table is array (Source_Loans.Slot range <>) of Source_Loan;
   type Loan_Store (Capacity : Source_Loans.Slot) is record
      Policy : Source_Loans.State (Capacity);
      Loans  : Source_Loan_Table (1 .. Capacity);
   end record;
   package Loan_Heap is new Desktop_Heap_Objects (Loan_Store);
   loans : Loan_Heap.Reference;
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

   package Window_Chrome renames CuBit.UI.Window_Chrome;

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
      -- The app's identity colour (its launcher entry's, else the theme
      -- accent) and the stripe colours precomputed from it (refreshStripe).
      appColor : Desktop_Launch.App_Color := Desktop_Launch.Theme_Accent;
      stripe   : Window_Chrome.Window_Colors;
      --  The Deskbar (Desktop_Task_Groups): the app this window belongs to
      --  (its launcher entry, else its own process), the entry's icon and
      --  label, and when the window opened (buttons stand in that order).
      appKey   : Desktop_Task_Groups.Group_Key := 0;
      appIcon  : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
      appLabel : String (1 .. Desktop_Launch.Maximum_Label) := [others => ' '];
      appLabelLength : Natural range 0 .. Desktop_Launch.Maximum_Label := 0;
      taskOrder : Unsigned_64 := 0;
      --  The task button's label widths in the taskbar's font, measured
      --  once whenever the title or the app changes (not per frame): the
      --  title (one window) and the app's label (a group).
      labelsMeasured : Boolean := False;
      titleWidth, appLabelWidth : Natural := 0;
   end record;

   package Focus_Policy renames Compositor_Focus;
   subtype SurfaceIndex is Natural range 0 .. MAX_SURFACES - 1;
   type SurfaceTable is array (SurfaceIndex range <>) of Surface;

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

   --  A client blocks in OP_INPUT_WAIT while desktop retains its saved
   --  one-use reply. One per surface makes waiter ownership explicit and
   --  prevents one client from consuming another client's wake.
   type InputWaiter is record
      active      : Boolean := False;
      owner       : Process_ID := No_Process;
      target      : Unsigned_64 := 0;
      afterSerial : Unsigned_64 := 0;
      deadline    : Unsigned_64 := 0;
      saved       : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
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
     array (SurfaceIndex range <>) of SurfaceInputChannel;

   --  What the task buttons look like (Desktop_Task_Groups), by place
   --  (from 0): a frame repaints only the buttons that changed since the
   --  last one (opened, closed, regrouped, focused, minimized, moved).
   type Task_Look is record
      Box       : Rect := (others => 0);
      Key       : Desktop_Task_Groups.Group_Key := 0;
      First     : Unsigned_64 := 0;
      Members   : Desktop_Task_Groups.Window_Count := 0;
      Active    : Boolean := False;
      Minimized : Boolean := False;
   end record;
   type Task_Looks is array (Natural range <>) of Task_Look;

   --  The surface table (back to front), each surface's input channel and
   --  the task buttons drawn last, all one size (Last + 1 entries).
   type Surface_Store (Last : SurfaceIndex) is record
      Surfaces : SurfaceTable (0 .. Last);
      Channels : SurfaceInputChannelTable (0 .. Last);
      Tasks    : Task_Looks (0 .. Last);
   end record;
   package Store_Heap is new Desktop_Heap_Objects (Surface_Store);
   use type Store_Heap.Reference;
   store : Store_Heap.Reference;
   function surfaceLast return SurfaceIndex is (store.Last);

   -- These states survive channel clearing and surface destruction. Each
   -- maintenance turn retries one exact retained acquisition at most once.
   package Input_Transfers is new Compositor_Input_Delivery_Pool
     (Desktop_Input_Transfer.Engine);
   package Transfer_Heap is new Desktop_Heap_Objects (Input_Transfers.State);
   inputTransfers : Transfer_Heap.Reference;

   --  Make the surface table Count surfaces large, with the tables sized by
   --  it (input channels, task buttons, source loans, input transfers),
   --  keeping every entry where it is. All or nothing: Grown is False when
   --  the heap cannot hold the larger tables, and nothing changes.
   procedure resizeSurfaceTables (Count : Positive; Grown : out Boolean)
     with Pre => Count <= MAX_SURFACES and then (store = null or else Count > store.Last + 1);
   procedure resizeSurfaceTables (Count : Positive; Grown : out Boolean) is
      subtype New_Store is Surface_Store (Count - 1);
      subtype New_Loans is Loan_Store (Count * LOANS_PER_SURFACE);
      subtype New_Transfers is Input_Transfers.State (Count);
      storeAt : constant System.Address :=
        Store_Heap.Allocate (New_Store'Max_Size_In_Storage_Elements);
      loansAt : constant System.Address :=
        (if storeAt = System.Null_Address then System.Null_Address
         else Loan_Heap.Allocate (New_Loans'Max_Size_In_Storage_Elements));
      transfersAt : constant System.Address :=
        (if loansAt = System.Null_Address then System.Null_Address
         else Transfer_Heap.Allocate (New_Transfers'Max_Size_In_Storage_Elements));
   begin
      Grown := False;
      if transfersAt = System.Null_Address then
         Loan_Heap.Free (loansAt);
         Store_Heap.Free (storeAt);
         return;
      end if;
      declare
         --  Declared without Import: every entry starts empty.
         pragma Warnings (Off, "*may modify overlaid storage*");
         pragma Warnings (Off, "*to suppress initialization*");
         newStore : New_Store with Address => storeAt;
         newLoans : New_Loans with Address => loansAt;
         newTransfers : New_Transfers with Address => transfersAt;
         pragma Warnings (On, "*to suppress initialization*");
         pragma Warnings (On, "*may modify overlaid storage*");
      begin
         if store /= null then
            newStore.Surfaces (store.Surfaces'Range) := store.Surfaces;
            newStore.Channels (store.Channels'Range) := store.Channels;
            newStore.Tasks (store.Tasks'Range) := store.Tasks;
            Source_Loans.Grow (loans.Policy, newLoans.Policy);
            newLoans.Loans (loans.Loans'Range) := loans.Loans;
            Input_Transfers.Grow (inputTransfers.all, newTransfers);
         end if;
      end;
      Store_Heap.Free (store);
      Loan_Heap.Free (loans);
      Transfer_Heap.Free (inputTransfers);
      store := Store_Heap.To_Reference (storeAt);
      loans := Loan_Heap.To_Reference (loansAt);
      inputTransfers := Transfer_Heap.To_Reference (transfersAt);
      Grown := True;
   end resizeSurfaceTables;

   --  A free slot for a new surface, growing the table (doubling) when every
   --  slot is used. Slot is -1 when the table cannot grow: it is at the
   --  protocol's bound, or the heap is out of memory.
   procedure freeSurfaceSlot (Slot : out Integer) is
      Grown : Boolean;
      Used : constant Positive := store.Last + 1;
   begin
      for I in store.Surfaces'Range loop
         if not store.Surfaces (I).used then
            Slot := I;
            return;
         end if;
      end loop;
      Slot := -1;
      if Used < MAX_SURFACES then
         resizeSurfaceTables (Natural'Min (2 * Used, MAX_SURFACES), Grown);
         if Grown then
            Slot := Used;
            debugPrint ("desktop: surface table grown to" & Natural'Image (store.Last + 1) & LF);
         end if;
      end if;
   end freeSurfaceSlot;
   inputQueueOverflows : Unsigned_64 := 0;

   cursorX : Natural := 80;
   cursorY : Natural := 80;
   cursorStyle : Pointer_Cursor_Style := POINTER_DEFAULT;
   lastButtons : Unsigned_64 := 0;

   MAX_INPUT_SOURCES : constant Natural := 8;
   subtype InputSourceIndex is Natural range 0 .. MAX_INPUT_SOURCES - 1;
   type InputSourceState is record
      used       : Boolean := False;
      identity   : Unsigned_64 := 0;  --  the publisher, as the kernel names it
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
   --  The integrated Deskbar (docs/development-backlog.md UI-027): the
   --  panel above the status button, the notification centre, one app's
   --  window list, and keyboard focus on the taskbar.
   package DN renames Desktop_Notifications;
   package TG renames Desktop_Task_Groups;
   package DBL renames Desktop_Deskbar_Layout;
   package DBS renames Desktop_Deskbar_Style;
   package DSI renames Desktop_Status_Items;
   package DT renames Desktop_Tooltips;
   package TTP renames Client_Tooltip_Policy;
   use type DT.Target_Kind;
   use type DN.Serial, DN.Count, DN.Urgency, TG.Group_Count, TG.Window_Count, TG.Group_Key;
   notifications : DN.History;
   deskbarPanelOpen : Boolean := False;
   --  A press on the status button, panel, window list or toast keeps its
   --  release; a press on the volume slider drags it.
   deskbarPointerCapture : Boolean := False;
   deskbarSliderDragging : Boolean := False;
   --  The app whose window list is open (by app, so restacking keeps it)
   --  and its selected row.
   groupListOpen : Boolean := False;
   groupListKey : TG.Group_Key := 0;
   groupListRow : TG.Window_Count := 0;
   --  Keyboard focus on the taskbar (Super+T): Apps, a task button (by
   --  place) or the status button.
   type Deskbar_Focus is (No_Focus, Apps_Focus, Task_Focus, Status_Focus);
   deskbarFocus : Deskbar_Focus := No_Focus;
   deskbarFocusTask : TG.Group_Count := 0;
   --  The panel's keyboard row.
   type Panel_Row is (Volume_Row, Quiet_Row, Note_Row, Clear_Row);
   panelFocus : Panel_Row := Volume_Row;
   panelFocusNote : DN.Count := 0;
   --  The panel drawn last (its height follows the history), for damage.
   panelShown : Rect := (others => 0);
   --  Every window gets the next opening order.
   nextTaskOrder : Unsigned_64 := 1;
   --  The date and minute under the panel's clock, from the clock service.
   clockDateKnown : Boolean := False;
   clockDate : CuBit.Clocks.Snapshot;
   --  The volume keys and panel slider move in these steps.
   Volume_Step : constant := 5;
   --  Clipboard manager popup (docs/clipboard.md), toggled by Super+V.
   clipboardMenuOpen : Boolean := False;
   --  The surface the last routed key press went to (queueKey).
   keysRoutedTo : Unsigned_64 := 0;
   clipboardMenu : Clipboard_Menu.Menu_State;
   --  Super is a modifier: the Apps menu opens when Super is released
   --  without having been part of a chord such as Super+V.
   desktopSuperDown : Boolean := False;
   desktopSuperChord : Boolean := False;
   clockText : String (1 .. 5) := "--:--";
   --  The clock service's last reported quality, logged when it changes
   --  (why the Deskbar shows "--:--" is then in the boot log).
   clockQualityLogged : Boolean := False;
   clockQuality : CuBit.Clocks.Time_Quality := CuBit.Clocks.Unknown_Time;
   statusDueMs : Unsigned_64 := 0;
   --  Status area clock and volume refresh period (and the latest next read).
   Status_Interval_Ms : constant := 10_000;
   desktopExtendedPrefix : Boolean := False;
   desktopShiftDown : Boolean := False;
   desktopCtrlDown  : Boolean := False;
   desktopAltDown   : Boolean := False;
   desktopCapsLockOn : Boolean := False;
   --  Keyboard auto-repeat (Key_Repeat): Desktop synthesizes repeats for
   --  every keyboard and application, and drops the devices' own.
   package KR renames Key_Repeat;
   use type KR.Destination, KR.Press_Outcome;
   keyRepeat : KR.State;
   keyRepeatSettings : KR.Settings := KR.DEFAULT_SETTINGS;
   --  desktop.display.scale (Load_Display_Scale): Auto until the user picks
   --  a scale in Settings > Displays.
   displayScale : Desktop_Display_Scale_Settings.Preference := Desktop_Display_Scale_Settings.Default;

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

   --  The caption (border, app stripe, title bar) grabs moves; the client
   --  starts below the title/menu rule (CuBit.UI.Window_Chrome).
   TITLE_HEIGHT : constant Natural := Window_Chrome.Client_Top - Window_Chrome.Rule_Height;
   BORDER_SIZE  : constant Natural := 6;
   CLIENT_INSET_X : constant Natural := Window_Chrome.Border_Width;
   CLIENT_INSET_TOP : constant Natural := Window_Chrome.Client_Top;
   CLIENT_INSET_BOTTOM : constant Natural := Window_Chrome.Border_Width;
   DROP_SHADOW_DEPTH : constant Positive := 3;
   --  Compositor decorations extend beyond the client-visible window
   --  geometry. Damage retains one extra pixel beyond the shadow so every
   --  pixel painted by the old geometry is restored during movement.
   WINDOW_VISUAL_MARGIN : constant Natural := DROP_SHADOW_DEPTH + 1;
   TASKBAR_H    : constant Natural := DBS.Bar_Height;
   MENU_W       : constant Natural := 250;
   --  Menu layout: category rows every 34 pixels from 42, a separator,
   --  then Power; a submenu beside the open category, rows every 34.
   LAUNCH_FIRST_Y : constant Natural := 42;
   LAUNCH_STEP    : constant Natural := 34;
   SUBMENU_PAD    : constant Natural := 6;

   function MENU_H return Natural is
     (LAUNCH_FIRST_Y + launchRows * LAUNCH_STEP - 2 + 8 + LAUNCH_STEP);
   pragma Compile_Time_Error
     (MAX_SURFACES /= TG.Max_Windows, "Desktop_Task_Groups covers the surface table");
   pragma Compile_Time_Error
     (MAX_SURFACES /= DBL.Max_Buttons, "Desktop_Deskbar_Layout covers the surface table");
   pragma Compile_Time_Error
     (MAX_SURFACES /= DT.MAXIMUM_WINDOWS, "Desktop_Tooltips covers the surface table");
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
   chromePalette : Window_Chrome.Palette := Window_Chrome.Alloy_Light;
   --  The window button under the pointer (hover feedback), if any.
   chromeHoverId     : Unsigned_64 := 0;
   chromeHoverButton : Pointer_Action := DRAG_NONE;
   --  Tooltips on the window buttons and the Deskbar's status icons
   --  (Desktop_Tooltips): when (the policy), where it is drawn, and the
   --  focused control a keyboard tip hangs off (empty: it follows the
   --  pointer).
   tipState   : TTP.Tooltip_State;
   --  The client window the pointer last hovered inside (0: none). When the
   --  pointer leaves it, the window gets one last move just past its
   --  client area's right edge, so its own hover state and tooltips end.
   hoverInsideId : Unsigned_64 := 0;
   tipBox     : Rect := (others => 0);
   tipControl : Rect := (others => 0);

   --  A window's launcher entry: the one that started its owner (or the
   --  Settings entry for the built-in Settings window); 0 for none.
   function launchEntryOf (Owner : Process_ID; Kind : App_Kind) return Natural is
      use type Desktop_Launch.Entry_Kind;
   begin
      for I in 1 .. launchMenu.Count loop
         if (Kind = APP_SETTINGS and then launchMenu.Entries (I).Kind = Desktop_Launch.Internal_Settings)
           or else (Owner /= No_Process and then launchPids (I) = Owner)
         then
            return I;
         end if;
      end loop;
      return 0;
   end launchEntryOf;

   --  An app's Deskbar group: FNV-1a over its program (its label for a
   --  built-in entry), so every window of one app shares a button whatever
   --  process opened it, and a menu refresh never changes it.
   function appKeyOf (Item : Desktop_Launch.Entry_Info) return TG.Group_Key is
      FNV_Offset : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
      FNV_Prime  : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
      Hash : Unsigned_64 := FNV_Offset;
      Name : constant String :=
        (if Item.Program_Length > 0 then Desktop_Launch.Program_Of (Item)
         else Desktop_Launch.Label_Of (Item));
   begin
      for C of Name loop
         Hash := (Hash xor Unsigned_64 (Character'Pos (C))) * FNV_Prime;
      end loop;
      return TG.Group_Key (Hash);
   end appKeyOf;

   procedure refreshStripe (S : in out Surface) is
   begin
      S.stripe := Window_Chrome.For_App
        ((if S.appColor.Declared then CuBit.UI.Theme_Color (S.appColor.Value)
          else CuBit.UI.Current_Theme.accent), chromePalette);
   end refreshStripe;

   --  The launcher entry whose program Owner runs, however it was started
   --  (by init, another app or the launcher); 0 for none.
   function launchEntryByProgram (Owner : Process_ID) return Natural;

   --  A new window's app: its entry's colour (window stripe and task
   --  button), icon, label and group, and its opening order. The entry is
   --  the one that started the owner, else the one for the program it runs.
   --  A window of a program with no entry is its own process's app, drawn
   --  with the CuBit logo.
   procedure assignApp (S : in out Surface; Owner : Process_ID) is
      Started_By : constant Natural := launchEntryOf (Owner, S.appKind);
      Index : constant Natural :=
        (if Started_By > 0 then Started_By else launchEntryByProgram (Owner));
   begin
      if Index in 1 .. launchMenu.Count then
         S.appColor := launchMenu.Entries (Index).Color;
         S.appIcon := launchMenu.Entries (Index).Icon;
         S.appLabel := launchMenu.Entries (Index).Label;
         S.appLabelLength := launchMenu.Entries (Index).Label_Length;
         S.appKey := appKeyOf (launchMenu.Entries (Index));
      else
         S.appColor := Desktop_Launch.Theme_Accent;
         S.appIcon := Desktop_Icons.Start;
         S.appLabelLength := 0;
         S.appKey := TG.Group_Key (To_Word (Owner));
      end if;
      S.taskOrder := nextTaskOrder;
      nextTaskOrder := nextTaskOrder + 1;
      S.labelsMeasured := False;
      refreshStripe (S);
   end assignApp;

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
      chromePalette := Window_Chrome.Colors (appearance.Scheme);
      for S of store.Surfaces loop
         if S.used then refreshStripe (S); end if;
      end loop;
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
   function deskbarColors return DBS.Palette is (DBS.Colors (appearance.Scheme));

   statsStartMs      : Unsigned_64 := 0;
   statsEvents       : Unsigned_64 := 0;
   statsKeyboardEvents : Unsigned_64 := 0;
   --  Auto-repeats delivered, and skipped because the client had not taken
   --  the previous one; device repeats (PS/2 typematic) dropped.
   statsKeyRepeats : Unsigned_64 := 0;
   statsKeyRepeatsSkipped : Unsigned_64 := 0;
   statsDeviceRepeats : Unsigned_64 := 0;
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

   --  desktop.input.key-repeat (Desktop_Key_Repeat_Settings). Absent keeps
   --  the defaults; an invalid value is reported and keeps them too. The
   --  effective settings are logged either way.
   --  gpu.power.waitboost (Desktop_GPU_Power_Settings): logged either way,
   --  handed to the GPU driver once its queue is open.
   procedure Load_GPU_Power is
      use type CuBit.Config.ConfigStatus;
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      On     : Boolean := Desktop_GPU_Power_Settings.Default_Waitboost;
      OK     : Boolean := False;
   begin
      CuBit.Config.get (Desktop_GPU_Power_Settings.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. Desktop_GPU_Power_Settings.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            Desktop_GPU_Power_Settings.Decode (Source, On, OK);
         end;
         if not OK then
            On := Desktop_GPU_Power_Settings.Default_Waitboost;
            debugPrint ("desktop: invalid " & Desktop_GPU_Power_Settings.Config_Key &
                        "; using the default" & LF);
         end if;
      end if;
      Desktop_GPU_Wake.Set_Waitboost (On);
      debugPrint ("desktop: gpu waitboost " & (if On then "on" else "off") &
                  (if OK then " (configured)" else " (default)") & LF);
   end Load_GPU_Power;
   --  desktop.display.modeset (Desktop_Display_Modeset_Settings): whether
   --  to ask for an Intel output's best admitted mode; logged either way.
   displayModeset : Boolean := Desktop_Display_Modeset_Settings.Default_Enabled;
   procedure Load_Display_Modeset is
      use type CuBit.Config.ConfigStatus;
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      OK     : Boolean := False;
   begin
      CuBit.Config.get (Desktop_Display_Modeset_Settings.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. Desktop_Display_Modeset_Settings.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            Desktop_Display_Modeset_Settings.Decode (Source, displayModeset, OK);
         end;
         if not OK then
            displayModeset := Desktop_Display_Modeset_Settings.Default_Enabled;
            debugPrint ("desktop: invalid " & Desktop_Display_Modeset_Settings.Config_Key &
                        "; using the default" & LF);
         end if;
      end if;
      debugPrint ("desktop: display modeset " & (if displayModeset then "on" else "off") &
                  (if OK then " (configured)" else " (default)") & LF);
   end Load_Display_Modeset;
   procedure Load_Key_Repeat is
      use type CuBit.Config.ConfigStatus;
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      Loaded : KR.Settings;
      OK     : Boolean := False;
   begin
      CuBit.Config.get (Desktop_Key_Repeat_Settings.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. Desktop_Key_Repeat_Settings.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            Desktop_Key_Repeat_Settings.Decode (Source, Loaded, OK);
         end;
      end if;
      if OK then
         keyRepeatSettings := Loaded;
      elsif Status /= CuBit.Config.NotFound then
         debugPrint ("desktop: invalid " & Desktop_Key_Repeat_Settings.Config_Key &
                     "; using the default key repeat" & LF);
      end if;
      debugPrint ("desktop: key repeat " &
                  (if keyRepeatSettings.Enabled then "on" else "off") &
                  " delay_ms=" & Decimal (keyRepeatSettings.Initial_Delay) &
                  " rate_hz=" & Decimal (keyRepeatSettings.Rate) &
                  (if OK then " (configured)" else " (default)") & LF);
   end Load_Key_Repeat;

   --  desktop.display.scale (Desktop_Display_Scale_Settings). Absent means
   --  Auto; an invalid value is reported and keeps Auto. Logged either way.
   procedure Load_Display_Scale is
      use type CuBit.Config.ConfigStatus;
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      Loaded : Desktop_Display_Scale_Settings.Preference;
      OK     : Boolean := False;
   begin
      CuBit.Config.get (Desktop_Display_Scale_Settings.Config_Key, Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. Desktop_Display_Scale_Settings.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            Desktop_Display_Scale_Settings.Decode (Source, Loaded, OK);
         end;
      end if;
      if OK then
         displayScale := Loaded;
      elsif Status /= CuBit.Config.NotFound then
         debugPrint ("desktop: invalid " & Desktop_Display_Scale_Settings.Config_Key &
                     "; using the automatic scale" & LF);
      end if;
      debugPrint ("desktop: display scale " & Desktop_Display_Scale_Settings.Encode (displayScale) &
                  (if OK then " (configured)" else " (default)") & LF);
   end Load_Display_Scale;

   --  The scale an output starts at: the configured one, or (Auto, or a
   --  configured scale that would leave it less than the arrangement
   --  minimum) the one its size asks for (Desktop_Auto_Scale).
   function startupScale (Screen : DG.Output) return DG.UI_Scale is
      package LR renames Compositor_Layout_Resize;
      Width : constant DG.Physical_Extent := LR.Upright_Width (Screen);
      Height : constant DG.Physical_Extent := LR.Upright_Height (Screen);
   begin
      if not displayScale.Auto and then
        LR.Logical_Extent (Width, displayScale.Scale) >= CuBit.Display_Arrangement.Minimum_Width and then
        LR.Logical_Extent (Height, displayScale.Scale) >= CuBit.Display_Arrangement.Minimum_Height
      then
         return displayScale.Scale;
      end if;
      return CuBit.Display_Arrangement.Factor (Desktop_Auto_Scale.Choose (Width, Height));
   end startupScale;

   --  The Settings Monitor page's contents (body after callDisplay).
   function monitorInfo return Desktop_Settings.Monitor_Info;
   --  Settings > Monitor: switch the primary output to the chosen row
   --  (then ask "Keep this display mode?"), and take the answer.
   procedure applyMonitorChoice;
   procedure pumpModeTrial;

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
      Cursor_X, Cursor_Y, Max_Frame_Us, Max_Motion_Us, Forced,
      Key_Repeats, Key_Repeats_Skipped, Device_Repeats : Unsigned_64 := 0;
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
                     " key_repeat=" & Decimal (C.Key_Repeats) &
                     " key_repeat_skip=" & Decimal (C.Key_Repeats_Skipped) &
                     " device_repeat=" & Decimal (C.Device_Repeats) &
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
         Forced => reportsForced, Key_Repeats => statsKeyRepeats,
         Key_Repeats_Skipped => statsKeyRepeatsSkipped, Device_Repeats => statsDeviceRepeats);
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
      --  This process's mailbox drops (kernel READ_STATISTICS); Last when
      --  the kernel refused.
      declare
         Statistics : CuBit.System_Facts.SP.Statistics;
         Read : Boolean;
      begin
         CuBit.System_Facts.Read_Statistics (Statistics, Read);
         eventDrops := (if Read then Statistics.Event_Drops else Unsigned_64'Last);
      end;
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
      statsKeyRepeats := 0;
      statsKeyRepeatsSkipped := 0;
      statsDeviceRepeats := 0;
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
      return clampRect ((x => primaryBounds.x + DBS.Bar_Inset, y => taskbarY + DBS.Button_Top,
                         w => DBS.Apps_Width, h => DBS.Button_Height));
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

   function uiTextWidth (s : String; face : CuBit.Fonts.Face := CuBit.Fonts.Sans) return Natural;
   function surfaceTitle (s : Surface) return String;
   --  Whether the notification toast shows (defined with the drawing).
   function toastShown return Boolean;
   --  The tooltip, drawn over everything (defined with its targets).
   procedure drawTooltip;

   --  The Deskbar's task buttons (Desktop_Task_Groups): one per app, in
   --  the order its first window opened, whatever the stacking.
   function taskWindows return TG.Windows is
      W : TG.Windows (0 .. TG.Window_Slot (surfaceLast));
   begin
      for I in store.Surfaces'Range loop
         W (TG.Window_Slot (I)) :=
           (Present   => store.Surfaces (I).used and then
                         (store.Surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0,
            Key       => store.Surfaces (I).appKey,
            Order     => store.Surfaces (I).taskOrder,
            Minimized => store.Surfaces (I).minimized,
            Focused   => store.Surfaces (I).id = focusSurface);
      end loop;
      return W;
   end taskWindows;

   function taskModel (W : TG.Windows) return TG.Model is
      M : TG.Model (W'Last, TG.Group_Index (W'Length));
   begin
      TG.Build (W, M);
      return M;
   end taskModel;

   --  The group of app Key (0 if it has no window).
   function groupOfKey (M : TG.Model; Key : TG.Group_Key) return TG.Group_Count is
   begin
      for G in 1 .. M.Count loop
         if M.Groups (G).Key = Key then
            return G;
         end if;
      end loop;
      return 0;
   end groupOfKey;

   --  What the status button shows (Desktop_Status_Items): an item only
   --  while Desktop holds the granted service behind it. Volume: the mixer
   --  (Desktop's "mixer" startup handle) answered. Network, Bluetooth,
   --  Battery, Power_Profile and Brightness have no service yet.
   function statusShown return DSI.Presence is
     ([DSI.Volume => masterAudio.Available, others => False]);

   --  The status button: notification count (when unread), the shown
   --  items, then the clock.
   function statusWidth return Natural is
     (2 * DBS.Status_Padding +
      (if DN.Unread (notifications) > 0 then DBS.Badge_Size + DBS.Status_Item_Gap else 0) +
      DSI.Count (statusShown) * (Desktop_Icons.ICON_SIZE + DBS.Status_Item_Gap) +
      uiTextWidth ("00:00"));

   function statusRect return Rect is
      width : constant Natural := statusWidth;
   begin
      if primaryBounds.w < width + 2 * DBS.Bar_Inset then
         return (others => 0);
      end if;
      return clampRect
        ((x => primaryBounds.x + primaryBounds.w - DBS.Bar_Inset - width,
          y => taskbarY + DBS.Status_Top, w => width, h => DBS.Status_Height));
   end statusRect;

   --  Task buttons share the bar between Apps (and its separator) and the
   --  status button.
   function taskAreaLeft return Natural is
     (primaryBounds.x + DBS.Bar_Inset + DBS.Apps_Width + 2 * DBS.Separator_Margin + 1);
   function taskAreaRight return Natural is
     (if statusRect.x > taskAreaLeft + DBS.Button_Gap then statusRect.x - DBS.Button_Gap
      else taskAreaLeft);

   --  Each window's label widths, measured where they changed.
   procedure measureTaskLabels is
   begin
      for I in store.Surfaces'Range loop
         if store.Surfaces (I).used and then not store.Surfaces (I).labelsMeasured then
            --  In the emphasis face, the wider one a label is drawn in
            --  (the focused window's button), so focus never resizes it.
            store.Surfaces (I).titleWidth :=
              uiTextWidth (surfaceTitle (store.Surfaces (I)), CuBit.Fonts.Sans_Semibold);
            store.Surfaces (I).appLabelWidth :=
              uiTextWidth (store.Surfaces (I).appLabel (1 .. store.Surfaces (I).appLabelLength),
                           CuBit.Fonts.Sans_Semibold);
            store.Surfaces (I).labelsMeasured := True;
         end if;
      end loop;
   end measureTaskLabels;

   --  A group's button as wide as its content (as drawTaskButtons lays it
   --  out): icon, label (the app's for a group, else the title), the count
   --  badge, padding; between Task_Minimum and Task_Maximum.
   function taskContentWidth (G : TG.Group) return DBL.Button_Width_Range is
      S : Surface renames store.Surfaces (SurfaceIndex (G.First));
      label : constant Natural :=
        (if G.Members > 1 and then S.appLabelLength > 0 then S.appLabelWidth else S.titleWidth);
   begin
      return Natural'Max
        (DBS.Task_Minimum,
         Natural'Min
           (DBS.Task_Maximum,
            DBS.Icon_Left + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap + label +
            (if G.Members > 1 then DBS.Count_Gap + DBS.Count_Width else 0) + DBS.Label_Right));
   end taskContentWidth;

   --  A task button that shows only its app's icon: the last step before
   --  buttons no longer fit.
   TASK_ICON_ONLY : constant := DBS.Icon_Left + Desktop_Icons.ICON_SIZE + DBS.Icon_Left;

   --  Every task button's place on the bar (Desktop_Deskbar_Layout.Spans):
   --  content-wide, shrinking to Task_Minimum with their labels cut when
   --  crowded, then to icons only when even that does not fit.
   --  W and M: the table's windows and their model, computed once by the
   --  caller, which then places every button from these spans (placeRect).
   function spansOf (W : TG.Windows; M : TG.Model) return DBL.Span_List is
      left  : constant Natural := taskAreaLeft;
      right : constant Natural := taskAreaRight;
      wanted : DBL.Width_List (1 .. Natural (M.Count)) := [others => DBS.Task_Minimum];
      active : DBL.Button_Count := 0;
   begin
      if right <= left or else right > DBL.Max_Coordinate then
         return [1 .. Natural (M.Count) => (X => 0, W => 0)];
      end if;
      measureTaskLabels;
      for G in 1 .. M.Count loop
         declare
            P : constant TG.Group_Index := TG.Position (W, M, G);
         begin
            wanted (Natural (P)) := taskContentWidth (M.Groups (G));
            if M.Groups (G).Active then
               active := Natural (P);
            end if;
         end;
      end loop;
      declare
         readable : constant DBL.Span_List :=
           DBL.Spans (left, right, wanted, DBS.Task_Minimum, DBS.Button_Gap, active);
      begin
         if (for all P in readable'Range => readable (P).W /= 0) then
            return readable;
         end if;
         return DBL.Spans (left, right, wanted, TASK_ICON_ONLY, DBS.Button_Gap, active);
      end;
   end spansOf;

   --  The button at place P, of these spans.
   function placeRect (spans : DBL.Span_List; P : TG.Group_Index) return Rect is
      span : DBL.Span;
   begin
      if Natural (P) > spans'Last then
         return (others => 0);
      end if;
      span := spans (Natural (P));
      if span.W = 0 then
         return (others => 0);
      end if;
      return clampRect ((x => span.X, y => taskbarY + DBS.Button_Top,
                         w => span.W, h => DBS.Button_Height));
   end placeRect;


   --  A window's task button: its app's.
   function taskButtonRect (slot : SurfaceIndex) return Rect is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      G : constant TG.Group_Count := M.Of_Window (TG.Window_Slot (slot));
   begin
      if G = 0 then
         return (others => 0);
      end if;
      return placeRect (spansOf (W, M), TG.Position (W, M, G));
   end taskButtonRect;

   --  The Deskbar panel, right-aligned above the status button: a header
   --  (clock, date), quick settings (volume, do not disturb), then the
   --  newest notifications and a hint line.
   function panelNoteRows return Natural is
     (Natural'Min (Natural (DN.Length (notifications)), DBS.Panel_Visible_Notes));

   function panelHeight return Natural is
     (DBS.Panel_Stripe + DBS.Panel_Header + 2 * DBS.Panel_Section + 2 * DBS.Panel_Row +
      (if panelNoteRows = 0 then DBS.Panel_Row else panelNoteRows * DBS.Panel_Note_Row) +
      DBS.Panel_Footer);

   --  Panel and toast share the right edge and width.
   function deskbarPopupRect (height : Natural) return Rect is
      right : constant Natural := primaryBounds.x + primaryBounds.w;
   begin
      if right < DBS.Panel_Width + DBS.Bar_Inset or else
        taskbarY < height + DBS.Panel_Margin
      then
         return (others => 0);
      end if;
      return clampRect ((x => right - DBS.Bar_Inset - DBS.Panel_Width,
                         y => taskbarY - DBS.Panel_Margin - height,
                         w => DBS.Panel_Width, h => height));
   end deskbarPopupRect;

   function panelRect return Rect is (deskbarPopupRect (panelHeight));
   function toastRect return Rect is (deskbarPopupRect (DBS.Toast_Height));

   function panelVolumeRowRect return Rect is
     ((x => panelRect.x, y => panelRect.y + DBS.Panel_Stripe + DBS.Panel_Header + DBS.Panel_Section,
       w => panelRect.w, h => DBS.Panel_Row));
   function panelQuietRowRect return Rect is
     ((x => panelRect.x, y => panelVolumeRowRect.y + DBS.Panel_Row, w => panelRect.w, h => DBS.Panel_Row));
   function panelNotesHeaderRect return Rect is
     ((x => panelRect.x, y => panelQuietRowRect.y + DBS.Panel_Row, w => panelRect.w, h => DBS.Panel_Section));
   function panelNoteRect (row : Positive) return Rect is
     ((x => panelRect.x, y => panelNotesHeaderRect.y + DBS.Panel_Section + (row - 1) * DBS.Panel_Note_Row,
       w => panelRect.w, h => DBS.Panel_Note_Row));
   function panelNoteCloseRect (row : Positive) return Rect is
     ((x => panelNoteRect (row).x + panelNoteRect (row).w - DBS.Panel_Padding - DBS.Close_Box,
       y => panelNoteRect (row).y + (DBS.Panel_Note_Row - DBS.Close_Box) / 2,
       w => DBS.Close_Box, h => DBS.Close_Box));
   Clear_All_Text : constant String := "Clear all";
   function panelClearRect return Rect is
     ((x => panelNotesHeaderRect.x + panelNotesHeaderRect.w - DBS.Panel_Padding - uiTextWidth (Clear_All_Text),
       y => panelNotesHeaderRect.y, w => uiTextWidth (Clear_All_Text), h => DBS.Panel_Section));
   function muteIconRect return Rect is
     ((x => panelVolumeRowRect.x + DBS.Panel_Padding,
       y => panelVolumeRowRect.y + (DBS.Panel_Row - Desktop_Icons.ICON_SIZE) / 2,
       w => Desktop_Icons.ICON_SIZE, h => Desktop_Icons.ICON_SIZE));
   --  The slider's hit area: its track, as tall as the thumb.
   function volumeTrackRect return Rect is
     ((x => muteIconRect.x + Desktop_Icons.ICON_SIZE + 2 * DBS.Status_Item_Gap,
       y => panelVolumeRowRect.y + (DBS.Panel_Row - DBS.Thumb_Height) / 2,
       w => DBS.Slider_Width, h => DBS.Thumb_Height));
   function quietToggleRect return Rect is
     ((x => panelQuietRowRect.x + panelQuietRowRect.w - DBS.Panel_Padding - DBS.Toggle_Width,
       y => panelQuietRowRect.y + (DBS.Panel_Row - DBS.Toggle_Height) / 2,
       w => DBS.Toggle_Width, h => DBS.Toggle_Height));

   --  How many windows the open list shows (its app's).
   function groupListMembers return TG.Window_Count is
      M : constant TG.Model := taskModel (taskWindows);
      G : constant TG.Group_Count := groupOfKey (M, groupListKey);
   begin
      return (if G = 0 then 0 else M.Groups (G).Members);
   end groupListMembers;

   --  One app's window list, above its button.
   function groupListRect return Rect is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      G : constant TG.Group_Count := groupOfKey (M, groupListKey);
      button : Rect;
      height, x : Natural;
   begin
      if not groupListOpen or else G = 0 then
         return (others => 0);
      end if;
      button := placeRect (spansOf (W, M), TG.Position (W, M, G));
      height := 2 * DBS.List_Padding + Natural (M.Groups (G).Members) * DBS.List_Row;
      if isEmpty (button) or else taskbarY < height + DBS.Panel_Margin then
         return (others => 0);
      end if;
      x := Natural'Min (button.x, primaryBounds.x + primaryBounds.w - DBS.Bar_Inset - DBS.List_Width);
      return clampRect ((x => x, y => taskbarY - DBS.Panel_Margin - height,
                         w => DBS.List_Width, h => height));
   end groupListRect;

   function groupListRowRect (row : TG.Window_Rank) return Rect is
     ((x => groupListRect.x + DBS.List_Padding,
       y => groupListRect.y + DBS.List_Padding + (Natural (row) - 1) * DBS.List_Row,
       w => (if groupListRect.w > 2 * DBS.List_Padding then groupListRect.w - 2 * DBS.List_Padding else 0),
       h => DBS.List_Row));

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

   --  The task button under (x, y): its app's first window, -1 for none.
   function hitTaskButton (x, y : Natural) return Integer is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      spans : constant DBL.Span_List := spansOf (W, M);
   begin
      if not pointInRect (x, y, taskbarRect) then
         return -1;
      end if;
      for G in 1 .. M.Count loop
         if pointInRect (x, y, placeRect (spans, TG.Position (W, M, G))) then
            return Integer (M.Groups (G).First);
         end if;
      end loop;
      return -1;
   end hitTaskButton;

   --  The window list row under (x, y), 0 for none.
   function hitGroupListRow (x, y : Natural) return TG.Window_Count is
   begin
      if not pointInRect (x, y, groupListRect) then
         return 0;
      end if;
      for R in 1 .. groupListMembers loop
         if pointInRect (x, y, groupListRowRect (R)) then
            return R;
         end if;
      end loop;
      return 0;
   end hitGroupListRow;

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

      --  Any process handle with Read lists processes: Desktop's own.
      count := CuBit.Handles.Control.Process_List
        (CuBit.Handles.Startup.Process_Named (CuBit.Startup_Tables.Self_Process_Text),
         psBufAddr, PS_BUF_SIZE);
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

   function launchEntryByProgram (Owner : Process_ID) return Natural is
      count : Unsigned_64;
   begin
      if Owner = No_Process or else not ensureProcessListBuffer then
         return 0;
      end if;
      count := CuBit.Handles.Control.Process_List
        (CuBit.Handles.Startup.Process_Named (CuBit.Startup_Tables.Self_Process_Text),
         psBufAddr, PS_BUF_SIZE);
      if count = 0 or else count = Unsigned_64'Last then
         return 0;
      end if;
      for i in 0 .. count - 1 loop
         declare
            Item : constant CuBit.Process_List.Process_Entry := CuBit.Process_List.Get (psBufAddr, Natural (i));
         begin
            if Item.Identity = Owner then
               declare
                  --  The kernel keeps the first Name_Bytes of the program name.
                  Name : constant String := CuBit.Process_List.Name_Of (Item);
               begin
                  for E in 1 .. launchMenu.Count loop
                     declare
                        Program : constant String := Desktop_Launch.Program_Of (launchMenu.Entries (E));
                        Kept : constant Natural := Natural'Min (Program'Length, CuBit.Process_List.Name_Bytes);
                     begin
                        if Name'Length > 0 and then Kept = Name'Length
                          and then Program (Program'First .. Program'First + Kept - 1) = Name
                        then
                           return E;
                        end if;
                     end;
                  end loop;
               end;
               return 0;
            end if;
         end;
      end loop;
      return 0;
   end launchEntryByProgram;

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

   --  Window buttons sit in fixed slots, right to left: close, maximize,
   --  minimize (Window_Chrome geometry), vertically centred in the title bar.
   type Chrome_Button is (Close_Button, Maximize_Button, Minimize_Button);

   function chromeButtonRect (s : Surface; b : Chrome_Button) return Rect is
      Slot : constant Natural := Chrome_Button'Pos (b);
      Right : constant Natural :=
        Window_Chrome.Border_Width + Window_Chrome.Button_Right_Inset +
        (Slot + 1) * Window_Chrome.Button_Width + Slot * Window_Chrome.Button_Gap;
   begin
      if s.w < Right + Window_Chrome.Title_Left then
         return (others => 0);
      end if;
      return clampRect
        ((x => s.x + s.w - Right,
          y => s.y + Window_Chrome.Bar_Top +
                 (Window_Chrome.Bar_Height - Window_Chrome.Button_Height) / 2,
          w => Window_Chrome.Button_Width,
          h => Window_Chrome.Button_Height));
   end chromeButtonRect;

   function minimizeButtonRect (s : Surface) return Rect is
     (if hasWindowFlag (s, WINDOW_FLAG_MINIMIZABLE)
      then chromeButtonRect (s, Minimize_Button) else (others => 0));

   --  Always drawn (greyed when unavailable) so the buttons never shift;
   --  only a window that may maximize hit-tests it (maximizeAvailable).
   function maximizeButtonRect (s : Surface) return Rect is
     (chromeButtonRect (s, Maximize_Button));

   function maximizeAvailable (s : Surface) return Boolean is
     (hasWindowFlag (s, WINDOW_FLAG_MAXIMIZABLE) and then
      not hasWindowFlag (s, WINDOW_FLAG_FIXED_SIZE));

   function closeButtonRect (s : Surface) return Rect is
     (if hasWindowFlag (s, WINDOW_FLAG_CLOSEABLE)
      then chromeButtonRect (s, Close_Button) else (others => 0));

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

   function shellSurfaceVisible return Boolean;

   --  The topmost shown window at (x, y), -1 for none. The Deskbar is above
   --  every window, so a point on it is never a window's.
   function hitSurface (x, y : Natural) return Integer is
   begin
      if shellSurfaceVisible and then pointInRect (x, y, taskbarRect) then
         return -1;
      end if;
      for i in reverse store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            not store.Surfaces (i).minimized and then
            (store.Surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0 and then
            x >= store.Surfaces (i).x and then y >= store.Surfaces (i).y and then
            x < store.Surfaces (i).x + store.Surfaces (i).w and then
            y < store.Surfaces (i).y + store.Surfaces (i).h
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
      elsif maximizeAvailable (s) and then pointInRect (x, y, maximizeButtonRect (s)) then
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
           (store.Surfaces (SurfaceIndex (idx)), cursorX, cursorY);
      end if;

      case action is
         when DRAG_RESIZE_E => return POINTER_RESIZE_HORIZONTAL;
         when DRAG_RESIZE_S => return POINTER_RESIZE_VERTICAL;
         when DRAG_RESIZE_SE => return POINTER_RESIZE_DIAGONAL;
         when others => null;
      end case;

      if idx >= 0 and then
        pointInRect
          (cursorX, cursorY, clientRect (store.Surfaces (SurfaceIndex (idx))))
      then
         return store.Surfaces (SurfaceIndex (idx)).pointerCursor;
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

   --  Log a launch failure and post it to the notification centre
   --  (defined with the drawing). Process: the child, if one started.
   procedure postLaunchNotice
     (Notice : Desktop_Launch_Notice.Notice; Process : Process_ID);

   procedure collectLaunch (C : CompletionEntry) is
      M : Message renames C.msg;
      Envelope_OK : constant Boolean := C.valid and C.status = COMPLETION_OK and
        M.tag.length = 1 and M.tag.protocol = 0 and M.tag.reserved = 0 and
        M.words (1) = 0 and M.words (2) = 0 and M.words (3) = 0;
      --  The reply names the new process by its full identity (KERN-003).
      Success : constant Boolean := Envelope_OK and M.tag.label = REPLY_OK and
        Is_Process (From_Word (M.words (0)));
      --  The error reply names why (CuBit.Launch_Arguments.Launch_Failure).
      Rejected : constant Boolean := Envelope_OK and M.tag.label = REPLY_ERR and
        CuBit.Launch_Arguments.Is_Failure (M.words (0));
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
         Desktop_Launch_Notice.Watch
           (launchWatch, From_Word (M.words (0)), launchLabel (1 .. launchLabelLength));
         debugPrint ("desktop: launch complete token=" & Decimal (C.token) &
           " pid=" & Decimal (M.words (0)) & LF);
      elsif Rejected then
         debugPrint ("desktop: launch rejected token=" & Decimal (C.token) & LF);
         declare
            Refusal : Desktop_Launch_Notice.Notice;
         begin
            Desktop_Launch_Notice.Show_Refusal
              (Refusal, launchLabel (1 .. launchLabelLength),
               CuBit.Launch_Arguments.Failure_Of (M.words (0)));
            postLaunchNotice (Refusal, No_Process);
         end;
      else
         debugPrint ("desktop: launch reply malformed token=" & Decimal (C.token) & LF);
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
         elsif Desktop_GPU_Wake.Matches (completion.token) then
            Desktop_GPU_Wake.Collect (completion, nowMs);
         elsif outputWatchToken /= 0 and then completion.token = outputWatchToken then
            --  Handled in the main loop (handleOutputChange), never here:
            --  re-layout releases outputs, which this dispatcher serves.
            outputWatchToken := 0;
            declare
               Answer : constant OM.Revision_Decoding :=
                 (if completion.valid and then completion.status = COMPLETION_OK
                  then OM.Decode_Watch (CuBit.Output_Discovery.Display_Broker,
                                        CuBit.Desktop_Messages.To_Wire (completion.msg))
                  else (Valid => False));
            begin
               if Answer.Valid then
                  outputChangeNoticed := True;
               else
                  outputWatchUsable := False;
                  debugPrint ("desktop: output mode watch unavailable" & LF);
               end if;
            end;
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
                    completion.msg.tag.length = 1 and then completion.msg.tag.protocol = 0 and then
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
                     Reply : constant CP.Completion :=
                       (Kernel_Valid => completion.valid,
                        Kernel_OK => completion.status = COMPLETION_OK,
                        Payload_Valid => result.Valid and then result.Value.Buffer = BP.Displayed (P.Pool).Buffer,
                        Kernel_Token => completion.token,
                        Payload_Session => (if result.Valid then result.Value.Result.Session else 0),
                        Payload_Frame => (if result.Valid then result.Value.Result.Frame else 0),
                        Published => result.Valid and then result.Value.Result.Outcome = DSP.Published,
                        Released => result.Valid and then result.Value.Result.Buffer_State = DSP.Released);
                     Expected_Token : constant Unsigned_64 := CP.Token (P.Transfer);
                     Dropped : constant Boolean := CP.Drops (P.Transfer, Reply);
                  begin
                     -- The non-reused kernel completion token selects the
                     -- output. Payload session/frame values only validate it.
                     if Dropped then
                        --  Display returned the buffer without showing the
                        --  frame (the GPU refused it with certainty): reuse
                        --  the buffer and redraw everything, a bounded
                        --  number of times in a row.
                        CP.Complete_Dropped (P.Transfer, Reply);
                        if P.Dropped_In_Row < Natural'Last then P.Dropped_In_Row := P.Dropped_In_Row + 1; end if;
                        if droppedFrames < Unsigned_64'Last then droppedFrames := droppedFrames + 1; end if;
                        if P.Dropped_In_Row = 1 or else droppedFrames mod 64 = 0 then
                           Desktop_Logs.Warn ("desktop: Display dropped frame" & Reply.Payload_Frame'Image &
                             " (not shown, buffer released); full redraw, in a row" &
                             P.Dropped_In_Row'Image & ", total" & droppedFrames'Image);
                        end if;
                        framePending := True;
                        frameDamage := (x => 0, y => 0, w => fbWidth, h => fbHeight);
                     else
                        CP.Complete (P.Transfer, Reply);
                        if CP.Writable (P.Transfer) then P.Dropped_In_Row := 0; end if;
                     end if;
                     BP.Retire_Display (P.Pool, BP.Displayed (P.Pool), CP.Writable (P.Transfer));
                     if not CP.Writable (P.Transfer) or else BP.Faulted (P.Pool) or else
                       P.Dropped_In_Row > Max_Dropped_In_Row
                     then
                        Desktop_Logs.Warn ("desktop: asynchronous transfer quarantined: kernel_valid=" &
                          Reply.Kernel_Valid'Image & " kernel_ok=" & Reply.Kernel_OK'Image &
                          " payload_valid=" & Reply.Payload_Valid'Image &
                          " decoded=" & result.Valid'Image &
                          " token=" & Reply.Kernel_Token'Image & "/" & Expected_Token'Image &
                          " frame=" & Reply.Payload_Frame'Image &
                          " session=" & Reply.Payload_Session'Image & "/" & CP.Session (P.Transfer)'Image &
                          " published=" & Reply.Published'Image & " released=" & Reply.Released'Image &
                          " pool_faulted=" & BP.Faulted (P.Pool)'Image &
                          " dropped_in_row=" & P.Dropped_In_Row'Image);
                        exitCompositor (1);
                     elsif Dropped then
                        null;
                     else
                        --  Boot timeline (docs/boot-timeline.md): the first
                        --  frame Display published. One branch after that.
                        if result.Valid and then result.Value.Result.Outcome = DSP.Published then
                           CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.First_Frame_Presented, Startup.Registration);
                        end if;
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
      Accepted := capSubmit (Desktop_Endpoints.Display_Service, request, CP.Token (P.Transfer));
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
      --  Display copies only a frame's damage: the first scene frame after
      --  the bootstrap screen must cover the whole output (headless check).
      if not Bootstrap_CPU and then not firstSceneReported (Output) then
         firstSceneReported (Output) := True;
         debugPrint ("DESKTOP-FIRST-SCENE: output=" & Output'Image &
           " damage=" & Natural'Image (r.x) & Natural'Image (r.y) &
           Natural'Image (r.w) & Natural'Image (r.h) &
           " size=" & Natural'Image (Natural (P.Geometry.Width)) &
           Natural'Image (Natural (P.Geometry.Height)) & LF);
      end if;
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
            for I in store.Surfaces'Range loop
               if store.Surfaces (I).used and then not store.Surfaces (I).minimized and then
                 store.Surfaces (I).id /= compositionExcludedSurface and then
                 (store.Surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0
               then
                  declare
                     Body_Area : constant Rect := clampRect
                       ((store.Surfaces (I).x, store.Surfaces (I).y, store.Surfaces (I).w, store.Surfaces (I).h));
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

   --  Application icons are drawn 1:1 from the exported edge nearest the
   --  output's density (Icon_Edge_Choice), centred in the physical span of
   --  their logical box, never resampled: crisp at 100%, 125% or 200%.
   Exported_Edges : constant Icon_Edge_Choice.Edge_List :=
     [for I in 1 .. Desktop_Icons.Edges'Length => Desktop_Icons.Edges (Desktop_Icons.Edge_Index (I))];

   --  A logical coordinate's physical pixel edge on Screen, before rotation,
   --  rounded as Compositor_Affine rounds surface edges (half down).
   function unrotatedEdge (Logical : Integer; Origin : DG.Output_Origin; Scale : DG.UI_Scale)
     return Integer
   is
      Numerator : constant Long_Long_Integer :=
        2 * Long_Long_Integer (Scale.Numerator) * (Long_Long_Integer (Logical) - Long_Long_Integer (Origin)) -
        Long_Long_Integer (Scale.Denominator);
      Denominator : constant Long_Long_Integer := 2 * Long_Long_Integer (Scale.Denominator);
   begin
      --  Ceiling division; Ada's "/" truncates toward zero.
      return Integer (if Numerator <= 0 then Numerator / Denominator
                      else (Numerator + Denominator - 1) / Denominator);
   end unrotatedEdge;

   --  One native-storage pixel of the active output, through the selected
   --  renderer when it captures the output (putPixel's rule), else written.
   procedure putNativePixel (px, py : DG.Pixel_Index; color : Unsigned_32) is
      O : Output_Presentation renames presentations (activeOutput);
      Drawn, Must_Restart : Boolean;
   begin
      if Desktop_Compositor.Full_Output and then not Bootstrap_CPU then
         Desktop_Compositor.Draw_Fill
           ((O.Buffer, Unsigned_32 (O.Geometry.Width), Unsigned_32 (O.Geometry.Height), Unsigned_32 (O.Pitch), 1),
            Unsigned_64 (O.Pitch) * Unsigned_64 (O.Geometry.Height), (px, py, px + 1, py + 1), color,
            activeOutput = 1, Drawn, Must_Restart);
         if Must_Restart then exitCompositor (1); end if;
         if Drawn then return; end if;
      end if;
      if O.Buffer = System.Null_Address then return; end if;
      declare
         pixel : Unsigned_32 with
           Import, Address => O.Buffer + Storage_Offset (Natural (py) * O.Pitch + Natural (px) * 4);
      begin
         pixel := color;
      end;
   end putNativePixel;

   --  Icon id, designed design logical pixels square at (x, y), over bg.
   procedure drawIcon
      (id : Desktop_Icons.Icon_ID;
       x, y : Natural;
       bg   : Unsigned_32;
       design : Icon_Edge_Choice.Edge := Desktop_Icons.ICON_SIZE)
   is
      pixel : Unsigned_32;
   begin
      if not nativeOutputPass then
         --  The legacy direct framebuffer is 1x: the design edge's export
         --  if there is one, else the nearest.
         declare
            Edge : constant Desktop_Icons.Edge_Index :=
              Desktop_Icons.Edge_Index (Icon_Edge_Choice.Nearest (Exported_Edges, design, 1, 1));
            Item : constant Desktop_Icon_Pixels.Asset := (Desktop_Icon_Pixels.Application, id, Edge);
            Size : constant Positive := Desktop_Icons.Edges (Edge);
            Left : constant Integer := Icon_Edge_Choice.Centered (x, x + design, Size);
            Top : constant Integer := Icon_Edge_Choice.Centered (y, y + design, Size);
         begin
            for yy in 0 .. Size - 1 loop
               for xx in 0 .. Size - 1 loop
                  pixel := Desktop_Icon_Pixels.Pixel (Item, xx, yy);
                  if Shift_Right (pixel, 24) /= 0 and then Left + xx >= 0 and then Top + yy >= 0 then
                     putPixel (Left + xx, Top + yy, iconPixelOver (pixel, bg));
                  end if;
               end loop;
            end loop;
         end;
         return;
      end if;
      declare
         O : Output_Presentation renames presentations (activeOutput);
         Screen : constant DG.Output := O.Geometry;
         Rotated : constant Boolean := Screen.Rotation in DG.Clockwise_90 | DG.Clockwise_270;
         --  The output's extent before rotation.
         Wide : constant Natural := Natural (if Rotated then Screen.Height else Screen.Width);
         High : constant Natural := Natural (if Rotated then Screen.Width else Screen.Height);
         Edge : constant Desktop_Icons.Edge_Index := Desktop_Icons.Edge_Index
           (Icon_Edge_Choice.Nearest (Exported_Edges, design,
              Icon_Edge_Choice.Scale_Part (Screen.Scale.Numerator),
              Icon_Edge_Choice.Scale_Part (Screen.Scale.Denominator)));
         Item : constant Desktop_Icon_Pixels.Asset := (Desktop_Icon_Pixels.Application, id, Edge);
         Size : constant Positive := Desktop_Icons.Edges (Edge);
         Box_Left : constant Integer := unrotatedEdge (x, Screen.X, Screen.Scale);
         Box_Top : constant Integer := unrotatedEdge (y, Screen.Y, Screen.Scale);
         Box_Right : constant Integer := unrotatedEdge (x + design, Screen.X, Screen.Scale);
         Box_Bottom : constant Integer := unrotatedEdge (y + design, Screen.Y, Screen.Scale);
         Left : constant Integer := Icon_Edge_Choice.Centered (Box_Left, Box_Right, Size);
         Top : constant Integer := Icon_Edge_Choice.Centered (Box_Top, Box_Bottom, Size);
         --  The square's part on the output (it may hang over an edge).
         First_X : constant Natural := Natural (Integer'Max (0, -Left));
         First_Y : constant Natural := Natural (Integer'Max (0, -Top));
         Last_X : constant Integer := Integer'Min (Size, Wide - Left) - 1;
         Last_Y : constant Integer := Integer'Min (Size, High - Top) - 1;
         --  Logical clip and damage, as native-storage pixels.
         Clip : constant DG.Physical_Rectangle :=
           (if clipEnabled then physicalClip (clipRect) else outputDamage);
         Drawn, Must_Restart : Boolean;
         px, py : Natural;
      begin
         if Last_X < Integer (First_X) or else Last_Y < Integer (First_Y) then return; end if;
         Desktop_Compositor.Draw_Icon
           ((O.Buffer, Unsigned_32 (Screen.Width), Unsigned_32 (Screen.Height), Unsigned_32 (O.Pitch), 1),
            Unsigned_64 (O.Pitch) * Unsigned_64 (Screen.Height), Screen, Item,
            (DG.Pixel_Edge (Left + Integer (First_X)), DG.Pixel_Edge (Top + Integer (First_Y)),
             DG.Pixel_Edge (Left + Last_X + 1), DG.Pixel_Edge (Top + Last_Y + 1)),
            First_X, First_Y, Clip, activeOutput = 1, Drawn, Must_Restart);
         if Must_Restart then exitCompositor (1); end if;
         if Drawn then return; end if;
         --  Software renderers: the same square, pixel for pixel, rotated
         --  into native storage as Compositor_Affine rotates surfaces.
         for yy in First_Y .. Last_Y loop
            for xx in First_X .. Last_X loop
               pixel := Desktop_Icon_Pixels.Pixel (Item, xx, yy);
               if Shift_Right (pixel, 24) /= 0 then
                  case Screen.Rotation is
                     when DG.Unrotated => px := Left + xx; py := Top + yy;
                     when DG.Clockwise_90 => px := Natural (Screen.Width) - 1 - (Top + yy); py := Left + xx;
                     when DG.Clockwise_180 =>
                        px := Natural (Screen.Width) - 1 - (Left + xx);
                        py := Natural (Screen.Height) - 1 - (Top + yy);
                     when DG.Clockwise_270 => px := Top + yy; py := Natural (Screen.Height) - 1 - (Left + xx);
                  end case;
                  if px >= Natural (Clip.Left) and then px < Natural (Clip.Right) and then
                    py >= Natural (Clip.Top) and then py < Natural (Clip.Bottom)
                  then
                     putNativePixel (DG.Pixel_Index (px), DG.Pixel_Index (py), iconPixelOver (pixel, bg));
                  end if;
               end if;
            end loop;
         end loop;
      end;
   end drawIcon;

   --  The toolkit's one measure (CuBit.Fonts.Width): kerned, unrounded
   --  advances, the pen positions drawUIText places glyphs at.
   function uiTextWidth (s : String; face : CuBit.Fonts.Face := CuBit.Fonts.Sans) return Natural is
     (CuBit.Fonts.Width (face, s));

   --  CPU text (bootstrap and fallback): blend ch's ink with its pen at Pen
   --  (quarter-pixel phase), coverage through the ink's Glyph_Gamma row like
   --  every other glyph path. The run's background is already drawn.
   procedure drawUIGlyph
      (Pen  : Glyph_Pen.Position;
       y    : Natural;
       ch   : Character;
       fg   : Unsigned_32;
       face : CuBit.Fonts.Face)
   is
      glyph : constant CuBit.Fonts.Glyph_Access :=
        CuBit.Fonts.Get (face, ch, CuBit.Fonts.Normal, Glyph_Pen.Phase_Of (Pen));
      Ink_Row : constant Glyph_Gamma.Bucket := Glyph_Gamma.Bucket_Of (fg);
      --  The ink box in logical coordinates; it may start left of 0.
      Left : constant Integer := Glyph_Pen.Pixel_Of (Pen) - CuBit.Fonts.Origin + Natural (glyph.Ink_Left);
      Top : constant Natural := y + Natural (glyph.Ink_Top);
      Width : constant Natural := Natural (glyph.Ink_Width);
      Height : constant Natural := Natural (glyph.Ink_Height);
      Column0 : constant Natural := Natural (glyph.Ink_Left);
      Row0 : constant Natural := Natural (glyph.Ink_Top);
   begin
      if Width = 0 or else Height = 0 or else Column0 + Width > CuBit.Fonts.Max_Width or else
        Row0 + Height > CuBit.Fonts.Coverage'Length (1)
      then return; end if;

      if nativeOutputPass then
         declare
            O : Output_Presentation renames presentations (activeOutput);
            Visible_Left : constant Natural := Natural (Integer'Max (0, Left));
            Damage : constant DG.Physical_Rectangle :=
              (if Left + Width <= 0 then DG.Empty
               else physicalClip ((Visible_Left, Top, Left + Width - Visible_Left, Height)));
            Cell : constant DG.Logical_Rectangle :=
              (DG.Logical_Coordinate (Left), DG.Logical_Coordinate (Top),
               DG.Logical_Coordinate (Left + Width), DG.Logical_Coordinate (Top + Height));
         begin
            if Damage.Left >= Damage.Right or else Damage.Top >= Damage.Bottom then return; end if;
            for Y in Natural (Damage.Top) .. Natural (Damage.Bottom) - 1 loop
               for X in Natural (Damage.Left) .. Natural (Damage.Right) - 1 loop
                  declare
                     M : constant Compositor_Sampling.Sample := Compositor_Sampling.Map
                       (O.Geometry, (DG.Pixel_Index (X), DG.Pixel_Index (Y)), Cell,
                        DG.Physical_Extent (Width), DG.Physical_Extent (Height));
                  begin
                     if M.Valid then
                        declare
                           Target : Unsigned_32 with Import, Address => O.Buffer + Storage_Offset (Y * O.Pitch + X * 4);
                           Coverage : constant Unsigned_8 :=
                             glyph.Alpha (Row0 + Natural (M.Y), Column0 + Natural (M.X));
                        begin
                           if Coverage /= 0 then
                              Target := Client_Glyph_Blend.Ink (fg, Target, Ink_Row, Coverage);
                           end if;
                        end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         return;
      end if;
      for yy in 0 .. Height - 1 loop
         for xx in 0 .. Width - 1 loop
            declare
               Coverage : constant Unsigned_8 := glyph.Alpha (Row0 + yy, Column0 + xx);
            begin
               if Coverage /= 0 and then Left + xx >= 0 then
                  putPixel (Left + xx, Top + yy, Client_Glyph_Blend.Ink
                    (fg, readBackPixel (Left + xx, Top + yy), Ink_Row, Coverage));
               end if;
            end;
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
         procedure Place (Index : Positive; C : Character; Pen : Glyph_Pen.Position) is
            pragma Unreferenced (Index);
            Glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Sans, C);
            Left : constant Integer := Glyph_Pen.Nearest (Pen) - CuBit.Fonts.Origin;
         begin
            for YY in 0 .. CuBit.Fonts.Line_Height - 1 loop
               for XX in 0 .. CuBit.Fonts.Max_Width - 1 loop
                  if Glyph.Alpha (YY, XX) >= 128 and then Left + XX >= 0 then
                     Pixel (Left + XX, Y + YY, 16#00FFFFFF#);
                  end if;
               end loop;
            end loop;
         end Place;
         procedure Each is new CuBit.Fonts.Lay_Out (Place);
      begin
         Each (CuBit.Fonts.Sans, Text, 8);
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
       transparent : Boolean := False;
       face : CuBit.Fonts.Face := CuBit.Fonts.Sans)
   is
      package T renames Compositor_Text;
      pragma Compile_Time_Error
        (CuBit.Fonts.Face'Pos (CuBit.Fonts.Face'Last) /= T.L.Face_Index'Last,
         "the compositor's face numbers must be CuBit.Fonts.Face'Pos");
      Batch : T.Glyphs;
      Pens : array (T.Index) of Glyph_Pen.Position := [others => 0];
      Chars : String (T.Index) := [others => ' '];
      Count : T.Count := 0;
      Stopped : Boolean := False;
      Target : System.Address := backBufferAddr;
      Target_W : Natural := fbWidth;
      Target_H : Natural := fbHeight;
      Target_Pitch : Natural := fbPitch;
      Screen : DG.Output := (DG.Physical_Extent (fbWidth),
        DG.Physical_Extent (fbHeight), DG.Unrotated, (1, 1), 0, 0);
      Damage : DG.Physical_Rectangle;
      Area : Rect;

      --  One compositor batch: each glyph's cell is the whole raster (its
      --  pen origin CuBit.Fonts.Origin in), never clipped to the advance, at
      --  the pen's whole pixel; the pen's fraction picks the mask's phase.
      procedure Flush is
         Drawn, Repaint, Must_Restart : Boolean;
      begin
         if Count = 0 or else Stopped then Count := 0; return; end if;
         Desktop_Compositor.Draw_Text
           ((Target, Unsigned_32 (Target_W), Unsigned_32 (Target_H), Unsigned_32 (Target_Pitch), 1),
            Unsigned_64 (Target_Pitch) * Unsigned_64 (Target_H), Screen,
            Batch, Count, Damage, fg or 16#FF00_0000#, CuBit.Fonts.Face'Pos (face),
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
            Stopped := True;
         elsif not Drawn then
            if not mesaTextFallbackAnnounced then
               debugPrint ("desktop: CPU text fallback active" & LF); mesaTextFallbackAnnounced := True;
            end if;
            for I in 1 .. Count loop
               drawUIGlyph (Pens (I), y, Chars (I), fg, face);
            end loop;
         elsif Desktop_Compositor.Software_Text then
            if not retainedTextAnnounced then
               debugPrint ("desktop: retained software text active" & LF); retainedTextAnnounced := True;
            end if;
         elsif not mesaTextAnnounced then
            debugPrint ("desktop: Mesa retained-mask text active" & LF); mesaTextAnnounced := True;
         end if;
         Count := 0;
      end Flush;

      pragma Compile_Time_Error (Glyph_Pen.Unit /= T.L.Pen_Unit or else Glyph_Pen.Phases /= T.L.Phases,
                                 "toolkit pens and compositor masks must share pen units and phases");
      procedure Add (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
         pragma Unreferenced (Index);
         Whole : constant Natural := Pen / Glyph_Pen.Unit;
         Left : constant Integer := Whole - CuBit.Fonts.Origin;
      begin
         if Stopped or else Whole >= fbWidth then return; end if;
         Count := Count + 1;
         Pens (Count) := Pen; Chars (Count) := Ch;
         Batch (Count) :=
           (Code => (if Character'Pos (Ch) in 32 .. 126 then Character'Pos (Ch) else 63),
            Cell => (T.G.Logical_Coordinate (Left), T.G.Logical_Coordinate (y),
                     T.G.Logical_Coordinate (Left + CuBit.Fonts.Max_Width),
                     T.G.Logical_Coordinate (y + CuBit.Fonts.Line_Height)),
            Offset => Pen mod Glyph_Pen.Unit);
         if Count = T.Count'Last then Flush; end if;
      end Add;

      procedure CPU_Glyph (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
         pragma Unreferenced (Index);
      begin
         if Glyph_Pen.Pixel_Of (Pen) < fbWidth then drawUIGlyph (Pen, y, Ch, fg, face); end if;
      end CPU_Glyph;
      procedure Each_CPU is new CuBit.Fonts.Lay_Out (CPU_Glyph);
      procedure Each_Batched is new CuBit.Fonts.Lay_Out (Add);
   begin
      if x >= fbWidth or else y >= fbHeight or else s'Length = 0 then return; end if;
      --  One background for the run; glyphs overlap their neighbours' cells
      --  (kerning, ink past the advance), so none paints its own.
      if not transparent then
         fillRect (x, y, uiTextWidth (s, face), CuBit.Fonts.Line_Height, bg);
      end if;
      -- Bootstrap must not create a Mesa owner before renderer selection.
      if Bootstrap_CPU or else not Desktop_Compositor.Selected or else textSceneRetry or else
        (not nativeOutputPass and then backBufferAddr = System.Null_Address) or else fbBpp /= 32
      then
         Each_CPU (face, s, x);
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
      Each_Batched (face, s, x);
      Flush;
   end drawUIText;

   --  What a window is called (title bar and task button).
   function surfaceTitle (s : Surface) return String is
     (case s.appKind is
         when APP_DOOM     => "DOOM",
         when APP_SETTINGS => "Settings",
         when others       =>
           (if s.title.Length > 0 then s.title.Text else "Application"));

   procedure drawSurfaceTitle
      (s       : Surface;
       x, y    : Natural;
       fg, bg  : Unsigned_32;
       transparent : Boolean := False;
       face    : CuBit.Fonts.Face := CuBit.Fonts.Sans)
   is
   begin
      drawUIText (x, y, surfaceTitle (s), fg, bg, transparent, face);
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
      minBtn : constant Rect := minimizeButtonRect (s);
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

      if not isEmpty (minBtn) then
         rightLimit := minBtn.x - 6;
      elsif not isEmpty (maxBtn) then
         rightLimit := maxBtn.x - 6;
      elsif not isEmpty (closeBtn) then
         rightLimit := closeBtn.x - 6;
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
      Desktop_Pointer_Plane.Retry (requestSequence);
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
      if isEmpty (r) or else launchMenuOpen or else deskbarPanelOpen or else
         groupListOpen or else clipboardMenuOpen or else toastShown or else TTP.Showing (tipState) or else
         not backBufferReady or else backBufferAddr = System.Null_Address
      then
         return False;
      end if;

      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            not store.Surfaces (i).minimized and then
            store.Surfaces (i).bufferAttached and then
            store.Surfaces (i).bufferAddr /= System.Null_Address and then
            store.Surfaces (i).bufferFormat = PIXEL_FORMAT_BGRA8888 and then
            store.Surfaces (i).bufferW > 0 and then store.Surfaces (i).bufferH > 0 and then
            store.Surfaces (i).bufferW <= store.Surfaces (i).bufferPitch / 4 and then
            (store.Surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            c := clientRect (store.Surfaces (i));
            -- A partial/smaller source cannot replace all client pixels;
            -- route it through drawWindow's complete background fill instead.
            if rectContains (c, r) and then
              (if store.Surfaces (i).publicationMode then store.Surfaces (i).bufferLogicalW else store.Surfaces (i).bufferW) >= c.w and then
              (if store.Surfaces (i).publicationMode then store.Surfaces (i).bufferLogicalH else store.Surfaces (i).bufferH) >= c.h
            then
               occluded := False;

               if i < surfaceLast then
                  for j in i + 1 .. surfaceLast loop
                     if store.Surfaces (j).used and then
                        not store.Surfaces (j).minimized and then
                        (store.Surfaces (j).flags and SURFACE_FLAG_WINDOW) /= 0
                        and then rectIntersects (r, surfaceRect (store.Surfaces (j)))
                     then
                        occluded := True;
                        exit;
                     end if;
                  end loop;
               end if;

               --  The Deskbar is above every window.
               if shellSurfaceVisible and then rectIntersects (r, taskbarRect) then
                  occluded := True;
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
                     drawClientBuffer (store.Surfaces (i), c.x, c.y, c.w, c.h);
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

   --  One window button: a square, bordered tile with a 10-pixel stroke
   --  glyph, all solid fills (Window_Chrome geometry and palette).
   type Button_Glyph is (Minimize_Glyph, Maximize_Glyph, Restore_Glyph, Close_Glyph);

   procedure drawChromeButton
      (r     : Rect;
       look  : Window_Chrome.Button_Colors;
       glyph : Button_Glyph)
   is
      G  : constant Natural := Window_Chrome.Glyph_Size;
      --  A window too narrow for its buttons has empty button rects
      --  (minimizeButtonRect and the others): nothing to draw, and no
      --  glyph offset to compute.
      gx : constant Natural := r.x + (if r.w > G then (r.w - G) / 2 else 0);
      gy : constant Natural := r.y + (if r.h > G then (r.h - G) / 2 else 0);
   begin
      if isEmpty (r) or else r.w < G or else r.h < G then
         return;
      end if;
      fillRect (r.x, r.y, r.w, r.h, look.Fill);
      strokeRect (r.x, r.y, r.w, r.h, look.Edge, look.Edge);
      case glyph is
         when Minimize_Glyph =>
            fillRect (gx + 1, gy + 7, G - 2, 1, look.Glyph);
         when Maximize_Glyph =>
            strokeRect (gx + 1, gy + 1, G - 2, G - 2, look.Glyph, look.Glyph);
         when Restore_Glyph =>
            --  A back square behind a filled front square.
            strokeRect (gx + 3, gy + 1, G - 4, G - 4, look.Glyph, look.Glyph);
            fillRect (gx + 1, gy + 3, G - 4, G - 4, look.Fill);
            strokeRect (gx + 1, gy + 3, G - 4, G - 4, look.Glyph, look.Glyph);
         when Close_Glyph =>
            for I in 0 .. G - 4 loop
               fillRect (gx + 2 + I, gy + 2 + I, 1, 1, look.Glyph);
               fillRect (gx + G - 2 - I, gy + 2 + I, 1, 1, look.Glyph);
            end loop;
      end case;
   end drawChromeButton;

   procedure drawWindow (s : Surface) is
      minW   : constant Natural := 80;
      minH   : constant Natural := 60;
      active  : constant Boolean := s.id = focusSurface;
      frame   : Surface := s;
      chrome  : Window_Chrome.Palette renames chromePalette;
      bar     : constant Unsigned_32 :=
        (if active then CuBit.UI.Current_Theme.titleBar else chrome.Inactive_Bar);
      stripe  : constant Unsigned_32 :=
        (if active then s.stripe.Stripe else s.stripe.Muted_Stripe);
      titleText : constant Unsigned_32 :=
        (if active then chrome.Active_Title else chrome.Inactive_Title);
      idle    : constant Window_Chrome.Button_Colors :=
        (chrome.Button.Fill, chrome.Button.Edge,
         (if active then chrome.Button.Glyph else chrome.Inactive_Glyph));
      x      : constant Natural := s.x;
      y      : constant Natural := s.y;
      w      : Natural := s.w;
      h      : Natural := s.h;
      inner  : Natural;

      function look (b : Pointer_Action) return Window_Chrome.Button_Colors is
        (if s.id /= chromeHoverId or else b /= chromeHoverButton then idle
         elsif b = HIT_CLOSE then chrome.Close_Hover
         else chrome.Button_Hover);
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

      frame.w := w;
      frame.h := h;
      inner := w - 2 * Window_Chrome.Border_Width;

      drawDappledShadow (x, y, w, h);
      strokeRect (x, y, w, h,
                  (if active then chrome.Active_Border else chrome.Inactive_Border),
                  (if active then chrome.Active_Border else chrome.Inactive_Border));
      fillRect (x + Window_Chrome.Border_Width, y + Window_Chrome.Border_Width,
                inner, Window_Chrome.Stripe_Height, stripe);
      fillRect (x + Window_Chrome.Border_Width, y + Window_Chrome.Bar_Top,
                inner, Window_Chrome.Bar_Height, bar);
      fillRect (x + Window_Chrome.Border_Width,
                y + Window_Chrome.Bar_Top + Window_Chrome.Bar_Height,
                inner, Window_Chrome.Rule_Height, chrome.Rule);
      --  The title tile: the app's icon (its launcher entry's).
      drawIcon (s.appIcon, x + Window_Chrome.Tile_Left,
                y + Window_Chrome.Bar_Top + (Window_Chrome.Bar_Height - Window_Chrome.Tile_Size) / 2,
                bar, Window_Chrome.Tile_Size);
      declare
         textY : constant Natural :=
           y + Window_Chrome.Bar_Top + (Window_Chrome.Bar_Height - CuBit.Fonts.Line_Height) / 2;
      begin
         --  The active window's title in the emphasis face.
         drawSurfaceTitle (s, x + Window_Chrome.Title_Left, textY, titleText, bar, True,
                           (if active then CuBit.Fonts.Sans_Semibold else CuBit.Fonts.Sans));
      end;
      drawStreamBadges (s, y + Window_Chrome.Bar_Top + 2);

      --  Window controls are compositor-owned because they mutate focus,
      --  visibility, and eventually client lifecycle authority.
      drawChromeButton (minimizeButtonRect (frame), look (HIT_MINIMIZE), Minimize_Glyph);
      drawChromeButton
        (maximizeButtonRect (frame),
         (if maximizeAvailable (frame) then look (HIT_MAXIMIZE) else chrome.Unavailable),
         (if s.maximized then Restore_Glyph else Maximize_Glyph));
      drawChromeButton (closeButtonRect (frame), look (HIT_CLOSE), Close_Glyph);

      --  Before the first client buffer, the client area keeps the neutral
      --  window fill; afterwards the buffer covers it (see below).
      if s.appKind /= APP_SETTINGS and then not s.bufferAttached then
         declare
            c : constant Rect := clientRect (frame);
         begin
            fillRect (c.x, c.y, c.w, c.h, C_WIN);
         end;
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
            end if;
      end case;
   end drawWindow;

   --  Where a taskbar button's label goes: its line box centred in the
   --  button, the same for the Apps button and every window button, so the
   --  baselines line up.
   function taskbarTextY (r : Rect) return Natural is
     (r.y + (if r.h > CuBit.Fonts.Line_Height then (r.h - CuBit.Fonts.Line_Height) / 2 else 0));

   --  Square, flat and bordered (the window chrome's look): one fill, one edge.
   procedure drawBox (r : Rect; fill, edge : Unsigned_32) is
   begin
      if isEmpty (r) then
         return;
      end if;
      fillRect (r.x, r.y, r.w, r.h, fill);
      strokeRect (r.x, r.y, r.w, r.h, edge, edge);
   end drawBox;

   --  The emphasis face, IBM Plex Sans SemiBold.
   procedure drawBoldText (x, y : Natural; s : String; fg, bg : Unsigned_32) is
   begin
      drawUIText (x, y, s, fg, bg, face => CuBit.Fonts.Sans_Semibold);
   end drawBoldText;

   --  s cut to fit width pixels, ending in "..." when cut.
   --  The toolkit's proved fit (Text_Fit), one instance per face: s, its
   --  longest prefix that fits followed by "...", or "" when not even the
   --  ellipsis fits.
   function uiSansWidth (s : String) return Natural is (uiTextWidth (s, CuBit.Fonts.Sans));
   function uiSansBoundaryAt (s : String; x : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, s, x));
   function uiSemiboldWidth (s : String) return Natural is (uiTextWidth (s, CuBit.Fonts.Sans_Semibold));
   function uiSemiboldBoundaryAt (s : String; x : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans_Semibold, s, x));
   package Sans_Fit is new Text_Fit (uiSansWidth, uiSansBoundaryAt);
   package Semibold_Fit is new Text_Fit (uiSemiboldWidth, uiSemiboldBoundaryAt);
   function fitText (s : String; width : Natural;
                     face : CuBit.Fonts.Face := CuBit.Fonts.Sans) return String is
     (if s'Last > Sans_Fit.Last_Index then s
      elsif CuBit.Fonts."=" (face, CuBit.Fonts.Sans_Semibold) then Semibold_Fit.Fitted (s, width)
      else Sans_Fit.Fitted (s, width));

   --  A count without Natural'Image's leading space.
   function countText (n : Natural) return String is
      image : constant String := Natural'Image (n);
   begin
      return image (image'First + 1 .. image'Last);
   end countText;

   --  The keyboard focus ring, inside a button or row.
   procedure drawFocusRing (r : Rect) is
   begin
      if r.w > 2 * DBS.Focus_Inset and then r.h > 2 * DBS.Focus_Inset then
         strokeRect (r.x + DBS.Focus_Inset, r.y + DBS.Focus_Inset,
                     r.w - 2 * DBS.Focus_Inset, r.h - 2 * DBS.Focus_Inset,
                     deskbarColors.Focus, deskbarColors.Focus);
      end if;
   end drawFocusRing;

   --  An icon centred vertically in r, Left in from its left edge.
   procedure drawIconIn (id : Desktop_Icons.Icon_ID; r : Rect; left : Natural; bg : Unsigned_32) is
   begin
      drawIcon (id, r.x + left,
                r.y + (if r.h > Desktop_Icons.ICON_SIZE then (r.h - Desktop_Icons.ICON_SIZE) / 2 else 0),
                bg);
   end drawIconIn;

   --  One task button per app: its app-colour stripe (muted when every
   --  window is minimized), icon and label; the active window's button is
   --  white and semibold; a group shows its window count.
   procedure drawTaskButtons is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      spans : constant DBL.Span_List := spansOf (W, M);
      C : constant DBS.Palette := deskbarColors;
   begin
      for G in 1 .. M.Count loop
         declare
            Group : constant TG.Group := M.Groups (G);
            Place : constant TG.Group_Index := TG.Position (W, M, G);
            r     : constant Rect := placeRect (spans, Place);
            S     : Surface renames store.Surfaces (SurfaceIndex (Group.First));
            fill  : constant Unsigned_32 := (if Group.Active then C.Active else C.Button);
            edge  : constant Unsigned_32 := (if Group.Active then C.Active_Edge else C.Button_Edge);
            ink   : constant Unsigned_32 :=
              (if Group.Active then C.Active_Text
               elsif Group.Minimized then C.Dimmed_Text else C.Button_Text);
            textX : constant Natural := r.x + DBS.Icon_Left + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap;
            --  An icon-only button (crowded bar) has no room for the count.
            counted : constant Boolean := Group.Members > 1 and then r.w >= DBS.Task_Minimum;
            countW : constant Natural :=
              (if counted then DBS.Count_Width + DBS.Count_Gap else 0);
            room  : constant Natural :=
              (if r.x + r.w > textX + DBS.Label_Right + countW
               then r.x + r.w - textX - DBS.Label_Right - countW else 0);
            label : constant String := fitText
              ((if Group.Members > 1 and then S.appLabelLength > 0
                then S.appLabel (1 .. S.appLabelLength) else surfaceTitle (S)), room,
               (if Group.Active then CuBit.Fonts.Sans_Semibold else CuBit.Fonts.Sans));
         begin
            if not isEmpty (r) then
               drawBox (r, fill, edge);
               fillRect (r.x, r.y, r.w, DBS.Stripe_Height,
                         (if Group.Minimized then S.stripe.Muted_Stripe else S.stripe.Stripe));
               drawIconIn (S.appIcon, r, DBS.Icon_Left, fill);
               if Group.Active then
                  drawBoldText (textX, taskbarTextY (r), label, ink, fill);
               else
                  drawUIText (textX, taskbarTextY (r), label, ink, fill);
               end if;
               if counted then
                  declare
                     box : constant Rect :=
                       (x => r.x + r.w - DBS.Label_Right - DBS.Count_Width,
                        y => r.y + (r.h - DBS.Count_Height) / 2, w => DBS.Count_Width,
                        h => DBS.Count_Height);
                     text : constant String := countText (Natural (Group.Members));
                  begin
                     drawBox (box, fill, edge);
                     drawUIText (box.x + (if box.w > uiTextWidth (text) then (box.w - uiTextWidth (text)) / 2 else 0),
                                 taskbarTextY (r), text, ink, fill);
                  end;
               end if;
               if deskbarFocus = Task_Focus and then deskbarFocusTask = Place then
                  drawFocusRing (r);
               end if;
            end if;
         end;
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
          arrow    : Boolean;
          open     : Boolean := False)
      is
         --  A category whose submenu is open while the keyboard is in that
         --  submenu: the Deskbar's active look (as the Apps button).
         D : constant DBS.Palette := deskbarColors;
         bg : constant Unsigned_32 :=
           (if selected then C_ACCENT elsif open then D.Active else C_PANEL);
         textColor : constant Unsigned_32 :=
           (if selected then C_WHITE elsif open then D.Active_Text else fg);
      begin
         if isEmpty (item) then
            return;
         end if;
         if selected or else open then
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
            C_TEXT, True, open => launchMenuState.Open = row);
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

   --  Apps: the CuBit logo and "Apps", then the separator before the tasks.
   procedure drawAppsButton is
      r : constant Rect := launchButtonRect;
      C : constant DBS.Palette := deskbarColors;
      fill : constant Unsigned_32 := (if launchMenuOpen then C.Active else C.Button);
   begin
      if isEmpty (r) then
         return;
      end if;
      drawBox (r, fill, (if launchMenuOpen then C.Active_Edge else C.Button_Edge));
      drawIconIn (Desktop_Icons.Start, r, DBS.Icon_Left, fill);
      drawBoldText (r.x + DBS.Icon_Left + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap,
                    taskbarTextY (r), "Apps", C.Active_Text, fill);
      if deskbarFocus = Apps_Focus then
         drawFocusRing (r);
      end if;
      fillRect (r.x + r.w + DBS.Separator_Margin,
                taskbarY + (TASKBAR_H - DBS.Separator_Height) / 2,
                1, DBS.Separator_Height, C.Separator);
   end drawAppsButton;

   --  The status button: unread notifications, the shown status items and
   --  the clock. It opens the Deskbar panel.
   procedure drawStatusButton is
      r : constant Rect := statusRect;
      C : constant DBS.Palette := deskbarColors;
      shown : constant DSI.Presence := statusShown;
      unread : constant Natural := Natural (DN.Unread (notifications));
      x : Natural := r.x + DBS.Status_Padding;
      Badge_Limit : constant := 9;
   begin
      if isEmpty (r) then
         return;
      end if;
      drawBox (r, C.Status, (if deskbarPanelOpen then C.Status_Open else C.Status_Edge));
      if unread > 0 then
         declare
            badge : constant Rect :=
              (x => x, y => r.y + (r.h - DBS.Badge_Size) / 2, w => DBS.Badge_Size, h => DBS.Badge_Size);
            text : constant String := (if unread > Badge_Limit then "9+" else countText (unread));
         begin
            fillRect (badge.x, badge.y, badge.w, badge.h, C.Badge);
            drawUIText (badge.x + (if badge.w > uiTextWidth (text) then (badge.w - uiTextWidth (text)) / 2 else 0),
                        taskbarTextY (r), text, C.Badge_Text, C.Badge);
            x := x + DBS.Badge_Size + DBS.Status_Item_Gap;
         end;
      end if;
      for I in DSI.Item loop
         if shown (I) then
            drawIconIn (DSI.Icon (I, masterAudio.Muted), r, x - r.x, C.Status);
            x := x + Desktop_Icons.ICON_SIZE + DBS.Status_Item_Gap;
         end if;
      end loop;
      drawUIText (x, taskbarTextY (r), clockText, C.Active_Text, C.Status);
      if deskbarFocus = Status_Focus then
         drawFocusRing (r);
      end if;
   end drawStatusButton;

   --  "16:42" for a minute of the day.
   function minuteText (m : DN.Minute_Of_Day) return String is
      hours : constant Natural := Natural (m) / 60;
      minutes : constant Natural := Natural (m) mod 60;
      function digit (v : Natural) return Character is (Character'Val (Character'Pos ('0') + v));
   begin
      return [digit (hours / 10), digit (hours mod 10), ':', digit (minutes / 10), digit (minutes mod 10)];
   end minuteText;

   function noteAccent (N : DN.Notification) return Unsigned_32 is
     (if N.Accent.Declared then Unsigned_32 (N.Accent.Value) else CuBit.UI.Current_Theme.accent);

   --  A section heading inside the panel.
   procedure drawPanelSection (r : Rect; label : String) is
      C : constant DBS.Palette := deskbarColors;
   begin
      fillRect (r.x + 1, r.y, (if r.w > 2 then r.w - 2 else 0), r.h, C.Section);
      fillRect (r.x + 1, r.y, (if r.w > 2 then r.w - 2 else 0), 1, C.Row_Rule);
      drawBoldText (r.x + DBS.Panel_Padding, r.y + (r.h - CuBit.Fonts.Line_Height) / 2,
                    label, C.Section_Text, C.Section);
   end drawPanelSection;

   procedure drawDeskbarPanel is
      r : constant Rect := panelRect;
      C : constant DBS.Palette := deskbarColors;
      lineY : constant Natural := (DBS.Panel_Row - CuBit.Fonts.Line_Height) / 2;
      inner : constant Natural := (if r.w > 2 then r.w - 2 else 0);
   begin
      if not deskbarPanelOpen or else isEmpty (r) then
         return;
      end if;
      drawDappledShadow (r.x, r.y, r.w, r.h);
      drawBox (r, C.Panel, C.Panel_Edge);
      fillRect (r.x, r.y, r.w, DBS.Panel_Stripe, C.Panel_Stripe);

      --  The clock and the date.
      declare
         y : constant Natural := r.y + DBS.Panel_Stripe + (DBS.Panel_Header - CuBit.Fonts.Line_Height) / 2;
      begin
         drawBoldText (r.x + DBS.Panel_Padding, y, clockText, C.Active_Text, C.Panel);
         if clockDateKnown then
            declare
               date : constant String :=
                 DBL.Weekday_Name (DBL.Day_Of_Week (clockDate.Year, clockDate.Month, clockDate.Day)) &
                 Natural'Image (clockDate.Day) & " " & DBL.Month_Name (clockDate.Month);
            begin
               drawUIText (r.x + r.w - DBS.Panel_Padding - uiTextWidth (date), y, date, C.Muted, C.Panel);
            end;
         end if;
      end;

      --  Quick settings: what has a granted service behind it.
      drawPanelSection ((r.x, r.y + DBS.Panel_Stripe + DBS.Panel_Header, r.w, DBS.Panel_Section),
                        "Quick settings");
      declare
         row : constant Rect := panelVolumeRowRect;
         track : constant Rect := volumeTrackRect;
         barY : constant Natural := track.y + (track.h - DBS.Slider_Height) / 2;
         filled : constant Natural := DBL.Offset_Of (track.w, masterAudio.Level);
      begin
         fillRect (row.x + 1, row.y, inner, row.h, C.Row);
         if masterAudio.Available then
            drawIconIn (DSI.Icon (DSI.Volume, masterAudio.Muted), row, muteIconRect.x - row.x, C.Row);
            fillRect (track.x, barY, track.w, DBS.Slider_Height, C.Track);
            fillRect (track.x, barY, filled, DBS.Slider_Height,
                      (if masterAudio.Muted then C.Muted else C.Focus));
            drawBox ((x => track.x + filled - (if filled >= DBS.Thumb_Width / 2 then DBS.Thumb_Width / 2 else filled),
                      y => track.y, w => DBS.Thumb_Width, h => DBS.Thumb_Height), C.Thumb, C.Thumb_Edge);
            drawUIText (track.x + track.w + 2 * DBS.Status_Item_Gap, row.y + lineY,
                        (if masterAudio.Muted then "Muted" else countText (masterAudio.Level) & "%"),
                        C.Active_Text, C.Row);
         else
            drawUIText (row.x + DBS.Panel_Padding, row.y + lineY, "Sound: no mixer", C.Muted, C.Row);
         end if;
         if panelFocus = Volume_Row then
            drawFocusRing (row);
         end if;
      end;
      declare
         row : constant Rect := panelQuietRowRect;
         toggle : constant Rect := quietToggleRect;
         on : constant Boolean := DN.Quiet (notifications);
         state : constant String := (if on then "On" else "Off");
      begin
         fillRect (row.x + 1, row.y, inner, row.h, C.Row);
         fillRect (row.x + 1, row.y, inner, 1, C.Row_Rule);
         drawUIText (row.x + DBS.Panel_Padding, row.y + lineY, "Do not disturb", C.Active_Text, C.Row);
         drawBox (toggle, (if on then C.Focus else C.Row), (if on then C.Focus else C.Track));
         drawUIText (toggle.x + (toggle.w - uiTextWidth (state)) / 2,
                     toggle.y + (toggle.h - CuBit.Fonts.Line_Height) / 2, state,
                     (if on then C.Focus_Text else C.Muted), (if on then C.Focus else C.Row));
         if panelFocus = Quiet_Row then
            drawFocusRing (row);
         end if;
      end;

      --  The notification history, newest first.
      declare
         header : constant Rect := panelNotesHeaderRect;
         total : constant Natural := Natural (DN.Length (notifications));
      begin
         drawPanelSection (header, (if total = 0 then "Notifications"
                                    else "Notifications (" & countText (total) & ")"));
         if total > 0 then
            drawUIText (panelClearRect.x, header.y + (header.h - CuBit.Fonts.Line_Height) / 2,
                        Clear_All_Text, C.Focus, C.Section);
            if panelFocus = Clear_Row then
               drawFocusRing (panelClearRect);
            end if;
         end if;
         if total = 0 then
            declare
               row : constant Rect := (header.x, header.y + DBS.Panel_Section, header.w, DBS.Panel_Row);
            begin
               fillRect (row.x + 1, row.y, inner, row.h, C.Row);
               drawUIText (row.x + DBS.Panel_Padding, row.y + lineY, "No notifications", C.Muted, C.Row);
            end;
         end if;
         for K in 1 .. panelNoteRows loop
            declare
               N : constant DN.Notification := DN.Item (notifications, DN.Position (K));
               row : constant Rect := panelNoteRect (K);
               close : constant Rect := panelNoteCloseRect (K);
               textX : constant Natural := row.x + DBS.Panel_Padding + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap;
               stamp : constant String :=
                 (if N.Posted_At.Known then minuteText (N.Posted_At.Minute) else "");
               room : constant Natural :=
                 (if close.x > textX + DBS.Status_Item_Gap then close.x - textX - DBS.Status_Item_Gap else 0);
               titleRoom : constant Natural :=
                 (if room > uiTextWidth (stamp) + DBS.Status_Item_Gap
                  then room - uiTextWidth (stamp) - DBS.Status_Item_Gap else 0);
               titleInk : constant Unsigned_32 :=
                 (if N.Level = DN.Critical then C.Badge else C.Active_Text);
            begin
               fillRect (row.x + 1, row.y, inner, row.h, C.Row);
               fillRect (row.x + 1, row.y, inner, 1, C.Row_Rule);
               --  The source app's colour, as on its task button.
               fillRect (row.x + 1, row.y + 1, DBS.Stripe_Height, row.h - 1, noteAccent (N));
               drawIcon (N.Icon, row.x + DBS.Panel_Padding, row.y + DBS.Panel_Padding, C.Row);
               drawBoldText (textX, row.y + DBS.Panel_Padding - 2, fitText (DN.Title_Of (N), titleRoom, CuBit.Fonts.Sans_Semibold),
                             titleInk, C.Row);
               if stamp'Length > 0 then
                  drawUIText (close.x - DBS.Status_Item_Gap - uiTextWidth (stamp), row.y + DBS.Panel_Padding - 2,
                              stamp, C.Muted, C.Row);
               end if;
               drawUIText (textX, row.y + DBS.Panel_Padding - 2 + CuBit.Fonts.Line_Height + 1,
                           fitText (DN.Detail_Of (N), room), C.Muted, C.Row);
               drawBox (close, C.Row, C.Row_Rule);
               drawUIText (close.x + (close.w - uiTextWidth ("x")) / 2,
                           close.y + (close.h - CuBit.Fonts.Line_Height) / 2, "x", C.Muted, C.Row);
               if panelFocus = Note_Row and then Natural (panelFocusNote) = K then
                  drawFocusRing (row);
               end if;
            end;
         end loop;
      end;
      drawUIText (r.x + DBS.Panel_Padding, r.y + r.h - DBS.Panel_Footer + (DBS.Panel_Footer - CuBit.Fonts.Line_Height) / 2,
                  "Esc closes   Arrows move   Del dismisses", C.Muted, C.Panel);
   end drawDeskbarPanel;

   --  The toasted notification: shown briefly above the status button.
   function toastPosition return DN.Count is
     (if deskbarPanelOpen or else DN.Toast (notifications) = DN.No_Notification then 0
      else DN.Find (notifications, DN.Toast (notifications)));
   function toastShown return Boolean is (toastPosition /= 0);

   procedure drawToast is
      r : constant Rect := toastRect;
      C : constant DBS.Palette := deskbarColors;
      P : constant DN.Count := toastPosition;
   begin
      if P = 0 or else isEmpty (r) then
         return;
      end if;
      declare
         N : constant DN.Notification := DN.Item (notifications, P);
         textX : constant Natural := r.x + DBS.Panel_Padding + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap;
         room : constant Natural := (if r.x + r.w > textX + DBS.Panel_Padding then r.x + r.w - textX - DBS.Panel_Padding else 0);
         top : constant Natural := r.y + DBS.Panel_Stripe + DBS.Panel_Padding - 2;
      begin
         drawDappledShadow (r.x, r.y, r.w, r.h);
         drawBox (r, C.Row, C.Panel_Edge);
         fillRect (r.x, r.y, r.w, DBS.Panel_Stripe, noteAccent (N));
         drawIcon (N.Icon, r.x + DBS.Panel_Padding, top, C.Row);
         drawBoldText (textX, top, fitText (DN.Title_Of (N), room, CuBit.Fonts.Sans_Semibold),
                       (if N.Level = DN.Critical then C.Badge else C.Active_Text), C.Row);
         drawUIText (textX, top + CuBit.Fonts.Line_Height + 2, fitText (DN.Detail_Of (N), room), C.Muted, C.Row);
      end;
   end drawToast;

   --  One app's windows, in the order they opened.
   procedure drawGroupList is
      r : constant Rect := groupListRect;
      C : constant DBS.Palette := deskbarColors;
      W : constant TG.Windows := taskWindows;
      found : Boolean;
      slot : TG.Window_Slot;
   begin
      if isEmpty (r) then
         return;
      end if;
      drawDappledShadow (r.x, r.y, r.w, r.h);
      drawBox (r, C.Row, C.Panel_Edge);
      for R in TG.Window_Rank loop
         TG.Member_At (W, groupListKey, R, found, slot);
         exit when not found;
         declare
            S : Surface renames store.Surfaces (SurfaceIndex (slot));
            row : constant Rect := groupListRowRect (R);
            selected : constant Boolean := groupListRow = R;
            fill : constant Unsigned_32 := (if selected then C.Section else C.Row);
            textX : constant Natural := row.x + DBS.Icon_Left + Desktop_Icons.ICON_SIZE + DBS.Icon_Label_Gap;
            label : constant String := fitText
              (surfaceTitle (S), (if row.x + row.w > textX + DBS.Label_Right then row.x + row.w - textX - DBS.Label_Right else 0),
               (if S.id = focusSurface and then not S.minimized then CuBit.Fonts.Sans_Semibold else CuBit.Fonts.Sans));
            textY : constant Natural := row.y + (row.h - CuBit.Fonts.Line_Height) / 2;
         begin
            fillRect (row.x, row.y, row.w, row.h, fill);
            drawIconIn (S.appIcon, row, DBS.Icon_Left, fill);
            if S.id = focusSurface and then not S.minimized then
               drawBoldText (textX, textY, label, C.Active_Text, fill);
            else
               drawUIText (textX, textY, label, (if S.minimized then C.Dimmed_Text else C.Button_Text), fill);
            end if;
            if selected then
               drawFocusRing (row);
            end if;
         end;
      end loop;
   end drawGroupList;

   --  Clipboard manager geometry: a fixed-size panel centred above the
   --  taskbar, Clipboard_Menu.Visible_Rows rows of two text lines each.
   CLIP_MENU_W : constant := 480;
   CLIP_HEADER_H : constant := 38;
   CLIP_ROW_H : constant := 44;
   CLIP_FOOTER_H : constant := 28;
   CLIP_PREVIEW_CHARS : constant := 52;

   function clipboardMenuRect return Rect is
      h : constant Natural :=
        CLIP_HEADER_H + Clipboard_Menu.Visible_Rows * CLIP_ROW_H + CLIP_FOOTER_H;
   begin
      return clampRect
        ((x => primaryBounds.x +
                (if primaryBounds.w > CLIP_MENU_W
                 then (primaryBounds.w - CLIP_MENU_W) / 2 else 0),
          y => (if taskbarY > h + 12 then taskbarY - h - 12 else 0),
          w => CLIP_MENU_W, h => h));
   end clipboardMenuRect;

   function clipboardRowRect (offset : Natural) return Rect is
      r : constant Rect := clipboardMenuRect;
   begin
      return (x => r.x + 6, y => r.y + CLIP_HEADER_H + offset * CLIP_ROW_H,
              w => (if r.w > 12 then r.w - 12 else 0), h => CLIP_ROW_H - 2);
   end clipboardRowRect;

   --  The window title of the copying app (any of its surfaces), for the
   --  manager's second line. The source is the kernel-authenticated sender.
   function clipboardSourceName (source : Clipboard_Store.Source_Identity)
     return String is
   begin
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
           To_Word (store.Surfaces (i).owner) = Unsigned_64 (source) and then
           store.Surfaces (i).title.Length > 0
         then
            return store.Surfaces (i).title.Text;
         end if;
      end loop;
      return "closed app";
   end clipboardSourceName;

   procedure reloadClipboardMenu is
      use type Clipboard_Store.Slot_Count;
      rows : Clipboard_Menu.Row_Slots := [others => 0];
      count : Clipboard_Menu.Row_Count := 0;
      below : Clipboard_Store.Entry_Name := Clipboard_Store.No_Entry;
      slot : Clipboard_Store.Slot_Count;
   begin
      loop
         slot := Clipboard_Store.Older (Desktop_Clipboard.History, below);
         exit when slot = 0 or else count = Clipboard_Menu.Row_Count'Last;
         count := count + 1;
         rows (count) := slot;
         below := Clipboard_Store.Info (Desktop_Clipboard.History, slot).Name;
      end loop;
      Clipboard_Menu.Reload (clipboardMenu, rows, count);
   end reloadClipboardMenu;

   procedure drawClipboardMenu is
      use type Clipboard_Store.Slot_Count, Clipboard_Store.Sensitivity,
        Clipboard_Store.Entry_Phase, Clipboard_Store.Payload_Length;
      package CS renames Clipboard_Store;
      r : constant Rect := clipboardMenuRect;

      --  Printable ASCII only; line breaks and tabs show as spaces.
      function previewText (slot : CS.Slot_Index) return String is
         length : constant CS.Payload_Length := CS.Preview_Length
           (Desktop_Clipboard.History, slot, CLIP_PREVIEW_CHARS);
         text : String (1 .. Natural (length));
         b : Unsigned_8;
      begin
         for i in text'Range loop
            b := CS.Preview_Byte (Desktop_Clipboard.History, slot,
                                  CS.Payload_Index (i));
            text (i) := (if b in 32 .. 126 then Character'Val (b) else ' ');
         end loop;
         return text;
      end previewText;

      function typeName (slot : CS.Slot_Index) return String is
         meta : constant CS.Metadata := CS.Info (Desktop_Clipboard.History, slot);
         text : String (1 .. Natural'Min (Natural (meta.Shape.Type_Length), 32));
      begin
         for i in text'Range loop
            text (i) := Character'Val
              (CS.Byte (Desktop_Clipboard.History, slot, CS.Payload_Index (i)));
         end loop;
         return text;
      end typeName;
   begin
      if not clipboardMenuOpen or else isEmpty (r) then
         return;
      end if;
      drawDappledShadow (r.x, r.y, r.w, r.h);
      fillRect (r.x, r.y, r.w, r.h, C_PANEL);
      strokeRect (r.x, r.y, r.w, r.h, C_EDGE, C_SHADOW);
      fillRect (r.x, r.y, 4, r.h, C_ACCENT);
      drawIcon (Desktop_Icons.Clipboard, r.x + 12, r.y + 7, C_PANEL);
      drawUIText (r.x + 44, r.y + 11, "Clipboard", C_TEXT, C_PANEL);
      if clipboardMenu.Count = 0 then
         drawUIText (r.x + 44, r.y + CLIP_HEADER_H + 14,
                     "Nothing copied yet. Ctrl+C copies, Ctrl+V pastes.",
                     C_MUTED, C_PANEL);
      end if;
      for offset in 0 .. Clipboard_Menu.Visible_Rows - 1 loop
         declare
            row : constant Natural := Natural (clipboardMenu.First) + offset;
         begin
            exit when row > Natural (clipboardMenu.Count);
            declare
               slot : constant CS.Slot_Index :=
                 clipboardMenu.Slots (Clipboard_Menu.Row_Index (row));
               meta : constant CS.Metadata :=
                 CS.Info (Desktop_Clipboard.History, slot);
               item : constant Rect := clipboardRowRect (offset);
               selected : constant Boolean :=
                 Natural (clipboardMenu.Selected) = row;
               bg : constant Unsigned_32 := (if selected then C_ACCENT else C_PANEL);
               fg : constant Unsigned_32 := (if selected then C_WHITE else C_TEXT);
               dim : constant Unsigned_32 := (if selected then C_WHITE else C_MUTED);
               secret : constant Boolean := meta.Kind = CS.Sensitive;
               typed : constant Boolean := meta.Shape.CCL_Length > 0;
               hint : constant String :=
                 (if row <= 9 then Natural'Image (row) else "");
            begin
               if selected then
                  fillRect (item.x, item.y, item.w, item.h, bg);
               end if;
               drawIcon ((if secret then Desktop_Icons.Clip_Mask
                          elsif typed then Desktop_Icons.Clip_Value
                          else Desktop_Icons.Clip_Text),
                         item.x + 8, item.y + 10, bg);
               --  A secret never shows content, length or type: the mask
               --  and its state are all the manager knows how to say.
               drawUIText (item.x + 42, item.y + 5,
                           (if secret then "Secret" else previewText (slot)),
                           fg, bg);
               drawUIText
                 (item.x + 42, item.y + 23,
                  clipboardSourceName (meta.Source) & " - " &
                  (if secret then
                     (case meta.Phase is
                        when CS.Available => "paste once",
                        when CS.Consumed => "pasted, wiped",
                        when CS.Expired => "expired, wiped",
                        when CS.Vacant => "")
                   elsif typed then "CCL " & typeName (slot)
                   else "text," & meta.Shape.Text_Length'Image & " bytes"),
                  dim, bg);
               if meta.Pinned then
                  drawIcon (Desktop_Icons.Clip_Pin, item.x + item.w - 52,
                            item.y + 10, bg);
               end if;
               if hint'Length > 0 then
                  drawUIText (item.x + item.w - 22, item.y + 14, hint, dim, bg);
               end if;
            end;
         end;
      end loop;
      drawUIText (r.x + 14, r.y + r.h - CLIP_FOOTER_H + 6,
                  "Enter paste  1-9 quick  P pin  Del delete  Shift+Del clear",
                  C_MUTED, C_PANEL);
   end drawClipboardMenu;

   --  Deskbar state changes. Each adds what it changes to damage.
   procedure damagePanel (damage : in out Rect) is
   begin
      damage := unionRect (damage, unionRect (panelShown, panelRect));
   end damagePanel;

   procedure closeDeskbarPanel (damage : in out Rect) is
   begin
      if deskbarPanelOpen then
         damagePanel (damage);
         damage := unionRect (damage, statusRect);
         deskbarPanelOpen := False;
         deskbarSliderDragging := False;
         --  A toast still due shows again.
         damage := unionRect (damage, toastRect);
      end if;
   end closeDeskbarPanel;

   procedure closeGroupList (damage : in out Rect) is
   begin
      if groupListOpen then
         damage := unionRect (damage, groupListRect);
         groupListOpen := False;
         groupListRow := 0;
      end if;
   end closeGroupList;

   procedure openDeskbarPanel (damage : in out Rect) is
      oldStatus : constant Rect := statusRect;
   begin
      closeGroupList (damage);
      if launchMenuOpen then
         damage := unionRect (damage, inflateRect (unionRect (launchButtonRect, launchMenuArea), 4));
         launchMenuOpen := False;
      end if;
      --  The panel shows the toasted notice too: the toast is done.
      damage := unionRect (damage, toastRect);
      DN.Hide_Toast (notifications);
      deskbarPanelOpen := True;
      panelFocus := Volume_Row;
      panelFocusNote := 0;
      --  Opening the panel shows every notification: none is unread now.
      DN.Mark_Read (notifications);
      damage := unionRect (damage, unionRect (oldStatus, statusRect));
      damagePanel (damage);
   end openDeskbarPanel;

   procedure toggleDeskbarPanel (damage : in out Rect) is
   begin
      if deskbarPanelOpen then
         closeDeskbarPanel (damage);
      else
         openDeskbarPanel (damage);
      end if;
   end toggleDeskbarPanel;

   procedure toggleGroupList (Key : TG.Group_Key; damage : in out Rect) is
   begin
      if groupListOpen and then groupListKey = Key then
         closeGroupList (damage);
         return;
      end if;
      closeGroupList (damage);
      closeDeskbarPanel (damage);
      groupListOpen := True;
      groupListKey := Key;
      groupListRow := 1;
      damage := unionRect (damage, groupListRect);
   end toggleGroupList;

   --  The Deskbar (Desktop_Deskbar_Style): flat bar, its top rule, Apps,
   --  the task buttons and the status button.
   procedure drawDeskbar is
      barY : constant Natural := taskbarY;
      C    : constant DBS.Palette := deskbarColors;
   begin
      --  Only damage is repainted: skip the task-button layout when the
      --  clip misses the bar.
      if clipEnabled and then not rectIntersects (clipRect, taskbarRect) then
         return;
      end if;
      fillRect (primaryBounds.x, barY, primaryBounds.w, TASKBAR_H, C.Bar);
      fillRect (primaryBounds.x, barY, primaryBounds.w, DBS.Bar_Edge, C.Bar_Edge);
      drawAppsButton;
      drawTaskButtons;
      drawStatusButton;
   end drawDeskbar;

   procedure drawDesktopShell is
   begin
      if fbWidth = 0 or else fbHeight = 0 then
         return;
      end if;

      --  First shell renderer: deliberately Win95-simple. The compositor owns
      --  pixels for now; the shell owns policy and talks through the protocol.
      --  Shared client buffers can replace this drawing path later without
      --  changing the surface/session shape.
      drawWallpaper (Cull_Windows => True);

      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            not store.Surfaces (i).minimized and then
            store.Surfaces (i).id /= compositionExcludedSurface and then
            (store.Surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            drawWindow (store.Surfaces (i));
         end if;
      end loop;

      --  The Deskbar is the top layer under popups, menus and tooltips.
      drawDeskbar;
      drawLaunchMenu;
      drawGroupList;
      drawDeskbarPanel;
      panelShown := (if deskbarPanelOpen then panelRect else (others => 0));
      drawClipboardMenu;
      drawToast;
      drawTooltip;
   end drawDesktopShell;

   procedure drawCurrentScene is
      shellVisible : Boolean := False;
   begin
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            (store.Surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
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

   --  Below the bootstrap text (CAP-001 NUC diagnostics): what Desktop does
   --  next and since when, the latest Warning-or-worse log records
   --  (device-memory denials, intel-gpu startup failures) and the kernel's
   --  physical map, as gathered just before this frame (Desktop_Bootstrap_Logs).
   procedure drawBootstrapDiagnostics is
      package Notes renames Desktop_Bootstrap_Notes;
      Fg     : constant Unsigned_32 := 16#00FFFFFF#;
      Dim    : constant Unsigned_32 := 16#00B0C4D8#;
      Warn   : constant Unsigned_32 := 16#00FFD060#;
      Bg     : constant Unsigned_32 := 16#00203648#;
      Left   : constant := 24;
      First_Row : constant := 140;
      Row_Height : constant := 24;
      Section_Gap : constant := 10;
      Kept   : constant Notes.Notes := Desktop_Bootstrap_Logs.Current;
      Y      : Natural := First_Row;
      procedure Row (Text : String; Color : Unsigned_32) is
      begin
         if Y + Row_Height <= fbHeight then
            drawUIText (Left, Y, Text, Color, Bg);
         end if;
         Y := Y + Row_Height;
      end Row;
   begin
      Row ("Stage: " & bootstrapStage.all & "  (t=" &
           Unsigned_64'Image (Desktop_Startup_Clock.Elapsed_Ms) & " ms since Desktop started)", Fg);
      Row ("Waiting on: " & bootstrapWaiting.all, Fg);
      if rendererWorkerActive then
         declare
            Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
            Running : constant Unsigned_64 :=
              (if Now >= rendererStartedMs then Now - rendererStartedMs else 0);
         begin
            Row ("Renderer startup step " &
                 Desktop_Breadcrumbs.Name (Desktop_Startup_Progress.Current) & " for" &
                 Unsigned_64'Image (Running) & " ms; software at" &
                 Unsigned_64'Image (Unsigned_64'(Renderer_Startup_Budget_Ms)) &
                 " ms if not done", Fg);
         end;
      end if;
      Row ("Frame" & Unsigned_64'Image (bootstrapFrames) & "; redrawn every" &
           Unsigned_64'Image (Unsigned_64'(Bootstrap_Tick_Ms)) &
           " ms while startup runs (t= frozen: Desktop or Display stopped)", Dim);
      if Kept.Driver.Length > 0 then
         Row ("intel-gpu latest: " & Notes.Text_Of (Kept.Driver), Fg);
      else
         Row ("intel-gpu latest: no line in the log yet", Dim);
      end if;
      Row ("Logs as of t=" & Unsigned_64'Image (Desktop_Bootstrap_Logs.Gathered_At_Ms) &
           " ms; kernel device-memory denials:" &
           Unsigned_64'Image (Desktop_Bootstrap_Logs.Kernel_Denials) &
           (if Unsigned_64 (Kept.Highest_Denial) >= Desktop_Bootstrap_Logs.Kernel_Denials
            then " (all in the log)"
            else " (" & Natural'Image (Kept.Highest_Denial) & " reached the log)"), Fg);
      --  Processes the kernel killed for a fault (count read directly; the
      --  lines come through logstore), shown apart from other warnings.
      Row ("Kernel process kills:" & Unsigned_64'Image (Desktop_Bootstrap_Logs.Kernel_Kills) &
           (if Unsigned_64 (Kept.Highest_Kill) >= Desktop_Bootstrap_Logs.Kernel_Kills
            then " (all in the log)"
            else " (" & Natural'Image (Kept.Highest_Kill) & " reached the log)"),
           (if Desktop_Bootstrap_Logs.Kernel_Kills = 0 then Fg else Warn));
      for I in 1 .. Kept.Kills_Used loop
         Row (Notes.Text_Of (Kept.Kills (I)), Warn);
      end loop;
      Y := Y + Section_Gap;
      if Kept.Recent_Used = 0 then
         Row ("Recent warnings: none (or logstore unavailable)", Dim);
      else
         Row ("Recent warnings (latest" & Natural'Image (Kept.Recent_Used) & " of" &
              Natural'Image (Kept.Shown_Total) & "):", Fg);
         for I in 1 .. Kept.Recent_Used loop
            Row (Notes.Text_Of (Kept.Recent (I)), Warn);
         end loop;
      end if;
      if Kept.Map_Used > 0 then
         Y := Y + Section_Gap;
         for I in 1 .. Kept.Map_Used loop
            Row (Notes.Text_Of (Kept.Map (I)), Dim);
         end loop;
      end if;
   end drawBootstrapDiagnostics;

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
         drawBootstrapDiagnostics;
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
      drawWindow (store.Surfaces (SurfaceIndex (idx)));
      --  The Deskbar stays above the moving window; the retained layer can
      --  also outlive a minute boundary or media-key press. Composite the
      --  current Deskbar over it, within this damage clip.
      drawDeskbar;
      drawGroupList;
      drawDeskbarPanel;
      drawClipboardMenu;
      drawToast;
      drawTooltip;
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

   --  Post N to the notification centre: the status button's count, the
   --  toast, and the open panel change.
   procedure postNotification (N : DN.Notification) is
      oldStatus : constant Rect := statusRect;
      id : DN.Serial;
   begin
      DN.Post (notifications, N, nowMs, id);
      if id = DN.No_Notification then
         return;
      end if;
      if deskbarPanelOpen then
         DN.Mark_Read (notifications);
         scheduleRedrawRect (unionRect (panelShown, panelRect));
      end if;
      debugPrint ("desktop: notification posted id=" & Decimal (Unsigned_64 (id)) &
                  " source=" & DN.Name_Of (N) & " urgency=" &
                  (case N.Level is
                     when DN.Low => "low", when DN.Normal => "normal", when DN.Critical => "critical") & LF);
      scheduleRedrawRect (unionRect (oldStatus, statusRect));
      scheduleRedrawRect (toastRect);
   end postNotification;

   --  The wall-clock minute, when the clock service has given one.
   function wallNow return DN.Wall_Time is
     (if clockDateKnown
      then (Known => True, Minute => DN.Minute_Of_Day (clockDate.Hour * 60 + clockDate.Minute))
      else (Known => False));

   procedure postLaunchNotice
     (Notice : Desktop_Launch_Notice.Notice; Process : Process_ID)
   is
      package LN renames Desktop_Launch_Notice;
      Icon : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
      Accent : DN.Accent_Color := (Declared => False);
   begin
      Desktop_Logs.Warn ("desktop: launch failed: " & LN.Title (Notice) & ". " & LN.Detail (Notice));
      --  The notice names the app by its menu label: its icon and colour.
      for I in 1 .. launchMenu.Count loop
         if Desktop_Launch.Label_Of (launchMenu.Entries (I)) = LN.Name_Of (Notice) then
            Icon := launchMenu.Entries (I).Icon;
            if launchMenu.Entries (I).Color.Declared then
               Accent := (Declared => True, Value => launchMenu.Entries (I).Color.Value);
            end if;
            exit;
         end if;
      end loop;
      postNotification
        (DN.Make (Kind => DN.Application, Process => To_Word (Process), Name => LN.Name_Of (Notice),
                  Level => DN.Normal, Icon => Icon, Accent => Accent, Now_Ms => nowMs,
                  At_Wall => wallNow, Title => LN.Title (Notice), Detail => LN.Detail (Notice)));
   end postLaunchNotice;

   --  Why Desktop could not give a window: the surface table is at the
   --  protocol's bound, the heap cannot grow it, the surface names ran out,
   --  or there is no display to show it on.
   type Window_Refusal is (Table_Full, Out_Of_Memory, Names_Exhausted, Display_Not_Ready);

   --  The process whose last window request was refused: told once, not on
   --  every retry, until it gets a window.
   windowRefusedOwner : Process_ID := No_Process;

   --  A refused window never fails silently: "Files could not open a
   --  window", with the reason, in the notification centre (the app's
   --  launcher icon and colour when it has an entry).
   procedure noteWindowRefused (Owner : Process_ID; Kind : App_Kind; Why : Window_Refusal) is
      Index : constant Natural := launchEntryOf (Owner, Kind);
      Accent : DN.Accent_Color := (Declared => False);
      Icon : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
      Name : constant String :=
        (if Index in 1 .. launchMenu.Count then Desktop_Launch.Label_Of (launchMenu.Entries (Index))
         elsif Owner = No_Process then "Desktop"
         else "Process" & Decimal (To_Word (Owner)));
      Detail : constant String :=
        (case Why is
            when Table_Full =>
              "Desktop already shows the most windows it can (" & Decimal (MAX_SURFACES) & ").",
            when Out_Of_Memory => "Desktop has no memory for another window. Close some windows and try again.",
            when Names_Exhausted => "Desktop has run out of window names. Restart the session.",
            when Display_Not_Ready => "There is no display ready to show it on.");
   begin
      if Owner /= No_Process and then Owner = windowRefusedOwner then
         return;
      end if;
      windowRefusedOwner := Owner;
      Desktop_Logs.Warn ("desktop: window refused: " & Name & ". " & Detail);
      if Index in 1 .. launchMenu.Count then
         Icon := launchMenu.Entries (Index).Icon;
         if launchMenu.Entries (Index).Color.Declared then
            Accent := (Declared => True, Value => launchMenu.Entries (Index).Color.Value);
         end if;
      end if;
      postNotification
        (DN.Make (Kind => (if Owner = No_Process then DN.Service else DN.Application),
                  Process => To_Word (Owner), Name => Name,
                  Level => DN.Normal, Icon => Icon, Accent => Accent, Now_Ms => nowMs,
                  At_Wall => wallNow, Title => Name & " could not open a window", Detail => Detail));
   end noteWindowRefused;

   --  A new client window's size and place (Desktop_Window_Placement): a
   --  share of the primary work area or the request, cascaded by slot.
   procedure cascadePlace
     (Slot : Natural; Sizing : DP.Window_Sizing; W, H : in out Natural; X, Y : out Natural) is
      Work : constant Rect := windowWorkArea (primaryBounds);
      Bound : constant := Desktop_Window_Placement.Maximum_Extent;
      Spot : constant Desktop_Window_Placement.Placement :=
        Desktop_Window_Placement.Place
          (Work_Width => Natural'Min (Work.w, Bound), Work_Height => Natural'Min (Work.h, Bound),
           Requested_Width => Natural'Min (W, Bound), Requested_Height => Natural'Min (H, Bound),
           Minimum_Width => MIN_WIN_W, Minimum_Height => MIN_WIN_H, Slot => Slot,
           Policy => (case Sizing is
                         when DP.Desktop_Sized => Desktop_Window_Placement.At_Least_Share,
                         when DP.Preferred_Size => Desktop_Window_Placement.As_Requested));
   begin
      X := Spot.X;
      Y := Spot.Y;
      W := Spot.Width;
      H := Spot.Height;
   end cascadePlace;


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


   function taskbarChanges return Rect is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      spans : constant DBL.Span_List := spansOf (W, M);
      now : Task_Looks (store.Tasks'Range);
      changed : Rect := (others => 0);
   begin
      for G in 1 .. M.Count loop
         declare
            P : constant TG.Group_Index := TG.Position (W, M, G);
         begin
            now (Natural (P) - 1) := (Box => placeRect (spans, P), Key => M.Groups (G).Key,
                        First => store.Surfaces (SurfaceIndex (M.Groups (G).First)).id,
                        Members => M.Groups (G).Members, Active => M.Groups (G).Active,
                        Minimized => M.Groups (G).Minimized);
         end;
      end loop;
      for P in now'Range loop
         if now (P) /= store.Tasks (P) then
            changed := unionRect (changed, unionRect (now (P).Box, store.Tasks (P).Box));
         end if;
      end loop;
      store.Tasks := now;
      return changed;
   end taskbarChanges;

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
      damage := unionRect (damage, taskbarChanges);
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
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then store.Surfaces (i).id = id then
            return Integer (i);
         end if;
      end loop;

      return -1;
   end findSurface;

   function anySurfaceUsed return Boolean is
   begin
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used then
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

      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            (store.Surfaces (i).flags and SURFACE_FLAG_SHELL) /= 0
         then
            return True;
         end if;
      end loop;

      return False;
   end shellSurfaceVisible;

   procedure queueConfigure (surfaceId, w, h : Unsigned_64);
   procedure restoreSurface (idx : SurfaceIndex; damage : in out Rect);

   procedure focusTopmostVisibleWindow (damage : in out Rect) is
      Eligible : Focus_Policy.Candidates (store.Surfaces'Range);
      Chosen : Focus_Policy.Selection;
   begin
      for I in store.Surfaces'Range loop
         Eligible (I) := store.Surfaces (I).used and then not store.Surfaces (I).minimized and then
           (store.Surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0;
      end loop;
      Chosen := Focus_Policy.Topmost (Eligible);
      if Chosen.Found then
         focusSurface := store.Surfaces (Chosen.Slot).id;
         damage := unionRect (damage, inflateRect (surfaceRect (store.Surfaces (Chosen.Slot)), 4));
         damage := unionRect (damage, inflateRect (taskButtonRect (Chosen.Slot), 4));
      else
         focusSurface := 0;
      end if;
   end focusTopmostVisibleWindow;

   procedure raiseSurface (idx : SurfaceIndex) is
      moved : Surface := store.Surfaces (idx);
      last  : SurfaceIndex := idx;
   begin
      if (store.Surfaces (idx).flags and SURFACE_FLAG_WINDOW) = 0 then
         return;
      end if;
      if idx = store.Surfaces'Last then
         return;
      end if;

      for i in idx + 1 .. store.Surfaces'Last loop
         if store.Surfaces (i).used and then
            (store.Surfaces (i).flags and SURFACE_FLAG_WINDOW) /= 0
         then
            store.Surfaces (last) := store.Surfaces (i);
            last := i;
         end if;
      end loop;

      store.Surfaces (last) := moved;
   end raiseSurface;

   procedure damagePreviousFocus (New_Focus : Unsigned_64; Damage : in out Rect) is
      Previous : constant Integer := findSurface (focusSurface);
   begin
      if focusSurface = New_Focus or else Previous < 0 then return; end if;
      declare
         S : Surface renames store.Surfaces (SurfaceIndex (Previous));
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
      oldBounds : constant Rect := surfaceRect (store.Surfaces (idx));
      oldTask   : constant Rect := taskButtonRect (idx);
      id        : constant Unsigned_64 := store.Surfaces (idx).id;
      raisedIdx : Integer;
      Already_Top : Boolean := True;
   begin
      if idx < store.Surfaces'Last then
         for I in idx + 1 .. store.Surfaces'Last loop
            if store.Surfaces (I).used and then
              (store.Surfaces (I).flags and SURFACE_FLAG_WINDOW) /= 0
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
            inflateRect (surfaceRect (store.Surfaces (SurfaceIndex (raisedIdx))), 4));
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

      for step in store.Surfaces'Range loop
         probe := (base + step) mod store.Surfaces'Length;
         if store.Surfaces (SurfaceIndex (probe)).used and then
            (store.Surfaces (SurfaceIndex (probe)).flags and
             SURFACE_FLAG_WINDOW) /= 0
         then
            chosen := Integer (probe);
            exit;
         end if;
      end loop;

      if chosen >= 0 then
         if store.Surfaces (SurfaceIndex (chosen)).minimized then
            restoreSurface (SurfaceIndex (chosen), damage);
         end if;
         focusAndRaiseSurface (SurfaceIndex (chosen), damage);
      end if;
   end cycleFocus;

   procedure minimizeSurface (idx : SurfaceIndex; damage : in out Rect) is
      oldBounds : constant Rect := surfaceRect (store.Surfaces (idx));
      button    : constant Rect := taskButtonRect (idx);
   begin
      store.Surfaces (idx).minimized := True;
      store.Surfaces (idx).dirty := True;

      if focusSurface = store.Surfaces (idx).id then
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
      store.Surfaces (idx).minimized := False;
      store.Surfaces (idx).dirty := True;
      damagePreviousFocus (store.Surfaces (idx).id, damage);
      focusSurface := store.Surfaces (idx).id;
      bounds := surfaceRect (store.Surfaces (idx));

      damage := unionRect (damage, inflateRect (bounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));
   end restoreSurface;

   procedure toggleMaximizeSurface
      (idx    : SurfaceIndex;
       damage : in out Rect)
   is
      oldBounds : constant Rect := surfaceRect (store.Surfaces (idx));
      newBounds : Rect;
      workArea  : constant Rect := windowWorkArea (oldBounds);
      nextW     : Natural;
      nextH     : Natural;
   begin
      if not hasWindowFlag (store.Surfaces (idx), WINDOW_FLAG_MAXIMIZABLE) or else
         hasWindowFlag (store.Surfaces (idx), WINDOW_FLAG_FIXED_SIZE)
      then
         return;
      end if;

      if store.Surfaces (idx).maximized then
         store.Surfaces (idx).x := store.Surfaces (idx).restoreX;
         store.Surfaces (idx).y := store.Surfaces (idx).restoreY;
         store.Surfaces (idx).w := store.Surfaces (idx).restoreW;
         store.Surfaces (idx).h := store.Surfaces (idx).restoreH;
         store.Surfaces (idx).maximized := False;
      else
         store.Surfaces (idx).restoreX := store.Surfaces (idx).x;
         store.Surfaces (idx).restoreY := store.Surfaces (idx).y;
         store.Surfaces (idx).restoreW := store.Surfaces (idx).w;
         store.Surfaces (idx).restoreH := store.Surfaces (idx).h;
         nextW := workArea.w;
         nextH := workArea.h;
         clampSurfaceSize (store.Surfaces (idx), nextW, nextH);
         store.Surfaces (idx).x := workArea.x;
         store.Surfaces (idx).y := workArea.y;
         store.Surfaces (idx).w := nextW;
         store.Surfaces (idx).h := nextH;
         store.Surfaces (idx).maximized := True;
      end if;

      store.Surfaces (idx).minimized := False;
      store.Surfaces (idx).dirty := True;
      store.Surfaces (idx).serial := store.Surfaces (idx).serial + 1;
      damagePreviousFocus (store.Surfaces (idx).id, damage);
      focusSurface := store.Surfaces (idx).id;
      newBounds := surfaceRect (store.Surfaces (idx));

      if store.Surfaces (idx).owner /= No_Process then
         queueConfigure (store.Surfaces (idx).id,
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
      Source_Loans.Reserve (loans.Policy, Loan);
      if Loan = Source_Loans.No_Ticket then
         Result := DP.Resources_Exhausted;
         return;
      end if;
      MG.Acquire (Grant, Owner, 0, Bytes, MG.Read_Access, Address, Acquired);
      if not Acquired then
         Source_Loans.Cancel (loans.Policy, Loan);
         Loan := Source_Loans.No_Ticket;
         Result := DP.Denied;
         return;
      end if;
      loans.Loans (Source_Loans.Index (Loan)) := (Grant, Address);
      Source_Loans.Activate (loans.Policy, Loan);
      Result := DP.Success;
   end acquireSourceLoan;

   procedure pumpSourceLoan (Loan : Source_Loans.Ticket) is
      Renderer : Desktop_Compositor.Source_Release;
      Confirmed : Boolean;
   begin
      if Source_Loans.Current (loans.Policy, Loan) = Source_Loans.Renderer_Pending then
         Desktop_Compositor.Forget_Source
           (loans.Loans (Source_Loans.Index (Loan)).Address, Renderer);
         Source_Loans.Observe_Renderer
           (loans.Policy, Loan,
            (case Renderer is
               when Desktop_Compositor.Source_Retired => Source_Loans.Retired,
               when Desktop_Compositor.Source_Busy => Source_Loans.Busy,
               when Desktop_Compositor.Source_Unsafe => Source_Loans.Uncertain));
      end if;
      if Source_Loans.Current (loans.Policy, Loan) = Source_Loans.Grant_Pending then
         MG.Return_Acquisition (loans.Loans (Source_Loans.Index (Loan)).Grant, Confirmed);
         Source_Loans.Observe_Grant (loans.Policy, Loan, Confirmed);
         if Confirmed then
            loans.Loans (Source_Loans.Index (Loan)) := (others => <>);
         end if;
      end if;
      if Source_Loans.Current (loans.Policy, Loan) = Source_Loans.Quarantined then
         debugPrint ("desktop: source retirement uncertain; restart required" & LF);
         exitCompositor (1);
      end if;
   end pumpSourceLoan;

   procedure retireSourceLoan (Loan : Source_Loans.Ticket) is
   begin
      if Source_Loans.Current (loans.Policy, Loan) in
        Source_Loans.Attached | Source_Loans.Renderer_Pending | Source_Loans.Grant_Pending
      then
         Source_Loans.Retire (loans.Policy, Loan);
         pumpSourceLoan (Loan);
      end if;
   end retireSourceLoan;

   procedure pumpSourceRetirements is
   begin
      for I in 1 .. loans.Capacity loop
         pumpSourceLoan (Source_Loans.At_Slot (loans.Policy, I));
      end loop;
   end pumpSourceRetirements;

   function sourceRetirementPending return Boolean is
   begin
      for I in 1 .. loans.Capacity loop
         if Source_Loans.Current (loans.Policy, Source_Loans.At_Slot (loans.Policy, I)) in
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
         if Source_Loans.Current (loans.Policy, S.publicationBuffers (I).Loan) /=
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
      oldBounds : constant Rect := surfaceRect (store.Surfaces (idx));
      oldId     : constant Unsigned_64 := store.Surfaces (idx).id;
      oldOwner  : constant Process_ID := store.Surfaces (idx).owner;
      button    : constant Rect := taskButtonRect (idx);
      ignore    : Unsigned_64;
   begin
      if oldOwner /= No_Process and then
        (store.Surfaces (idx).windowFlags and DP.Window_Feature'Enum_Rep (DP.Graceful_Close)) /= 0
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

      releaseSurfaceBuffer (store.Surfaces (idx));
      retireSurfaceSource (store.Surfaces (idx));
      store.Surfaces (idx) := (others => <>);

      if focusSurface = oldId then
         focusSurface := 0;
      end if;

      damage := unionRect (damage, inflateRect (oldBounds, 4));
      damage := unionRect (damage, inflateRect (button, 4));

      if focusSurface = 0 then
         focusTopmostVisibleWindow (damage);
      end if;

      --  Desktop holds no process authority over its clients: closing a
      --  window does not stop its owner (the kernel refused the old kill
      --  too). Stopping the owner belongs to its launcher (procmgr).
      null;
   end closeSurface;

   procedure reapDeadClientSurfaces (damage : in out Rect) is
      oldBounds : Rect;
      oldTask   : Rect;
      changed   : Boolean := False;
   begin
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            store.Surfaces (i).owner /= No_Process and then
            not processAlive (store.Surfaces (i).owner)
         then
            -- The acquisition keeps mappings alive across owner exit. Reaping
            -- releases it; processAlive is no longer the memory-safety fence.
            oldBounds := surfaceRect (store.Surfaces (i));
            oldTask := taskButtonRect (i);
            if pointerSurfaceId = store.Surfaces (i).id then
               pointerSurfaceId := 0;
            end if;
            if dragSurfaceId = store.Surfaces (i).id then
               endDrag;
            end if;
            clearInputForTarget (store.Surfaces (i).id);
            if focusSurface = store.Surfaces (i).id then
               focusSurface := 0;
            end if;
            if store.Surfaces (i).bufferAttached or else store.Surfaces (i).publicationMode then
               releaseSurfaceBuffer (store.Surfaces (i));
               debugPrint ("desktop: dead client buffer retirement requested" & LF);
            end if;
            retireSurfaceSource (store.Surfaces (i));
            store.Surfaces (i) := (others => <>);
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
      for i in store.Surfaces'Range loop
         if store.Surfaces (i).used and then
            store.Surfaces (i).owner = No_Process and then
            store.Surfaces (i).appKind = appKind
         then
            existing := Integer (i);
            exit;
         end if;
      end loop;

      if existing >= 0 then
         restoreSurface (SurfaceIndex (existing), damage);
         existing := findSurface (store.Surfaces (SurfaceIndex (existing)).id);
         if existing >= 0 then
            focusAndRaiseSurface (SurfaceIndex (existing), damage);
         end if;
         return;
      end if;

      case appKind is
         when APP_SETTINGS =>
            winW := 742;
            winH := 420;
            declare
               Zone : Time_Zones.Time_Zone;
               Known : Boolean;
            begin
               Desktop_Time_Zone_Setting.Load (Zone, Known);
               if not Known then
                  debugPrint ("desktop: clock.time-zone is not in the bundled database; Settings shows UTC" & LF);
               end if;
               Desktop_Settings.Open (settingsView, appearance, desktopLayout,
                 desktopLayout.Items (Natural (primaryOutput) + 1).Display, displayScale.Auto, Zone);
               Desktop_Settings.Set_Monitor (settingsView, monitorInfo);
            end;
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
      if id = 0 then
         noteWindowRefused
           (No_Process, appKind,
            (if store.Last + 1 >= MAX_SURFACES then Table_Full else Out_Of_Memory));
      end if;

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
      slot : Integer;
   begin
      id := 0;
      freeSurfaceSlot (slot);
      if slot < 0 then
         return;
      end if;

      store.Surfaces (SurfaceIndex (slot)) :=
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
         publicationInputAfter => 0,
         others => <>);
      assignApp (store.Surfaces (SurfaceIndex (slot)), No_Process);
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
      for i in store.Channels'Range loop
         if store.Channels (i).target = target and then target /= 0 then
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

      c := clientRect (store.Surfaces (SurfaceIndex (surfaceSlot)));
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

      for i in store.Channels'Range loop
         if store.Channels (i).target = 0 then
            store.Channels (i) :=
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
         Channel : SurfaceInputChannel renames store.Channels (SurfaceIndex (Slot));
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
         queue : PendingInputQueue renames store.Channels (idx).events;
         snapshot : InputSnapshot renames store.Channels (idx).snapshot;
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
            snapshot.modifiers := Shift_Right (payload1, 32);
         elsif kind = INPUT_POINTER_WHEEL then
            snapshot.pointerPosition := payload0;
            snapshot.buttons := Shift_Right (payload1, 32);
         elsif kind = INPUT_KEY_DOWN or else kind = INPUT_KEY_UP then
            snapshot.modifiers := payload1 and 16#FFFF_FFFF#;
         end if;

         IQ.Push
           (queue, store.Channels (idx).nextSerial,
            (True, 0, kind, target, payload0, payload1),
            (True, 0, INPUT_RESYNC, target, snapshot.pointerPosition,
             (snapshot.buttons and 16#FFFF_FFFF#) or
               Shift_Left (snapshot.modifiers and 16#FFFF_FFFF#, 32)),
            (if Close_Policy.May_Coalesce
               (store.Channels (idx).pendingClose,
                (if IQ.Newest (queue) = -1 then 0
                 else queue (IQ.Newest (queue)).Serial)) and then
               (IQ.Newest (queue) = -1 or else
                queue (IQ.Newest (queue)).Serial > store.Channels (idx).exposedThrough)
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
         Channel : SurfaceInputChannel renames store.Channels (SurfaceIndex (channelSlot));
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
           (store.Channels (SurfaceIndex (channelSlot)).pendingClose, afterSerial) or else
         IQ.Has_After (store.Channels (SurfaceIndex (channelSlot)).events, afterSerial));
   end hasInputAfter;

   procedure completeInputWaiter (target : Unsigned_64) is
      channelSlot : constant Integer := findInputChannel (target);
      event : PendingInput;
      found : Boolean;
      response : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if channelSlot < 0 or else
         not store.Channels (SurfaceIndex (channelSlot)).waiter.active
      then
         return;
      end if;

      declare
         waiter : InputWaiter renames
           store.Channels (SurfaceIndex (channelSlot)).waiter;
      begin
         dequeueInput (target, waiter.afterSerial, found, event);
         if not found then
            return;
         end if;

         response.tag :=
           (label => OP_INPUT_WAIT, length => 4,
            protocol =>
              (if hasInputAfter (target, event.serial)
               then INPUT_REPLY_MORE_PENDING else 0), ipc => <>,
            reserved => 0);
         response.words (0) := event.kind;
         response.words (1) := event.serial;
         response.words (2) := event.payload0;
         response.words (3) := event.payload1;

         --  Clear software ownership before consuming the one-use kernel
         --  authority. A failed reply cannot leave an immortal waiter.
         declare
            saved : constant CuBit.Handles.Reply := waiter.saved;
         begin
            waiter := (others => <>);
            ignore := replyCap (saved, response);
         end;
      end;
   end completeInputWaiter;

   function nextInputDeadline return Unsigned_64 is
      deadline : Unsigned_64 := 0;
   begin
      for channel of store.Channels loop
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
      Input_Transfers.Poll (inputTransfers.all);
      for channel of store.Channels loop
         if channel.waiter.active and then channel.waiter.deadline /= 0 and then
           now >= channel.waiter.deadline
         then
            -- Input publication completes its waiter immediately. An expiry
            -- consumes the same one-use reply, without inventing an input
            -- serial or clearing queued events.
            response := NULL_MESSAGE;
            response.tag := (label => OP_INPUT_WAIT, length => 4,
                             protocol => 0, ipc => <>, reserved => 0);
            response.words (0) := INPUT_NONE;
            response.words (1) := channel.waiter.afterSerial;
            declare
               saved : constant CuBit.Handles.Reply := channel.waiter.saved;
            begin
               channel.waiter := (others => <>);
               ignore := replyCap (saved, response);
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
      KR.Cancel (keyRepeat);
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

      for i in store.Channels'Range loop
         if store.Channels (i).target /= 0 then
            declare
               surfaceSlot : constant Integer :=
                 findSurface (store.Channels (i).target);
               localX : Natural := 0;
               localY : Natural := 0;
               Accepted : Boolean;
            begin
               if surfaceSlot >= 0 then
                  declare
                     c : constant Rect :=
                       clientRect (store.Surfaces (SurfaceIndex (surfaceSlot)));
                  begin
                     if cursorX >= c.x then
                        localX := cursorX - c.x;
                     end if;
                     if cursorY >= c.y then
                        localY := cursorY - c.y;
                     end if;
                  end;
               end if;

               store.Channels (i).snapshot.pointerPosition :=
                 packU32Pair (localX, localY);
               store.Channels (i).snapshot.buttons := lastButtons;
               store.Channels (i).snapshot.modifiers := mods;
               if store.Channels (i).snapshot.generation < Unsigned_64'Last then
                  store.Channels (i).snapshot.generation :=
                    store.Channels (i).snapshot.generation + 1;
               end if;
               IQ.Recover
                 (store.Channels (i).events, store.Channels (i).nextSerial,
                  (True, 0, INPUT_RESYNC, store.Channels (i).target,
                   store.Channels (i).snapshot.pointerPosition,
                   (lastButtons and 16#FFFF_FFFF#) or
                     Shift_Left (mods and 16#FFFF_FFFF#, 32)), Accepted);
               if not Accepted then
                  debugPrint ("desktop: input serials exhausted" & LF);
                  exitCompositor (1);
               end if;
               completeInputWaiter (store.Channels (i).target);
            end;
         end if;
      end loop;
   end forceInputResynchronization;

   procedure clearInputQueue is
   begin
      store.Channels := [others => (others => <>)];
   end clearInputQueue;

   procedure clearInputForTarget (target : Unsigned_64) is
      channelSlot : constant Integer := findInputChannel (target);
      response : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if target = 0 or else channelSlot < 0 then
         return;
      end if;

      if store.Channels (SurfaceIndex (channelSlot)).waiter.active then
         response.tag :=
           (label => OP_INPUT_WAIT, length => 4, protocol => 0, ipc => <>, reserved => 0);
         response.words (0) := INPUT_RESYNC;
         IQ.Reserve (store.Channels (SurfaceIndex (channelSlot)).nextSerial,
           response.words (1));
         if response.words (1) = 0 then
            debugPrint ("desktop: input serials exhausted" & LF);
            exitCompositor (1);
         end if;
         declare
            saved : constant CuBit.Handles.Reply :=
              store.Channels (SurfaceIndex (channelSlot)).waiter.saved;
         begin
            store.Channels (SurfaceIndex (channelSlot)).waiter := (others => <>);
            ignore := replyCap (saved, response);
         end;
      end if;

      store.Channels (SurfaceIndex (channelSlot)) := (others => <>);
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

   --  repeat: a press the desktop synthesized for a held key; the key and
   --  text events carry the protocol's repeat flag.
   procedure queueKey (raw : Unsigned_8; repeat : Boolean) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_64 := Unsigned_64 (raw and 16#7F#);
      ch      : Character;
      mods    : constant Unsigned_64 := modifierState;
      flags   : constant Unsigned_64 := (if repeat then DP.Key_Repeat_Flag else 0);
   begin
      if focusSurface = 0 then
         return;
      end if;

      if not release then
         --  A copy or paste chord routed to this surface is the only paste
         --  authority it gets (docs/clipboard.md); issue it before the app
         --  can see the key.
         Desktop_Clipboard.Note_Key
           (focusSurface, raw and 16#7F#, mods, nowMs);
         if focusSurface /= keysRoutedTo then
            --  Name the window keys now go to, once per change (tests and
            --  traces read it; the clipboard's gestures follow it).
            keysRoutedTo := focusSurface;
            declare
               idx : constant Integer := findSurface (focusSurface);
            begin
               if idx >= 0 then
                  --  A window without a title is named by its surface.
                  debugPrint ("desktop: keys to " &
                    (if store.Surfaces (SurfaceIndex (idx)).title.Text'Length > 0
                     then store.Surfaces (SurfaceIndex (idx)).title.Text
                     else "untitled surface " & Decimal (focusSurface)) & LF);
               end if;
            end;
         end if;
      end if;
      enqueueInput ((if release then INPUT_KEY_UP else INPUT_KEY_DOWN),
                    focusSurface,
                    code,
                    mods or flags);
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
                          flags);
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
      if idx < 0 or else store.Surfaces (SurfaceIndex (idx)).owner = No_Process then
         return;
      end if;

      c := clientRect (store.Surfaces (SurfaceIndex (idx)));
      if screenX >= c.x then
         localX := screenX - c.x;
      end if;
      if screenY >= c.y then
         localY := screenY - c.y;
      end if;

      --  The buttons, and in the high word the modifiers held now: a click's
      --  modifiers never depend on key events the app may have missed.
      enqueueInput (kind,
                    target,
                    packU32Pair (localX, localY),
                    (buttons and 16#FFFF_FFFF#) or
                      Shift_Left (modifierState and 16#FFFF_FFFF#, 32));
      if kind = INPUT_POINTER_DOWN or else kind = INPUT_POINTER_UP then
         --  A press or release is the user's own gesture: it may authorize
         --  a copy from a menu or button, never a paste (docs/clipboard.md).
         Desktop_Clipboard.Note_Activation (target, nowMs);
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
         store.Surfaces (SurfaceIndex (idx)).owner /= No_Process and then
         pointInRect (screenX, screenY,
                      clientRect (store.Surfaces (SurfaceIndex (idx))))
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
      if idx < 0 or else store.Surfaces (SurfaceIndex (idx)).owner = No_Process then
         return;
      end if;

      c := clientRect (store.Surfaces (SurfaceIndex (idx)));
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
      r := registerDriver (DRIVER_KEYBOARD, Startup.Keyboard);
      if r = Unsigned_64'Last then
         debugPrint ("desktop: register keyboard failed" & LF);
      else
         debugPrint ("desktop: registered keyboard" & LF);
      end if;

      r := registerDriver (DRIVER_MOUSE, Startup.Mouse);
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
            --  The client allocates its buffer from its own memory quota.
            Natural'Last);
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
      for S of store.Surfaces loop
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
      if Desktop_Clipboard.Handles (request.tag.label) then
         declare
            focusIdx : constant Integer := findSurface (focusSurface);
         begin
            statsRequests := statsRequests + 1;
            replyMsg := Desktop_Clipboard.Handle
              (from, request, focusSurface,
               (if focusIdx >= 0 then store.Surfaces (SurfaceIndex (focusIdx)).owner
                else No_Process),
               nowMs);
            ignore := reply (from, replyMsg);
            if clipboardMenuOpen then
               reloadClipboardMenu;
               scheduleRedrawRect (inflateRect (clipboardMenuRect, 4));
            end if;
         end;
         return;
      end if;
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
               elsif from = No_Process or else store.Surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  ensureInputChannel (Decoded.Value.Surface, Channel_Slot);
                  if Channel_Slot < 0 then Result := (Status => DP.Resources_Exhausted);
                  elsif store.Channels (SurfaceIndex (Channel_Slot)).waiter.active then
                     Result := (Status => DP.Bad_State);
                  else
                     declare
                        Channel : SurfaceInputChannel renames store.Channels (SurfaceIndex (Channel_Slot));
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
                             (inputTransfers.all, To_Word (from), Decoded.Value.Surface,
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
               elsif store.Surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  declare
                     S : Surface renames store.Surfaces (SurfaceIndex (Index));
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
               elsif store.Surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  declare
                     S : Surface renames store.Surfaces (SurfaceIndex (Index));
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
               elsif store.Surfaces (SurfaceIndex (Index)).owner /= from then
                  Result := (Status => DP.Denied);
               else
                  Result := currentPublicationConfiguration
                    (store.Surfaces (SurfaceIndex (Index)));
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
         if request.tag.length = 1 and then request.tag.protocol = 0 and then
           request.tag.reserved = 0 and then request.words (0) <= 3 and then
           request.words (1) = 0 and then request.words (2) = 0 and then request.words (3) = 0
         then
            declare
               Data : constant CuBit.UI.Theme_Data.Color_Chunk :=
                 CuBit.UI.Theme_Data.Chunk (CuBit.UI.Theme_Data.Colors (CuBit.UI.Current_Theme),
                   CuBit.UI.Theme_Data.Chunk_Index (request.words (0)));
            begin
               replyMsg.tag := (label => DP.Code (DP.Get_Appearance), length => 4,
                                protocol => 0, ipc => <>, reserved => 0);
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
            replyMsg.tag := (label => OP_SURFACE_PRESENT, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
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
               (if idx >= 0 then To_Word (store.Surfaces (SurfaceIndex (idx)).owner) else 0),
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
                                        DP.Surface_Capacity'Last)));

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

               if displayReady then
                  freeSurfaceSlot (slot);
               end if;

               if not displayReady then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result ((Status => DP.Bad_State)));
                  noteWindowRefused (from, APP_CLIENT, Display_Not_Ready);
               elsif slot < 0 or else nextSurfaceId = Unsigned_64'Last then
                  -- Never wrap the name allocator and alias a live object.
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result ((Status => DP.Resources_Exhausted)));
                  noteWindowRefused
                    (from, APP_CLIENT,
                     (if slot >= 0 then Names_Exhausted
                      elsif store.Last + 1 >= MAX_SURFACES then Table_Full else Out_Of_Memory));
               else
                  if (request.words (2) and SURFACE_FLAG_WINDOW) /= 0 then
                     --  The request is a preference (Desktop policy sizes
                     --  it) or the application's declared preferred size.
                     cascadePlace (Natural (slot), creation.Value.Sizing, reqW, reqH, surfX, surfY);
                  else
                     if reqW = 0 or else reqW > primaryBounds.w then
                        reqW := primaryBounds.w;
                     end if;
                     if reqH = 0 or else reqH > primaryBounds.h then
                        reqH := primaryBounds.h;
                     end if;
                  end if;

                  surfX := surfX + primaryBounds.x;
                  surfY := surfY + primaryBounds.y;
                  store.Surfaces (SurfaceIndex (slot)) :=
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
         publicationInputAfter => 0,
         others => <>);
                  assignApp (store.Surfaces (SurfaceIndex (slot)), from);
                  --  A reused slot can be below other windows: the new
                  --  window opens on top, focused.
                  raiseSurface (SurfaceIndex (slot));
                  focusSurface := nextSurfaceId;
                  if windowRefusedOwner = from then
                     windowRefusedOwner := No_Process;
                  end if;
                  --  Window geometry, for tests that drive the pointer
                  --  onto a title bar (desktop-vm drag check).
                  debugPrint ("desktop: surface" & Natural'Image (Natural (nextSurfaceId mod 2 ** 31)) &
                    " at" & Natural'Image (surfX) & "," & Natural'Image (surfY) &
                    " size" & Natural'Image (reqW) & " x" & Natural'Image (reqH) & LF);

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Creation_Result
                       ((DP.Success, DP.Live_Surface_Name (nextSurfaceId),
                         DP.Positive_Extent (reqW), DP.Positive_Extent (reqH), 1)));

                  queueConfigure (nextSurfaceId,
                                  Unsigned_64 (reqW),
                                  Unsigned_64 (reqH));
                  nextSurfaceId := nextSurfaceId + 1;
                  --  A launched app with a window has started.
                  Desktop_Launch_Notice.Window_Opened (launchWatch, from);
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
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Resize_Result ((Status => DP.Denied)));
               else
                  clampSurfaceSize (store.Surfaces (SurfaceIndex (idx)), newW, newH);
                  if newW = 0 or else newW > fbWidth then
                     newW := fbWidth;
                  end if;
                  if newH = 0 or else newH > fbHeight then
                     newH := fbHeight;
                  end if;
                  if store.Surfaces (SurfaceIndex (idx)).x + newW > fbWidth then
                     newW := fbWidth - store.Surfaces (SurfaceIndex (idx)).x;
                  end if;
                  if store.Surfaces (SurfaceIndex (idx)).y + newH > fbHeight then
                     newH := fbHeight - store.Surfaces (SurfaceIndex (idx)).y;
                  end if;

                  oldBounds := surfaceRect (store.Surfaces (SurfaceIndex (idx)));
                  store.Surfaces (SurfaceIndex (idx)).w := newW;
                  store.Surfaces (SurfaceIndex (idx)).h := newH;
                  store.Surfaces (SurfaceIndex (idx)).serial :=
                     store.Surfaces (SurfaceIndex (idx)).serial + 1;
                  store.Surfaces (SurfaceIndex (idx)).dirty := True;
                  newBounds := surfaceRect (store.Surfaces (SurfaceIndex (idx)));

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Resize_Result
                       ((DP.Success, DP.Pixel_Extent (newW),
                         DP.Pixel_Extent (newH),
                         store.Surfaces (SurfaceIndex (idx)).serial)));

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
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  store.Surfaces (SurfaceIndex (idx)).pointerCursor :=
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
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
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

                  oldBounds := surfaceRect (store.Surfaces (SurfaceIndex (idx)));
                  store.Surfaces (SurfaceIndex (idx)).minW := minW;
                  store.Surfaces (SurfaceIndex (idx)).minH := minH;
                  store.Surfaces (SurfaceIndex (idx)).maxW := maxW;
                  store.Surfaces (SurfaceIndex (idx)).maxH := maxH;
                  store.Surfaces (SurfaceIndex (idx)).windowFlags := winFlags;
                  nextW := store.Surfaces (SurfaceIndex (idx)).w;
                  nextH := store.Surfaces (SurfaceIndex (idx)).h;
                  clampSurfaceSize (store.Surfaces (SurfaceIndex (idx)),
                                    nextW, nextH);
                  store.Surfaces (SurfaceIndex (idx)).w := nextW;
                  store.Surfaces (SurfaceIndex (idx)).h := nextH;
                  store.Surfaces (SurfaceIndex (idx)).serial :=
                     store.Surfaces (SurfaceIndex (idx)).serial + 1;
                  store.Surfaces (SurfaceIndex (idx)).dirty := True;
                  newBounds := surfaceRect (store.Surfaces (SurfaceIndex (idx)));

                  queueConfigure (store.Surfaces (SurfaceIndex (idx)).id,
                                  Unsigned_64 (newBounds.w),
                                  Unsigned_64 (newBounds.h));

                  replyMsg := CuBit.Desktop_Messages.From_Wire
                    (DP.Encode_Limits_Result
                       ((DP.Success,
                         (DP.Pixel_Extent (minW), DP.Pixel_Extent (minH),
                          DP.Pixel_Extent (maxW), DP.Pixel_Extent (maxH)),
                         store.Surfaces (SurfaceIndex (idx)).serial)));

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
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  store.Surfaces (SurfaceIndex (idx)).title := titleRequest.Value.Title;
                  store.Surfaces (SurfaceIndex (idx)).labelsMeasured := False;
                  store.Surfaces (SurfaceIndex (idx)).dirty := True;
                  result := DP.Success;
                  scheduleRedrawRect
                    (inflateRect
                       (surfaceRect (store.Surfaces (SurfaceIndex (idx))), 2));
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
               replyMsg.tag := (label => OP_SURFACE_ATTACH_BUFFER, length => 1,
                                protocol => 0, ipc => <>, reserved => 0);
               if not decoded.Valid then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (DP.Invalid_Request);
               elsif idx < 0 then
                  replyMsg.words (0) := UI_ERR_BAD_OBJECT;
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
                  replyMsg.words (0) := UI_ERR_DENIED;
               elsif store.Surfaces (SurfaceIndex (idx)).publicationMode then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               elsif store.Surfaces (SurfaceIndex (idx)).serial = Unsigned_64'Last then
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
                     releaseSurfaceBuffer (store.Surfaces (SurfaceIndex (idx)));
                     store.Surfaces (SurfaceIndex (idx)).bufferLoan := Loan;
                     store.Surfaces (SurfaceIndex (idx)).bufferGrant := decoded.Value.Grant;
                     store.Surfaces (SurfaceIndex (idx)).bufferAddr := mapped;
                     store.Surfaces (SurfaceIndex (idx)).bufferW :=
                       Natural (decoded.Value.Layout.Width);
                     store.Surfaces (SurfaceIndex (idx)).bufferH :=
                       Natural (decoded.Value.Layout.Height);
                     store.Surfaces (SurfaceIndex (idx)).bufferPitch := decoded.Value.Layout.Pitch;
                     store.Surfaces (SurfaceIndex (idx)).bufferFormat := PIXEL_FORMAT_BGRA8888;
                     store.Surfaces (SurfaceIndex (idx)).bufferAttached := True;
                     store.Surfaces (SurfaceIndex (idx)).dirty := True;
                     store.Surfaces (SurfaceIndex (idx)).serial :=
                       store.Surfaces (SurfaceIndex (idx)).serial + 1;
                     noteContentChange (store.Surfaces (SurfaceIndex (idx)),
                       bufferRows (store.Surfaces (SurfaceIndex (idx)).bufferH));
                     replyMsg.words (0) := UI_OK;
                     scheduleRedrawRect
                       (inflateRect
                          (surfaceRect (store.Surfaces (SurfaceIndex (idx))), 4));
                  end if;
               end if;
            end;

         when OP_SURFACE_PRESENT =>
            replyMsg.tag := (label  => OP_SURFACE_PRESENT,
                             length => 1,
                             protocol  => 0, ipc => <>,
                             reserved  => 0);
            replyMsg.words (0) := UI_OK;
            declare
               idx : constant Integer := findSurface (request.words (0));
               accessStatus : constant DP.Status_Code := DP.Surface_Access
                 (idx >= 0,
                  (if idx >= 0 then To_Word (store.Surfaces (SurfaceIndex (idx)).owner) else 0),
                  To_Word (from));
            begin
               if accessStatus /= DP.Success then
                  replyMsg.words (0) := DP.Status_Code'Enum_Rep (accessStatus);
               elsif store.Surfaces (SurfaceIndex (idx)).publicationMode then
                  replyMsg.words (0) := UI_ERR_BAD_STATE;
               elsif
                  (store.Surfaces (SurfaceIndex (idx)).flags and
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
                        clientRect (store.Surfaces (SurfaceIndex (idx)));
                     damage : constant DP.Rectangle :=
                       DP.Clip (present.Value.Area, client.w, client.h);
                     bufferH : constant Natural := store.Surfaces (SurfaceIndex (idx)).bufferH;
                  begin
                     -- Client-local damage rows are buffer rows only at 1:1.
                     noteContentChange (store.Surfaces (SurfaceIndex (idx)),
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
                  noteContentChange (store.Surfaces (SurfaceIndex (idx)),
                    bufferRows (store.Surfaces (SurfaceIndex (idx)).bufferH));
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
               elsif store.Surfaces (SurfaceIndex (idx)).owner /= from then
                  result := DP.Denied;
               else
                  if pointerSurfaceId = target then
                     pointerSurfaceId := 0;
                  end if;
                  if dragSurfaceId = target then
                     endDrag;
                  end if;
                  clearInputForTarget (target);
                  releaseSurfaceBuffer (store.Surfaces (SurfaceIndex (idx)));
                  retireSurfaceSource (store.Surfaces (SurfaceIndex (idx)));
                  store.Surfaces (SurfaceIndex (idx)) := (others => <>);
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
                             protocol  => 0, ipc => <>,
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
                     replyMsg.tag.protocol := INPUT_REPLY_MORE_PENDING;
                  end if;
                  replyMsg.words (0) := event.kind;
                  replyMsg.words (1) := event.serial;
                  replyMsg.words (2) := event.payload0;
                  replyMsg.words (3) := event.payload1;
               elsif request.tag.label = OP_INPUT_WAIT and then
                 (deadline = 0 or else deadline > nowMs) and then
                 channelSlot >= 0 and then
                 not store.Channels
                   (SurfaceIndex (channelSlot)).waiter.active
               then
                  declare
                     saved : constant CuBit.Handles.IPC.Saved_Reply :=
                       CuBit.Handles.IPC.Save_Reply;
                  begin
                     if saved.Saved then
                        store.Channels (SurfaceIndex (channelSlot)).waiter :=
                          (active      => True,
                           owner       => from,
                           target      => target,
                           afterSerial => afterSerial,
                           deadline    => deadline,
                           saved       => saved.Handle);
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
            for i in store.Surfaces'Range loop
               if store.Surfaces (i).used and then store.Surfaces (i).owner = from then
                  if pointerSurfaceId = store.Surfaces (i).id then
                     pointerSurfaceId := 0;
                  end if;
                  if dragSurfaceId = store.Surfaces (i).id then
                     endDrag;
                  end if;
                  clearInputForTarget (store.Surfaces (i).id);
                  releaseSurfaceBuffer (store.Surfaces (i));
                  retireSurfaceSource (store.Surfaces (i));
                  store.Surfaces (i) := (others => <>);
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
                             protocol  => 0, ipc => <>,
                             reserved  => 0);
            replyMsg.words (0) := UI_ERR_UNSUPPORTED;
      end case;

      if replyNow then
         ignore := reply (from, replyMsg);
      end if;
   end handleRequest;

   procedure closeClipboardMenu (damage : in out Rect) is
   begin
      if clipboardMenuOpen then
         clipboardMenuOpen := False;
         damage := unionRect (damage, inflateRect (clipboardMenuRect, 4));
      end if;
   end closeClipboardMenu;

   procedure toggleClipboardMenu (damage : in out Rect) is
      ignore : Boolean;
   begin
      if clipboardMenuOpen then
         closeClipboardMenu (damage);
         return;
      end if;
      if launchMenuOpen then
         launchMenuOpen := False;
         damage := unionRect (damage, inflateRect (unionRect (launchButtonRect, launchMenuArea), 4));
      end if;
      closeDeskbarPanel (damage);
      closeGroupList (damage);
      ignore := Desktop_Clipboard.Expire (nowMs);
      clipboardMenu := (others => <>);
      reloadClipboardMenu;
      clipboardMenuOpen := True;
      damage := unionRect (damage, inflateRect (clipboardMenuRect, 4));
      debugPrint ("desktop: clipboard manager open entries=" &
                  clipboardMenu.Count'Image & LF);
   end toggleClipboardMenu;

   --  Paste the chosen entry into the focused app exactly as its own Ctrl+V
   --  would: the entry becomes current, the surface gets a paste ticket and
   --  the chord is delivered to it.
   procedure clipboardPasteChosen (slot : Clipboard_Store.Slot_Index) is
      KEY_V : constant Unsigned_64 := 16#2F#;
      done : Boolean;
   begin
      Desktop_Clipboard.Paste_From_Manager (slot, focusSurface, nowMs, done);
      if done and then focusSurface /= 0 then
         enqueueInput (INPUT_KEY_DOWN, focusSurface, KEY_V, KEYMOD_CTRL);
         enqueueInput (INPUT_KEY_UP, focusSurface, KEY_V, KEYMOD_CTRL);
         debugPrint ("desktop: clipboard manager paste" & LF);
      end if;
   end clipboardPasteChosen;

   procedure handleClipboardMenuKey (code : Unsigned_8; damage : in out Rect) is
      package CM renames Clipboard_Menu;
      KEY_P      : constant Unsigned_8 := 16#19#;
      KEY_HOME   : constant Unsigned_8 := 16#47#;
      KEY_END    : constant Unsigned_8 := 16#4F#;
      KEY_DELETE : constant Unsigned_8 := 16#53#;
      KEY_1      : constant Unsigned_8 := 16#02#;
      KEY_9      : constant Unsigned_8 := 16#0A#;
      result : CM.Action := CM.Nothing;
      slot : Clipboard_Store.Slot_Count := 0;
      digit : CM.Digit_Value := 1;
      use type CM.Action;
   begin
      if code = KEY_UP then
         CM.Press (clipboardMenu, CM.Up, digit, result, slot);
      elsif code = KEY_DOWN then
         CM.Press (clipboardMenu, CM.Down, digit, result, slot);
      elsif code = KEY_HOME then
         CM.Press (clipboardMenu, CM.Home, digit, result, slot);
      elsif code = KEY_END then
         CM.Press (clipboardMenu, CM.Last, digit, result, slot);
      elsif code = KEY_ENTER then
         CM.Press (clipboardMenu, CM.Enter, digit, result, slot);
      elsif code = KEY_P then
         CM.Press (clipboardMenu, CM.Pin, digit, result, slot);
      elsif code = KEY_DELETE then
         CM.Press (clipboardMenu,
                   (if desktopShiftDown then CM.Clear else CM.Delete),
                   digit, result, slot);
      elsif code = KEY_ESCAPE then
         CM.Press (clipboardMenu, CM.Escape, digit, result, slot);
      elsif code in KEY_1 .. KEY_9 then
         digit := CM.Digit_Value (code - KEY_1 + 1);
         CM.Press (clipboardMenu, CM.Number, digit, result, slot);
      end if;
      damage := unionRect (damage, inflateRect (clipboardMenuRect, 4));
      case result is
         when CM.Nothing => null;
         when CM.Close => closeClipboardMenu (damage);
         when CM.Paste =>
            closeClipboardMenu (damage);
            clipboardPasteChosen (slot);
         when CM.Toggle_Pin =>
            Desktop_Clipboard.Toggle_Pin (slot);
            reloadClipboardMenu;
         when CM.Remove =>
            Desktop_Clipboard.Delete (slot);
            reloadClipboardMenu;
         when CM.Remove_All =>
            Desktop_Clipboard.Clear;
            reloadClipboardMenu;
      end case;
   end handleClipboardMenuKey;

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
        (slot      => Desktop_Endpoints.Process_Manager,
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

   procedure trySpawnApplication (name : String; label : String) is
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
                  protocol  => 0, ipc => <>,
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
      launchLabelLength := Natural'Min (label'Length, Desktop_Launch_Notice.Maximum_Name);
      launchLabel (1 .. launchLabelLength) :=
        label (label'First .. label'First + launchLabelLength - 1);
      launchInputAnnounced := False;
      if capSubmit (Desktop_Endpoints.Process_Manager, msg, New_Token) then
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
      for S of store.Surfaces loop
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
      for S of store.Surfaces loop
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
      deskbarPanelOpen := False;
      groupListOpen := False;
      clipboardMenuOpen := False;
      settingsView.Applied_Layout := Candidate;
      settingsView.Applied_Primary := Choice.Display;
      settingsView.Dragging := False;
      settingsView.Layout_Status := Desktop_Settings.Session_Only;
      --  desktop.display.scale: Auto, or the user's choice (the primary
      --  output's scale), for the next session too.
      declare
         use type CuBit.Config.ConfigStatus;
         Wanted : constant Desktop_Display_Scale_Settings.Preference :=
           (if settingsView.Pending_Auto then Desktop_Display_Scale_Settings.Default
            else (Auto => False, Scale => Candidate.Items (Choice.Index).Geometry.Scale));
         Text : constant String := Desktop_Display_Scale_Settings.Encode (Wanted);
         Status : CuBit.Config.ConfigStatus;
      begin
         CuBit.Config.set (Desktop_Display_Scale_Settings.Config_Key, Text'Address, Text'Length, Status);
         displayScale := Wanted;
         settingsView.Applied_Auto := settingsView.Pending_Auto;
         if Status = CuBit.Config.OK then
            settingsView.Layout_Status := Desktop_Settings.Saved_In_Config;
         end if;
         debugPrint ("desktop: display scale " & Text & LF);
      end;
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
      for S of store.Surfaces loop
         if S.used and then S.owner /= No_Process then
            queueConfigure (S.id, Unsigned_64 (S.w), Unsigned_64 (S.h));
         end if;
      end loop;
      debugPrint ("desktop: appearance applied" & LF);
   end Apply_Appearance;

   --  Settings > Date & Time: clock.time-zone, which the clock service reads
   --  for every snapshot (the Deskbar clock follows on its next update).
   procedure Apply_Time_Zone is
      Saved : Boolean;
   begin
      Desktop_Time_Zone_Setting.Store (settingsView.Pending_Zone, Saved);
      if Saved then
         settingsView.Applied_Zone := settingsView.Pending_Zone;
      end if;
      settingsView.Zone_Status := (if Saved then Desktop_Settings.Saved_In_Config else Desktop_Settings.Rejected);
      debugPrint ("desktop: time zone " & Time_Zones.Name (settingsView.Pending_Zone) &
                  (if Saved then " saved in Config" else " refused by Config") & LF);
   end Apply_Time_Zone;

   --  Apply the Settings page on show (Index: the Settings window).
   procedure Apply_Settings_Page (Index : SurfaceIndex; damage : in out Rect) is
   begin
      case settingsView.Current_Page is
         when Desktop_Settings.Appearance => Apply_Appearance (damage);
         when Desktop_Settings.Displays => Apply_Arrangement (damage);
         when Desktop_Settings.Date_Time =>
            Apply_Time_Zone;
            damage := unionRect (damage, surfaceRect (store.Surfaces (Index)));
         when Desktop_Settings.Monitor =>
            applyMonitorChoice;
            damage := unionRect (damage, surfaceRect (store.Surfaces (Index)));
      end case;
   end Apply_Settings_Page;

   procedure Settings_Pointer (Index : SurfaceIndex; Down, Pressed, Released : Boolean;
                               damage : in out Rect) is
      Bounds : constant Rect := clientRect (store.Surfaces (Index));
      Before : constant Desktop_Settings.State := settingsView;
      Apply : Boolean;
      use type Desktop_Settings.State;
   begin
      Desktop_Settings.Pointer (settingsView, (Bounds.x, Bounds.y, Bounds.w, Bounds.h),
        (cursorX, cursorY, Down, Pressed, Released, True), Apply);
      if Apply then
         Apply_Settings_Page (Index, damage);
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

      if store.Surfaces (SurfaceIndex (idx)).owner /= No_Process then
         return False;
      end if;

      case store.Surfaces (SurfaceIndex (idx)).appKind is
         when APP_SETTINGS =>
            if raw < 128 then
               declare Apply : Boolean; begin
                  Desktop_Settings.Key (settingsView, Natural (raw), desktopShiftDown, Apply);
                  if Apply then
                     Apply_Settings_Page (SurfaceIndex (idx), damage);
                  else damage := unionRect (damage, surfaceRect (store.Surfaces (SurfaceIndex (idx)))); end if;
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
                  trySpawnApplication (name, Desktop_Launch.Label_Of (item));
               end if;
         end case;
      end;
   end performLaunchAction;

   --  A pointer source either moved by a displacement (PS/2 and USB mice,
   --  positive DY up) or reports where it is in desktop coordinates
   --  (tablets: absoluteTarget).
   type Pointer_Motion (Absolute : Boolean := False) is record
      case Absolute is
         when False => DX, DY : Integer := 0;
         when True  => X, Y : Natural := 0;
      end case;
   end record;

   --  An absolute pointer is bound to output zero: QEMU's first console,
   --  virtio-gpu head 0. Its normalized position names a native pixel of
   --  that output (the exact inverse of QEMU's host-pixel scaling), which
   --  maps into desktop space with the output's scale and rotation.
   function absoluteTarget (R : Absolute_Pointer.Report) return Pointer_Motion is
   begin
      if desktopLayout.Count > 0 then
         declare
            G : DG.Output renames desktopLayout.Items (1).Geometry;
            Mapped : constant DG.Point_Mapping := DG.To_Desktop
              (G, (X => DG.Pixel_Index (Absolute_Pointer.To_Pixel (R.X, Natural (G.Width))),
                   Y => DG.Pixel_Index (Absolute_Pointer.To_Pixel (R.Y, Natural (G.Height)))));
         begin
            if Mapped.Valid then
               return (Absolute => True,
                       X => Natural (DG.Logical_Coordinate'Max (0, Mapped.Value.X)),
                       Y => Natural (DG.Logical_Coordinate'Max (0, Mapped.Value.Y)));
            end if;
         end;
      end if;
      --  Before any layout: the whole framebuffer.
      return (Absolute => True,
              X => (if fbWidth > 0 then Absolute_Pointer.To_Pixel (R.X, Natural'Min (fbWidth, Absolute_Pointer.Maximum_Extent)) else 0),
              Y => (if fbHeight > 0 then Absolute_Pointer.To_Pixel (R.Y, Natural'Min (fbHeight, Absolute_Pointer.Maximum_Extent)) else 0));
   end absoluteTarget;

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
      --  The title bar stays in its work area, above the Deskbar, where it
      --  can be grabbed (as Apply_Arrangement keeps it when outputs change).
      declare
         work : constant Rect := windowWorkArea (ret);
      begin
         if work.h >= CLIENT_INSET_TOP and then ret.y + CLIENT_INSET_TOP > work.y + work.h then
            ret.y := work.y + work.h - CLIENT_INSET_TOP;
         end if;
      end;

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
      if fresh and then (not clockQualityLogged or else stamp.Quality /= clockQuality) then
         clockQuality := stamp.Quality;
         clockQualityLogged := True;
         debugPrint ("desktop: clock quality " & CuBit.Clocks.Name (stamp.Quality) &
                     (if CuBit.Clocks.Is_Valid_Wall_Time (stamp.Quality) then ""
                      else " (Deskbar shows no time; see the ""clock:"" boot lines)") & LF);
      end if;
      if fresh then
         if CuBit.Clocks.Is_Valid_Wall_Time (stamp.Quality) then
            nextText := [digit (stamp.Hour / 10), digit (stamp.Hour mod 10), ':',
                         digit (stamp.Minute / 10), digit (stamp.Minute mod 10)];
            if not clockDateKnown or else clockDate.Day /= stamp.Day then
               if deskbarPanelOpen then scheduleRedrawRect (panelRect); end if;
            end if;
            clockDate := stamp;
            clockDateKnown := True;
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
            if deskbarPanelOpen then scheduleRedrawRect (panelRect); end if;
         end if;
      end if;
      Desktop_Status_Refresh.Take_Audio (audio, fresh);
      if fresh and then audio /= masterAudio then
         declare
            oldStatus : constant Rect := statusRect;
         begin
            masterAudio := audio;
            --  The volume item appears or goes with the mixer.
            scheduleRedrawRect (unionRect (oldStatus, statusRect));
         end;
         if deskbarPanelOpen then scheduleRedrawRect (panelVolumeRowRect); end if;
      end if;
      declare
         ended : Boolean;
      begin
         DN.Expire_Toast (notifications, now, ended);
         if ended then scheduleRedrawRect (toastRect); end if;
      end;
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

   --  The volume slider's level at pointer x.
   function sliderLevel (x : Natural) return CuBit.Audio_Control.Percent is
      track : constant Rect := volumeTrackRect;
   begin
      if track.w = 0 or else track.x > DBL.Max_Coordinate or else track.w > DBL.Max_Coordinate then
         return masterAudio.Level;
      end if;
      return DBL.Level_At (track.x, track.w, Natural'Min (x, DBL.Max_Coordinate));
   end sliderLevel;

   procedure stepVolume (up : Boolean) is
   begin
      setMasterAudio
        (Natural (Integer'Max (0, Integer'Min (100, Integer (masterAudio.Level) +
           (if up then Volume_Step else -Volume_Step)))), masterAudio.Muted);
   end stepVolume;

   --  Raise one window (restoring it if minimized); a press on the active
   --  window's own button minimizes it instead.
   procedure activateWindow (slot : SurfaceIndex; minimizeIfActive : Boolean; damage : in out Rect) is
      id : constant Unsigned_64 := store.Surfaces (slot).id;
      idx : Integer;
   begin
      if minimizeIfActive and then id = focusSurface and then not store.Surfaces (slot).minimized then
         minimizeSurface (slot, damage);
         return;
      end if;
      if store.Surfaces (slot).minimized then
         restoreSurface (slot, damage);
      end if;
      idx := findSurface (id);
      if idx >= 0 then
         focusAndRaiseSurface (SurfaceIndex (idx), damage);
      end if;
   end activateWindow;

   --  A task button: one window toggles; a group opens its window list.
   procedure activateTask (slot : SurfaceIndex; damage : in out Rect) is
      W : constant TG.Windows := taskWindows;
      M : constant TG.Model := taskModel (W);
      G : constant TG.Group_Count := M.Of_Window (TG.Window_Slot (slot));
   begin
      if G = 0 then
         return;
      elsif M.Groups (G).Members > 1 then
         toggleGroupList (M.Groups (G).Key, damage);
      else
         closeGroupList (damage);
         activateWindow (slot, True, damage);
      end if;
   end activateTask;

   --  The window list's row R: raise that window and close the list.
   procedure activateListRow (R : TG.Window_Rank; damage : in out Rect) is
      found : Boolean;
      slot : TG.Window_Slot;
   begin
      TG.Member_At (taskWindows, groupListKey, R, found, slot);
      closeGroupList (damage);
      if found then
         activateWindow (SurfaceIndex (slot), False, damage);
      end if;
   end activateListRow;

   --  A press inside the open panel.
   procedure panelPress (x, y : Natural; damage : in out Rect) is
   begin
      if masterAudio.Available and then pointInRect (x, y, muteIconRect) then
         setMasterAudio (masterAudio.Level, not masterAudio.Muted);
      elsif masterAudio.Available and then pointInRect (x, y, volumeTrackRect) then
         deskbarSliderDragging := True;
         setMasterAudio (sliderLevel (x), masterAudio.Muted);
      elsif pointInRect (x, y, quietToggleRect) then
         DN.Set_Quiet (notifications, not DN.Quiet (notifications));
         damagePanel (damage);
      elsif DN.Length (notifications) > 0 and then pointInRect (x, y, panelClearRect) then
         DN.Clear (notifications);
         panelFocus := Volume_Row;
         damagePanel (damage);
      else
         for K in 1 .. panelNoteRows loop
            if pointInRect (x, y, panelNoteCloseRect (K)) then
               damagePanel (damage);
               DN.Dismiss (notifications, DN.Item (notifications, DN.Position (K)).Id);
               panelFocus := Volume_Row;
               exit;
            end if;
         end loop;
      end if;
   end panelPress;

   --  A press the Deskbar owns: the status button, the open panel or
   --  window list (a press outside closes it), or the toast (opens the
   --  panel).
   procedure deskbarPress (x, y : Natural; damage : in out Rect) is
   begin
      if pointInRect (x, y, statusRect) then
         toggleDeskbarPanel (damage);
      elsif deskbarPanelOpen then
         if pointInRect (x, y, panelRect) then
            panelPress (x, y, damage);
         else
            closeDeskbarPanel (damage);
         end if;
      elsif groupListOpen then
         if hitGroupListRow (x, y) /= 0 then
            activateListRow (hitGroupListRow (x, y), damage);
         else
            closeGroupList (damage);
         end if;
      elsif toastShown and then pointInRect (x, y, toastRect) then
         DN.Hide_Toast (notifications);
         openDeskbarPanel (damage);
      end if;
   end deskbarPress;

   --  Keyboard focus moves over Apps, the task buttons and the status button.
   procedure moveTaskbarFocus (forward : Boolean; damage : in out Rect) is
      count : constant TG.Group_Count := taskModel (taskWindows).Count;
   begin
      case deskbarFocus is
         when No_Focus | Apps_Focus =>
            if forward then
               if count > 0 then
                  deskbarFocus := Task_Focus;
                  deskbarFocusTask := 1;
               else
                  deskbarFocus := Status_Focus;
               end if;
            else
               deskbarFocus := Status_Focus;
            end if;
         when Task_Focus =>
            if forward then
               if deskbarFocusTask < count then
                  deskbarFocusTask := deskbarFocusTask + 1;
               else
                  deskbarFocus := Status_Focus;
               end if;
            elsif deskbarFocusTask > 1 then
               deskbarFocusTask := deskbarFocusTask - 1;
            else
               deskbarFocus := Apps_Focus;
            end if;
         when Status_Focus =>
            if forward then
               deskbarFocus := Apps_Focus;
            elsif count > 0 then
               deskbarFocus := Task_Focus;
               deskbarFocusTask := count;
            else
               deskbarFocus := Apps_Focus;
            end if;
      end case;
      damage := unionRect (damage, taskbarRect);
   end moveTaskbarFocus;

   procedure enterTaskbarFocus (damage : in out Rect) is
   begin
      closeDeskbarPanel (damage);
      closeGroupList (damage);
      deskbarFocus := Apps_Focus;
      damage := unionRect (damage, taskbarRect);
   end enterTaskbarFocus;

   procedure leaveTaskbarFocus (damage : in out Rect) is
   begin
      if deskbarFocus /= No_Focus then
         deskbarFocus := No_Focus;
         damage := unionRect (damage, taskbarRect);
      end if;
   end leaveTaskbarFocus;

   --  The panel's rows for the keyboard: volume, do not disturb, each shown
   --  notification, then Clear all.
   procedure movePanelFocus (forward : Boolean) is
      notes : constant DN.Count := DN.Count (panelNoteRows);
   begin
      case panelFocus is
         when Volume_Row =>
            if forward then panelFocus := Quiet_Row; end if;
         when Quiet_Row =>
            if forward and then notes > 0 then
               panelFocus := Note_Row;
               panelFocusNote := 1;
            elsif not forward then
               panelFocus := Volume_Row;
            end if;
         when Note_Row =>
            if forward then
               if panelFocusNote < notes then
                  panelFocusNote := panelFocusNote + 1;
               else
                  panelFocus := Clear_Row;
               end if;
            elsif panelFocusNote > 1 then
               panelFocusNote := panelFocusNote - 1;
            else
               panelFocus := Quiet_Row;
            end if;
         when Clear_Row =>
            if not forward then
               panelFocus := (if notes > 0 then Note_Row else Quiet_Row);
               panelFocusNote := notes;
            end if;
      end case;
   end movePanelFocus;

   procedure panelKey (code : Unsigned_8; damage : in out Rect) is
   begin
      damagePanel (damage);
      if code = KEY_ESCAPE then
         closeDeskbarPanel (damage);
      elsif code = KEY_UP or else (code = KEY_TAB and then desktopShiftDown) then
         movePanelFocus (False);
      elsif code = KEY_DOWN or else code = KEY_TAB then
         movePanelFocus (True);
      elsif panelFocus = Volume_Row and then code in KEY_LEFT | KEY_RIGHT then
         stepVolume (code = KEY_RIGHT);
      elsif code in KEY_ENTER | KEY_SPACE then
         case panelFocus is
            when Volume_Row => setMasterAudio (masterAudio.Level, not masterAudio.Muted);
            when Quiet_Row => DN.Set_Quiet (notifications, not DN.Quiet (notifications));
            when Clear_Row =>
               DN.Clear (notifications);
               panelFocus := Volume_Row;
            when Note_Row => null;
         end case;
      elsif code = KEY_DELETE and then panelFocus = Note_Row and then
        panelFocusNote in 1 .. DN.Length (notifications)
      then
         DN.Dismiss (notifications, DN.Item (notifications, panelFocusNote).Id);
         if panelFocusNote > DN.Count (panelNoteRows) then
            panelFocusNote := DN.Count (panelNoteRows);
         end if;
         if panelFocusNote = 0 then
            panelFocus := Quiet_Row;
         end if;
      end if;
   end panelKey;

   procedure listKey (code : Unsigned_8; damage : in out Rect) is
      members : constant TG.Window_Count := groupListMembers;
   begin
      damage := unionRect (damage, groupListRect);
      if code = KEY_ESCAPE or else members = 0 then
         closeGroupList (damage);
      elsif code = KEY_UP and then groupListRow > 1 then
         groupListRow := groupListRow - 1;
      elsif code = KEY_DOWN and then groupListRow < members then
         groupListRow := groupListRow + 1;
      elsif code in KEY_ENTER | KEY_SPACE and then groupListRow in 1 .. members then
         activateListRow (groupListRow, damage);
      end if;
   end listKey;

   procedure taskbarKey (code : Unsigned_8; damage : in out Rect) is
   begin
      if code = KEY_ESCAPE then
         leaveTaskbarFocus (damage);
      elsif code = KEY_LEFT or else (code = KEY_TAB and then desktopShiftDown) then
         moveTaskbarFocus (False, damage);
      elsif code in KEY_RIGHT | KEY_TAB then
         moveTaskbarFocus (True, damage);
      elsif code in KEY_ENTER | KEY_SPACE | KEY_UP then
         case deskbarFocus is
            when Apps_Focus =>
               leaveTaskbarFocus (damage);
               launchMenuOpen := True;
               Desktop_Launch_Refresh.Request;
               LM.Reset (launchMenuState);
               damage := unionRect (damage, inflateRect (unionRect (launchButtonRect, launchMenuArea), 4));
            when Task_Focus =>
               declare
                  W : constant TG.Windows := taskWindows;
                  M : constant TG.Model := taskModel (W);
                  G : constant TG.Group_Count :=
                    (if deskbarFocusTask = 0 then 0 else TG.At_Position (W, M, deskbarFocusTask));
               begin
                  if G /= 0 then
                     activateTask (SurfaceIndex (M.Groups (G).First), damage);
                     if not groupListOpen then
                        leaveTaskbarFocus (damage);
                     end if;
                  end if;
               end;
            when Status_Focus =>
               openDeskbarPanel (damage);
            when No_Focus =>
               null;
         end case;
      end if;
   end taskbarKey;

   --  The Deskbar owns the keyboard while its panel or a window list is
   --  open, or the taskbar has keyboard focus.
   procedure deskbarKey (code : Unsigned_8; damage : in out Rect) is
   begin
      if deskbarPanelOpen then
         panelKey (code, damage);
      elsif groupListOpen then
         listKey (code, damage);
      else
         taskbarKey (code, damage);
      end if;
   end deskbarKey;

   ---------------------------------------------------------------------------
   --  Tooltips: window buttons and the status button's parts.

   --  The status button's part at (x, y): the unread badge, a shown item's
   --  icon (as drawStatusButton lays them out), else the button itself.
   function statusTargetAt (x, y : Natural) return DT.Target is
      r : constant Rect := statusRect;
      shown : constant DSI.Presence := statusShown;
      left : Natural := r.x + DBS.Status_Padding;
   begin
      if not pointInRect (x, y, r) then
         return DT.NO_TARGET;
      end if;
      if DN.Unread (notifications) > 0 then
         if x >= left and then x < left + DBS.Badge_Size then
            return (Kind => DT.Unread_Count);
         end if;
         left := left + DBS.Badge_Size + DBS.Status_Item_Gap;
      end if;
      for I in DSI.Item loop
         if shown (I) then
            if x >= left and then x < left + Desktop_Icons.ICON_SIZE then
               return (Kind => DT.Status_Icon, Item => I);
            end if;
            left := left + Desktop_Icons.ICON_SIZE + DBS.Status_Item_Gap;
         end if;
      end loop;
      return (Kind => DT.Status_Button);
   end statusTargetAt;

   --  What has a tip under (x, y): a window button of the topmost window
   --  there (a greyed maximize too), or a part of the status button.
   function tipTargetAt (x, y : Natural) return DT.Target is
      idx : Integer;
   begin
      if dragMode /= DRAG_NONE or else launchMenuOpen or else clipboardMenuOpen or else groupListOpen then
         return DT.NO_TARGET;
      end if;
      if shellSurfaceVisible and then not deskbarPanelOpen and then pointInRect (x, y, statusRect) then
         return statusTargetAt (x, y);
      end if;
      idx := hitSurface (x, y);
      if idx in 0 .. surfaceLast then
         declare
            s : Surface renames store.Surfaces (SurfaceIndex (idx));
         begin
            if pointInRect (x, y, minimizeButtonRect (s)) then
               return (Kind => DT.Button_Of_Window, Slot => idx, Button => DT.Minimize_Button);
            elsif pointInRect (x, y, maximizeButtonRect (s)) then
               return (Kind => DT.Button_Of_Window, Slot => idx, Button => DT.Maximize_Button);
            elsif pointInRect (x, y, closeButtonRect (s)) then
               return (Kind => DT.Button_Of_Window, Slot => idx, Button => DT.Close_Button);
            end if;
         end;
      end if;
      return DT.NO_TARGET;
   end tipTargetAt;

   function tipText (T : DT.Target) return String is
      unread : constant Natural := Natural (DN.Unread (notifications));
   begin
      case T.Kind is
         when DT.Nothing => return "";
         when DT.Button_Of_Window =>
            if T.Slot <= surfaceLast and then store.Surfaces (T.Slot).used and then not store.Surfaces (T.Slot).minimized then
               return DT.Button_Text (T.Button, store.Surfaces (T.Slot).maximized, maximizeAvailable (store.Surfaces (T.Slot)));
            end if;
            return "";
         when DT.Status_Icon => return DSI.Name (T.Item, masterAudio.Muted);
         when DT.Unread_Count =>
            return countText (unread) & (if unread = 1 then " unread notification" else " unread notifications");
         when DT.Status_Button => return "Notifications and status";
      end case;
   end tipText;

   --  The shortcut a tip shows: the status button opens with Super+N.
   function tipHint (T : DT.Target) return String is
     (if T.Kind = DT.Status_Button then "Super+N" else "");

   --  Where the showing tip goes: below the pointer (or the focused
   --  control), else above it, on screen (Client_Popup_Layout.Place_Tip).
   function tipLayout return Rect is
      package PL renames Client_Popup_Layout;
      T : constant DT.Target := DT.Decode (tipState.On);
      text : constant String := tipText (T);
      hint : constant String := tipHint (T);
      focused : constant Boolean := not isEmpty (tipControl);
   begin
      if not TTP.Showing (tipState) or else text'Length = 0 or else fbWidth = 0 or else fbHeight = 0 then
         return (others => 0);
      end if;
      declare
         M : constant := PL.MAXIMUM_COORDINATE;
         W : constant Natural :=
           2 * DT.PADDING + uiTextWidth (text) + (if hint'Length > 0 then DT.HINT_GAP + uiTextWidth (hint) else 0);
         H : constant Natural := 2 * DT.PADDING + CuBit.Fonts.Line_Height;
         leftX : constant Natural := (if focused then tipControl.x else tipState.X + DT.POINTER_GAP);
         belowY : constant Natural :=
           (if focused then tipControl.y + tipControl.h + DT.FOCUS_GAP else tipState.Y + DT.POINTER_GAP);
         aboveY : constant Natural :=
           (if focused then tipControl.y - Natural'Min (DT.FOCUS_GAP, tipControl.y) else tipState.Y);
         P : constant PL.Box :=
           PL.Place_Tip (Natural'Min (leftX, M), Natural'Min (belowY, M), Natural'Min (aboveY, M),
                         Natural'Min (W, M), Natural'Min (H, M),
                         (0, 0, Natural'Min (fbWidth, M), Natural'Min (fbHeight, M)));
      begin
         return (P.X, P.Y, P.W, P.H);
      end;
   end tipLayout;

   --  After the policy changed: the old box and the new one repaint.
   procedure relayoutTip (before : Rect) is
   begin
      tipBox := tipLayout;
      if not isEmpty (before) then
         scheduleRedrawRect (before);
      end if;
      if not isEmpty (tipBox) then
         scheduleRedrawRect (tipBox);
      end if;
   end relayoutTip;

   --  The pointer rests or moves at (x, y): the target's tip after the
   --  delay, at once when sliding from one shown tip to the next.
   procedure pointTip (x, y : Natural) is
      before : constant Rect := tipBox;
      wasShown : constant Boolean := TTP.Showing (tipState);
      wasOn : constant TTP.Target := tipState.On;
   begin
      tipControl := (others => 0);
      TTP.Pointer_At (tipState, DT.Encode (tipTargetAt (x, y)), x, y, nowMs);
      if wasShown and then (not TTP.Showing (tipState) or else tipState.On /= wasOn) then
         relayoutTip (before);
      end if;
   end pointTip;

   --  A press, release, wheel or key: hidden until another target.
   procedure dismissTip is
   begin
      if TTP.Showing (tipState) and then not isEmpty (tipBox) then
         scheduleRedrawRect (tipBox);
      end if;
      TTP.Dismiss (tipState);
      tipBox := (others => 0);
   end dismissTip;

   --  Keyboard users: the focused status button's tip, below or above it,
   --  after the same delay (call after the key's dismissTip).
   procedure focusTip is
   begin
      if deskbarFocus = Status_Focus and then shellSurfaceVisible and then not deskbarPanelOpen then
         tipControl := statusRect;
         TTP.Pointer_At (tipState, DT.Encode ((Kind => DT.Status_Button)), tipControl.x, tipControl.y, nowMs);
      end if;
   end focusTip;

   --  The delay ran out: the tip shows (the loop's wait includes the
   --  policy's deadline; nothing polls).
   procedure tickTip (now : Unsigned_64) is
      changed : Boolean;
   begin
      TTP.Tick (tipState, now, changed);
      if changed then
         relayoutTip ((others => 0));
      end if;
   end tickTip;

   --  Flat and square like the window chrome: the theme's panel, a 1-pixel
   --  edge, the text, the shortcut right-aligned and quiet.
   procedure drawTooltip is
      r : constant Rect := tipBox;
      colors : constant CuBit.UI.Theme := CuBit.UI.Current_Theme;
      T : constant DT.Target := DT.Decode (tipState.On);
   begin
      if not TTP.Showing (tipState) or else isEmpty (r) then
         return;
      end if;
      declare
         text : constant String := tipText (T);
         hint : constant String := tipHint (T);
      begin
         drawBox (r, colors.panel, colors.edge);
         drawUIText (r.x + DT.PADDING, r.y + DT.PADDING, text, colors.text, colors.panel);
         if hint'Length > 0 and then r.w > DT.PADDING + uiTextWidth (hint) then
            drawUIText (r.x + r.w - DT.PADDING - uiTextWidth (hint), r.y + DT.PADDING, hint, colors.muted, colors.panel);
         end if;
      end;
   end drawTooltip;

   procedure damageChromeButton
      (id : Unsigned_64; button : Pointer_Action; damage : in out Rect)
   is
      idx : constant Integer := (if id = 0 then -1 else findSurface (id));
   begin
      if idx < 0 then
         return;
      end if;
      declare
         S : Surface renames store.Surfaces (SurfaceIndex (idx));
      begin
         case button is
            when HIT_CLOSE => damage := unionRect (damage, closeButtonRect (S));
            when HIT_MAXIMIZE => damage := unionRect (damage, maximizeButtonRect (S));
            when HIT_MINIMIZE => damage := unionRect (damage, minimizeButtonRect (S));
            when others => null;
         end case;
      end;
   end damageChromeButton;

   --  Hover feedback for the topmost window's buttons; an unavailable
   --  maximize never hit-tests, so it never lights up.
   procedure updateChromeHover (damage : in out Rect) is
      idx : constant Integer :=
        (if dragMode = DRAG_NONE then hitSurface (cursorX, cursorY) else -1);
      nextId : Unsigned_64 := 0;
      nextButton : Pointer_Action := DRAG_NONE;
   begin
      if idx >= 0 then
         nextButton := hitMode (store.Surfaces (SurfaceIndex (idx)), cursorX, cursorY);
         if nextButton in HIT_MINIMIZE | HIT_CLOSE | HIT_MAXIMIZE then
            nextId := store.Surfaces (SurfaceIndex (idx)).id;
         else
            nextButton := DRAG_NONE;
         end if;
      end if;
      if nextId /= chromeHoverId or else nextButton /= chromeHoverButton then
         damageChromeButton (chromeHoverId, chromeHoverButton, damage);
         chromeHoverId := nextId;
         chromeHoverButton := nextButton;
         damageChromeButton (nextId, nextButton, damage);
      end if;
   end updateChromeHover;

   procedure handleMouseMotion
      (buttons : Unsigned_64;
       Motion  : Pointer_Motion;
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
      pointerMoved : constant Boolean :=
        (if Motion.Absolute then Motion.X /= cursorX or else Motion.Y /= cursorY
         else Motion.DX /= 0 or else Motion.DY /= 0);
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

      if Motion.Absolute then
         cursorX := clampPointerCoord (Motion.X, maxX);
         cursorY := clampPointerCoord (Motion.Y, maxY);
      else
         --  PS/2 reports positive Y as upward motion; screen coordinates
         --  grow downward.
         cursorX := clampPointerCoord (Integer (cursorX) + Motion.DX, maxX);
         cursorY := clampPointerCoord (Integer (cursorY) - Motion.DY, maxY);
      end if;
      if desktopLayout.Count > 0 then
         declare
            Visible : constant DL.Pointer_Position := DL.Confine
              (desktopLayout, (DG.Logical_Coordinate (cursorX), DG.Logical_Coordinate (cursorY)));
         begin
            cursorX := Natural (Visible.Point.X);
            cursorY := Natural (Visible.Point.Y);
         end;
      end if;

      --  Tooltips follow the resting pointer; a press, release or wheel
      --  hides them.
      if leftTransition or else dz /= 0 or else buttons /= lastButtons then
         dismissTip;
      elsif pointerMoved then
         pointTip (cursorX, cursorY);
      end if;

      --  The clipboard manager takes the pointer while open: a click on a
      --  row pastes it, a click elsewhere closes the manager.
      if clipboardMenuOpen and then pointerSurfaceId = 0 and then
        dragMode = DRAG_NONE
      then
         if leftDown and then not leftWasDown then
            declare
               r : constant Rect := clipboardMenuRect;
               hit : Boolean := False;
               use type Clipboard_Store.Slot_Count;
            begin
               if pointInRect (cursorX, cursorY, r) and then
                 cursorY >= r.y + CLIP_HEADER_H
               then
                  Clipboard_Menu.Select_Visible
                    (clipboardMenu, (cursorY - r.y - CLIP_HEADER_H) / CLIP_ROW_H, hit);
               end if;
               damage := unionRect (damage, inflateRect (r, 4));
               clipboardMenuOpen := False;
               if hit and then clipboardMenu.Selected /= 0 then
                  clipboardPasteChosen (clipboardMenu.Slots (clipboardMenu.Selected));
               end if;
               scheduleRedrawRect (damage);
            end;
         end if;
         lastButtons := buttons;
         cursorStyle := POINTER_DEFAULT;
         scheduleCursorPresent;
         return;
      end if;

      --  The Deskbar (UI-027) owns presses on its status button, its open
      --  panel or window list and the toast, and keeps each gesture through
      --  its release: never half a gesture to the focused application. A
      --  press outside an open panel or list closes it.
      if shellSurfaceVisible and then pointerSurfaceId = 0 and then
        dragMode = DRAG_NONE and then
        (deskbarPointerCapture or else
         (leftDown and then not leftWasDown and then
          (pointInRect (cursorX, cursorY, statusRect) or else deskbarPanelOpen or else
           groupListOpen or else (toastShown and then pointInRect (cursorX, cursorY, toastRect)))))
      then
         if leftDown and then not leftWasDown then
            deskbarPointerCapture := True;
            leaveTaskbarFocus (damage);
            deskbarPress (cursorX, cursorY, damage);
         elsif deskbarSliderDragging and then leftDown then
            setMasterAudio (sliderLevel (cursorX), masterAudio.Muted);
         end if;
         if not leftDown then
            deskbarSliderDragging := False;
            deskbarPointerCapture := False;
         end if;
         scheduleRedrawRect (damage);
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
         if idx >= 0 and then store.Surfaces (SurfaceIndex (idx)).appKind = APP_SETTINGS then
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
            declare
               fidx : constant Integer := findSurface (focusSurface);
               inside : constant Boolean :=
                 fidx >= 0 and then pointInRect (cursorX, cursorY, clientRect (store.Surfaces (SurfaceIndex (fidx))));
               oldIdx : constant Integer := (if hoverInsideId = 0 then -1 else findSurface (hoverInsideId));
            begin
               if oldIdx >= 0 and then (hoverInsideId /= focusSurface or else not inside) then
                  declare
                     c : constant Rect := clientRect (store.Surfaces (SurfaceIndex (oldIdx)));
                  begin
                     queuePointer (INPUT_POINTER_MOVE, hoverInsideId, c.x + c.w, c.y, buttons);
                  end;
               end if;
               hoverInsideId := (if inside then focusSurface else 0);
            end;
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
                 (store.Surfaces (SurfaceIndex (wheelIdx)).id,
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
         leaveTaskbarFocus (damage);
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
            activateTask (SurfaceIndex (taskIdx), damage);
            handledChromeClick := True;
         end if;

         idx := hitSurface (cursorX, cursorY);
         if not handledChromeClick and then idx >= 0 then
            clickedId := store.Surfaces (SurfaceIndex (idx)).id;
            dragMode := hitMode (store.Surfaces (SurfaceIndex (idx)), cursorX, cursorY);
            titlePress :=
              cursorY < store.Surfaces (SurfaceIndex (idx)).y + TITLE_HEIGHT and then
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
                           Boolean'Pos (store.Surfaces (SurfaceIndex (idx)).maximized),
                           0);
                     end if;
                     dragMode := DRAG_NONE;
                  elsif titlePress and then store.Surfaces (SurfaceIndex (idx)).maximized then
                     --  Maximized captions are still chrome, not client area.
                     --  Single presses do not start a move; a pair restores.
                     null;
                  elsif dragMode = DRAG_NONE then
                     pointerSurfaceId := clickedId;
                     if store.Surfaces (SurfaceIndex (idx)).appKind = APP_SETTINGS then
                        Settings_Pointer (SurfaceIndex (idx), True, True, False, damage);
                     end if;
                     queuePointerIfClient (INPUT_POINTER_DOWN,
                                           clickedId,
                                           cursorX,
                                           cursorY,
                                           buttons);
                  else
                     dragSurfaceId := clickedId;
                     dragOffsetX := cursorX - store.Surfaces (SurfaceIndex (idx)).x;
                     dragOffsetY := cursorY - store.Surfaces (SurfaceIndex (idx)).y;
                     dragPreviewRect := surfaceRect (store.Surfaces (SurfaceIndex (idx)));
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
               oldBounds := surfaceRect (store.Surfaces (SurfaceIndex (idx)));
               -- The release packet may carry the final pointer movement.
               -- Commit that position even when no held-button motion packet
               -- updated the preview before release.
               newBounds := previewRectFromPointer (store.Surfaces (SurfaceIndex (idx)));

               store.Surfaces (SurfaceIndex (idx)).x := newBounds.x;
               store.Surfaces (SurfaceIndex (idx)).y := newBounds.y;
               store.Surfaces (SurfaceIndex (idx)).w := newBounds.w;
               store.Surfaces (SurfaceIndex (idx)).h := newBounds.h;
               store.Surfaces (SurfaceIndex (idx)).serial :=
                  store.Surfaces (SurfaceIndex (idx)).serial + 1;
               if dragMode /= DRAG_MOVE and then
                 store.Surfaces (SurfaceIndex (idx)).owner /= No_Process
               then
                  queueConfigure (store.Surfaces (SurfaceIndex (idx)).id,
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
               dragPreviewRect := previewRectFromPointer (store.Surfaces (SurfaceIndex (idx)));
               if dragPreviewRect /= Previous then sceneDamage := True; end if;
            end;
            if dragMode = DRAG_MOVE then
               store.Surfaces (SurfaceIndex (idx)).x := dragPreviewRect.x;
               store.Surfaces (SurfaceIndex (idx)).y := dragPreviewRect.y;
            end if;
            --  The compositor frame compares the last drag geometry actually
            --  presented with this latest position. Intermediate mouse
            --  positions drained in the same pass require no repaint at all.
         end if;
      end if;

      updateChromeHover (damage);
      cursorStyle := cursorStyleAtPointer;
      lastButtons := buttons;
      --  The hardware pointer follows every motion, including drags, menus
      --  and hovers that also schedule a scene repaint (the repaint path
      --  never moves the plane).
      updatePointerPlane;
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
            inputSources (i).identity = report.sourceIdentity and then
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
          report.delivery /= CuBit.Input.ACCUMULABLE_DISPLACEMENT) or else
         (report.device = CuBit.Input.ABSOLUTE_POINTER and then
          report.delivery /= CuBit.Input.REPLACEABLE_STATE)
      then
         statsSourceRejects := statsSourceRejects + 1;
         if slot < 0 then DI.Reject (Input_Diagnostic, DI.Table_Full);
         else DI.Reject (Input_Diagnostic, DI.Delivery); end if;
         return;
      end if;
      if report.device = CuBit.Input.ABSOLUTE_POINTER and then
        not Absolute_Pointer.Valid_Payload (report.payload)
      then
         statsSourceRejects := statsSourceRejects + 1;
         DI.Reject (Input_Diagnostic, DI.Malformed);
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

         if not source.used then
            DI.New_Source (Input_Diagnostic);
            if report.device = CuBit.Input.ABSOLUTE_POINTER then
               debugPrint ("desktop: absolute pointer source on output 0" & LF);
            end if;
         end if;
         source.used := True;
         source.identity := report.sourceIdentity;
         source.device := report.device;
         source.generation := report.generation;
         source.sequence := report.sequence;
         if report.device in CuBit.Input.RELATIVE_POINTER | CuBit.Input.ABSOLUTE_POINTER then
            source.buttons := report.snapshot and 16#FF#;
         end if;
      end;

      seatButtons := 0;
      for i in inputSources'Range loop
         if inputSources (i).used and then
            inputSources (i).device in CuBit.Input.RELATIVE_POINTER | CuBit.Input.ABSOLUTE_POINTER
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

   --  Keyboard auto-repeat (Key_Repeat). A key goes to one destination,
   --  chosen from the state below before delivering it changes anything;
   --  its repeats go the same way or stop.
   function keyDestination (raw : Unsigned_8; extended : Boolean) return KR.Destination is
      code : constant Unsigned_8 := raw and 16#7F#;
   begin
      if extended and then code in KEY_MUTE | KEY_VOLUME_DOWN | KEY_VOLUME_UP then
         return KR.Volume_Control;
      elsif extended and then (code = KEY_LEFT_SUPER or else code = KEY_RIGHT_SUPER) then
         return KR.Super_Key;
      elsif desktopSuperDown then
         return KR.Super_Chord;
      elsif deskbarPanelOpen or else groupListOpen or else deskbarFocus /= No_Focus then
         return KR.Deskbar;
      elsif clipboardMenuOpen then
         return KR.Clipboard_Manager;
      elsif launchMenuOpen then
         return KR.Apps_Menu;
      elsif toastShown and then not extended and then code = KEY_ESCAPE then
         return KR.Notice_Toast;
      --  Once a shell/client surface has focus, keyboard events belong to
      --  that surface. The service-level Q/Esc escape remains available
      --  only before a client has connected.
      elsif shouldCycleFocusKey (raw) then
         return KR.Focus_Cycle;
      elsif focusSurface /= 0 then
         return KR.Focused_Surface;
      elsif shouldExitKey (raw) then
         return KR.Exit_Key;
      end if;
      return KR.Nowhere;
   end keyDestination;

   function keyRoute (where : KR.Destination) return KR.Route is
     ((Where => where,
       Surface => (if where = KR.Focused_Surface then focusSurface else 0)));

   --  Deliver one key transition (set-1 byte, E0 context) to where.
   --  repeat: an auto-repeat of a held key (presses only).
   procedure deliverKey
     (raw : Unsigned_8; extended : Boolean; where : KR.Destination;
      repeat : Boolean; running : in out Boolean)
   is
      release  : constant Boolean := (raw and 16#80#) /= 0;
      code     : constant Unsigned_8 := raw and 16#7F#;
      damage   : Rect := cursorRect;
   begin
      if not release then
         dismissTip;
      end if;
      case where is
         when KR.Volume_Control =>
            if not release then
               if code = KEY_MUTE then
                  setMasterAudio (masterAudio.Level, not masterAudio.Muted);
               else
                  stepVolume (code = KEY_VOLUME_UP);
               end if;
            end if;
         when KR.Deskbar =>
            --  Both edges, so the focused app never sees half of a key.
            if not release then
               deskbarKey (code, damage);
               scheduleRedrawRect (inflateRect (damage, 2));
            end if;
         when KR.Super_Key =>
            --  Super is a modifier. Its press arms the Apps menu; the
            --  release opens it unless Super was part of a chord. A held
            --  Super's device repeats never get here (Key_Repeat).
            if not release then
               desktopSuperChord := False;
               desktopSuperDown := True;
            elsif desktopSuperDown and then not desktopSuperChord and then
              shellSurfaceVisible
            then
               desktopSuperDown := False;
               closeClipboardMenu (damage);
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
            else
               desktopSuperDown := False;
               scheduleRedrawRect (inflateRect (damage, 2));
            end if;
         when KR.Super_Chord =>
            --  A Super chord belongs to the desktop, press and release:
            --  the focused app never sees half of it.
            if not release then
               desktopSuperChord := True;
               if code = KEY_CLIPBOARD_V and then shellSurfaceVisible then
                  toggleClipboardMenu (damage);
                  scheduleRedrawRect (inflateRect (damage, 2));
               elsif code = KEY_DESKBAR_N and then shellSurfaceVisible then
                  closeClipboardMenu (damage);
                  leaveTaskbarFocus (damage);
                  toggleDeskbarPanel (damage);
                  scheduleRedrawRect (inflateRect (damage, 2));
               elsif code = KEY_TASKBAR_T and then shellSurfaceVisible then
                  closeClipboardMenu (damage);
                  enterTaskbarFocus (damage);
                  scheduleRedrawRect (inflateRect (damage, 2));
               end if;
            end if;
         when KR.Clipboard_Manager =>
            --  The manager owns the keyboard while open (both edges).
            if not release then
               handleClipboardMenuKey (code, damage);
               scheduleRedrawRect (inflateRect (damage, 2));
            end if;
         when KR.Apps_Menu =>
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
         when KR.Notice_Toast =>
            --  Escape hides the toast; the notice stays in the history.
            if not release then
               damage := unionRect (damage, toastRect);
               DN.Hide_Toast (notifications);
               debugPrint ("desktop: notification toast dismissed" & LF);
               scheduleRedrawRect (inflateRect (damage, 2));
            end if;
         when KR.Focus_Cycle =>
            cycleFocus (damage);
            scheduleRedrawRect (inflateRect (damage, 2));
         when KR.Focused_Surface =>
            if handleInternalKey (raw, damage) then
               scheduleRedrawRect (inflateRect (damage, 2));
            else
               queueKey (raw, repeat);
            end if;
         when KR.Exit_Key =>
            debugPrint ("desktop: exit key" & LF);
            running := False;
         when KR.Nowhere =>
            null;
      end case;
      if not release then
         focusTip;
      end if;
   end deliverKey;

   --  The focused client has not yet taken the last repeat: skip this one
   --  rather than queue another behind it, so a slow client never builds a
   --  backlog that keeps "typing" after the key is released.
   function repeatPending (target : Unsigned_64) return Boolean is
      channelSlot : constant Integer := findInputChannel (target);
   begin
      return channelSlot >= 0 and then
        (for some E of store.Channels (SurfaceIndex (channelSlot)).events =>
           E.Valid and then E.Kind = INPUT_KEY_DOWN and then
           (E.Payload1 and DP.Key_Repeat_Flag) /= 0);
   end repeatPending;

   --  Deliver a due auto-repeat. The main loop calls this only once its
   --  input queue is drained, so a release already queued wins over the
   --  repeat it would cancel.
   procedure pumpKeyRepeat (running : in out Boolean) is
      now : constant KR.Time := KR.Clamp (CuBit.Monotonic.Milliseconds);
   begin
      if not KR.Armed (keyRepeat) or else KR.Next_Due (keyRepeat) > now then
         return;
      end if;
      declare
         held  : constant KR.Key := KR.Armed_Key (keyRepeat);
         raw   : constant Unsigned_8 := Unsigned_8 (held.Code);
         where : constant KR.Destination := keyDestination (raw, held.Extended);
         fire  : Boolean;
         key   : KR.Key;
      begin
         KR.Tick (keyRepeat, now, keyRoute (where), fire, key);
         if not fire then
            null;  --  The route changed (focus, a menu): repeat stopped.
         elsif where = KR.Focused_Surface and then repeatPending (focusSurface) then
            statsKeyRepeatsSkipped := statsKeyRepeatsSkipped + 1;
         else
            statsKeyRepeats := statsKeyRepeats + 1;
            deliverKey (raw, key.Extended, where, True, running);
         end if;
      end;
   end pumpKeyRepeat;

   procedure handleEvent (eventMsg : Message; publisher : Process_ID;
                          running : in out Boolean) is
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
      --  The kernel's notice that a child Desktop launched has ended
      --  (Desktop is its parent): asynchronous, never a call back.
      if publisher = No_Process and then
        eventMsg.tag.label = CuBit.Child_Exits.Event_Label
      then
         if CuBit.Child_Exits.Valid
              (eventMsg.tag.length, eventMsg.words (0), eventMsg.words (1), eventMsg.words (2))
         then
            declare
               Ending : constant CuBit.Child_Exits.Report :=
                 CuBit.Child_Exits.Decode (eventMsg.words (0), eventMsg.words (1), eventMsg.words (2));
               Ended_Early : Desktop_Launch_Notice.Notice;
               Shown : Boolean;
            begin
               Desktop_Launch_Notice.Ended (launchWatch, Ended_Early, Ending, Shown);
               if Shown then
                  postLaunchNotice (Ended_Early, Ending.Process);
               end if;
            end;
         end if;
         return;
      end if;
      DI.Observe_Raw (Input_Diagnostic, eventMsg.tag.label = CuBit.Input.OP_SOURCE_REPORT, eventMsg.tag.label,
        eventMsg.words (1), eventMsg.words (2), eventMsg.words (3), Diagnostic_Last_Us);

      if eventMsg.tag.label = CuBit.Input.OP_SOURCE_REPORT then
         CuBit.Input.Decode (eventMsg, publisher, sourceReport, sourceValid);
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
      if sourceAccepted or else eventMsg.tag.label = EVENT_KEYBOARD or else
        eventMsg.tag.label = EVENT_MOUSE
      then
         --  Boot timeline (docs/boot-timeline.md); one branch after the first.
         CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.First_Input_Accepted, Startup.Registration);
         --  A frame follows: let the GPU clock rise now (one-way, coalesced).
         Desktop_GPU_Wake.Input_Hint;
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
            desktopSuperDown := False;
            desktopExtendedPrefix := False;
            --  Which keys are down is unknown: no repeat, nothing held.
            KR.Reset (keyRepeat);
            --  No chord survives a gap in the report stream.
            Desktop_Clipboard.Revoke_Gestures;
            --  Resynchronize clients before this report: it begins the new
            --  stream, so its transition follows the RESYNC (a recovery
            --  after delivery would discard it, e.g. a source's first key).
            forceInputResynchronization;
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
               key      : constant KR.Key := (KR.Scan_Code (raw and 16#7F#), extended);
               where    : constant KR.Destination := keyDestination (raw, extended);
               outcome  : KR.Press_Outcome := KR.New_Press;
            begin
               desktopExtendedPrefix := False;
               if (raw and 16#80#) /= 0 then
                  KR.Release (keyRepeat, key);
               else
                  KR.Press (keyRepeat, key, KR.Clamp (CuBit.Monotonic.Milliseconds),
                            keyRepeatSettings, keyRoute (where), outcome);
               end if;
               if outcome = KR.Held_Repeat then
                  --  A held key's device repeat (PS/2 typematic, a VM host's
                  --  auto-repeat): Desktop repeats keys itself, at its rate.
                  statsDeviceRepeats := statsDeviceRepeats + 1;
               else
                  if updateDesktopModifierKey (raw) then
                     null;
                  end if;
                  deliverKey (raw, extended, where, False, running);
               end if;
            end;
         end if;
      elsif eventMsg.tag.label = EVENT_MOUSE or else
         (sourceAccepted and then
          sourceReport.device in CuBit.Input.RELATIVE_POINTER | CuBit.Input.ABSOLUTE_POINTER)
      then
         statsMouseEvents := statsMouseEvents + 1;
         if Diagnostic_Mouse < Unsigned_64'Last then Diagnostic_Mouse := Diagnostic_Mouse + 1; end if;
         if sourceAccepted and then sourceDiscontinuity then
            --  As for keys: recover first, then apply this report's motion
            --  and button transitions, which begin the new stream.
            forceInputResynchronization;
         end if;
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
         if sourceAccepted and then sourceReport.device = CuBit.Input.ABSOLUTE_POINTER then
            --  Validated in acceptSourceReport.
            declare
               Report : constant Absolute_Pointer.Report := Absolute_Pointer.Decode (packed);
            begin
               --  The host's pointer is this pointer: a host-pointer cursor
               --  plane can show it with no compositing and no frame.
               if not Desktop_Pointer_Plane.Absolute then
                  Desktop_Pointer_Plane.Set_Absolute (True, requestSequence);
               end if;
               if Report.Wheel /= 0 then
                  statsWheelEvents := statsWheelEvents + 1;
               end if;
               declare
                  Target : constant Pointer_Motion := absoluteTarget (Report);
               begin
                  --  Button transitions of a VM or tablet pointer, with the
                  --  desktop position they apply at (a few per click).
                  if seatButtons /= lastButtons then
                     debugPrint ("desktop: absolute buttons" & Unsigned_64'Image (seatButtons) &
                       " at " & Decimal (Unsigned_64 (Target.X)) & "," &
                       Decimal (Unsigned_64 (Target.Y)) & LF);
                  end if;
                  handleMouseMotion
                    (buttons => seatButtons,
                     Motion  => Target,
                     dz      => Report.Wheel,
                     Observed_Ms => CuBit.Input.Pointer_Time (sourceReport.snapshot));
               end;
            end;
         else
            if Desktop_Pointer_Plane.Absolute then
               Desktop_Pointer_Plane.Set_Absolute (False, requestSequence);
            end if;
            if signed8 (Shift_Right (packed, 32)) /= 0 then
               statsWheelEvents := statsWheelEvents + 1;
            end if;
            handleMouseMotion
              (buttons =>
                 (if sourceAccepted then seatButtons
                  else packed and 16#FF#),
               Motion  => (Absolute => False,
                           DX => signed12 (Shift_Right (packed, 8)),
                           DY => signed12 (Shift_Right (packed, 20))),
               dz      => signed8 (Shift_Right (packed, 32)),
               Observed_Ms => (if sourceAccepted then CuBit.Input.Pointer_Time (sourceReport.snapshot)
                               else Unsigned_64'Last));
         end if;
      elsif eventMsg.tag.label = OP_STREAM_AVAILABLE then
         rememberStreams
           (From_Word (eventMsg.words (0)),
            eventMsg.words (1));
         scheduleRedrawRect ((x => 0, y => 0, w => fbWidth, h => fbHeight));
      end if;
   end handleEvent;

   --  Desktop's calls to display.svc never wait forever: a display
   --  stuck behind the GPU driver fails the call (the caller treats it
   --  as a refused request) instead of freezing the desktop.
   Display_Call_Deadline_Ms : constant := 10_000;
   function callDisplay
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0;
       Output : DSP.Output_Number := 0) return Message
   is
      msg : Message :=
        (tag      => (label => label, length => 4, protocol => 0, ipc => <>,
                     reserved => Unsigned_16 (Output)),
         authorityTag => 0,
         words    => [w0, w1, w2, w3]);
      tag : MessageTag;
   begin
      tag := capCall (Desktop_Endpoints.Display_Service, msg, CuBit.Messages.Deadline_After (Display_Call_Deadline_Ms));
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
      if Pages not in 1 .. Unsigned_64 (Positive'Last) / 4096 then return; end if;
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
                 capSubmit (Desktop_Endpoints.Display_Service, Request, Token));
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
      --  Display dropped the pointer's plane request with output zero's
      --  lease; the reopen re-creates it and re-sends the shape.
      Desktop_Pointer_Plane.Reset;
      pointerShapeSent := False;
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
        Info.tag.protocol /= 0 or else Info.tag.reserved /= 0 or else
        Info.words (0) not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Info.words (1) not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Info.words (2) > Unsigned_64 (DP.Buffer_Pitch'Last) or else Info.words (3) /= 32
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
           (Desktop_Endpoints.Display_Service, P.Targets (B).Address, Natural (Pages), False,
            P.Targets (B).Grant, Granted);
         P.Targets (B).Granted := Granted;
         if not Granted then closeOutput (Output); return; end if;
         Response := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
           (PW.Encode (PW.Attachment'(B, (P.Targets (B).Grant, Layout))), Output));
         Response.tag := capCall (Desktop_Endpoints.Display_Service, Response, CuBit.Messages.Deadline_After (Display_Call_Deadline_Ms));
         if Response.tag.label /= PW.Attach_Buffer or else Response.tag.length /= 1 or else
           Response.tag.protocol /= 0 or else Response.tag.reserved /= 0 or else Response.words (0) /= 0
         then closeOutput (Output); return; end if;
      end loop;
      Response := CuBit.Desktop_Messages.From_Wire (DSP.With_Output (PW.Encode_Open, Output));
      Response.tag := capCall (Desktop_Endpoints.Display_Service, Response, CuBit.Messages.Deadline_After (Display_Call_Deadline_Ms));
      if Response.tag.label /= PW.Open_Session or else Response.tag.length /= 4 or else
        Response.tag.protocol /= 0 or else Response.tag.reserved /= 0 or else
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

   --  The outputs reopened at new modes (a resized VM window): keep every
   --  window on the output it was on, inside that output's new work area;
   --  maximized windows fill it; the shell spans the new desktop.
   --  "WxH" with no padding (log markers the tests match).
   function Dimensions (Width, Height : Natural) return String is
      W : constant String := Width'Image;
      H : constant String := Height'Image;
   begin
      return W (W'First + 1 .. W'Last) & "x" & H (H'First + 1 .. H'Last);
   end Dimensions;

   procedure relayoutAfterModeChange (Old : DL.Layout) is
      function Translate (Value : Natural; Old_Origin, New_Origin : DG.Output_Origin;
                          Extent, Object_Extent : Natural) return Natural is
         Low : constant Integer := Integer (New_Origin);
         High : constant Integer := Low + Integer'Max (0, Integer (Extent) - Integer (Object_Extent));
      begin
         return Natural (Integer'Max (Low, Integer'Min (High,
           Integer (Value) - Integer (Old_Origin) + Low)));
      end Translate;
      Pointer : DL.Pointer_Position;
   begin
      if Old.Count = 0 or else desktopLayout.Count = 0 then return; end if;
      for S of store.Surfaces loop
         if S.used and then (S.flags and SURFACE_FLAG_WINDOW) /= 0 then
            declare
               Owner : constant DL.Pointer_Position := DL.Confine (Old,
                 (DG.Logical_Coordinate (S.x + Natural'Min (S.w / 2, 64)),
                  DG.Logical_Coordinate (S.y + Natural'Min (S.h / 2, 12))));
               Screen : constant DL.Viewport_Index :=
                 (if Owner.Screen <= desktopLayout.Count then Owner.Screen else 1);
               Old_G : DG.Output renames Old.Items (Owner.Screen).Geometry;
               New_G : DG.Output renames desktopLayout.Items (Screen).Geometry;
               B : constant Rect := logicalBounds (New_G);
               Work_Height : constant Natural := B.h -
                 (if Screen = Natural (primaryOutput) + 1 then Natural'Min (B.h, TASKBAR_H) else 0);
            begin
               if S.maximized then
                  declare
                     Area : constant Rect := windowWorkArea (B);
                     W : Natural := Area.w;
                     H : Natural := Area.h;
                  begin
                     clampSurfaceSize (S, W, H);
                     S.x := Area.x; S.y := Area.y;
                     if S.w /= W or S.h /= H then
                        S.w := W; S.h := H; S.serial := S.serial + 1;
                        if S.owner /= No_Process then
                           queueConfigure (S.id, Unsigned_64 (W), Unsigned_64 (H));
                        end if;
                     end if;
                  end;
               else
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
                  S.x := Translate (S.x, Old_G.X, New_G.X, B.w, S.w);
                  S.y := Translate (S.y, Old_G.Y, New_G.Y, Work_Height, Natural'Min (S.h, Work_Height));
               end if;
               S.restoreX := Translate (S.restoreX, Old_G.X, New_G.X, B.w, S.restoreW);
               S.restoreY := Translate (S.restoreY, Old_G.Y, New_G.Y, Work_Height,
                                        Natural'Min (S.restoreH, TITLE_HEIGHT));
               S.dirty := True;
            end;
         elsif S.used and then (S.flags and SURFACE_FLAG_SHELL) /= 0 then
            S.x := 0; S.y := 0; S.w := fbWidth; S.h := fbHeight;
            S.dirty := True;
         end if;
      end loop;
      Pointer := DL.Confine (desktopLayout,
        (DG.Logical_Coordinate (cursorX), DG.Logical_Coordinate (cursorY)));
      cursorX := Natural (Pointer.Point.X);
      cursorY := Natural (Pointer.Point.Y);
      launchMenuOpen := False;
      deskbarPanelOpen := False;
      groupListOpen := False;
      settingsView.Applied_Layout := desktopLayout;
      settingsView.Pending_Layout := desktopLayout;
      for I in 1 .. desktopLayout.Count loop
         declare
            B : constant Rect := logicalBounds (desktopLayout.Items (I).Geometry);
         begin
            debugPrint ("desktop: output" & Natural'Image (I - 1) & " re-laid out " &
              Dimensions (Natural (desktopLayout.Items (I).Geometry.Width),
                          Natural (desktopLayout.Items (I).Geometry.Height)) &
              " logical " & Dimensions (B.w, B.h) & " scale" &
              desktopLayout.Items (I).Geometry.Scale.Numerator'Image & "/" &
              desktopLayout.Items (I).Geometry.Scale.Denominator'Image & LF);
         end;
      end loop;
   end relayoutAfterModeChange;

   procedure setupDisplayBuffer (ok : out Boolean; Wait_For_Owner : Boolean := True) is
      First_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO);
      Second_Info : constant Message := callDisplay (OP_DISPLAY_GET_INFO, Output => 1);
      Prepared : Boolean;
      Layout : DP.Buffer_Layout;
      Candidate : DL.Layout;
      Choice : DL.Primary_Selection;
      Raw, Pages, Drag_Raw : Unsigned_64;
      Total_Width, Total_Height : Natural;
      Previous : constant DL.Layout := desktopLayout;
      Modes_Changed : Boolean := False;
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
         --  New modes (a resized VM window): carry the scale over and pack
         --  the outputs again; otherwise restore the arrangement exactly.
         Modes_Changed := not Compositor_Layout_Restore.Compatible (desktopLayout, Candidate);
         if Modes_Changed and then displayScale.Auto then
            --  Auto scale follows the new mode (a 4K monitor switched from
            --  the firmware's 1080p gets 200%), packed as at startup.
            declare
               Wanted : DL.Layout := Candidate;
            begin
               for I in 1 .. Wanted.Count loop
                  Wanted.Items (I).Geometry.Scale := startupScale (Wanted.Items (I).Geometry);
               end loop;
               Candidate := Compositor_Layout_Resize.Carry_Settings
                 (Wanted, Candidate,
                  (CuBit.Display_Arrangement.Minimum_Width, CuBit.Display_Arrangement.Minimum_Height));
            end;
         elsif Modes_Changed then
            Candidate := Compositor_Layout_Resize.Carry_Settings
              (desktopLayout, Candidate,
               (CuBit.Display_Arrangement.Minimum_Width, CuBit.Display_Arrangement.Minimum_Height));
         end if;
         Candidate := Compositor_Layout_Restore.Choose (desktopLayout, Candidate);
      else
         --  A fresh session: each output at its startup scale
         --  (desktop.display.scale), packed left to right; a packing the
         --  layout rules reject keeps every output at 100%.
         declare
            Wanted : DL.Layout := Candidate;
         begin
            for I in 1 .. Wanted.Count loop
               Wanted.Items (I).Geometry.Scale := startupScale (Wanted.Items (I).Geometry);
            end loop;
            Candidate := Compositor_Layout_Resize.Carry_Settings
              (Wanted, Candidate,
               (CuBit.Display_Arrangement.Minimum_Width, CuBit.Display_Arrangement.Minimum_Height));
         end;
      end if;
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
      if Modes_Changed then
         relayoutAfterModeChange (Previous);
      end if;
      syncPointerPlane;
      ok := True;
   end setupDisplayBuffer;

   procedure armOutputWatch is
      Token : Unsigned_64;
   begin
      if outputWatchToken /= 0 or else not outputWatchUsable or else
        not backBufferReady or else outputDrainRequested or else outputReopenPending
      then
         return;
      end if;
      CR.Allocate (requestSequence, Token);
      if Token /= 0 and then capSubmit (Desktop_Endpoints.Display_Service, CuBit.Desktop_Messages.From_Wire
           (OM.Encode_Watch (CuBit.Output_Discovery.Display_Broker, outputRevisionSeen)), Token)
      then
         outputWatchToken := Token;
      end if;
   end armOutputWatch;

   --  The monitor on Output as its EDID names it (display.svc Identify);
   --  invalid for an output without EDID.
   function monitorIdentity (Output : Output_Index) return OM.Identity_Decoding is
     (OM.Decode_Identity (CuBit.Output_Discovery.Display_Broker, CuBit.Desktop_Messages.To_Wire
        (callDisplay (OM.Code (CuBit.Output_Discovery.Display_Broker, OM.Identify),
                      Output => DSP.Output_Number (Output)))));

   --  The mode saved for a monitor (desktop.display.mode.<monitor>): Auto
   --  when none is saved or the stored value is not a canonical Display_Mode.
   function savedMode (Monitor : OM.Monitor_Identity) return Desktop_Display_Mode_Settings.Preference is
      package S renames Desktop_Display_Mode_Settings;
      use type CuBit.Config.ConfigStatus;
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      Result : S.Preference := S.Default;
      OK     : Boolean;
   begin
      CuBit.Config.get (S.Key_For (Monitor), Value, Length, Status);
      if Status = CuBit.Config.OK and then Value /= System.Null_Address and then
        Length in 1 .. S.Maximum_Source
      then
         declare
            Source : String (1 .. Length) with Import, Address => Value;
         begin
            S.Decode (Source, Result, OK);
         end;
         if not OK then
            debugPrint ("desktop: invalid " & S.Key_For (Monitor) & "; using Auto" & LF);
         end if;
      end if;
      return Result;
   end savedMode;

   --  "WxH@60.000Hz" (or "@best") for logs.
   function Request_Text (Item : OM.Mode_Request) return String is
      function Rate (Millihertz : OM.Millihertz) return String is
         Whole : constant String := Natural'Image (Natural (Millihertz / 1000));
         Part : constant String := Natural'Image (1000 + Natural (Millihertz mod 1000));
      begin
         return Whole (Whole'First + 1 .. Whole'Last) & "." & Part (Part'First + 2 .. Part'Last) & "Hz";
      end Rate;
   begin
      return Dimensions (Natural (Item.Size.Width), Natural (Item.Size.Height)) & "@" &
        (case Item.Rate is
           when OM.Exact_Rate => Rate (Item.Refresh),
           when OM.Best_Rate => "best");
   end Request_Text;

   --  Display's catalog changed: find outputs whose host preference changed
   --  since we last acted on it, and re-lay them out. The first pass (at
   --  startup) also applies each monitor's saved mode (Settings > Monitor,
   --  DISP-016): the user kept it on that monitor, so it is applied even
   --  while desktop.display.modeset (Auto's switch to an Intel output's best
   --  mode) stays off.
   procedure handleOutputChange is
      package OD renames CuBit.Output_Discovery;
      use type OD.Output_Source, OD.Output_Role;
      Summary : OD.Summary_Decoding;
      Item : OD.Description_Decoding;
   begin
      outputChangeNoticed := False;
      Summary := OD.Decode_Summary (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire
        (callDisplay (OD.Code (OD.Display_Broker, OD.Get_Catalog))));
      if not Summary.Valid then return; end if;
      outputRevisionSeen := Summary.Value.Revision;
      for Index in 1 .. Summary.Value.Count loop
         Item := OD.Decode_Description (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire
           (callDisplay (OD.Code (OD.Display_Broker, OD.Get_Description),
                         Summary.Value.Revision, Unsigned_64 (Index))));
         exit when not Item.Valid;
         declare
            D : constant OD.Description := Item.Value.Item;
         begin
            if (D.Source = OD.Virtio_GPU or else D.Source = OD.Intel_Display)
              and then D.Role /= OD.Detected_Only and then
              D.Native_Number <= Natural (Output_Index'Last) and then
              presentations (Output_Index (D.Native_Number)).Enabled
            then
               declare
                  Output : constant Output_Index := Output_Index (D.Native_Number);
                  Preferred : constant OM.Mode := (D.Advertised_Width, D.Advertised_Height);
                  Current : constant OM.Mode := (D.Current_Width, D.Current_Height);
                  Monitor : constant OM.Identity_Decoding :=
                    (if firstOutputPass then monitorIdentity (Output) else (Valid => False));
                  Saved : constant Desktop_Display_Mode_Settings.Preference :=
                    (if Monitor.Valid then savedMode (Monitor.Value) else Desktop_Display_Mode_Settings.Default);
               begin
                  if not Saved.Auto then
                     --  A refusal (a mode the monitor no longer lists) is
                     --  fine: the reopen uses whatever the output has.
                     actedPreference (Output) := Preferred;
                     wantedModes (Output) := (True, Desktop_Display_Mode_Settings.Request_Of (Saved));
                     modeChangePending := True;
                     debugPrint ("desktop: output" & Output'Image & " saved mode " &
                       Request_Text (wantedModes (Output).Mode) & LF);
                  elsif (firstOutputPass and then D.Source = OD.Virtio_GPU) or else
                    (D.Source = OD.Intel_Display and then not displayModeset)
                  then
                     --  At startup a virtio-gpu host preference is only
                     --  recorded; it is followed when the host changes it.
                     --  An Intel output's best mode (advertised) waits for
                     --  desktop.display.modeset.
                     actedPreference (Output) := Preferred;
                  elsif Preferred /= actedPreference (Output) and then Preferred /= Current then
                     actedPreference (Output) := Preferred;
                     wantedModes (Output) := (True, (OM.Best_Rate, Preferred));
                     modeChangePending := True;
                     debugPrint ("desktop: output" & Output'Image & " host mode" &
                       " " & Dimensions (Preferred.Width, Preferred.Height) & LF);
                  else
                     actedPreference (Output) := Preferred;
                  end if;
               end;
            end if;
         end;
      end loop;
      firstOutputPass := False;
      if modeChangePending then
         releaseDisplayBuffer;
      end if;
   end handleOutputChange;

   --  DISP-005/016: the primary output's monitor (display.svc catalog: EDID
   --  size, best admitted mode; Identify: its name) and its modes with the
   --  GPU's verdicts (List_Mode), sorted for Settings > Monitor, with the
   --  choice saved for it.
   function monitorInfo return Desktop_Settings.Monitor_Info is
      package OD renames CuBit.Output_Discovery;
      package S renames Desktop_Display_Mode_Settings;
      use type OD.Output_Role, OD.Output_Source;
      Info : Desktop_Settings.Monitor_Info;
      Summary : OD.Summary_Decoding;
      Item : OD.Description_Decoding;
      Monitor : constant OM.Identity_Decoding := monitorIdentity (primaryOutput);
      Count : OM.Mode_Count := 0;
      function Img (Value : Natural) return String is
         T : constant String := Value'Image;
      begin
         return T (T'First + 1 .. T'Last);
      end Img;
      --  Integer square root (the diagonal in millimetres).
      function Root (Value : Natural) return Natural is
         R : Natural := 0;
      begin
         while (R + 1) * (R + 1) <= Value loop
            R := R + 1;
         end loop;
         return R;
      end Root;
      function Name return String is
         Text : String (1 .. (if Monitor.Valid then Monitor.Value.Name_Last else 0));
      begin
         for I in Text'Range loop
            Text (I) := Monitor.Value.Name (I);
         end loop;
         return (if Text'Length = 0 then "" else Text & ": ");
      end Name;
      function Listed (Index : OM.Mode_Index) return OM.Listing_Decoding is
        (OM.Decode_Listing (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire
           (callDisplay (OM.Code (OD.Display_Broker, OM.List_Mode), Unsigned_64 (Index),
                         Output => DSP.Output_Number (primaryOutput)))));
   begin
      Info.Summary := Desktop_Settings.Line_Of ("No monitor information (firmware output without EDID)");
      Summary := OD.Decode_Summary (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire
        (callDisplay (OD.Code (OD.Display_Broker, OD.Get_Catalog))));
      if Summary.Valid then
         for Index in 1 .. Summary.Value.Count loop
            Item := OD.Decode_Description (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire
              (callDisplay (OD.Code (OD.Display_Broker, OD.Get_Description),
                            Summary.Value.Revision, Unsigned_64 (Index))));
            exit when not Item.Valid;
            declare
               D : constant OD.Description := Item.Value.Item;
            begin
               if D.Native_Number = Natural (primaryOutput) and then D.Role /= OD.Detected_Only then
                  if D.Width_MM /= 0 and then D.Height_MM /= 0 then
                     declare
                        Diagonal_MM : constant Natural := Root (D.Width_MM * D.Width_MM + D.Height_MM * D.Height_MM);
                        Diagonal_Tenths : constant Natural := (Diagonal_MM * 100 + 127) / 254;
                        DPI : constant Natural :=
                          (Natural (D.Current_Width) * 254 + D.Width_MM * 5) / (D.Width_MM * 10);
                     begin
                        Info.Summary := Desktop_Settings.Line_Of
                          (Name & Img (D.Width_MM) & " x " & Img (D.Height_MM) & " mm (" &
                           Img (Diagonal_Tenths / 10) & "." & Img (Diagonal_Tenths mod 10) & " in), " &
                           Img (DPI) & " dpi at " & Img (D.Current_Width) & " x " & Img (D.Current_Height));
                     end;
                  else
                     Info.Summary := Desktop_Settings.Line_Of (Name & "size not stated by its EDID");
                  end if;
                  Info.Detail := Desktop_Settings.Line_Of
                    ("Best mode " & Img (D.Advertised_Width) & " x " & Img (D.Advertised_Height) &
                     (if D.Source = OD.Intel_Display
                      then (if displayModeset then "; Auto applies it at startup (desktop.display.modeset on)"
                            else "; Auto keeps the firmware mode at startup (desktop.display.modeset off)")
                      else ""));
               end if;
            end;
         end loop;
      end if;
      declare
         First : constant OM.Listing_Decoding := Listed (1);
      begin
         if First.Valid then
            Count := First.Value.Count;
         end if;
      end;
      if Count > 0 then
         Info.Modes := new Desktop_Settings.Monitor_Modes (1 .. Natural (Count));
         declare
            Modes : Desktop_Settings.Monitor_Modes renames Info.Modes.all;
            Filled : Natural := 0;
         begin
            for Index in 1 .. Count loop
               declare
                  L : constant OM.Listing_Decoding := Listed (Index);
                  Position : Natural;
               begin
                  exit when not L.Valid or else L.Value.Count /= Count;
                  --  Insertion keeps the monitor's order among equal modes.
                  Position := Filled + 1;
                  while Position > 1 and then OM.Before (L.Value.Item, Modes (Position - 1)) loop
                     Modes (Position) := Modes (Position - 1);
                     Position := Position - 1;
                  end loop;
                  Modes (Position) := L.Value.Item;
                  Filled := Filled + 1;
               end;
            end loop;
            if Filled < Natural (Count) then
               --  The list changed while it was read: show none rather than
               --  a mix (Settings re-reads it after the next change).
               debugPrint ("desktop: mode list changed while read" & LF);
               Desktop_Settings.Free (Info.Modes);
            end if;
         end;
      end if;
      Info.Identified := Monitor.Valid;
      if Monitor.Valid then
         declare
            Saved : constant S.Preference := savedMode (Monitor.Value);
         begin
            Info.Saved_Auto := Saved.Auto;
            if not Saved.Auto and then Info.Modes /= null then
               for Row in Info.Modes'Range loop
                  if OM.Names (S.Request_Of (Saved), Info.Modes (Row)) then
                     Info.Saved_Row := Row;
                     exit;
                  end if;
               end loop;
            end if;
         end;
      end if;
      return Info;
   end monitorInfo;

   function settingsOpen return Boolean is
     (for some S of store.Surfaces => S.used and then S.appKind = APP_SETTINGS);
   procedure damageSettings is
   begin
      for S of store.Surfaces loop
         if S.used and then S.appKind = APP_SETTINGS then
            scheduleRedrawRect (surfaceRect (S));
         end if;
      end loop;
   end damageSettings;

   --  The Monitor page's rows: the one the output runs, and Auto's (the
   --  monitor's preferred admitted mode, else the first admitted one: the
   --  largest, fastest). Auto_Row when none.
   function currentRow (Info : Desktop_Settings.Monitor_Info) return Desktop_Settings.Mode_Row is
   begin
      if Info.Modes /= null then
         for Row in Info.Modes'Range loop
            if Info.Modes (Row).Current then return Row; end if;
         end loop;
      end if;
      return Desktop_Settings.Auto_Row;
   end currentRow;
   function bestRow (Info : Desktop_Settings.Monitor_Info) return Desktop_Settings.Mode_Row is
      use type OM.Admission;
      First : Desktop_Settings.Mode_Row := Desktop_Settings.Auto_Row;
   begin
      if Info.Modes /= null then
         for Row in Info.Modes'Range loop
            if Info.Modes (Row).Verdict = OM.Admitted then
               if Info.Modes (Row).Preferred then return Row; end if;
               if First = Desktop_Settings.Auto_Row then First := Row; end if;
            end if;
         end loop;
      end if;
      return First;
   end bestRow;

   --  Save the kept choice for the primary output's monitor.
   procedure storeModeChoice (Auto : Boolean; Trial : OM.Mode_Request) is
      package S renames Desktop_Display_Mode_Settings;
      use type CuBit.Config.ConfigStatus;
      Monitor : constant OM.Identity_Decoding := monitorIdentity (modeSwitch.Output);
      Status : CuBit.Config.ConfigStatus;
   begin
      settingsView.Monitor_Status := Desktop_Settings.Mode_Kept;
      if not Monitor.Valid then
         return;
      end if;
      if not Auto and then Trial.Rate /= OM.Exact_Rate then
         settingsView.Monitor_Status := Desktop_Settings.Mode_Saved_Session_Only;
         return;
      end if;
      declare
         Choice : constant S.Preference :=
           (if Auto then S.Default
            else (Auto => False, Size => Trial.Size, Refresh => Trial.Refresh));
         Text : constant String := S.Encode (Choice);
         Key : constant String := S.Key_For (Monitor.Value);
      begin
         CuBit.Config.set (Key, Text'Address, Text'Length, Status);
         if Status /= CuBit.Config.OK then
            settingsView.Monitor_Status := Desktop_Settings.Mode_Saved_Session_Only;
         end if;
         debugPrint ("desktop: display mode " & Key & " " & Text &
           (if Status = CuBit.Config.OK then " saved" else " refused by Config") & LF);
      end;
   end storeModeChoice;

   --  Ask for Request on Output: the outputs drain, switch and reopen
   --  (applyWantedModes, then setupDisplayBuffer in the main loop).
   procedure switchMode (Output : Output_Index; Request : OM.Mode_Request) is
   begin
      wantedModes (Output) := (True, Request);
      modeChangePending := True;
      monitorRefreshPending := True;
      releaseDisplayBuffer;
   end switchMode;

   procedure applyMonitorChoice is
      Info : Desktop_Settings.Monitor_Info renames settingsView.Monitor_View;
      Choice : constant Desktop_Settings.Mode_Row := settingsView.Mode_Choice;
      Auto : constant Boolean := Choice = Desktop_Settings.Auto_Row;
      Target : constant Desktop_Settings.Mode_Row := (if Auto then bestRow (Info) else Choice);
      Running : constant Desktop_Settings.Mode_Row := currentRow (Info);
      Previous : constant OM.Mode_Request :=
        (if Running /= Desktop_Settings.Auto_Row then OM.Request_For (Info.Modes (Running))
         else (OM.Best_Rate, (CuBit.Output_Discovery.Extent (presentations (primaryOutput).Geometry.Width),
                              CuBit.Output_Discovery.Extent (presentations (primaryOutput).Geometry.Height))));
   begin
      if modeSwitch.Kind /= No_Switch or else modeTrial.Current = Desktop_Mode_Trial.Asking or else
        Info.Modes = null or else Target = Desktop_Settings.Auto_Row or else Target > Info.Modes'Last
      then
         return;
      end if;
      modeSwitch := (Trial_Switch, primaryOutput, Previous, OM.Request_For (Info.Modes (Target)), Auto, False);
      if Target = Running then
         --  Already there: nothing to try, only the choice to keep.
         storeModeChoice (Auto, modeSwitch.Trial);
         modeSwitch.Kind := No_Switch;
         Desktop_Settings.Set_Monitor (settingsView, monitorInfo);
         damageSettings;
         return;
      end if;
      debugPrint ("desktop: Settings mode " & Request_Text (modeSwitch.Trial) &
        (if Auto then " (Auto)" else "") & " from " & Request_Text (Previous) & LF);
      settingsView.Monitor_Status := Desktop_Settings.Mode_Switching;
      damageSettings;
      switchMode (primaryOutput, modeSwitch.Trial);
   end applyMonitorChoice;

   --  Go back to the mode before the trial (Revert, or no answer in time).
   procedure revertTrial (Why : Desktop_Settings.Mode_Status) is
      Back : constant OM.Mode_Request := modeTrial.Previous;
   begin
      debugPrint ("desktop: display mode trial " &
        (if Why = Desktop_Settings.Mode_Timed_Out then "timed out" else "reverted") & "; back to " &
        Request_Text (Back) & LF);
      modeSwitch := (Revert_Switch, modeSwitch.Output, modeTrial.Trial, Back, False, False);
      modeTrial := (Current => Desktop_Mode_Trial.Idle);
      settingsView.Trial_Open := False;
      settingsView.Focused := Desktop_Settings.Mode_List;
      settingsView.Monitor_Status := Why;
      damageSettings;
      switchMode (modeSwitch.Output, Back);
   end revertTrial;

   --  Each turn: the user's answer, the deadline, the countdown; and, once
   --  the outputs have reopened after a switch, the question or the outcome.
   procedure pumpModeTrial is
      Now : constant Desktop_Mode_Trial.Time := Desktop_Mode_Trial.Time_Of (nowMs);
   begin
      if modeSwitch.Kind /= No_Switch and then monitorRefreshPending and then backBufferReady and then
        not outputDrainRequested and then not outputReopenPending
      then
         monitorRefreshPending := False;
         if modeSwitch.Kind = Trial_Switch then
            if modeSwitch.Accepted and then
              Now <= Desktop_Mode_Trial.Time'Last - Desktop_Mode_Trial.Confirmation_Window
            then
               modeTrial := Desktop_Mode_Trial.Start (modeSwitch.Previous, modeSwitch.Trial, modeSwitch.Auto, Now);
               settingsView.Trial_Open := True;
               settingsView.Trial_Seconds := Desktop_Mode_Trial.Seconds_Left (modeTrial, Now);
               trialSecondsShown := settingsView.Trial_Seconds;
               settingsView.Focused := Desktop_Settings.Keep_Mode;
               settingsView.Monitor_Status := Desktop_Settings.Mode_Ready;
               debugPrint ("desktop: keep this display mode? (" & Request_Text (modeSwitch.Trial) & ")" & LF);
            else
               settingsView.Monitor_Status := Desktop_Settings.Mode_Refused;
               debugPrint ("desktop: Settings mode refused" & LF);
            end if;
         end if;
         modeSwitch.Kind := No_Switch;
         if settingsOpen then
            declare
               Status : constant Desktop_Settings.Mode_Status := settingsView.Monitor_Status;
            begin
               Desktop_Settings.Set_Monitor (settingsView, monitorInfo);
               settingsView.Monitor_Status := Status;
               if modeTrial.Current = Desktop_Mode_Trial.Asking then
                  --  The list shows the mode on trial chosen.
                  Desktop_Settings.Choose_Mode (settingsView, currentRow (settingsView.Monitor_View));
               end if;
            end;
            damageSettings;
         end if;
      end if;
      if modeTrial.Current /= Desktop_Mode_Trial.Asking then
         settingsView.Answer := Desktop_Settings.No_Response;
         return;
      end if;
      case settingsView.Answer is
         when Desktop_Settings.Keep_Response =>
            settingsView.Answer := Desktop_Settings.No_Response;
            debugPrint ("desktop: display mode kept " & Request_Text (modeTrial.Trial) & LF);
            storeModeChoice (modeTrial.Auto, modeTrial.Trial);
            modeTrial := (Current => Desktop_Mode_Trial.Idle);
            settingsView.Trial_Open := False;
            settingsView.Focused := Desktop_Settings.Mode_List;
            declare
               Status : constant Desktop_Settings.Mode_Status := settingsView.Monitor_Status;
            begin
               Desktop_Settings.Set_Monitor (settingsView, monitorInfo);
               settingsView.Monitor_Status := Status;
            end;
            damageSettings;
            return;
         when Desktop_Settings.Revert_Response =>
            settingsView.Answer := Desktop_Settings.No_Response;
            revertTrial (Desktop_Settings.Mode_Reverted);
            return;
         when Desktop_Settings.No_Response => null;
      end case;
      if Desktop_Mode_Trial.Expired (modeTrial, Now) then
         revertTrial (Desktop_Settings.Mode_Timed_Out);
      elsif Desktop_Mode_Trial.Seconds_Left (modeTrial, Now) /= trialSecondsShown then
         trialSecondsShown := Desktop_Mode_Trial.Seconds_Left (modeTrial, Now);
         settingsView.Trial_Seconds := trialSecondsShown;
         damageSettings;
      end if;
   end pumpModeTrial;

   --  Before reopening: switch each output that wants a new mode. A refusal
   --  (or a fitted mode) is fine: the reopen uses whatever the output has.
   procedure applyWantedModes is
      Call : Message;
      Answer : OM.Result_Decoding;
   begin
      modeChangePending := False;
      for Output in Output_Index loop
         if wantedModes (Output).Pending then
            wantedModes (Output).Pending := False;
            Call := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
              (OM.Encode_Set_Mode (CuBit.Output_Discovery.Display_Broker,
                                   wantedModes (Output).Mode), Output));
            Call.tag := capCall (Desktop_Endpoints.Display_Service, Call,
                                 CuBit.Messages.Deadline_After (MODE_CALL_DEADLINE_MS));
            Answer := OM.Decode_Result (CuBit.Output_Discovery.Display_Broker,
                                        CuBit.Desktop_Messages.To_Wire (Call));
            if Answer.Valid then
               debugPrint ("desktop: output" & Output'Image & " mode " &
                 Request_Text (wantedModes (Output).Mode) & " -> " &
                 Dimensions (Answer.Value.Current.Width, Answer.Value.Current.Height) &
                 (if Answer.Value.Status = DP.Success then "" else " (refused)") & LF);
            else
               debugPrint ("desktop: output" & Output'Image & " mode change refused" & LF);
            end if;
            if modeSwitch.Kind /= No_Switch and then Output = modeSwitch.Output then
               modeSwitch.Accepted := Answer.Valid and then Answer.Value.Status = DP.Success;
            end if;
         end if;
      end loop;
   end applyWantedModes;

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

   --  The CPU bootstrap frame (no renderer/GPU owner). Submit_Bootstrap draws
   --  and submits one; Poll_Bootstrap advances it once, without waiting, and
   --  says when Display has released it (storage writable, nothing shown
   --  from it, no fault): only then may a renderer use the pool.
   procedure Submit_Bootstrap (Submitted : out Boolean) is
      P : Output_Presentation renames presentations (primaryOutput);
   begin
      Submitted := False;
      if not backBufferReady or else not P.Enabled or else
        not CP.Writable (P.Transfer) or else not BP.Writable (P.Pool, BP.Writer (P.Pool))
      then return; end if;
      -- This CPU frame has no renderer/GPU owner. Do not call Begin_Output:
      -- early renderer admission would irreversibly select software.
      Bootstrap_CPU := True;
      Compositor_Damage.Add (P.Damage, (0, 0, Natural (P.Geometry.Width), Natural (P.Geometry.Height)));
      pumpOutput (primaryOutput);
      Submitted := CP.Current (P.Transfer) in CP.Prepared | CP.In_Flight;
      if Submitted and then bootstrapFrames < Unsigned_64'Last then
         bootstrapFrames := bootstrapFrames + 1;
      end if;
   end Submit_Bootstrap;

   procedure Poll_Bootstrap (Released : out Boolean) is
      P : Output_Presentation renames presentations (primaryOutput);
   begin
      Released := False;
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
         Released := True;
         if not bootstrapReleased then
            bootstrapReleased := True;
            debugPrint ("DESKTOP-BOOTSTRAP: released; renderer may start" & LF);
         end if;
      end if;
   end Poll_Bootstrap;

   --  Wait for the bootstrap frame in flight to be released, at most
   --  Limit_Ms (the clock checked between 1 ms sleeps, attempts bounded too).
   Bootstrap_Release_Limit_Ms : constant := 2_000;
   procedure Await_Bootstrap (Limit_Ms : Unsigned_64; Ready : out Boolean) is
      Started : constant Unsigned_64 := nowMs;
      Ignore : Unsigned_64;
   begin
      Ready := False;
      if Started = Unsigned_64'Last then return; end if;
      for Attempt in 1 .. Bootstrap_Release_Limit_Ms loop
         -- Bound attempts even if the clock freezes. Exhaustion never
         -- authorizes renderer startup or release of uncertain storage.
         Poll_Bootstrap (Ready);
         if Ready then return; end if;
         declare Now : constant Unsigned_64 := nowMs; begin
            if Now = Unsigned_64'Last or else Now < Started or else Now - Started >= Limit_Ms then return; end if;
         end;
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
   end Await_Bootstrap;

   procedure Present_Bootstrap (Ready : out Boolean) is
      Submitted : Boolean;
   begin
      Ready := False;
      Submit_Bootstrap (Submitted);
      if Submitted then
         Await_Bootstrap (Bootstrap_Release_Limit_Ms, Ready);
      end if;
   end Present_Bootstrap;

   --  The renderer worker runs; keep the bootstrap screen alive and log the
   --  steps it passes, until it finishes or Renderer_Startup_Budget_Ms has
   --  passed. Finished: it did. Storage_Ready: the bootstrap pool is
   --  released (no frame of ours in flight), as a renderer requires.
   procedure Wait_For_Renderer (Finished, Storage_Ready : out Boolean) is
      Poll_Ms : constant := 10;
      Started : constant Unsigned_64 := nowMs;
      Next_Tick : Unsigned_64 := Started + Bootstrap_Tick_Ms;
      In_Flight : Boolean := False;
      Held_Reported : Boolean := False;
      Released, Submitted : Boolean;
      Now : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      Finished := False;
      Storage_Ready := False;
      loop
         Desktop_Breadcrumbs.Mark (Desktop_Startup_Progress.Current);
         Finished := Desktop_Renderer_Worker.Finished;
         exit when Finished;
         Now := nowMs;
         exit when Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Renderer_Startup_Budget_Ms;
         if In_Flight then
            Poll_Bootstrap (Released);
            In_Flight := not Released;
         end if;
         if Now >= Next_Tick then
            Next_Tick := Now + Bootstrap_Tick_Ms;
            if In_Flight then
               -- A whole tick without a release: Display still holds the
               -- frame, so nothing new is drawn into its storage. It is
               -- polled every turn; drawing resumes once it is released.
               if not Held_Reported then
                  Held_Reported := True;
                  debugPrint ("DESKTOP-BOOTSTRAP: frame" & Unsigned_64'Image (bootstrapFrames) &
                    " not released in" & Unsigned_64'Image (Unsigned_64'(Bootstrap_Tick_Ms)) &
                    " ms; waiting for Display" & LF);
               end if;
            else
               Desktop_Bootstrap_Logs.Gather (Desktop_Bootstrap_Logs.Call_Limit_Ms);
               Submit_Bootstrap (Submitted);
               In_Flight := Submitted;
            end if;
         end if;
         Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
      end loop;
      Desktop_Breadcrumbs.Mark (Desktop_Startup_Progress.Current);
      if In_Flight then
         Await_Bootstrap (Bootstrap_Release_Limit_Ms, Released);
         In_Flight := not Released;
      end if;
      Storage_Ready := not In_Flight;
   end Wait_For_Renderer;

   procedure Start_Renderer (Allow_GPU : Boolean) is
      package Selection renames Compositor_Backend_Selection;
      Evidence : Selection.Readiness := (others => False);
      Diagnostic : Desktop_Renderer_Startup.Pipeline_Diagnostic := (others => <>);
      Accepted : Boolean;
      Started, Finished, Storage_Ready : Boolean := False;
      Abandoned : Boolean := False;
      use type Desktop_Breadcrumbs.Stage;
   begin
      -- No GPU owner exists before the worker runs. On an unconfirmed CPU
      -- bootstrap, select software without entering foreign initialization.
      if Allow_GPU then
         Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_Before);
         Desktop_Renderer_Worker.Start
           (Configuration => backBufferReady and then desktopLayout.Count = 1 and then
              primaryOutput = 0 and then presentations (0).Enabled,
            Width => Unsigned_64 (presentations (0).Geometry.Width),
            Height => Unsigned_64 (presentations (0).Geometry.Height),
            Epoch => BP.Writer (presentations (0).Pool).Epoch,
            Started => Started);
         if not Started then
            debugPrint ("desktop: renderer worker thread unavailable" & LF);
         else
            rendererStartedMs := nowMs;
            rendererWorkerActive := True;
            Wait_For_Renderer (Finished, Storage_Ready);
            rendererWorkerActive := False;
            debugPrint ("DESKTOP-BOOTSTRAP: renderer startup " &
              (if Finished then "finished" else "unfinished") & " after" &
              Unsigned_64'Image (nowMs - rendererStartedMs) & " ms; bootstrap frames" &
              Unsigned_64'Image (bootstrapFrames) & LF);
            if Finished then
               Desktop_Renderer_Worker.Take (Evidence, Diagnostic);
               Desktop_Renderer_Startup.Report (Evidence, Diagnostic);
               if Desktop_Startup_Progress.Current < Desktop_Breadcrumbs.Init_After then
                  Desktop_Breadcrumbs.Mark (Desktop_Breadcrumbs.Init_After);
               end if;
               if not Storage_Ready then
                  debugPrint ("DESKTOP-BOOTSTRAP: frame still held by Display; software selected" & LF);
                  Evidence := (others => False);
               end if;
            else
               -- Never touch the worker's state again: the GPU stack may still
               -- be running on it. Software, and no call into the Mesa glue.
               Abandoned := True;
               Desktop_GPU_Wake.Disable;
               debugPrint ("DESKTOP-VULKAN: renderer startup exceeded" &
                 Unsigned_64'Image (Unsigned_64'(Renderer_Startup_Budget_Ms)) &
                 " ms at step " & Desktop_Breadcrumbs.Name (Desktop_Startup_Progress.Current) &
                 "; worker abandoned" & LF);
            end if;
         end if;
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
         if Allow_GPU and then Finished then Desktop_Renderer_Startup.Stop; end if;
         if Desktop_Renderer_Startup.GPU_Capable then
            softwareAnnounced := True;
            debugPrint ((if Abandoned then "desktop: software rendering (GPU startup did not finish in time)"
                         elsif Allow_GPU then "desktop: software rendering (GPU unavailable at startup)"
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
   declare
      Allocated : Boolean;
   begin
      resizeSurfaceTables (INITIAL_SURFACES, Allocated);
      if not Allocated then
         debugPrint ("desktop: no memory for the surface table" & LF);
         return;
      end if;
   end;
   --  Announce the session before anything slow (configuration, wallpaper
   --  reads): a framebuffer shell started next to the desktop checks this
   --  and leaves the display to it (shell setupDisplayPresent).
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("desktop: startup capabilities missing" & LF);
      return;
   end if;
   ret := registerDriver (DRIVER_DESKTOP, Startup.Registration);
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: register failed" & LF);
   end if;
   Read_Appearance;
   Desktop_Wallpaper_Assets.Prepare (appearance.Backdrop);
   Desktop_Icon_Assets.Load;
   Read_Trace_Configuration;
   Load_Launch_Menu;
   Load_Key_Repeat;
   Load_GPU_Power;
   Load_Display_Scale;
   Load_Display_Modeset;
   Desktop_Launch_Refresh.Initialize;

   ret := setLatencyContract
      (LATENCY_INTERACTIVE,
       4_167,   --  Advisory 240 Hz target; presentation follows the output.
       4_000);  --  Budget hint for input dispatch and compositor drawing.
   if ret = Unsigned_64'Last then
      debugPrint ("desktop: latency contract rejected" & LF);
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
      --  The one frame shown before renderer startup carries the log
      --  records gathered so far and names the step that follows, so a
      --  stall there is photographed with its likely cause.
      Desktop_Bootstrap_Logs.Gather_Before_Bootstrap;
      if Desktop_Renderer_Startup.GPU_Capable then
         bootstrapStage := Stage_Renderer'Access;
         bootstrapWaiting := Waiting_GPU'Access;
      end if;
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
         eventPublisher : Process_ID := No_Process;
         eventFound : Boolean := False;
         activity : Activity_Result;
         metricsDelayUs : Unsigned_64 := Unsigned_64'Last;
         inputBatch : DB.Input_Batch := DB.New_Input;
         procedure Drain_Events is
         begin
            DB.Begin_Input_Phase (inputBatch, dispatchNow);
            while running and then DB.Can_Input (inputBatch, dispatchNow) loop
               eventFound := Poll_Event (eventMsg, eventPublisher);
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
                  handleEvent (eventMsg, eventPublisher, running);
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
         if outputChangeNoticed and then not outputDrainRequested and then
           not outputReopenPending and then not shutdownRequested
         then
            handleOutputChange;
         end if;
         armOutputWatch;
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
         --  Auto-repeat only once input is drained (a queued release wins).
         if running and then not eventFound then
            pumpKeyRepeat (running);
         end if;
         if not running and then not shutdownRequested then
            shutdownRequested := True;
            outputReopenPending := False;
            for S of store.Surfaces loop
               if S.used then releaseSurfaceBuffer (S); end if;
            end loop;
            releaseDisplayBuffer;
         end if;
         pumpOutputRetirements;
         if outputReopenPending and then not outputDrainRequested and then not shutdownRequested then
            declare Ready : Boolean;
            begin
               if modeChangePending then
                  applyWantedModes;
               end if;
               setupDisplayBuffer (Ready, False);
               outputReopenPending := not Ready;
               if Ready then scheduleRedraw; end if;
            end;
         end if;
         refreshStatus;
         pumpModeTrial;
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
         tickTip (nowMs);
         --  Secrets are wiped at their deadline, not only on the next paste.
         if Desktop_Clipboard.Next_Deadline (Unsigned_64'Last) /= Unsigned_64'Last
           and then Desktop_Clipboard.Expire (nowMs) and then clipboardMenuOpen
         then
            reloadClipboardMenu;
            scheduleRedrawRect (inflateRect (clipboardMenuRect, 4));
         end if;
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
              Desktop_Clipboard.Next_Deadline (Unsigned_64'Last) = Unsigned_64'Last and then
              DN.Toast_Due (notifications) = DN.Never and then
              TTP.Next_Deadline (tipState) = TTP.NEVER and then
              modeTrial.Current = Desktop_Mode_Trial.Idle and then
              not KR.Armed (keyRepeat) and then
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
                  gpuWaitMode : Desktop_GPU_Wake.Wait_Mode;
                  use type Desktop_GPU_Wake.Wait_Mode;
               begin
                  if statusDueMs /= 0 and then
                    (nextDueMs = 0 or else statusDueMs < nextDueMs)
                  then nextDueMs := statusDueMs; end if;
                  declare
                     secretDue : constant Unsigned_64 :=
                       Desktop_Clipboard.Next_Deadline (Unsigned_64'Last);
                  begin
                     if secretDue /= Unsigned_64'Last and then
                       (nextDueMs = 0 or else secretDue < nextDueMs)
                     then nextDueMs := secretDue; end if;
                  end;
                  --  The toast's end (Desktop_Notifications; refreshStatus).
                  if DN.Toast_Due (notifications) /= DN.Never and then
                    (nextDueMs = 0 or else DN.Toast_Due (notifications) < nextDueMs)
                  then nextDueMs := DN.Toast_Due (notifications); end if;
                  --  A tooltip's delay (Client_Tooltip_Policy; GETTIME ms).
                  if TTP.Next_Deadline (tipState) /= TTP.NEVER and then
                    (nextDueMs = 0 or else TTP.Next_Deadline (tipState) < nextDueMs)
                  then nextDueMs := TTP.Next_Deadline (tipState); end if;
                  --  "Keep this display mode?": its countdown and deadline.
                  if modeTrial.Current = Desktop_Mode_Trial.Asking then
                     declare
                        trialDue : constant Unsigned_64 := Unsigned_64
                          (Desktop_Mode_Trial.Next_Due (modeTrial, Desktop_Mode_Trial.Time_Of (now)));
                     begin
                        if nextDueMs = 0 or else trialDue < nextDueMs then nextDueMs := trialDue; end if;
                     end;
                  end if;
                  --  The next auto-repeat (Key_Repeat; same clock as GETTIME).
                  if KR.Armed (keyRepeat) and then
                    (nextDueMs = 0 or else KR.Next_Due (keyRepeat) < nextDueMs)
                  then nextDueMs := KR.Next_Due (keyRepeat); end if;
                  if metricsDelayUs = 0 then
                     mayWait := False;
                  elsif mayWait and then metricsDelayUs /= Unsigned_64'Last then
                     declare Deadline : constant Unsigned_64 :=
                       Compositor_Metric_Batch_Policy.Wake_At_Ms (now, metricsDelayUs);
                     begin
                        if nextDueMs = 0 or else Deadline < nextDueMs then nextDueMs := Deadline; end if;
                     end;
                  end if;
                  -- GPU work in flight: sleep on its completion event when the
                  -- glue can arm one (Desktop_GPU_Wake), else the 1 ms timer.
                  Desktop_GPU_Wake.Prepare_Wait (renderingPending, requestSequence, now, gpuWaitMode);
                  if gpuWaitMode = Desktop_GPU_Wake.Look_Again then
                     mayWait := False;
                  elsif renderingPending or else sourceRetirementPending or else outputDrainRequested or else outputReopenPending then
                     -- Bounded fallback: the event's (a lost answer), or the
                     -- timer until renderer completion has an event. Input
                     -- can wake this wait immediately.
                     declare
                        Step : constant Unsigned_64 :=
                          (if gpuWaitMode = Desktop_GPU_Wake.Event and then not sourceRetirementPending
                             and then not outputDrainRequested and then not outputReopenPending
                           then Desktop_GPU_Wake.Event_Fallback_Ms else Desktop_GPU_Wake.Timer_Ms);
                     begin
                        if now < Unsigned_64'Last - Step then
                           if nextDueMs = 0 or else now + Step < nextDueMs then nextDueMs := now + Step; end if;
                        else mayWait := False;
                        end if;
                     end;
                  end if;
                  if not outputDrainRequested and then not outputReopenPending and then
                    (framePending or else cursorPresentPending) then
                     mayWait := False;
                  end if;

                  if mayWait and then nextDueMs /= 0 then
                     activity := Wait_For_Activity_Until (nextDueMs);
                     if activity = Unavailable then running := False; end if;
                     if renderingPending then
                        Desktop_GPU_Wake.Woke (gpuWaitMode, now, nowMs);
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

   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
