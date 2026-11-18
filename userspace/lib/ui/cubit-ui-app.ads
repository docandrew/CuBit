------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Minimal desktop-surface harness for native CuBit UI applications
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Grant_References;
with CuBit.Messages;
with CuBit.Async_Requests;
with Client_Frame_Pair;
with Client_Input_Provenance;
with Client_Surface_Ring;
with Compositor_Surface_Wire;

with CuBit.UI;
with CuBit.UI.Input;
with CuBit.UI.Controls;
with CuBit.UI.State;

package CuBit.UI.App is
   --  The event encoding lives in the pure CuBit.UI.Input; these names are
   --  kept for existing applications.
   INPUT_NONE          : Unsigned_64 renames CuBit.UI.Input.INPUT_NONE;
   INPUT_KEY_DOWN      : Unsigned_64 renames CuBit.UI.Input.INPUT_KEY_DOWN;
   INPUT_KEY_UP        : Unsigned_64 renames CuBit.UI.Input.INPUT_KEY_UP;
   INPUT_POINTER_MOVE  : Unsigned_64 renames CuBit.UI.Input.INPUT_POINTER_MOVE;
   INPUT_POINTER_DOWN  : Unsigned_64 renames CuBit.UI.Input.INPUT_POINTER_DOWN;
   INPUT_POINTER_UP    : Unsigned_64 renames CuBit.UI.Input.INPUT_POINTER_UP;
   INPUT_TEXT          : Unsigned_64 renames CuBit.UI.Input.INPUT_TEXT;
   INPUT_POINTER_WHEEL : Unsigned_64 renames CuBit.UI.Input.INPUT_POINTER_WHEEL;
   INPUT_CONFIGURE     : Unsigned_64 renames CuBit.UI.Input.INPUT_CONFIGURE;
   INPUT_RESYNC        : Unsigned_64 renames CuBit.UI.Input.INPUT_RESYNC;

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;
   KEY_F   : constant Unsigned_64 := 16#21#;
   KEY_R   : constant Unsigned_64 := 16#13#;
   KEY_M   : constant Unsigned_64 := 16#32#;

   KEYMOD_SHIFT : constant Unsigned_64 := 1;
   KEYMOD_CTRL  : constant Unsigned_64 := 2;
   KEYMOD_ALT   : constant Unsigned_64 := 4;
   KEYMOD_CAPS  : constant Unsigned_64 := 8;

   WINDOW_FLAG_DECORATED   : constant Unsigned_64 := 1;
   WINDOW_FLAG_RESIZABLE   : constant Unsigned_64 := 2;
   WINDOW_FLAG_MINIMIZABLE : constant Unsigned_64 := 4;
   WINDOW_FLAG_MAXIMIZABLE : constant Unsigned_64 := 8;
   WINDOW_FLAG_CLOSEABLE   : constant Unsigned_64 := 16;
   WINDOW_FLAG_FIXED_SIZE  : constant Unsigned_64 := 128;

   subtype Input_Event is CuBit.UI.Input.Input_Event;

   --  Normal controls repaint only when their visual state can change.
   --  Canvases and similar position-sensitive surfaces can explicitly opt in
   --  to receiving a repaint for every pointer-motion event.
   type Pointer_Repaint_Policy is
     (Repaint_Changed_Controls, Repaint_Every_Motion);

   type Pointer_Interaction is private;

   type Window is limited private;

   -- A removed surface stops input/painting, but retains frame retirement state.
   function Input_Stopped (win : Window) return Boolean;

   -- Protected frames are enabled per client during migration. Raw-renderer
   -- callers retain the existing attachment path until their density handling
   -- is adapted. Run manages acquisition, repair and publication automatically.
   -- Every window opens its surface ring (Compositor_Surface_Ring, IPC-004):
   -- after Open, presenting, publishing, retirement and input go through
   -- shared memory, with a system call only to wake a side that sleeps.
   -- Initial client dimensions and optional smaller resize minima are separate.
   -- A zero minimum preserves the initial-size minimum of existing callers.
   -- Explicit minima must not exceed initial dimensions; fixed-size windows
   -- retain initial dimensions regardless of smaller requested minima.
   -- There is no maximum: a resizable window grows to whatever Desktop
   -- offers (frame buffers grow with it); a fixed-size one keeps its size.
   procedure Open
      (win : in out Window;
       width, height : Natural;
       flags : Unsigned_64;
       ok : out Boolean;
       title : String := "Application";
       protected_frames : Boolean := False;
       minimum_width : Natural := 0;
       minimum_height : Natural := 0;
       preferred_size : Boolean := False);
   -- preferred_size: width x height is the size the application is designed
   -- for (a compact tool), and the window opens at it (cut only to fit the
   -- work area); otherwise it is a preference the desktop's placement policy
   -- may enlarge (Desktop_Protocol.Window_Sizing).

   -- Ask the desktop to resize the open window to width x height of client
   -- area, with new resize minima (for a layout that needs more room, such
   -- as another mode of the same app). The maxima Open set stay. ok: the
   -- desktop accepted both; the new size arrives as a configure event (it
   -- may be cut to fit the work area).
   procedure Request_Size
      (win : in out Window;
       width, height : Natural;
       minimum_width, minimum_height : Natural;
       ok : out Boolean);

   procedure Set_Title (win : Window; title : String);

   function Is_Open (win : Window) return Boolean;
   function Surface_ID (win : Window) return Unsigned_64;
   function Width (win : Window) return Natural;
   function Height (win : Window) return Natural;
   function Full_Rect (win : Window) return CuBit.UI.Rect;
   function Canvas (win : Window) return CuBit.UI.Canvas;
   function Canvas
      (win : Window; clip : CuBit.UI.Rect) return CuBit.UI.Canvas;

   --  The next event from the window's ring, if one has arrived; never
   --  blocks and makes no system call (it keeps one input request
   --  outstanding, answered with a batch of events when input is queued).
   procedure Poll_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean);

   --  Sleep until the next event (or the deadline). Desktop wakes the window
   --  only when it answers while the window sleeps: one system call per
   --  wake. Found=False: the window stopped (Wait_Input) or the deadline
   --  passed (Wait_Input_Until).
   procedure Wait_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean);

   --  Same event-driven wait, bounded by an absolute monotonic millisecond
   --  deadline. Found=False on expiry; it is not a window-close indication.
   --  Zero means no deadline, as in Wait_Input.
   procedure Wait_Input_Until
      (win : in out Window;
       deadline : Interfaces.Unsigned_64;
       event : out Input_Event;
       found : out Boolean);

   procedure Submit_Input_Wait
     (win : in out Window; token : Unsigned_64; accepted : out Boolean;
      deadline : Unsigned_64 := 0);
   function Input_Wait_Pending (win : Window) return Boolean;
   procedure Complete_Input_Wait
     (win : in out Window; receipt : CuBit.Messages.CompletionEntry;
      event : out Input_Event; found, consumed, healthy : out Boolean);
   -- Completion-queue alternative for loops that drive the completion queue
   -- themselves. Tokens are process-wide and never reused. Deliver authenticated
   -- completion-queue entries; foreign entries remain unconsumed. Do not mix
   -- with Poll_Input/Wait_Input/Run (the ring's input): whichever starts first
   -- owns the window's input. Closing the process retires the outstanding
   -- request; there is no borrowed buffer.

   --  True only when the compositor explicitly reported another queued event
   --  with the last input reply. It is a drain hint, never authority or an
   --  assertion that a later event cannot arrive.
   function Input_May_Remain (win : Window) return Boolean;
   -- Events already received from the ring and not yet taken; no IPC.
   function Cached_Input_Count (win : Window) return Natural;
   -- Consume at most one already-received event; never request more.
   -- Configure/resync application retains its normal theme/buffer work.
   procedure Poll_Cached_Input
     (win : in out Window; event : out Input_Event; found : out Boolean);

   type Input_Diagnostics is record
      Batch_Enabled, Channel_Disabled : Boolean := False;
      Successful_Fetches, Fetched_Events, Delivered_Events : Unsigned_64 := 0;
      Fallback_Polls, Cache_Rejections : Unsigned_64 := 0;
   end record;
   -- Read-only, allocation-free snapshot. Counters saturate, reset on Open,
   -- and remain readable after Close. Successful_Fetches counts the ring's
   -- input answers, Fetched_Events the events they carried (validated),
   -- Delivered_Events those taken. Fallback_Polls stays 0 (there is no
   -- synchronous fallback); Cache_Rejections counts dropped ring answers.
   -- Channel_Disabled: the window's ring is not open.
   -- Like other Window operations, call on the window's owning event thread.
   function Input_Statistics (win : Window) return Input_Diagnostics;

   -- Explicit handler boundaries, not delivery acknowledgements. Custom event
   -- loops must bracket each consumed event to opt into input/frame tracing.
   -- Incorrect ordering disables attribution until the window is reopened.
   procedure Begin_Input_Event (win : in out Window; event : Input_Event);
   procedure Finish_Input_Event (win : in out Window; event : Input_Event);
   procedure Cancel_Paint (win : in out Window);

   --  Wheel deltas use a signed 32-bit wire field in payload1. Decode it
   --  without a range-checked modular-to-signed conversion in each app.
   function Pointer_Wheel_Delta (event : Input_Event) return Integer
     renames CuBit.UI.Input.Pointer_Wheel_Delta;

   --  Request compositor-owned pointer feedback for this surface.  Widget
   --  applications normally get this automatically from Apply_Pointer_Event.
   procedure Set_Pointer_Cursor
      (win : Window; cursor : CuBit.UI.Pointer_Cursor_Style);

   -- Managed callers acquire before drawing and repaint the returned repair
   -- rectangle from retained state. Canvas is null outside the paint interval.
   function Frame_Pending (win : Window) return Boolean;
   procedure Begin_Paint
     (win : in out Window; changed : CuBit.UI.Rect;
      repair : out CuBit.UI.Rect; ready : out Boolean);
   procedure Present
      (win : in out Window; damage : CuBit.UI.Rect);

   procedure Apply_Pointer_Event
       (interaction : in out Pointer_Interaction;
       ui : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       win : Window;
       event : Input_Event;
       dirty : in out CuBit.UI.Rect;
       repaint : Pointer_Repaint_Policy := Repaint_Changed_Controls);

   --  Defaults for Run's optional timed-work hooks: no deadline.
   function No_Deadline return Interfaces.Unsigned_64 is (0);
   --  Default for Run's completion hook: no other asynchronous work.
   procedure No_Completion
      (win : in out Window;
       receipt : CuBit.Messages.CompletionEntry;
       consumed : out Boolean;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean);
   procedure No_Deadline_Work
      (win : in out Window;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean) is null;

   --  The default recovery hook: nothing unsaved to keep.
   procedure No_Recovery (win : in out Window; reason : CuBit.UI.Input.Close_Reason) is null;

   generic
      --  Run owns pointer routing and minimal damage. Migrated controls update
      --  retained state before paint; Render declaratively reconciles them by
      --  stable ID. Handle_Event owns only application/domain semantics.
      ui : in out CuBit.UI.State.UI_State;
      controls : in out CuBit.UI.Controls.Control_Map;
      pointerRepaint : Pointer_Repaint_Policy := Repaint_Changed_Controls;
      with procedure Render
         (win : in out Window; damage : CuBit.UI.Rect);
      with procedure Handle_Event
         (win : in out Window;
          event : Input_Event;
          dirty : in out CuBit.UI.Rect;
          running : in out Boolean);
      --  Optional timed work (for example an embedded engine's scheduler).
      --  Next_Deadline returns an absolute monotonic millisecond, or zero
      --  for none; Run then waits for input only until that deadline and
      --  calls On_Deadline when it passes. The defaults keep the original
      --  input-only behaviour.
      with function Next_Deadline return Interfaces.Unsigned_64
         is No_Deadline;
      with procedure On_Deadline
         (win : in out Window;
          dirty : in out CuBit.UI.Rect;
          running : in out Boolean)
         is No_Deadline_Work;
      --  Run never blocks in a call: input and frames go through the
      --  window's surface ring, and it sleeps once per wake for the ring's
      --  answers, the program's completions and its deadline.
      --  Activity_Wait: the program has completions of its own (a
      --  filesystem wake: CuBit.Filesystem_Sessions); after each wake every
      --  completion goes to On_Completion, which says whether it was its
      --  own. Program tokens start at APPLICATION_TOKEN_FIRST.
      Activity_Wait : Boolean := False;
      with procedure On_Completion
         (win : in out Window;
          receipt : CuBit.Messages.CompletionEntry;
          consumed : out Boolean;
          dirty : in out CuBit.UI.Rect;
          running : in out Boolean)
         is No_Completion;
      --  Closing (PWR-003): procmgr's Stop (the kernel's control event)
      --  reaches every app as an INPUT_CLOSE_REQUEST whose reason is
      --  System_Shutdown (CuBit.UI.Input.Close_Reason_Of), with no
      --  per-app plumbing. Before Handle_Event sees it, Run calls
      --  Save_Recovery: an app with unsaved work writes a recovery copy
      --  there (CuBit.UI.Recovery: its place and format), never over the
      --  user's file, and promptly: shutdown is never stalled by default
      --  (procmgr ends an app after its grace). Handle_Event then ends Run
      --  (running := False) as for any close. Programs that wait without
      --  Activity_Wait see the Stop at their next input or deadline.
      with procedure Save_Recovery
         (win : in out Window; reason : CuBit.UI.Input.Close_Reason)
         is No_Recovery;
   procedure Run (win : in out Window);
   APPLICATION_TOKEN_FIRST : constant Interfaces.Unsigned_64 := 2 ** 48;


   procedure Close (win : in out Window);

private
   type Pointer_Interaction is record
      hovered  : CuBit.UI.Controls.Control_ID :=
        CuBit.UI.Controls.NO_CONTROL;
      captured : CuBit.UI.Controls.Control_ID :=
        CuBit.UI.Controls.NO_CONTROL;
      cursor   : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;
      controlsValid : Boolean := True;
   end record;

   type Window is limited record
      frames : Client_Frame_Pair.Owner;
      protectedFrames : Boolean := False;
      deferredDamage : CuBit.UI.Rect := (others => 0);
      firstManagedFrame : Boolean := False;
      provenance : Client_Input_Provenance.State;
      provenanceHealthy : Boolean := True;
      densityNumerator, densityDenominator : Positive := 1;
      surfaceId : Unsigned_64 := 0;
      flags : Unsigned_64 := 0;
      -- The maximum surface size the window's limits allow (0: none).
      maximumW, maximumH : Unsigned_64 := 0;
      bufferAddr : System.Address := System.Null_Address;
      bufferGrant : CuBit.Grant_References.Reference;
      bufferPages : Unsigned_64 := 0;
      width : Natural := 0;
      height : Natural := 0;
      pitch : Natural := 0;
      lastEvent : Unsigned_64 := 0;
      inputMayRemain : Boolean := False;
      inputStopped : Boolean := False;
      inputErrorReported : Boolean := False;
      --  An input-wait stall was logged (CuBit.UI.App.Report_Stop).
      inputStallReported : Boolean := False;
      inputStats : Input_Diagnostics;
      --  The surface ring and what came through it: events received and
      --  not yet taken (ringNext .. ringCount), whether Desktop said more
      --  were queued, the last serial received (the next request's After).
      ring : Client_Surface_Ring.Connection;
      ringEvents : Compositor_Surface_Wire.Event_Array;
      ringCount, ringNext : Natural := 0;
      ringMore : Boolean := False;
      --  Desktop's unrequested-input generation when the last input
      --  request went out (Client_Surface_Ring.Input_Generation).
      inputGeneration : Interfaces.Unsigned_32 := 0;
      --  An input request went out on this ring (the first always wakes:
      --  input queued before the ring opened told no ring).
      inputRequested : Boolean := False;
      ringAcked : Unsigned_64 := 0;
      --  Attached-buffer damage waiting for a frame slot in the ring.
      presentBacklog : CuBit.UI.Rect := (others => 0);
      --  Protected frames: query the configuration again before painting
      --  (opened, configured or resynchronized, or a publication refused).
      configStale : Boolean := True;
      inputRequest : CuBit.Async_Requests.Tracker;
      sentBye : Boolean := False;
   end record;
end CuBit.UI.App;
