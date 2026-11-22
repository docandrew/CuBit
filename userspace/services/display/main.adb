------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display scanout service prototype
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Log;
with CuBit.Boot_Milestone_Codes;
with CuBit.Boot_Milestones;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.System_Facts;
with CuBit.Messages; use CuBit.Messages;
with Display_Logs;
with CuBit.Display_Protocol;
with CuBit.Present_Timing;
with CuBit.Monotonic;
with CuBit.Published_Clock;
with CuBit.Desktop_Messages;
with CuBit.Memory_Grants;
with CuBit.Process_IDs.Text;
with CuBit.Channel_Contracts;
with CuBit.Capability_Grants;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Handles.Devices;
with CuBit.Startup_Tables;
with Startup;
with CuBit.Presentation_State;
with CuBit.Display_Pool_Protocol;
with CuBit.Display_Pool_Registry;
with CuBit.Display_Outputs;
with CuBit.Output_Discovery;
with CuBit.Output_Modes;
with CuBit.Detected_Outputs;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with Presentation_Test_Policy;
with Compositor_Repaint;
with Compositor_Damage;
with Compositor_Requests;
with CuBit.Backend_Targets;
with CuBit.Display_Planes;
with CuBit.Display_Plane_Protocol;
with CuBit.GPU_Plane_Protocol;
with CuBit.Display_Frame_Damage;
with CuBit.Display_Frame_Damage.Shared;
with CuBit.Display_Box_Copy;
with Display_Frame_Ring;
with Display_Frame_Rings;
with CuBit.Call_Census;
with CuBit.Readiness;

with CuBit.Console_Route;
procedure main is
   --  Every line also reaches logstore (Display_Logs), not only the console.
   procedure debugPrint (Text : String) renames Display_Logs.Write;
   package DSP renames CuBit.Display_Protocol;
   use type DSP.Frame_Outcome;
   use type DSP.Buffer_Disposition;
   package MG renames CuBit.Memory_Grants;
   package PS is new CuBit.Presentation_State;
   package Pool_Wire renames CuBit.Display_Pool_Protocol;
   package Pool_Registry renames CuBit.Display_Pool_Registry;
   use type DSP.Output_Number;
   package Outputs renames CuBit.Display_Outputs.Registry;
   package OD renames CuBit.Output_Discovery;
   package OM renames CuBit.Output_Modes;
   package Detected renames CuBit.Detected_Outputs;
   use type OM.Mode, OM.DP.Status_Code, OM.Rate_Choice, OM.DP.Wire_Message;
   package GM renames CuBit.Graphics_Metrics;
   package Repair renames Compositor_Repaint;
   package BT renames CuBit.Backend_Targets;
   package DPL renames CuBit.Display_Planes;
   package PP renames CuBit.Display_Plane_Protocol;
   package GP renames CuBit.GPU_Plane_Protocol;
   package FD renames CuBit.Display_Frame_Damage;
   --  The frame being presented's damage boxes (LAT-002): its record
   --  (CuBit.Display_Frame_Damage), or its one announced area.
   frameBoxes : FD.Damage;
   frameRecordWarned : Boolean := False;
   function Single_Box (x, y, w, h : Natural) return FD.Damage is
     ((Count => 1, Boxes => [1 => (FD.DP.Pixel_Coordinate (x), FD.DP.Pixel_Coordinate (y),
                                   FD.DP.Pixel_Extent (w), FD.DP.Pixel_Extent (h)),
                             others => (others => <>)]));
   use type DPL.Plan, DPL.Plan_Epoch, DPL.Backing_Kind, DPL.Plane_Count,
            DPL.Request_Count, DPL.Plane_Kind, DPL.Anchor_Coordinate;
   use type PP.Operation;
   use type GP.Position, GP.Scanout_Source;
   use type BT.Phase;
   backendCopies, repairCopies : GM.Counter;
   backendReporter, repairReporter : CuBit.Graphics_Metrics_IO.Reporter;
   use type OD.Output_Role, OD.Output_Source, OD.Query;
   use type Outputs.Mutation_Result, Outputs.Output_Reference;
   use type PS.Phase, PS.Admission;
   use ASCII;

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Get_Information);
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Attach_Buffer);
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Present_Rectangle);
   OP_DISPLAY_CLEAR         : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Clear);
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Get_Status);
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Acquire_Display);
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Release_Display);
   OP_DISPLAY_MAP_BACKBUFFER : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Map_Backbuffer);
   OP_DISPLAY_PRESENT_IMMEDIATE_RECT : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Present_Immediate_Rectangle);
   OP_DISPLAY_PRESENT_REGION : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Present_Region);
   OP_DISPLAY_PRESENT_IMMEDIATE_REGION : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Present_Immediate_Region);
   OP_OPEN_SESSION : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Open_Presentation_Session);
   OP_SUBMIT_FRAME : constant Unsigned_32 := DSP.Operation'Enum_Rep (DSP.Submit_Frame);

   OP_GPU_CLEAR         : constant Unsigned_32 := 16#0A03#;
   OP_GPU_GET_STATUS    : constant Unsigned_32 := 16#0A04#;
   OP_GPU_MAP_FRAMEBUFFER : constant Unsigned_32 := 16#0A05#;
   OP_GPU_PRESENT_BUFFER : constant Unsigned_32 := 16#0A07#;

   DISPLAY_OK              : constant Unsigned_64 := 0;
   DISPLAY_ERR_DENIED      : constant Unsigned_64 := 1;
   DISPLAY_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   DISPLAY_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   DISPLAY_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   DISPLAY_BACKEND_LINEAR_FB : constant Unsigned_64 := 1;
   DISPLAY_BACKEND_VIRTIO_GPU : constant Unsigned_64 := 3;

   DISPLAY_CAP_COPY_PRESENT : constant Unsigned_64 := 16#0001#;
   DISPLAY_CAP_GPU_PRESENT  : constant Unsigned_64 := 16#0004#;
   DISPLAY_CAP_PAGE_FLIP    : constant Unsigned_64 := 16#0008#;
   --  UI-020: Lend_Buffer lends the scanout buffers (three) to the owner.
   DISPLAY_CAP_LEND_SCANOUT : constant Unsigned_64 := 16#0010#;
   --  OP_GPU_MAP_FRAMEBUFFER word 1: a forwardable grant (to derive loans).
   GPU_MAP_FORWARDABLE : constant Unsigned_64 := 1;

   --  The GPU service (optional: without one, the firmware framebuffer).
   function Lookup_GPU return CuBit.Handles.Endpoint is
     (if Startup.Gpu.Present then Startup.Gpu.Value else CuBit.Handles.No_Endpoint);
   GPU_Service : constant CuBit.Handles.Endpoint := Lookup_GPU;

   type Rect is record
      x, y, w, h : Natural := 0;
   end record;
   type Gpu_Rect_Array is array (BT.Buffer) of Rect;
   --  Two scanout buffers (virtio-gpu) or three (Intel, DISP-000a): the
   --  output's BT.Count. Only the target (never the on-screen buffer) is
   --  written; a buffer's stale area is the damage presented since it was
   --  last written, repaired from the active buffer before reuse.
   subtype Gpu_Buffer_Index is BT.Buffer;
   type Gpu_Flag_Array is array (Gpu_Buffer_Index) of Boolean;
   type Lend_Grant_Array is array (CuBit.Display_Pool_Protocol.Buffer_Slot) of MG.Grant_Reference;
   type Lend_Flag_Array is array (CuBit.Display_Pool_Protocol.Buffer_Slot) of Boolean;
   type Gpu_Address_Array is
     array (Gpu_Buffer_Index) of System.Address;
   type Gpu_Grant_Array is
     array (Gpu_Buffer_Index) of CuBit.Memory_Grants.Grant_Reference;

   --  Selection and health are independent. A failed native backend never
   --  becomes a firmware backend. In particular, a GPU output carries no
   --  firmware address which an error path could accidentally write through.
   type Backend_Kind is (Not_Configured, Firmware_Framebuffer, Native_GPU);
   type Backend_Selection (Kind : Backend_Kind := Not_Configured) is record
      case Kind is
         when Firmware_Framebuffer =>
            Address : System.Address;
         when Not_Configured | Native_GPU => null;
      end case;
   end record;

   --  This is an owner-local routing number, not a primary-display role.
   --  Request handlers are serialized, but GPU replies are deferred. Their
   --  continuations carry captured output/session identity explicitly.
   subtype Output_Index is DSP.Output_Number range 0 .. 1;
   selectedOutput : Output_Index := 0;
   type Output_State is record
      fbWidth  : Natural := 0;
      fbHeight : Natural := 0;
      fbPitch  : Natural := 0;
      backend : Backend_Selection;
      srcAddr   : System.Address := System.Null_Address;
      srcWidth  : Natural := 0;
      srcHeight : Natural := 0;
      srcPitch  : Natural := 0;
      srcOwner  : Process_ID := No_Process;
      srcGrant : MG.Grant_Reference;
      srcAcquired : Boolean := False;
      Pool : Pool_Registry.State;
      Pool_Slot : Natural range 0 .. 3 := 0;
      --  UI-020: the pool's slots are the backend's scanout buffers, lent to
      --  the compositor (Lend_Buffer); its grants are the slots' one-page
      --  damage records. No copy: a frame names its slot as the target.
      lentPool : Boolean := False;
      --  The owner's endpoint, offered with its first loan request: read
      --  only (no send), used for nothing but deriving its loans (never a
      --  call: Desktop is above display in the IPC DAG). Closed with the
      --  session.
      ownerEndpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      --  Per scanout buffer, the GPU driver's forwardable grant (acquired,
      --  kept across sessions until a mode change) and, per slot, the loan
      --  derived from it for the owner (display's, revoked at session end).
      lendParent : Gpu_Grant_Array;
      lendParentLive : Gpu_Flag_Array := [others => False];
      lendChild : Lend_Grant_Array;
      lendChildLive : Lend_Flag_Array := [others => False];
      activeSession : Unsigned_64 := 0;
      lastFrame : Unsigned_64 := 0;
      frameState : PS.State;
      presentationFault : Boolean := False;
      gpuScanoutAddr : Gpu_Address_Array := [others => System.Null_Address];
      --  The GPU's grants behind gpuScanoutAddr, returned on a mode change.
      gpuScanoutGrant : Gpu_Grant_Array;
      Targets : BT.State;
      displayOwner : Process_ID := No_Process;
      currentOutput : Outputs.Output_Reference := Outputs.No_Output;
      leasedOutput : Outputs.Output_Reference := Outputs.No_Output;
      sourceOutput : Outputs.Output_Reference := Outputs.No_Output;
      sessionOutput : Outputs.Output_Reference := Outputs.No_Output;
      outputRebound : Boolean := False;
      gpuStale : Gpu_Rect_Array;
      --  A legacy (non-pool) present in flight: completed in collectFrames.
      legacyToken : Unsigned_64 := 0;
      legacyTarget : Gpu_Buffer_Index := 0;
      legacyDamage : Rect;
      pendingPresent : Boolean := False;
      pendingRect : Rect;
      --  Captured continuation; never resolve a completion through whatever
      --  output the most recently received request happened to select.
      backendToken : Unsigned_64 := 0;
      --  When backendToken was submitted (CuBit.Published_Clock milliseconds).
      backendStarted : Unsigned_64 := 0;
      backendFrame : DSP.Frame_Result;
      backendID : PS.Submission_ID := PS.No_Submission;
      backendTarget : Gpu_Buffer_Index := 0;
      backendDamage : Rect;
      --  Plan epoch the in-flight frame was rendered for (No_Epoch: none).
      frameEpoch : DPL.Plan_Epoch := DPL.No_Epoch;
   end record;
   outputStates : array (Output_Index) of Output_State;
   readyOutputs : array (Output_Index) of Boolean := [others => False];
   sessionSequence : Unsigned_64 := 0;
   backendSequence : Unsigned_64 := 0;
   deferredReply : Boolean := False;
   --  Each output's frame, answered when its present completes: through
   --  the frame ring it came from (Desktop's), or the kernel's reply to a
   --  frame request message (display-check's, an unpooled source).
   package Frame_Rings renames Display_Frame_Rings;
   use type Frame_Rings.Ring_Number;
   type Answer_Route is (No_Answer, Kernel_Reply, Ring_Answer);
   type Frame_Debt is record
      Route    : Answer_Route := No_Answer;
      Saved    : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
      Ring     : Frame_Rings.Ring_Number := Frame_Rings.No_Ring;
      Tag      : Display_Frame_Ring.RC.Token := 0;
      Taken_Us : Unsigned_64 := 0;
   end record;
   frameDebts : array (Output_Index) of Frame_Debt;
   --  KERN-013: what this loop consumed of each queue, against the kernel's
   --  readiness page; a queue that did not move is not polled.
   displayReady : CuBit.Readiness.Tracker;
   --  The ring present being served (No_Ring: a request message), and its
   --  damage boxes as the ring entry carried them.
   servingRing    : Frame_Rings.Ring_Number := Frame_Rings.No_Ring;
   servingTag     : Display_Frame_Ring.RC.Token := 0;
   servingTakenUs : Unsigned_64 := 0;
   servingDamage  : CuBit.Display_Frame_Damage.Word_Array := [others => 0];

   --  Set the request being served aside; False when the kernel refused.
   function saveInto (Saved : out CuBit.Handles.Reply) return Boolean is
      Result : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
   begin
      Saved := (if Result.Saved then Result.Handle else CuBit.Handles.No_Reply);
      return Result.Saved;
   end saveInto;

   --  Answer a saved request; it is used up.
   function answerSaved (Saved : in out CuBit.Handles.Reply; Msg : Message) return Unsigned_64 is
      Result : constant Unsigned_64 := replyCap (Saved, Msg);
   begin
      Saved := CuBit.Handles.No_Reply;
      return Result;
   end answerSaved;

   --  The frame being served will be answered later (its present is in
   --  flight): remember where. False: the kernel would not keep the reply.
   function holdFrameAnswer (Output : Output_Index) return Boolean is
   begin
      if servingRing /= Frame_Rings.No_Ring then
         frameDebts (Output) := (Ring_Answer, CuBit.Handles.No_Reply, servingRing, servingTag,
                                 servingTakenUs);
         return True;
      end if;
      frameDebts (Output) := (Route => Kernel_Reply, others => <>);
      if saveInto (frameDebts (Output).Saved) then
         return True;
      end if;
      frameDebts (Output) := (others => <>);
      return False;
   end holdFrameAnswer;

   --  Answer Output's held frame (once; nothing when its ring ended since).
   function answerFrame (Output : Output_Index; Msg : Message) return Unsigned_64 is
      Debt : Frame_Debt := frameDebts (Output);
      Result : Unsigned_64 := 0;
   begin
      frameDebts (Output) := (others => <>);
      case Debt.Route is
         when Kernel_Reply =>
            Result := answerSaved (Debt.Saved, Msg);
         when Ring_Answer =>
            Frame_Rings.Answer (Debt.Ring, Debt.Tag, CuBit.Desktop_Messages.To_Wire (Msg), Debt.Taken_Us);
         when No_Answer =>
            null;
      end case;
      return Result;
   end answerFrame;

   --  Ring Ended is gone: what any output still owed it is dropped (the
   --  frame itself still completes and returns its buffer).
   procedure forgetRing (Ended : Frame_Rings.Ring_Number) is
   begin
      for Debt of frameDebts loop
         if Debt.Route = Ring_Answer and then Debt.Ring = Ended then
            Debt := (others => <>);
         end if;
      end loop;
   end forgetRing;
   gpuDetected : Boolean := False;
   --  DISP-000a: the GPU (intel-gpu) flips the firmware mode's own scanout:
   --  same geometry, driver buffers, no firmware framebuffer mapping.
   intelTakeover : Boolean := False;
   outputCatalog : OD.Catalog;
   catalogReady : Boolean := False;
   --  Output mode changes (CuBit.Output_Modes): the catalog's revision, the
   --  GPU catalog revision last read and its outstanding watch, and the one
   --  client watch (Desktop's) saved until the catalog changes.
   catalogRevision : OD.Catalog_Revision := 1;
   gpuRevision : OM.Revision := 1;
   gpuWatchToken : Unsigned_64 := 0;
   outputWatchArmed : Boolean := False;
   outputWatch : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   -- Bind registry lifetime to this process incarnation, not a reusable PID
   -- or a constant shared by successive Display instances. The fallback is
   -- only an inert discriminant: registration rejects a failed capture.
   displayIdentity : constant Process_ID := CuBit.Capability_Grants.Incarnation
     (CuBit.Capability_Grants.Capture
        (CuBit.Handles.Startup.Process_Named (CuBit.Startup_Tables.Self_Process_Text)));
   outputRegistry : Outputs.State
     (Outputs.Registry_Incarnation (if Is_Process (displayIdentity) then To_Word (displayIdentity) else 1));

   function outputUsable (item : Outputs.Output_Reference) return Boolean is
     (Outputs.Live (outputRegistry, item) and then
      Outputs.Presentable (Outputs.Describe (outputRegistry, item)));

   function sourceOnCurrentOutput return Boolean is
     (outputStates (selectedOutput).sourceOutput = outputStates (selectedOutput).currentOutput and then outputUsable (outputStates (selectedOutput).sourceOutput));

   function registerOutput return Boolean is
      result : Outputs.Mutation_Result;
      driverIdentity : Process_ID := displayIdentity;
   begin
      if not Is_Process (displayIdentity) then return False; end if;
      if outputStates (selectedOutput).backend.Kind = Native_GPU then
         driverIdentity := CuBit.Capability_Grants.Incarnation
           (CuBit.Capability_Grants.Capture (GPU_Service));
         if not Is_Process (driverIdentity) or else not CuBit.Capability_Grants.Endpoint_Matches
           (GPU_Service, driverIdentity)
         then return False; end if;
      elsif outputStates (selectedOutput).backend.Kind /= Firmware_Framebuffer then
         return False;
      end if;
      if outputStates (selectedOutput).fbWidth not in 1 .. Natural (Outputs.W.Extent'Last) or else
        outputStates (selectedOutput).fbHeight not in 1 .. Natural (Outputs.W.Extent'Last)
      then
         return False;
      end if;
      --  Provisional name, not an EDID identity or a saved monitor preference.
      --  Ready describes the selected mapped backend, not panel visibility.
      Outputs.Register
        (outputRegistry,
         (Backend => (Driver => Outputs.Driver_Incarnation (To_Word (driverIdentity)),
                      Number => Outputs.Output_Number (selectedOutput)),
          Area => (Display => Outputs.L.Named_Display_ID
                     (Natural (selectedOutput) + 1),
                   X => 0, Y => 0,
                   Width => Outputs.W.Extent (outputStates (selectedOutput).fbWidth),
                   Height => Outputs.W.Extent (outputStates (selectedOutput).fbHeight)),
          Presence => Outputs.Present, Power => Outputs.Enabled,
          Stage => Outputs.Ready), outputStates (selectedOutput).currentOutput, result);
      return result = Outputs.Applied;
   end registerOutput;

   statsStartMs  : Unsigned_64 := 0;
   --  The stats line covers Desktop's latency report period (LAT-002), not
   --  one second: a line every second fed log viewers a new record each
   --  second, and a full Boot Logs repainted its whole window for each
   --  (laptop, image 27b). Graphics metrics still publish every second.
   STATS_LINE_PERIOD_MS : constant := 5_000;
   METRICS_PERIOD_MS    : constant := 1_000;
   metricsStartMs : Unsigned_64 := 0;
   statsRequests : Unsigned_64 := 0;
   statsQueued   : Unsigned_64 := 0;
   statsPresents : Unsigned_64 := 0;
   statsWaitMs   : Unsigned_64 := 0;
   statsCopyMs   : Unsigned_64 := 0;
   statsPixels   : Unsigned_64 := 0;
   --  LAT-002: a pool frame's time in display.svc, from its request to the
   --  backend Present (target repair and copy included), in microseconds.
   statsForwardUs    : Unsigned_64 := 0;
   statsForwardMaxUs : Unsigned_64 := 0;
   statsForwarded    : Unsigned_64 := 0;

   function memcpy
      (dest : System.Address;
       src  : System.Address;
       len  : Storage_Count)
      return System.Address with
      Import => True,
      Convention => C,
      External_Name => "memcpy";

   function backendId return Unsigned_64 is
   begin
      if outputStates (selectedOutput).backend.Kind = Native_GPU then
         return DISPLAY_BACKEND_VIRTIO_GPU;
      else
         return DISPLAY_BACKEND_LINEAR_FB;
      end if;
   end backendId;

   function backendCaps return Unsigned_64 is
   begin
      if outputStates (selectedOutput).backend.Kind = Native_GPU then
         --  The owner may compose into the scanout buffers themselves
         --  (UI-020): display derives each loan from the GPU driver's
         --  forwardable grant (kernel-tracked, one buffer, display's to
         --  revoke), never re-granting its own mapping.
         return DISPLAY_CAP_COPY_PRESENT or DISPLAY_CAP_GPU_PRESENT or
           DISPLAY_CAP_PAGE_FLIP or
           (if BT.Count (outputStates (selectedOutput).Targets) = Pool_Wire.Buffer_Slot'Last
            then DISPLAY_CAP_LEND_SCANOUT else 0);
      else
         --  A bootloader/firmware linear framebuffer does not provide a
         --  trustworthy vertical-blank event. In particular, polling the
         --  legacy VGA status port on modern Intel hardware can spin forever
         --  (and used to cost one syscall per poll). Present immediately until
         --  a hardware display backend supplies real flip completion events.
         return DISPLAY_CAP_COPY_PRESENT;
      end if;
   end backendCaps;

   --  The longest display waits for one intel-gpu answer (IPC-004): past
   --  it the call fails like any refused request, never hangs the display.
   GPU_Call_Deadline_Ms : constant := 10_000;

   function callGpu
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label => label, length => 4, protocol => 0, ipc => <>,
                     reserved => Unsigned_16 (selectedOutput)),
         authorityTag => 0,
         words    => [w0, w1, w2, w3]);
      tag : MessageTag;
   begin
      tag := capCall (GPU_Service, msg,
                      CuBit.Messages.Deadline_After (GPU_Call_Deadline_Ms));
      if tag.label = CuBit.Messages.REPLY_TIMEOUT then
         debugPrint ("display: gpu call timed out" & LF);
      end if;
      msg.tag := tag;
      return msg;
   end callGpu;

   procedure setupBackend is
      status : Message;
      gpuPrimary : constant Boolean := CuBit.System_Facts.Boot_Display_Native;
      --  The firmware framebuffer (kernel system facts publication).
      Firmware : constant CuBit.System_Facts.SP.Framebuffer_Geometry :=
        CuBit.System_Facts.Framebuffer;
      State : Output_State renames outputStates (selectedOutput);
      Width, Height, Pitch, Bits, Mapping : Unsigned_64;
      Layout : DSP.Buffer_Layout;
   begin
      status := callGpu (OP_GPU_GET_STATUS);
      gpuDetected := status.tag = (label => OP_GPU_GET_STATUS, length => 4, protocol => 0, ipc => <>, reserved => 0) and then status.words (0) = 0 and then
        status.words (1) = 1;
      if not gpuPrimary and then gpuDetected then
         --  A driver that took over the firmware mode's own scanout (Intel,
         --  DISP-000a) describes it as that framebuffer, same geometry.
         declare
            Scanout : constant GP.Scanout_Decoding :=
              GP.Decode_Scanout (CuBit.Desktop_Messages.To_Wire
                (callGpu (GP.Code (GP.Describe_Scanout))));
         begin
            intelTakeover := Scanout.Valid and then
              Scanout.Value.Source = GP.Firmware_Framebuffer and then
              Unsigned_64 (Scanout.Value.Width) = Firmware.Width and then
              Unsigned_64 (Scanout.Value.Height) = Firmware.Height and then
              Unsigned_64 (Scanout.Value.Pitch) = Firmware.Pitch and then
              status.words (2) = Firmware.Width and then
              status.words (3) = Firmware.Height;
         end;
      end if;
      if intelTakeover then
         Width := status.words (2);
         Height := status.words (3);
         Pitch := Firmware.Pitch;
         Bits := 32;
      elsif gpuPrimary then
         --  Once the boot adapter is driven natively its old firmware mode is
         --  not a fallback, even if driver startup failed part-way through.
         if not gpuDetected then return; end if;
         Width := status.words (2);
         Height := status.words (3);
         Pitch := Width * 4;
         Bits := 32;
      else
         Width := Firmware.Width;
         Height := Firmware.Height;
         Pitch := Firmware.Pitch;
         Bits := Firmware.Bits_Per_Pixel;
      end if;
      if Width not in 1 .. Unsigned_64 (DSP.DP.Positive_Extent'Last) or else
        Height not in 1 .. Unsigned_64 (DSP.DP.Positive_Extent'Last) or else
        Pitch > Unsigned_64 (DSP.DP.Buffer_Pitch'Last) or else Bits /= 32
      then
         return;
      end if;
      Layout := (DSP.DP.Positive_Extent (Width), DSP.DP.Positive_Extent (Height),
                 Natural (Pitch));
      if not DSP.DP.Valid_Layout (Layout) then return; end if;
      if intelTakeover then
         --  The driver flips between its own three buffers at vblank; the
         --  firmware framebuffer is not mapped and is never written again.
         State.backend := (Kind => Native_GPU);
         BT.Set_Count (State.Targets, BT.Maximum_Buffers);
         debugPrint ("display: backend intel flips (tear-free, 3 buffers)" &
           Width'Image & " x" & Height'Image & LF);
      elsif gpuPrimary then
         --  devmgr publishing the boot display handoff retires kernel boot drawing before
         --  driver startup. Native outputs need no firmware mapping, even
         --  for diagnostics: kernel panic cannot reactivate that renderer.
         State.backend := (Kind => Native_GPU);
         --  Three buffers, as Intel: one shown, one latching, one composing.
         BT.Set_Count (State.Targets, BT.Maximum_Buffers);
         debugPrint ("display: backend virtio-gpu (3 buffers)" & LF);
         debugPrint ("display: native boot handoff" &
           Firmware.Width'Image & " x" & Firmware.Height'Image &
           " ->" & Width'Image & " x" & Height'Image & "; firmware rendering retired" & LF);
      else
         Mapping := (if Startup.Framebuffer.Present
                     then CuBit.Handles.Devices.Map_Framebuffer (Startup.Framebuffer.Value)
                     else Unsigned_64'Last);
         if Mapping = 0 or else Mapping = Unsigned_64'Last then
            --  The kernel's "kernel: device-memory denied" record names the
            --  physical pages and why; this names the framebuffer.
            CuBit.Log.Warning ("display: MAPFB refused for the firmware framebuffer" &
              Firmware.Width'Image & " x" & Firmware.Height'Image &
              " (see the kernel device-memory record)");
            return;
         end if;
         State.backend :=
           (Firmware_Framebuffer, To_Address (Integer_Address (Mapping)));
         if gpuDetected then
            debugPrint ("display: gpu not primary, using linear-fb" & LF);
         elsif status.tag = (label => OP_GPU_GET_STATUS, length => 4, protocol => 0, ipc => <>, reserved => 0) and then status.words (0) = 0 then
            --  intel-gpu answered but cannot flip (its log says why).
            debugPrint ("display: gpu scanout unavailable, using linear-fb" & LF);
         else
            debugPrint ("display: backend linear-fb" & LF);
         end if;
      end if;
      State.fbWidth := Natural (Width);
      State.fbHeight := Natural (Height);
      State.fbPitch := Natural (Pitch);
   end setupBackend;

   function connectorName (Item : OD.Connector_Kind) return String is
     (case Item is
        when OD.Unknown_Connector => "output", when OD.HDMI => "HDMI", when OD.DVI => "DVI",
        when OD.DisplayPort => "DisplayPort", when OD.Embedded_DisplayPort => "eDP");

   --  DISP-009/010: the GPU catalog's attached-but-not-lit outputs (entries
   --  after the first) replace Into's; Changed when Into differs. A walk
   --  that goes stale or invalid leaves Into as it was.
   procedure readDetected (From : OD.Summary; Into : in out OD.Catalog; Changed : out Boolean) is
      Fresh : OD.Catalog;
      Response : Message;
      Item : OD.Description_Decoding;
   begin
      Changed := False;
      for Index in 2 .. From.Count loop
         Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Description), From.Revision,
                              Unsigned_64 (Index));
         Item := OD.Decode_Description (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
         if not Item.Valid or else Item.Value.Requested /= (From.Revision, Index) then
            debugPrint ("display: intel detected-output walk failed at" & Index'Image & LF);
            return;
         end if;
         if Detected.Not_Lit (Item.Value.Item) and then Item.Value.Item.Source = OD.Intel_Display then
            Fresh.Count := Fresh.Count + 1;
            Fresh.Items (Fresh.Count) := Item.Value.Item;
         end if;
      end loop;
      Detected.Replace (Into, OD.Intel_Display, Fresh, Changed);
      for Index in 1 .. Into.Count loop
         if Detected.Not_Lit (Into.Items (Index)) then
            declare
               D : OD.Description renames Into.Items (Index);
            begin
               debugPrint ("display: intel output" & D.Native_Number'Image & " " &
                 connectorName (D.Connector) & " connected (not in use)" &
                 D.Width_MM'Image & " x" & D.Height_MM'Image & " mm, best mode" &
                 D.Advertised_Width'Image & " x" & D.Advertised_Height'Image & LF);
            end;
         end if;
      end loop;
   end readDetected;

   procedure discoverOutputs is
      Candidate : OD.Catalog;
      Seen : array (OD.Native_Output_Number) of Boolean := [others => False];
      Selected : Boolean := False;
      Response : Message;
      Summary : OD.Summary_Decoding;
      Item : OD.Description_Decoding;
   begin
      if outputStates (selectedOutput).backend.Kind /= Native_GPU or else intelTakeover then
         if outputStates (selectedOutput).fbWidth not in OD.Extent or else outputStates (selectedOutput).fbHeight not in OD.Extent then
            return;
         end if;
         Candidate.Count := 1;
         Candidate.Items (1) :=
           (OD.Selected_For_Desktop, OD.Boot_Framebuffer, 0,
            outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight, 0, 0,
            OD.Lit, OD.Unknown_Connector,
            outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight);
         Selected := True;
      end if;
      --  The Intel takeover drives the boot output: intel-gpu's catalog says
      --  which monitor it is (EDID physical size) and its best admitted
      --  mode (Advertised), which Desktop may ask for (Set_Mode).
      if intelTakeover then
         Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Catalog));
         Summary := OD.Decode_Summary (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
         if Summary.Valid and then Summary.Value.Count >= 1 then
            Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Description),
                                 Summary.Value.Revision, 1);
            Item := OD.Decode_Description (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
            if Item.Valid and then Item.Value.Item.Source = OD.Intel_Display and then
              Item.Value.Item.Role = OD.Backend_Ready and then
              Item.Value.Item.Current_Width = outputStates (selectedOutput).fbWidth and then
              Item.Value.Item.Current_Height = outputStates (selectedOutput).fbHeight
            then
               Candidate.Items (1) :=
                 (OD.Selected_For_Desktop, OD.Intel_Display, 0,
                  Item.Value.Item.Advertised_Width, Item.Value.Item.Advertised_Height,
                  Item.Value.Item.Width_MM, Item.Value.Item.Height_MM,
                  OD.Lit, Item.Value.Item.Connector,
                  outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight);
               debugPrint ("display: intel output monitor" &
                 Item.Value.Item.Width_MM'Image & " x" & Item.Value.Item.Height_MM'Image & " mm, best mode" &
                 Item.Value.Item.Advertised_Width'Image & " x" & Item.Value.Item.Advertised_Height'Image & LF);
               --  A better mode than the firmware's: start at revision 2 so
               --  Desktop's first watch (Known 1) sees it at once.
               if Item.Value.Item.Advertised_Width /= outputStates (selectedOutput).fbWidth or else
                 Item.Value.Item.Advertised_Height /= outputStates (selectedOutput).fbHeight
               then
                  catalogRevision := 2;
               end if;
               --  DISP-009/010: monitors attached to ports no pipe drives.
               gpuRevision := Summary.Value.Revision;
               declare
                  Changed : Boolean;
               begin
                  readDetected (Summary.Value, Candidate, Changed);
                  if not Changed then
                     debugPrint ("display: intel output catalog: no monitor attached beyond the lit one" & LF);
                  end if;
               end;
            end if;
         end if;
      end if;
      --  The Intel takeover drives the boot output itself (same mode); it
      --  is listed as that output, with no GPU catalog of its own yet.
      if gpuDetected and then not intelTakeover then
         Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Catalog));
         Summary := OD.Decode_Summary
           (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
         if not Summary.Valid or else Summary.Value.Count > 16 then
            return;
         end if;
         gpuRevision := Summary.Value.Revision;
         for Index in 1 .. Summary.Value.Count loop
            Response := callGpu
              (OD.Code (OD.GPU_Backend, OD.Get_Description),
               Summary.Value.Revision, Unsigned_64 (Index));
            Item := OD.Decode_Description
              (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
            if not Item.Valid or else
               Item.Value.Requested /= (Summary.Value.Revision, Index) or else
               Item.Value.Item.Source /= OD.Virtio_GPU or else
               Item.Value.Item.Role = OD.Selected_For_Desktop or else
               Seen (Item.Value.Item.Native_Number)
            then
               return;
            end if;
            Seen (Item.Value.Item.Native_Number) := True;
            if outputStates (selectedOutput).backend.Kind = Native_GPU and then Item.Value.Item.Native_Number = 0 then
               if Item.Value.Item.Role /= OD.Backend_Ready or else
                  Item.Value.Item.Current_Width /= outputStates (selectedOutput).fbWidth or else
                  Item.Value.Item.Current_Height /= outputStates (selectedOutput).fbHeight
               then
                  return;
               end if;
               Item.Value.Item :=
                 (OD.Selected_For_Desktop, OD.Virtio_GPU, 0,
                  Item.Value.Item.Advertised_Width,
                  Item.Value.Item.Advertised_Height, Item.Value.Item.Width_MM, Item.Value.Item.Height_MM,
                  OD.Lit, Item.Value.Item.Connector,
                  outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight);
               Selected := True;
            end if;
            Candidate.Count := Candidate.Count + 1;
            Candidate.Items (Candidate.Count) := Item.Value.Item;
         end loop;
      end if;
      --  Publish only the complete validated startup snapshot. Discovery
      --  failure must not take down an otherwise working boot desktop.
      if Selected then
         outputCatalog := Candidate;
         catalogReady := True;
      end if;
   end discoverOutputs;

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

   statsCensusAt : CuBit.Call_Census.Counts := [others => 0];
   procedure maybePrintStats is
      now : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
   begin
      if now = Unsigned_64'Last then
         return;
      end if;

      if statsStartMs = 0 then
         statsStartMs := now;
         metricsStartMs := now;
         return;
      end if;

      if now >= metricsStartMs and then now - metricsStartMs >= METRICS_PERIOD_MS then
         metricsStartMs := now;
         CuBit.Graphics_Metrics_IO.Publish
           (GM.Display_Backend, backendCopies, backendReporter);
         CuBit.Graphics_Metrics_IO.Publish
           (GM.Display_Repair, repairCopies, repairReporter);
      end if;

      if now < statsStartMs or else now - statsStartMs < STATS_LINE_PERIOD_MS then
         return;
      end if;

      if statsPresents > 0 then
         debugPrint ("display: stats req=");
         printDec (statsRequests);
         debugPrint (" queued=");
         printDec (statsQueued);
         debugPrint (" presents=");
         printDec (statsPresents);
         debugPrint (" wait_ms=");
         printDec (statsWaitMs);
         debugPrint (" present_ms=");
         printDec (statsCopyMs);
         debugPrint (" px=");
         printDec (statsPixels);
         debugPrint (" forward_us_avg=");
         printDec (if statsForwarded = 0 then 0 else statsForwardUs / statsForwarded);
         debugPrint (" forward_us_max=");
         printDec (statsForwardMaxUs);
         --  This period's calls by call number (CuBit.Call_Census).
         debugPrint (" calls");
         declare
            Taken : constant CuBit.Call_Census.Counts := CuBit.Call_Census.Census;
         begin
            for N in CuBit.Call_Census.Number loop
               if Taken (N) /= statsCensusAt (N) then
                  debugPrint (" ");
                  printDec (Unsigned_64 (N));
                  debugPrint (":");
                  printDec (Taken (N) - statsCensusAt (N));
               end if;
            end loop;
         end;
         debugPrint ("" & LF);
      end if;
      statsCensusAt := CuBit.Call_Census.Census;

      statsStartMs := now;
      statsRequests := 0;
      statsQueued := 0;
      statsPresents := 0;
      statsWaitMs := 0;
      statsCopyMs := 0;
      statsPixels := 0;
      statsForwardUs := 0;
      statsForwardMaxUs := 0;
      statsForwarded := 0;
   end maybePrintStats;

   function ownsDisplay (pid : Process_ID) return Boolean is
   begin
      return pid /= No_Process and then outputStates (selectedOutput).displayOwner = pid and then
        outputStates (selectedOutput).leasedOutput = outputStates (selectedOutput).currentOutput and then outputUsable (outputStates (selectedOutput).leasedOutput);
   end ownsDisplay;

   --  UI-020: the session's loans end (display owns the derived grants)
   --  and the owner's endpoint is closed.
   procedure endLoans (State : in out Output_State) is
      Revoked : Boolean;
      Closed : CuBit.Handles.IPC.Close_Result;
      pragma Unreferenced (Closed);
   begin
      for B in Pool_Wire.Buffer_Slot loop
         if State.lendChildLive (B) then
            MG.Revoke (State.lendChild (B), Revoked);
            if not Revoked then debugPrint ("display: loan revoke refused" & LF); end if;
            State.lendChildLive (B) := False;
         end if;
      end loop;
      if CuBit.Handles.Valid (State.ownerEndpoint) then
         Closed := CuBit.Handles.IPC.Close (State.ownerEndpoint);
         State.ownerEndpoint := CuBit.Handles.No_Endpoint;
      end if;
   end endLoans;

   procedure detachOwnerBuffer is
      returned : Boolean;
      Empty_Pool : Pool_Registry.State;
   begin
      -- Invalidate before releasing the attachment. Old queued frame requests
      -- can then only be rejected, never read after a release acknowledgement.
      outputStates (selectedOutput).activeSession := 0;
      outputStates (selectedOutput).sessionOutput := Outputs.No_Output;
      outputStates (selectedOutput).sourceOutput := Outputs.No_Output;
      outputStates (selectedOutput).lastFrame := 0;
      --  Admission rejects detach while the per-output frame model is busy.
      --  Discard queued damage and all local pointers before returning the pin.
      outputStates (selectedOutput).srcAddr := System.Null_Address;
      outputStates (selectedOutput).srcWidth := 0;
      outputStates (selectedOutput).srcHeight := 0;
      outputStates (selectedOutput).srcPitch := 0;
      outputStates (selectedOutput).srcOwner := No_Process;
      outputStates (selectedOutput).pendingPresent := False;
      outputStates (selectedOutput).pendingRect := (others => 0);
      outputStates (selectedOutput).gpuStale := [others => (others => 0)];
      outputStates (selectedOutput).lentPool := False;
      endLoans (outputStates (selectedOutput));
      for B in Pool_Wire.Buffer_Slot loop
         if Pool_Registry.Registered (outputStates (selectedOutput).Pool, B) then
            MG.Return_Acquisition (Pool_Registry.Item (outputStates (selectedOutput).Pool, B).Grant, returned);
            if not returned then outputStates (selectedOutput).presentationFault := True; end if;
         end if;
      end loop;
      if not outputStates (selectedOutput).presentationFault then
         outputStates (selectedOutput).Pool := Empty_Pool;
         outputStates (selectedOutput).Pool_Slot := 0;
      else
         Pool_Registry.Quarantine (outputStates (selectedOutput).Pool);
      end if;
      if outputStates (selectedOutput).srcAcquired then
         MG.Return_Acquisition (outputStates (selectedOutput).srcGrant, returned);
         if not returned then
            outputStates (selectedOutput).presentationFault := True;
            debugPrint ("display: buffer acquisition return failed" & LF);
         end if;
         outputStates (selectedOutput).srcAcquired := False;
      end if;
   end detachOwnerBuffer;

   --  BUG-001: an owner whose process ended (the desktop crashed) still
   --  holds the lease, so its restart would be refused forever. The grants
   --  of the buffers it lent end with it: re-acquiring one tells. False when
   --  it lent none (nothing to ask) or the grant is still live.
   function ownerEnded return Boolean is
      State : Output_State renames outputStates (selectedOutput);
      Grant : MG.Grant_Reference;
      Found : Boolean := False;
      Mapped : System.Address;
      Acquired, Returned : Boolean;
   begin
      if State.srcOwner = No_Process or else State.srcOwner /= State.displayOwner then
         return False;
      end if;
      for B in Pool_Wire.Buffer_Slot loop
         if not Found and then Pool_Registry.Registered (State.Pool, B) then
            Grant := Pool_Registry.Item (State.Pool, B).Grant;
            Found := True;
         end if;
      end loop;
      if not Found and then State.srcAcquired then
         Grant := State.srcGrant;
         Found := True;
      end if;
      if not Found then return False; end if;
      MG.Acquire (Grant, State.srcOwner, 0, CuBit.Channel_Contracts.Page_Bytes,
                  MG.Read_Access, Mapped, Acquired);
      if not Acquired then return True; end if;
      MG.Return_Acquisition (Grant, Returned);
      if not Returned then State.presentationFault := True; end if;
      return False;
   end ownerEnded;

   function isEmpty (r : Rect) return Boolean is
   begin
      return r.w = 0 or else r.h = 0;
   end isEmpty;

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

   procedure queuePresent (r : Rect) is
   begin
      if isEmpty (r) then
         return;
      end if;

      statsQueued := statsQueued + 1;

      if outputStates (selectedOutput).pendingPresent then
         outputStates (selectedOutput).pendingRect := unionRect (outputStates (selectedOutput).pendingRect, r);
      else
         outputStates (selectedOutput).pendingRect := r;
         outputStates (selectedOutput).pendingPresent := True;
      end if;
   end queuePresent;

   function unpackLo32 (x : Unsigned_64) return Natural is
   begin
      return Natural (x and 16#FFFF_FFFF#);
   end unpackLo32;

   function unpackHi32 (x : Unsigned_64) return Natural is
   begin
      return Natural (Shift_Right (x, 32));
   end unpackHi32;

   procedure clearFirmware (Address : System.Address; color : Unsigned_32) is
      line : array (Natural range 0 .. 1023) of Unsigned_32;
      ignore : System.Address;
   begin
      --  Use a bounded line buffer for small modes; wider firmware modes must
      --  not index it using the output width.
      if outputStates (selectedOutput).fbWidth <= line'Length then
         for x in 0 .. outputStates (selectedOutput).fbWidth - 1 loop
            line (x) := color;
         end loop;

         for y in 0 .. outputStates (selectedOutput).fbHeight - 1 loop
            ignore := memcpy
              (Address + Storage_Offset (y * outputStates (selectedOutput).fbPitch),
               line'Address,
               Storage_Count (outputStates (selectedOutput).fbWidth * 4));
         end loop;
         return;
      end if;

      for y in 0 .. outputStates (selectedOutput).fbHeight - 1 loop
         for x in 0 .. outputStates (selectedOutput).fbWidth - 1 loop
            declare
               pixel : Unsigned_32 with
                  Import, Address =>
                     Address + Storage_Offset (y * outputStates (selectedOutput).fbPitch + x * 4);
            begin
               pixel := color;
            end;
         end loop;
      end loop;
   end clearFirmware;

   function clearGpu (color : Unsigned_64) return Boolean is
      reply : Message;
   begin
      if BT.Current (outputStates (selectedOutput).Targets) not in BT.Unconfigured | BT.Idle then
         outputStates (selectedOutput).presentationFault := True;
         BT.Quarantine (outputStates (selectedOutput).Targets);
         return False;
      end if;
      reply := callGpu (OP_GPU_CLEAR, color, 0, 0, 0);
      if reply.tag = (label => OP_GPU_CLEAR, length => 1, protocol => 0, ipc => <>, reserved => 0) and then reply.words (0) = 0 then
         BT.Cleared (outputStates (selectedOutput).Targets);
         outputStates (selectedOutput).gpuStale := [others => (others => 0)];
         return True;
      end if;

      BT.Quarantine (outputStates (selectedOutput).Targets);
      debugPrint ("display: gpu clear failed" & LF);
      outputStates (selectedOutput).presentationFault := True;
      return False;
   end clearGpu;

   function clearOutput (color : Unsigned_64) return Boolean is
   begin
      case outputStates (selectedOutput).backend.Kind is
         when Native_GPU => return clearGpu (color);
         when Firmware_Framebuffer =>
            clearFirmware (outputStates (selectedOutput).backend.Address,
                           Unsigned_32 (color and 16#FFFF_FFFF#));
            return True;
         when Not_Configured => return False;
      end case;
   end clearOutput;

   --  The kernel's screen diagnostics (its stuck-call panel) write the boot
   --  framebuffer too, and erase their panel to black when it ends. Every
   --  source frame is complete, so the next present after an erasure copies
   --  all of it, not only its damage (CuBit.System_Facts.Scanout_Overwrites).
   firmwareOverwritesSeen : Unsigned_64 := 0;

   procedure presentFirmwareRect (Address : System.Address; damageX, damageY, damageW, damageH : Natural) is
      Overwrites : constant Unsigned_64 := CuBit.System_Facts.Scanout_Overwrites;
      Repair : constant Boolean := Overwrites /= firmwareOverwritesSeen;
      x : constant Natural := (if Repair then 0 else damageX);
      y : constant Natural := (if Repair then 0 else damageY);
      w : constant Natural := (if Repair then outputStates (selectedOutput).srcWidth else damageW);
      h : constant Natural := (if Repair then outputStates (selectedOutput).srcHeight else damageH);
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      ignore : System.Address;
   begin
      if outputStates (selectedOutput).srcAddr = System.Null_Address then
         return;
      end if;
      if Repair then
         firmwareOverwritesSeen := Overwrites;
         debugPrint ("display: kernel diagnostics erased the scanout; copying a whole frame" & LF);
      end if;
      if w = 0 or else h = 0 or else x >= outputStates (selectedOutput).fbWidth or else y >= outputStates (selectedOutput).fbHeight then
         return;
      end if;

      if maxX > outputStates (selectedOutput).fbWidth then
         maxX := outputStates (selectedOutput).fbWidth;
      end if;
      if maxY > outputStates (selectedOutput).fbHeight then
         maxY := outputStates (selectedOutput).fbHeight;
      end if;
      if maxX > outputStates (selectedOutput).srcWidth then
         maxX := outputStates (selectedOutput).srcWidth;
      end if;
      if maxY > outputStates (selectedOutput).srcHeight then
         maxY := outputStates (selectedOutput).srcHeight;
      end if;
      if x >= maxX or else y >= maxY then
         return;
      end if;

      --  Full-width damage is contiguous when source and scanout pitches
      --  match. Copy it as one span instead of issuing one memcpy per row;
      --  full-frame presents and large vertical bands are common during
      --  startup, shell switches, and simple compositor redraws.
      if x = 0 and then maxX = outputStates (selectedOutput).fbWidth and then outputStates (selectedOutput).srcPitch = outputStates (selectedOutput).fbPitch then
         ignore := memcpy
           (Address + Storage_Offset (y * outputStates (selectedOutput).fbPitch),
            outputStates (selectedOutput).srcAddr + Storage_Offset (y * outputStates (selectedOutput).srcPitch),
            Storage_Count ((maxY - y) * outputStates (selectedOutput).fbPitch));
         GM.Add (backendCopies, Unsigned_64 (maxY - y) * Unsigned_64 (outputStates (selectedOutput).fbPitch));
         return;
      end if;

      for row in y .. maxY - 1 loop
         ignore := memcpy
           (Address + Storage_Offset (row * outputStates (selectedOutput).fbPitch + x * 4),
            outputStates (selectedOutput).srcAddr + Storage_Offset (row * outputStates (selectedOutput).srcPitch + x * 4),
            Storage_Count ((maxX - x) * 4));
      end loop;
      GM.Add (backendCopies,
              Unsigned_64 (maxX - x) * Unsigned_64 (maxY - y) * 4);
   end presentFirmwareRect;

   function ensureGpuScanout return Boolean is
      gpuMap : Message;
      wire : DSP.Wire_Message;
      mapped : System.Address;
      acquired : Boolean;
   begin
      for index in 0 .. BT.Count (outputStates (selectedOutput).Targets) - 1 loop
         if outputStates (selectedOutput).gpuScanoutAddr (index) = System.Null_Address then
            gpuMap := callGpu
              (OP_GPU_MAP_FRAMEBUFFER, Unsigned_64 (index), 0, 0, 0);
            if gpuMap.tag.label /= OP_GPU_MAP_FRAMEBUFFER then return False; end if;
            wire := CuBit.Desktop_Messages.To_Wire (gpuMap);
            wire.Label := DSP.Code (DSP.Attach_Buffer);
            declare
               decoded : constant DSP.Attachment_Decoding := DSP.Decode_Attachment (wire);
            begin
               if not decoded.Valid then
                  debugPrint ("display: invalid GPU mapping" & LF);
                  return False;
               end if;
               --  After a mode change the stride is the backend's (fbPitch 0).
               if outputStates (selectedOutput).fbPitch = 0 and then index = 0 and then
                 decoded.Value.Layout.Pitch >= Natural (decoded.Value.Layout.Width) * 4
               then
                  outputStates (selectedOutput).fbPitch := decoded.Value.Layout.Pitch;
               end if;
               if Natural (decoded.Value.Layout.Width) /= outputStates (selectedOutput).fbWidth or else
                 Natural (decoded.Value.Layout.Height) /= outputStates (selectedOutput).fbHeight or else
                 decoded.Value.Layout.Pitch /= outputStates (selectedOutput).fbPitch
               then
                  --  Status and both owned mappings must describe one mode.
                  --  A backend cannot resize an already advertised output by
                  --  returning a different grant on attachment.
                  debugPrint ("display: GPU mapping differs from selected mode" & LF);
                  return False;
               end if;
               MG.Acquire_Via_Capability
                 (GPU_Service, decoded.Value.Grant, 0,
                  DSP.DP.Byte_Length (decoded.Value.Layout), MG.Write_Access, mapped, acquired);
               if not acquired then
                  debugPrint ("display: GPU mapping acquisition rejected" & LF);
                  return False;
               end if;
               -- Retain both acquisitions for this display instance. GPU death
               -- or grant revocation cannot recycle memory beneath a CPU copy.
               outputStates (selectedOutput).gpuScanoutAddr (index) := mapped;
               outputStates (selectedOutput).gpuScanoutGrant (index) := decoded.Value.Grant;
            end;
         end if;
      end loop;
      return True;
   end ensureGpuScanout;

   function clampGpuRect (r : Rect) return Rect is
      limitW : constant Natural := Natural'Min (outputStates (selectedOutput).srcWidth, outputStates (selectedOutput).fbWidth);
      limitH : constant Natural := Natural'Min (outputStates (selectedOutput).srcHeight, outputStates (selectedOutput).fbHeight);
   begin
      if r.w = 0 or else r.h = 0 or else
        r.x >= limitW or else r.y >= limitH
      then
         return (others => 0);
      end if;
      return
        (x => r.x,
         y => r.y,
         w => Natural'Min (r.w, limitW - r.x),
         h => Natural'Min (r.h, limitH - r.y));
   end clampGpuRect;

   procedure copyGpuRect
     (dest      : System.Address;
      destPitch : Natural;
      source    : System.Address;
      sourcePitch : Natural;
      r         : Rect;
      counter   : in out GM.Counter)
   is
      ignore : System.Address;
   begin
      if isEmpty (r) then
         return;
      end if;

      if r.x = 0 and then r.w = outputStates (selectedOutput).fbWidth and then
         sourcePitch = destPitch
      then
         ignore := memcpy
           (dest + Storage_Offset (r.y * destPitch),
            source + Storage_Offset (r.y * sourcePitch),
            Storage_Count (r.h * destPitch));
         GM.Add (counter, Unsigned_64 (r.h) * Unsigned_64 (destPitch));
      else
         for row in r.y .. r.y + r.h - 1 loop
            ignore := memcpy
              (dest + Storage_Offset (row * destPitch + r.x * 4),
               source + Storage_Offset (row * sourcePitch + r.x * 4),
               Storage_Count (r.w * 4));
         end loop;
         GM.Add (counter, Unsigned_64 (r.h) * Unsigned_64 (r.w) * 4);
      end if;
   end copyGpuRect;

   function prepareGpuRect (damage : Rect) return Message is
      r : constant Rect := clampGpuRect (damage);
      target : constant Gpu_Buffer_Index := BT.Target (outputStates (selectedOutput).Targets);
      previous : constant Rect := clampGpuRect (outputStates (selectedOutput).gpuStale (target));
      transfer : Rect := r;
      packedXY : Unsigned_64;
      packedWH : Unsigned_64;
      accepted : Boolean;
   begin
      if outputStates (selectedOutput).backend.Kind /= Native_GPU or else outputStates (selectedOutput).srcAddr = System.Null_Address or else
        isEmpty (r)
      then
         return NULL_MESSAGE;
      end if;

      BT.Prepare (outputStates (selectedOutput).Targets, accepted);
      if not accepted then return NULL_MESSAGE; end if;

      --  The target is stale by the damage presented since it was last
      --  written. Bring forward precisely that from the active buffer, then
      --  apply this frame's new pixels. This is buffer-age tracking: unchanged pixels never need a
      --  full-screen copy merely because the scanout resource alternates.
      if not isEmpty (previous) then
         -- New source pixels replace the overlap, so only preserve the
         -- previous damage outside it. The shared SPARK policy returns at
         -- most four nonempty outside strips when the cursor is empty.
         -- With several boxes the new pixels do not cover r: carry all of
         -- the previous damage forward, then the boxes over it.
         for Region of Repair.Before_Draw
           ((previous.x, previous.y, previous.x + previous.w, previous.y + previous.h),
            (r.x, r.y, r.x + r.w, r.y + r.h), (0, 0, 0, 0), frameBoxes.Count = 1)
         loop
            if Compositor_Damage.Valid (Region) then
               copyGpuRect
                 (outputStates (selectedOutput).gpuScanoutAddr (target), outputStates (selectedOutput).fbPitch,
                  outputStates (selectedOutput).gpuScanoutAddr (BT.Active (outputStates (selectedOutput).Targets)), outputStates (selectedOutput).fbPitch,
                  (Region.Left, Region.Top, Region.Right - Region.Left, Region.Bottom - Region.Top), repairCopies);
            end if;
         end loop;
         -- The backend still uploads the union once, including both repair
         -- strips and the new source rectangle; do not expose partial frames.
         transfer := unionRect (previous, r);
      end if;

      --  Only the frame's boxes (LAT-002 multi-rect submit), each clamped.
      for I in 1 .. frameBoxes.Count loop
         copyGpuRect
           (outputStates (selectedOutput).gpuScanoutAddr (target), outputStates (selectedOutput).fbPitch,
            outputStates (selectedOutput).srcAddr, outputStates (selectedOutput).srcPitch,
            clampGpuRect ((Natural (frameBoxes.Boxes (I).X), Natural (frameBoxes.Boxes (I).Y),
                           Natural (frameBoxes.Boxes (I).Width), Natural (frameBoxes.Boxes (I).Height))),
            backendCopies);
      end loop;

      packedXY := Unsigned_64 (transfer.x) or
        Shift_Left (Unsigned_64 (transfer.y), 32);
      packedWH := Unsigned_64 (transfer.w) or
        Shift_Left (Unsigned_64 (transfer.h), 32);

      return (tag => (label => OP_GPU_PRESENT_BUFFER, length => 4,
                      protocol => 0, ipc => <>, reserved => Unsigned_16 (selectedOutput)),
              authorityTag => 0,
              words => [Unsigned_64 (target), packedXY, packedWH, 0]);
   end prepareGpuRect;

   --  UI-020: lend scanout buffer Item.Buffer - 1 to Owner, which offered a
   --  read-only endpoint to itself with its first loan request (taken
   --  here, checked to be exactly that). The loan is a grant derived from
   --  the GPU driver's forwardable one: that buffer's pages, writable,
   --  owned (and revoked) by display. Accepted False: nothing lent.
   procedure lendSlot (State : in out Output_State; Owner : Process_ID;
                       Item : Pool_Wire.Attachment; Accepted : out Boolean) is
      package HIPC renames CuBit.Handles.IPC;
      Index : constant Gpu_Buffer_Index := Gpu_Buffer_Index (Item.Buffer - 1);
      Pages : constant Natural := Natural ((DSP.DP.Byte_Length (Item.Source.Layout) + 4095) / 4096);
      Mapped : System.Address;
      Ok, Revoked : Boolean;
      use type DSP.DP.Buffer_Layout, CuBit.Handles.Rights;
   begin
      Accepted := False;
      if not CuBit.Handles.Valid (State.ownerEndpoint) then
         declare
            Taken : constant HIPC.Accepted_Endpoint := HIPC.Accept_Endpoint (HIPC.Accept_Offered (Owner));
            Closed : CuBit.Handles.IPC.Close_Result;
            pragma Unreferenced (Closed);
         begin
            if not Taken.OK then
               debugPrint ("display: lend refused: the owner offered no endpoint" & LF);
               return;
            end if;
            declare
               D : constant HIPC.Description := HIPC.Describe (Taken.Value);
            begin
               --  Exactly a read-only endpoint to the owner: enough to derive
               --  a loan for it, never enough to call it.
               if not D.Current or else D.Object /= To_Word (Owner) or else
                 D.Held /= CuBit.Handles.Read_Only
               then
                  debugPrint ("display: lend refused: the offered endpoint is not read-only to the owner" & LF);
                  Closed := HIPC.Close (Taken.Value);
                  return;
               end if;
            end;
            State.ownerEndpoint := Taken.Value;
         end;
      end if;
      if not State.lendParentLive (Index) then
         declare
            Reply : constant Message := callGpu (OP_GPU_MAP_FRAMEBUFFER, Unsigned_64 (Index), GPU_MAP_FORWARDABLE, 0, 0);
            Wire : DSP.Wire_Message := CuBit.Desktop_Messages.To_Wire (Reply);
            Decoded : DSP.Attachment_Decoding;
         begin
            Wire.Label := DSP.Code (DSP.Attach_Buffer);
            Decoded := DSP.Decode_Attachment (Wire);
            if Reply.tag.label /= OP_GPU_MAP_FRAMEBUFFER or else not Decoded.Valid or else
              Decoded.Value.Layout /= Item.Source.Layout
            then
               debugPrint ("display: lend refused: the GPU made no forwardable grant for buffer" & Index'Image & LF);
               return;
            end if;
            MG.Acquire_Via_Capability (GPU_Service, Decoded.Value.Grant, 0,
              DSP.DP.Byte_Length (Item.Source.Layout), MG.Write_Access, Mapped, Ok);
            if not Ok then
               debugPrint ("display: lend refused: forwardable grant not acquired" & LF);
               return;
            end if;
            State.lendParent (Index) := Decoded.Value.Grant;
            State.lendParentLive (Index) := True;
         end;
      end if;
      if State.lendChildLive (Item.Buffer) then
         MG.Revoke (State.lendChild (Item.Buffer), Revoked);
         State.lendChildLive (Item.Buffer) := False;
      end if;
      MG.Derive_Via_Capability (State.ownerEndpoint, State.lendParent (Index), 0, Pages, True,
                                State.lendChild (Item.Buffer), Ok);
      if not Ok then
         debugPrint ("display: lend refused: the loan was not derived" & LF);
         return;
      end if;
      State.lendChildLive (Item.Buffer) := True;
      Accepted := True;
   end lendSlot;

   --  UI-020: a lent pool's frame: the compositor composed into the scanout
   --  buffer of its slot (and repaired what it missed); nothing to copy.
   --  The slot is the target (never the one on screen) and the frame's area
   --  is what the backend uploads (virtio-gpu) or flushes (Intel).
   function prepareLentRect (damage : Rect; Slot : Pool_Wire.Buffer_Slot) return Message is
      r : constant Rect := clampGpuRect (damage);
      accepted : Boolean;
   begin
      if outputStates (selectedOutput).backend.Kind /= Native_GPU or else isEmpty (r) then
         return NULL_MESSAGE;
      end if;
      BT.Prepare_Target (outputStates (selectedOutput).Targets, Gpu_Buffer_Index (Slot - 1), accepted);
      if not accepted then
         debugPrint ("display: lent frame named the buffer on screen; refused" & LF);
         return NULL_MESSAGE;
      end if;
      return (tag => (label => OP_GPU_PRESENT_BUFFER, length => 4,
                      protocol => 0, ipc => <>, reserved => Unsigned_16 (selectedOutput)),
              authorityTag => 0,
              words => [Unsigned_64 (Slot - 1),
                        Unsigned_64 (r.x) or Shift_Left (Unsigned_64 (r.y), 32),
                        Unsigned_64 (r.w) or Shift_Left (Unsigned_64 (r.h), 32), 0]);
   end prepareLentRect;

   --  A published flip made Target the active buffer with Damage new:
   --  every other buffer now lacks Damage, the new active one lacks nothing.
   procedure noteFlipped (State : in out Output_State; Target : Gpu_Buffer_Index;
                          Damage : Rect) is
   begin
      for B in 0 .. BT.Count (State.Targets) - 1 loop
         if B = Target then
            State.gpuStale (B) := (others => 0);
         elsif not isEmpty (Damage) then
            State.gpuStale (B) :=
              (if isEmpty (State.gpuStale (B)) then Damage
               else unionRect (State.gpuStale (B), Damage));
         end if;
      end loop;
   end noteFlipped;

   --  A backend's Present answer: the status alone, or (LAT-002) the status
   --  and the frame's present timing in words 1 .. 3.
   function presentAnswer (C : CompletionEntry) return Boolean is
     (C.msg.tag.label = OP_GPU_PRESENT_BUFFER and then C.msg.tag.length in 1 | 4 and then
      C.msg.tag.protocol = 0 and then C.msg.tag.reserved = 0);
   function presentTiming (C : CompletionEntry) return CuBit.Present_Timing.Timing is
     (if C.msg.tag.length = 4
      then CuBit.Present_Timing.Decode_Reply ([C.msg.words (1), C.msg.words (2), C.msg.words (3)])
      else CuBit.Present_Timing.No_Timing);

   procedure collectFrames;
   --  Legacy (non-pool) presentation on a native output: copy into the
   --  target and submit asynchronously, then wait (bounded) for collectFrames
   --  to complete it, so the legacy request still returns after its flip, as
   --  legacy clients expect. The wait is on display's own completion queue
   --  with an explicit deadline; the GPU never blocks display.
   --  Every present completes or fails within this (the driver answers
   --  an unlatched flip as stalled after one second).
   Present_Deadline_Ms : constant := 2_000;
   Legacy_Present_Deadline_Ms : constant := Present_Deadline_Ms;
   --  Wake at least this often while a present is outstanding.
   Present_Check_Ms : constant := 100;
   procedure awaitLegacyPresent is
      State : Output_State renames outputStates (selectedOutput);
      Started : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
      Now : Unsigned_64;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      while State.legacyToken /= 0 loop
         Now := CuBit.Published_Clock.Milliseconds;
         if Started = Unsigned_64'Last or else Now = Unsigned_64'Last or else
           Now - Started >= Legacy_Present_Deadline_Ms
         then
            debugPrint ("display: legacy present completion timed out" & LF);
            State.presentationFault := True;
            BT.Quarantine (State.Targets);
            return;
         end if;
         Activity := Wait_For_Activity_Until (Now + 1);
         collectFrames;
      end loop;
   end awaitLegacyPresent;

   function startGpuPresent (damage : Rect) return Boolean is
      State : Output_State renames outputStates (selectedOutput);
      request : Message;
      token : Unsigned_64;
   begin
      if BT.Current (State.Targets) /= BT.Idle then
         return False;
      end if;
      frameBoxes := Single_Box (damage.x, damage.y, damage.w, damage.h);
      request := prepareGpuRect (damage);
      if request.tag.length = 0 then
         BT.Quarantine (State.Targets);
         State.presentationFault := True;
         return False;
      end if;
      Compositor_Requests.Allocate (backendSequence, token);
      State.legacyTarget := BT.Target (State.Targets);
      State.legacyDamage := clampGpuRect (damage);
      BT.Seal (State.Targets, token);
      if BT.Current (State.Targets) /= BT.In_Flight or else
        not capSubmit (GPU_Service, request, token)
      then
         BT.Quarantine (State.Targets);
         State.presentationFault := True;
         debugPrint ("display: gpu page flip failed" & LF);
         return False;
      end if;
      State.legacyToken := token;
      awaitLegacyPresent;
      return BT.Current (State.Targets) = BT.Idle;
   end startGpuPresent;

   procedure flushPendingPresent is
      r : constant Rect := outputStates (selectedOutput).pendingRect;
      waitStart : Unsigned_64;
      copyStart : Unsigned_64;
      copyEnd   : Unsigned_64;
   begin
      if not outputStates (selectedOutput).pendingPresent then
         return;
      end if;

      outputStates (selectedOutput).pendingPresent := False;
      outputStates (selectedOutput).pendingRect := (others => 0);

      --  Legacy queued presentation. The selected backend is fixed for this
      --  output lifetime; an unsuccessful GPU operation cannot authorize
      --  writes to a different target. Neither backend promises vblank here.
      waitStart := CuBit.Published_Clock.Milliseconds;
      if outputStates (selectedOutput).presentationFault then return; end if;
      if outputStates (selectedOutput).backend.Kind = Native_GPU then
         if BT.Current (outputStates (selectedOutput).Targets) /= BT.Idle then
            --  A flip is in flight: keep the damage for its completion.
            outputStates (selectedOutput).pendingPresent := True;
            outputStates (selectedOutput).pendingRect := r;
            return;
         end if;
         copyStart := CuBit.Published_Clock.Milliseconds;
         if not startGpuPresent (r) then
            return;
         end if;
      elsif outputStates (selectedOutput).backend.Kind = Firmware_Framebuffer then
         copyStart := CuBit.Published_Clock.Milliseconds;
         presentFirmwareRect (outputStates (selectedOutput).backend.Address,
                              r.x, r.y, r.w, r.h);
      else
         return;
      end if;
      copyEnd := CuBit.Published_Clock.Milliseconds;

      statsPresents := statsPresents + 1;
      statsPixels := statsPixels + Unsigned_64 (r.w) * Unsigned_64 (r.h);
      if waitStart /= Unsigned_64'Last and then
         copyStart /= Unsigned_64'Last and then copyStart >= waitStart
      then
         statsWaitMs := statsWaitMs + (copyStart - waitStart);
      end if;
      if copyStart /= Unsigned_64'Last and then
         copyEnd /= Unsigned_64'Last and then copyEnd >= copyStart
      then
         statsCopyMs := statsCopyMs + (copyEnd - copyStart);
      end if;
   end flushPendingPresent;

   procedure presentPackedRegion (request : Message)
   is
      regionCount : constant Natural := Natural (request.tag.length);
      packed : Unsigned_64;
      r : Rect;
      waitStart : Unsigned_64;
      copyStart : Unsigned_64;
      copyEnd : Unsigned_64;
      pixels : Unsigned_64 := 0;
   begin
      if regionCount = 0 or else regionCount > request.words'Length then
         return;
      end if;

      --  A damage region is one scanout transaction. Wait once, then copy all
      --  constituent rectangles before replying to the compositor. This
      --  avoids multiplying IPC/vblank latency and prevents a moving window
      --  from exposing each strip as a separately timed frame.
      waitStart := CuBit.Published_Clock.Milliseconds;
      copyStart := CuBit.Published_Clock.Milliseconds;

      if outputStates (selectedOutput).backend.Kind = Native_GPU then
         --  A packed region is one visual frame. The current wire format can
         --  carry several rectangles but the swapchain flips once, so merge
         --  them here and preserve atomic presentation. Future display lists
         --  can retain disjoint damage through the GPU command queue.
         r := (others => 0);
         for i in 0 .. regionCount - 1 loop
            packed := request.words (i);
            declare
               item : constant Rect :=
                 (x => Natural (packed and 16#FFFF#),
                  y => Natural (Shift_Right (packed, 16) and 16#FFFF#),
                  w => Natural (Shift_Right (packed, 32) and 16#FFFF#),
                  h => Natural (Shift_Right (packed, 48) and 16#FFFF#));
            begin
               if not isEmpty (item) then
                  r := unionRect (r, item);
               end if;
            end;
         end loop;
         if not isEmpty (r) then
            pixels := Unsigned_64 (r.w) * Unsigned_64 (r.h);
            if BT.Current (outputStates (selectedOutput).Targets) /= BT.Idle then
               queuePresent (r);
            elsif not startGpuPresent (r) then
               return;
            end if;
         end if;
         copyEnd := CuBit.Published_Clock.Milliseconds;
         statsPresents := statsPresents + 1;
         statsPixels := statsPixels + pixels;
         if waitStart /= Unsigned_64'Last and then
           copyStart /= Unsigned_64'Last and then copyStart >= waitStart
         then
            statsWaitMs := statsWaitMs + (copyStart - waitStart);
         end if;
         if copyStart /= Unsigned_64'Last and then
           copyEnd /= Unsigned_64'Last and then copyEnd >= copyStart
         then
            statsCopyMs := statsCopyMs + (copyEnd - copyStart);
         end if;
         return;
      end if;

      for i in 0 .. regionCount - 1 loop
         packed := request.words (i);
         r :=
           (x => Natural (packed and 16#FFFF#),
            y => Natural (Shift_Right (packed, 16) and 16#FFFF#),
            w => Natural (Shift_Right (packed, 32) and 16#FFFF#),
            h => Natural (Shift_Right (packed, 48) and 16#FFFF#));
         if not isEmpty (r) then
            presentFirmwareRect (outputStates (selectedOutput).backend.Address,
                                 r.x, r.y, r.w, r.h);
            pixels := pixels + Unsigned_64 (r.w) * Unsigned_64 (r.h);
         end if;
      end loop;
      copyEnd := CuBit.Published_Clock.Milliseconds;

      statsPresents := statsPresents + 1;
      statsPixels := statsPixels + pixels;
      if waitStart /= Unsigned_64'Last and then
        copyStart /= Unsigned_64'Last and then copyStart >= waitStart
      then
         statsWaitMs := statsWaitMs + (copyStart - waitStart);
      end if;
      if copyStart /= Unsigned_64'Last and then
        copyEnd /= Unsigned_64'Last and then copyEnd >= copyStart
      then
         statsCopyMs := statsCopyMs + (copyEnd - copyStart);
      end if;
   end presentPackedRegion;

   ---------------------------------------------------------------------------
   --  Display planes (docs/display-planes.md). Requests live in desktop
   --  space and belong to the owner of output zero's lease. The pure planner
   --  (CuBit.Display_Planes) maps them onto each output's hardware planes;
   --  changes that move a request between a plane and the client composite
   --  commit only with a published frame rendered for that plan's epoch.
   ---------------------------------------------------------------------------
   Image_Pixels_Limit : constant :=
     DPL.Cursor_Extent_Limit * DPL.Cursor_Extent_Limit;
   type Image_Pixels is array (0 .. Image_Pixels_Limit - 1) of Unsigned_32;
   type Image_Store is array (DPL.Request_Id) of Image_Pixels;
   --  Display's own copy: a client grant is never read after the reply.
   requestImages : Image_Store := [others => [others => 0]];
   imageVersions : array (DPL.Request_Id) of Unsigned_64 := [others => 0];
   planeRequests : DPL.Request_Table;
   requestOwner : Process_ID := No_Process;
   planeOutputs : DPL.Output_Table;

   --  One mapped driver plane buffer and what it currently shows.
   type Plane_Buffer is record
      Mapped : System.Address := System.Null_Address;
      Width, Height : DPL.Cursor_Extent := DPL.Cursor_Extent'First;
      Shown : Boolean := False;
      Request : DPL.Request_Count := DPL.No_Request;
      Version : Unsigned_64 := 0;
      Hot_X, Hot_Y : DPL.Hotspot_Coordinate := 0;
      Position : GP.Position;
      --  Asynchronous move in flight; newer positions wait for it.
      Move_Token : Unsigned_64 := 0;
      Sent : GP.Position;
      Faulted : Boolean := False;
   end record;
   planeBuffers : array (Output_Index, DPL.Plane_Number) of Plane_Buffer;

   --  Proposed plans by epoch; frames may acknowledge any recent one.
   Plan_History_Size : constant := 8;
   type History_Slot is mod Plan_History_Size;
   type Proposal is record
      Epoch : DPL.Plan_Epoch := DPL.No_Epoch;
      Item : DPL.Plan;
   end record;
   planHistory : array (History_Slot) of Proposal;
   latestSlot : History_Slot := 0;
   latestEpoch : DPL.Plan_Epoch := DPL.No_Epoch;
   committedPlan : DPL.Plan;
   acknowledged : array (Output_Index) of DPL.Plan_Epoch :=
     [others => DPL.No_Epoch];

   function Output_Of (O : Output_Index) return DPL.Output_Id is
     (DPL.Output_Id (O));

   Plane_Call_Deadline_Ms : constant := 1_000;
   function callGpuOn (Output : Output_Index; Request : DSP.Wire_Message)
      return DSP.Wire_Message
   is
      Msg : Message := CuBit.Desktop_Messages.From_Wire
        (DSP.With_Output (Request, Output));
   begin
      --  Plane calls are answered at once by the driver; a short deadline
      --  keeps display.svc (and so the pointer) responsive if it is stuck.
      Msg.tag := capCall (GPU_Service, Msg,
                          CuBit.Messages.Deadline_After (Plane_Call_Deadline_Ms));
      if Msg.tag.label = CuBit.Messages.REPLY_TIMEOUT then
         debugPrint ("display: gpu plane call timed out" & LF);
      end if;
      return CuBit.Desktop_Messages.To_Wire (Msg);
   end callGpuOn;

   --  Ask the driver for its planes, map those that can carry cursors.
   procedure discoverPlanes (Output : Output_Index) is
      State : Output_State renames outputStates (Output);
      Item : DPL.Output_State renames planeOutputs (Output_Of (Output));
      Reply : DSP.Wire_Message;
      Described : GP.Description_Decoding;
      Mapping : GP.Buffer_Decoding;
      Mapped : System.Address;
      Acquired : Boolean;
      Cursor_Planes : Natural := 0;
   begin
      Item := (Present => True, X => 0, Y => 0,
               Width => DPL.Output_Extent (State.fbWidth),
               Height => DPL.Output_Extent (State.fbHeight),
               Count => 0, Planes => [others => <>]);
      if Output = 1 then
         Item.X := DPL.Space_Coordinate (outputStates (0).fbWidth);
      end if;
      if State.backend.Kind = Firmware_Framebuffer then
         --  A driver's planes on the firmware output only when the driver
         --  overlays this very framebuffer (Intel on the firmware mode); a
         --  secondary GPU never claims the firmware output's cursor.
         declare
            Scanout : constant GP.Scanout_Decoding :=
              GP.Decode_Scanout (callGpuOn (Output, GP.Describe_Request));
         begin
            if not Scanout.Valid or else
              Scanout.Value.Source /= GP.Firmware_Framebuffer or else
              Natural (Scanout.Value.Width) /= State.fbWidth or else
              Natural (Scanout.Value.Height) /= State.fbHeight or else
              Scanout.Value.Pitch /= State.fbPitch
            then
               return;
            end if;
            debugPrint ("display: driver overlays firmware output" & Output'Image &
                        State.fbWidth'Image & " x" & State.fbHeight'Image & LF);
         end;
      elsif State.backend.Kind /= Native_GPU then
         return;
      end if;
      for Plane in DPL.Plane_Number loop
         Reply := callGpuOn (Output, GP.Encode (GP.Query, Plane));
         Described := GP.Decode_Description (Reply);
         exit when not Described.Valid or else Plane > Described.Value.Count;
         Item.Planes (Plane) := Described.Value.Descriptor;
         Item.Count := Plane;
         if Described.Value.Descriptor.Kinds (DPL.Cursor) then
            Mapping := GP.Decode_Buffer
              (callGpuOn (Output, GP.Encode (GP.Map_Plane, Plane)));
            Acquired := False;
            if Mapping.Valid then
               MG.Acquire_Via_Capability
                 (GPU_Service, Mapping.Value.Grant, 0,
                  GP.Bytes (Mapping.Value), MG.Write_Access, Mapped, Acquired);
            end if;
            if Acquired then
               --  Retained for this instance, like the scanout mappings.
               planeBuffers (Output, Plane) :=
                 (Mapped => Mapped, Width => Mapping.Value.Width,
                  Height => Mapping.Value.Height, others => <>);
               --  Cursors use the mapped buffer, never more than it holds.
               Item.Planes (Plane).Max_Width := DPL.Surface_Extent'Min
                 (Item.Planes (Plane).Max_Width, Mapping.Value.Width);
               Item.Planes (Plane).Max_Height := DPL.Surface_Extent'Min
                 (Item.Planes (Plane).Max_Height, Mapping.Value.Height);
               Cursor_Planes := Cursor_Planes + 1;
            else
               Item.Planes (Plane).Kinds (DPL.Cursor) := False;
            end if;
         end if;
      end loop;
      debugPrint ("display: output" & Output'Image & " hardware cursor planes" &
                  Cursor_Planes'Image & LF);
   end discoverPlanes;

   procedure copyImage (Request : DPL.Request_Id; Buffer : Plane_Buffer) is
      R : DPL.Request_State renames planeRequests (Request);
      Pitch : constant Natural := Natural (Buffer.Width);
      Target : array (0 .. Natural (Buffer.Width) * Natural (Buffer.Height) - 1)
        of Unsigned_32 with Import, Volatile, Address => Buffer.Mapped;
      Width : constant Natural := Natural (R.Width);
   begin
      for Y in 0 .. Natural (Buffer.Height) - 1 loop
         for X in 0 .. Pitch - 1 loop
            Target (Y * Pitch + X) :=
              (if X < Width and then Y < Natural (R.Height)
               then requestImages (Request) (Y * Width + X) else 0);
         end loop;
      end loop;
   end copyImage;

   procedure sendMove (Output : Output_Index; Plane : DPL.Plane_Number) is
      B : Plane_Buffer renames planeBuffers (Output, Plane);
      Token : Unsigned_64;
   begin
      if B.Move_Token /= 0 or else B.Faulted or else B.Sent = B.Position then
         return;
      end if;
      Compositor_Requests.Allocate (backendSequence, Token);
      if capSubmit (GPU_Service, CuBit.Desktop_Messages.From_Wire
           (DSP.With_Output (GP.Encode (GP.Move'(Plane, B.Position)), Output)),
         Token)
      then
         B.Move_Token := Token;
         B.Sent := B.Position;
      else
         --  Busy: retryMoves (every loop turn) sends the newest position.
         null;
      end if;
   end sendMove;

   --  Once per loop turn: a move the GPU endpoint refused is sent again, so
   --  the newest position is always delivered (no later request needed).
   procedure retryMoves is
   begin
      for Output in Output_Index loop
         for Plane in DPL.Plane_Number loop
            if planeBuffers (Output, Plane).Shown then
               sendMove (Output, Plane);
            end if;
         end loop;
      end loop;
   end retryMoves;

   --  Make every mapped plane show what the committed plan says, using the
   --  requests' current images and positions.
   procedure syncPlanes is
      Reply : DSP.Wire_Message;
   begin
      for Output in Output_Index loop
         if readyOutputs (Output) then
            for Plane in DPL.Plane_Number loop
               declare
                  B : Plane_Buffer renames planeBuffers (Output, Plane);
                  Holder : constant DPL.Request_Count :=
                    committedPlan.Holders (Output_Of (Output), Plane);
                  Where : DPL.Placement;
               begin
                  if B.Mapped /= System.Null_Address and then not B.Faulted then
                     if Holder /= DPL.No_Request then
                        Where := DPL.Place (planeRequests (Holder),
                                            planeOutputs (Output_Of (Output)));
                     else
                        Where := (Visible => False);
                     end if;
                     if Where.Visible then
                        B.Position := (Where.X, Where.Y);
                        if not B.Shown or else B.Request /= Holder or else
                          B.Version /= imageVersions (Holder) or else
                          B.Hot_X /= DPL.Hotspot_Coordinate
                            (planeRequests (Holder).Anchor_X) or else
                          B.Hot_Y /= DPL.Hotspot_Coordinate
                            (planeRequests (Holder).Anchor_Y)
                        then
                           --  Shape change: rewrite the buffer, then upload
                           --  and show synchronously (rare; moves are async).
                           copyImage (Holder, B);
                           B.Hot_X := DPL.Hotspot_Coordinate
                             (planeRequests (Holder).Anchor_X);
                           B.Hot_Y := DPL.Hotspot_Coordinate
                             (planeRequests (Holder).Anchor_Y);
                           Reply := callGpuOn (Output, GP.Encode
                             (GP.Show'(Plane, B.Hot_X, B.Hot_Y, B.Position)));
                           if GP.Accepted_Reply (GP.Show_Plane, Reply) then
                              B.Shown := True;
                              B.Request := Holder;
                              B.Version := imageVersions (Holder);
                              B.Sent := B.Position;
                           else
                              B.Faulted := True;
                              debugPrint ("display: plane show failed" & LF);
                           end if;
                        else
                           sendMove (Output, Plane);
                        end if;
                     elsif B.Shown then
                        Reply := callGpuOn (Output,
                          GP.Encode (GP.Hide_Plane, Plane));
                        B.Shown := False;
                        B.Request := DPL.No_Request;
                        if not GP.Accepted_Reply (GP.Hide_Plane, Reply) then
                           B.Faulted := True;
                           debugPrint ("display: plane hide failed" & LF);
                        end if;
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end loop;
   end syncPlanes;

   function findProposal (Epoch : DPL.Plan_Epoch) return Natural is
   begin
      for Slot in History_Slot loop
         if Epoch /= DPL.No_Epoch and then planHistory (Slot).Epoch = Epoch then
            return Natural (Slot) + 1;
         end if;
      end loop;
      return 0;
   end findProposal;

   procedure commit (Item : DPL.Plan) is
   begin
      committedPlan := Item;
      syncPlanes;
   end commit;

   --  Commit an acknowledged proposal once every output its swaps touch has
   --  published a frame rendered for it; then any frame-free newer proposal.
   procedure tryCommit (Epoch : DPL.Plan_Epoch) is
      Found : constant Natural := findProposal (Epoch);
      Ready : Boolean := True;
   begin
      if Found /= 0 then
         declare
            Candidate : constant DPL.Plan :=
              planHistory (History_Slot (Found - 1)).Item;
         begin
            for Output in Output_Index loop
               if readyOutputs (Output) and then
                 DPL.Frame_Affects (committedPlan, Candidate, planeRequests,
                                    planeOutputs, Output_Of (Output)) and then
                 acknowledged (Output) /= Epoch
               then
                  Ready := False;
               end if;
            end loop;
            if Ready and then Candidate /= committedPlan then
               commit (Candidate);
            end if;
         end;
      end if;
      if latestEpoch /= DPL.No_Epoch and then
        not DPL.Needs_Frame (committedPlan, planHistory (latestSlot).Item) and then
        planHistory (latestSlot).Item /= committedPlan
      then
         commit (planHistory (latestSlot).Item);
      end if;
   end tryCommit;

   --  Recompute the plan after any request or output change.
   procedure replan is
      Next : constant DPL.Plan := DPL.Plan_Planes (planeRequests, planeOutputs);
   begin
      if latestEpoch = DPL.No_Epoch or else
        Next /= planHistory (latestSlot).Item
      then
         latestEpoch := latestEpoch + 1;
         latestSlot := latestSlot + 1;
         planHistory (latestSlot) := (latestEpoch, Next);
      end if;
      if not DPL.Needs_Frame (committedPlan, Next) then
         if Next /= committedPlan then
            commit (Next);
         else
            --  Same plan, new positions or images.
            syncPlanes;
         end if;
      else
         syncPlanes;
      end if;
   end replan;

   procedure acknowledgeFrame (Output : Output_Index; Epoch : DPL.Plan_Epoch) is
   begin
      if Epoch = DPL.No_Epoch then return; end if;
      acknowledged (Output) := Epoch;
      tryCommit (Epoch);
   end acknowledgeFrame;

   procedure resetRequests is
   begin
      planeRequests := [others => (others => <>)];
      requestOwner := No_Process;
      replan;
   end resetRequests;

   function toSet (Item : DPL.Plan; Kind : DPL.Backing_Kind)
      return PP.Request_Set is
     ([for R in DPL.Request_Id => Item.Backing (R) = Kind]);

   function planReport (Kind : PP.Operation; Status : DSP.DP.Status_Code)
      return Message
   is
      Latest : constant DPL.Plan := planHistory (latestSlot).Item;
   begin
      return CuBit.Desktop_Messages.From_Wire (PP.Encode (Kind,
        PP.Report'(Status => Status, Epoch => latestEpoch,
                   Proposed_Hardware => toSet (Latest, DPL.Hardware),
                   Proposed_Composited => toSet (Latest, DPL.Composited),
                   Committed_Hardware => toSet (committedPlan, DPL.Hardware),
                   Committed_Composited =>
                     toSet (committedPlan, DPL.Composited))));
   end planReport;

   --  Plane requests. Mutations need output zero's lease; the requester
   --  becomes the owner of every request until that lease is released.
   procedure handlePlaneRequest
     (from : Process_ID; Kind : PP.Operation; request : Message;
      replyMsg : out Message)
   is
      Wire : constant DSP.Wire_Message := CuBit.Desktop_Messages.To_Wire (request);
      Status : DSP.DP.Status_Code := DSP.DP.Bad_Object;
      Owner : constant Boolean :=
        ownsDisplay (from) and then
        (requestOwner = No_Process or else requestOwner = from);
      function Live (R : DPL.Request_Id) return Boolean is
        (planeRequests (R).Live);
   begin
      case Kind is
         when PP.Query_Output =>
            if PP.Valid_Empty (Wire, Kind) then
               declare
                  Item : DPL.Output_State renames
                    planeOutputs (Output_Of (selectedOutput));
                  Result : PP.Capability;
               begin
                  for Plane in 1 .. Item.Count loop
                     if Item.Planes (Plane).Host_Pointer then
                        if Item.Planes (Plane).Kinds (DPL.Cursor) then
                           Result.Host_Pointer_Cursors :=
                             Result.Host_Pointer_Cursors + 1;
                        end if;
                     else
                        for K in DPL.Plane_Kind loop
                           if Item.Planes (Plane).Kinds (K) then
                              Result.Planes (K) := Result.Planes (K) + 1;
                           end if;
                        end loop;
                     end if;
                     if Item.Planes (Plane).Kinds (DPL.Cursor) then
                        Result.Max_Width := DPL.Cursor_Extent'Max
                          (Result.Max_Width, DPL.Cursor_Extent
                             (Item.Planes (Plane).Max_Width));
                        Result.Max_Height := DPL.Cursor_Extent'Max
                          (Result.Max_Height, DPL.Cursor_Extent
                             (Item.Planes (Plane).Max_Height));
                     end if;
                  end loop;
                  replyMsg := CuBit.Desktop_Messages.From_Wire (PP.Encode (Result));
                  return;
               end;
            end if;
         when PP.Get_Plan =>
            if PP.Valid_Empty (Wire, Kind) then Status := DSP.DP.Success; end if;
         when PP.Place_Output =>
            declare
               D : constant PP.Origin_Decoding := PP.Decode_Origin (Wire);
            begin
               if not D.Valid then null;
               elsif not ownsDisplay (from) then Status := DSP.DP.Denied;
               else
                  planeOutputs (Output_Of (selectedOutput)).X := D.Value.X;
                  planeOutputs (Output_Of (selectedOutput)).Y := D.Value.Y;
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
         when PP.Create_Request | PP.Set_Priority =>
            declare
               D : constant PP.Identity_Decoding := PP.Decode_Identity (Kind, Wire);
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif Kind = PP.Create_Request then
                  if D.Value.Kind /= DPL.Cursor then
                     --  Overlay and primary requests are designed, not built.
                     Status := DSP.DP.Unsupported;
                  elsif Live (D.Value.Request) then
                     Status := DSP.DP.Bad_State;
                  else
                     requestOwner := from;
                     planeRequests (D.Value.Request) := DPL.Cursor_Request
                       (True, False, D.Value.Priority, 0, 0, 1, 1, 0, 0,
                        D.Value.Absolute);
                     requestImages (D.Value.Request) (0) := 0;
                     imageVersions (D.Value.Request) :=
                       imageVersions (D.Value.Request) + 1;
                     Status := DSP.DP.Success;
                     replan;
                  end if;
               elsif not Live (D.Value.Request) then
                  Status := DSP.DP.Bad_State;
               else
                  planeRequests (D.Value.Request).Priority := D.Value.Priority;
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
         when PP.Destroy_Request =>
            declare
               D : constant PP.Request_Decoding := PP.Decode_Destroy (Wire);
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif not Live (D.Value) then Status := DSP.DP.Bad_State;
               else
                  planeRequests (D.Value) := (others => <>);
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
         when PP.Set_Cursor_Image =>
            declare
               D : constant PP.Image_Decoding := PP.Decode_Image (Wire);
               Mapped : System.Address;
               Acquired, Returned : Boolean;
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif not Live (D.Value.Request) then Status := DSP.DP.Bad_State;
               else
                  MG.Acquire (D.Value.Grant, from, 0, PP.Image_Bytes (D.Value),
                              MG.Read_Access, Mapped, Acquired);
                  if Acquired then
                     declare
                        Count : constant Natural :=
                          Natural (D.Value.Width) * Natural (D.Value.Height);
                        Source : array (0 .. Count - 1) of Unsigned_32
                          with Import, Volatile, Address => Mapped;
                        Target : Image_Pixels renames
                          requestImages (D.Value.Request);
                     begin
                        for I in Source'Range loop
                           Target (I) := Source (I);
                        end loop;
                     end;
                     MG.Return_Acquisition (D.Value.Grant, Returned);
                     declare
                        R : DPL.Request_State renames
                          planeRequests (D.Value.Request);
                     begin
                        R.Width := D.Value.Width;
                        R.Height := D.Value.Height;
                        R.Shown_Width := D.Value.Width;
                        R.Shown_Height := D.Value.Height;
                        R.Anchor_X := D.Value.Hot_X;
                        R.Anchor_Y := D.Value.Hot_Y;
                     end;
                     imageVersions (D.Value.Request) :=
                       imageVersions (D.Value.Request) + 1;
                     Status := (if Returned then DSP.DP.Success
                                else DSP.DP.Bad_State);
                     replan;
                  end if;
               end if;
            end;
         when PP.Set_Anchor =>
            declare
               D : constant PP.Anchor_Decoding := PP.Decode_Anchor (Wire);
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif not Live (D.Value.Request) or else
                 Natural (D.Value.X) >= Natural (planeRequests (D.Value.Request).Width) or else
                 Natural (D.Value.Y) >= Natural (planeRequests (D.Value.Request).Height)
               then
                  Status := DSP.DP.Bad_State;
               else
                  planeRequests (D.Value.Request).Anchor_X := D.Value.X;
                  planeRequests (D.Value.Request).Anchor_Y := D.Value.Y;
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
         when PP.Move_Request =>
            declare
               D : constant PP.Move_Decoding := PP.Decode_Move (Wire);
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif not Live (D.Value.Request) then Status := DSP.DP.Bad_State;
               else
                  planeRequests (D.Value.Request).X := D.Value.X;
                  planeRequests (D.Value.Request).Y := D.Value.Y;
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
         when PP.Set_Visibility =>
            declare
               D : constant PP.Visibility_Decoding := PP.Decode_Visibility (Wire);
            begin
               if not D.Valid then null;
               elsif not Owner then Status := DSP.DP.Denied;
               elsif not Live (D.Value.Request) then Status := DSP.DP.Bad_State;
               else
                  planeRequests (D.Value.Request).Visible := D.Value.Visible;
                  Status := DSP.DP.Success;
                  replan;
               end if;
            end;
      end case;
      replyMsg := planReport (Kind, Status);
   end handlePlaneRequest;

   --  A plane move completion; False if Token is not a plane move.
   function collectPlaneMove (Completion : CompletionEntry) return Boolean is
   begin
      for Output in Output_Index loop
         for Plane in DPL.Plane_Number loop
            declare
               B : Plane_Buffer renames planeBuffers (Output, Plane);
            begin
               if B.Move_Token /= 0 and then B.Move_Token = Completion.token then
                  B.Move_Token := 0;
                  if not (Completion.valid and then
                          Completion.status = COMPLETION_OK and then
                          GP.Accepted_Reply (GP.Move_Plane,
                            CuBit.Desktop_Messages.To_Wire (Completion.msg)))
                  then
                     B.Faulted := True;
                     debugPrint ("display: plane move failed" & LF);
                  else
                     sendMove (Output, Plane);
                  end if;
                  return True;
               end if;
            end;
         end loop;
      end loop;
      return False;
   end collectPlaneMove;

   function frameResponse (Result : DSP.Frame_Result; Slot : Natural) return Message is
   begin
      if Slot = 0 then
         return CuBit.Desktop_Messages.From_Wire (DSP.Encode_Frame_Result (Result));
      else
         return CuBit.Desktop_Messages.From_Wire
           (Pool_Wire.Encode (Pool_Wire.Completion'(Pool_Wire.Buffer_Slot (Slot), Result)));
      end if;
   end frameResponse;

   procedure finishFrame
     (Output : Output_Index; ID : PS.Submission_ID;
      Result : in out DSP.Frame_Result; Published, Certain : Boolean;
      Response : out Message; Dropped : Boolean := False)
   is
      State : Output_State renames outputStates (Output);
      Released_State : PS.State;
      Applied, Obligation, Returned : Boolean;
   begin
      --  An uncertain backend failure is not cancellation. Quarantine instead
      --  of announcing reusable memory or advancing the swapchain.
      if Certain then
         PS.Apply (State.frameState, ID,
                   (if Published then PS.Was_Presented else PS.Discard),
                   Applied, Obligation);
         if Applied then
            if Published then Result.Outcome := DSP.Published; end if;
            Released_State := State.frameState;
            PS.Apply (Released_State, ID, PS.Release_Buffer,
                      Applied, Obligation);
            if Applied and then Obligation then
               MG.Return_Acquisition (State.srcGrant, Returned);
               if Returned then
                  State.frameState := Released_State;
                  Result.Buffer_State := DSP.Released;
                  PS.Apply (State.frameState, ID, PS.Retire,
                            Applied, Obligation);
               else
                  Applied := False;
               end if;
            end if;
         end if;
         if not Applied then State.presentationFault := True; end if;
      else
         State.presentationFault := True;
      end if;
      --  Dropped: the GPU refused the frame with certainty (nothing armed),
      --  the source is returned and the client redraws. Not a fault.
      if not Published and then not (Dropped and then Certain) then
         State.presentationFault := True;
      end if;
      if State.presentationFault then
         BT.Quarantine (State.Targets);
         PS.Close (State.frameState);
         debugPrint ("display: asynchronous presentation quarantined" & LF);
      end if;
      --  Compile-time fixture only: rotate metadata after a completed frame.
      if Presentation_Test_Policy.Rebind_Enabled and then
        not State.outputRebound and then not State.presentationFault and then
        Result.Frame = Presentation_Test_Policy.Rebind_After_Frame and then
        Result.Outcome = DSP.Published
      then
         declare
            Replacement : Outputs.Output_Reference;
            Changed : Outputs.Mutation_Result;
         begin
            Outputs.Update
              (outputRegistry, State.currentOutput,
               Outputs.Describe (outputRegistry, State.currentOutput),
               Replacement, Changed);
            if Changed = Outputs.Applied then
               State.currentOutput := Replacement;
               State.outputRebound := True;
               debugPrint ("display: test output generation advanced" & LF);
            else
               State.presentationFault := True;
            end if;
         end;
      end if;
      --  A published frame rendered for a plan epoch commits that plan's
      --  plane changes on this output (glitch-free swaps, see replan).
      if Result.Outcome = DSP.Published and then not State.presentationFault then
         acknowledgeFrame (Output, State.frameEpoch);
      end if;
      State.frameEpoch := DPL.No_Epoch;
      Response := frameResponse (Result, State.Pool_Slot);
      if State.Pool_Slot /= 0 and then Result.Buffer_State = DSP.Released then
         State.srcAddr := System.Null_Address;
      end if;
   end finishFrame;

   procedure collectGpuWatch (Completion : CompletionEntry);
   --  intel-gpu's Present answers that certify nothing was armed.
   GPU_Present_Refused    : constant Unsigned_64 := 3;
   GPU_Present_Superseded : constant Unsigned_64 := 6;
   droppedFrames : Unsigned_64 := 0;
   procedure collectFrames is
      Completion : aliased CompletionEntry;
      Response : Message;
      Ignored : Unsigned_64;
      Published : Boolean;
      Matched : Boolean;
   begin
      --  Nothing of display.svc's is out (no present, plane move or mode
      --  watch at the GPU: the firmware framebuffer's case): no completion
      --  can be waiting, so no poll to find the queue empty.
      if gpuWatchToken = 0 and then
        (for all State of outputStates => State.backendToken = 0 and then State.legacyToken = 0)
        and then (for all B of planeBuffers => B.Move_Token = 0)
      then
         return;
      end if;
      --  At most two backend operations are outstanding; do not let a faulty
      --  producer monopolize this loop with unbounded completion traffic.
      for Count in 1 .. COMPLETION_QUEUE_SIZE loop
         exit when not CuBit.Readiness.Pending (displayReady, CuBit.Readiness.Completions);
         declare
            Seen : constant CuBit.Readiness.Count := CuBit.Readiness.Produced (CuBit.Readiness.Completions);
         begin
            if Poll_Completion (Completion'Address) /= 1 then
               CuBit.Readiness.Caught_Up (displayReady, CuBit.Readiness.Completions, Seen);
               exit;
            end if;
         end;
         CuBit.Readiness.Took (displayReady, CuBit.Readiness.Completions);
         if gpuWatchToken /= 0 and then Completion.token = gpuWatchToken then
            gpuWatchToken := 0;
            collectGpuWatch (Completion);
            Matched := True;
         else
            Matched := collectPlaneMove (Completion);
         end if;
         for Output in Output_Index loop
            declare
               State : Output_State renames outputStates (Output);
            begin
               if not Matched and then State.legacyToken /= 0 and then
                 State.legacyToken = Completion.token
               then
                  Matched := True;
                  BT.Complete (State.Targets, Completion.token,
                               Completion.valid and then Completion.status = COMPLETION_OK and then
                               presentAnswer (Completion) and then
                               Completion.msg.words (0) = 0 and then
                               State.legacyTarget = BT.Target (State.Targets));
                  State.legacyToken := 0;
                  if BT.Current (State.Targets) = BT.Idle then
                     declare
                        Flipped : constant Gpu_Buffer_Index := State.legacyTarget;
                        Damage : constant Rect := State.legacyDamage;
                     begin
                        noteFlipped (State, Flipped, Damage);
                     end;
                     if State.pendingPresent then
                        declare
                           Saved : constant Output_Index := selectedOutput;
                        begin
                           selectedOutput := Output;
                           flushPendingPresent;
                           selectedOutput := Saved;
                        end;
                     end if;
                  else
                     State.presentationFault := True;
                     debugPrint ("display: gpu page flip failed" & LF);
                  end if;
               end if;
            end;
         end loop;
         for Output in Output_Index loop
            declare
               State : Output_State renames outputStates (Output);
            begin
               if not Matched and then State.backendToken /= 0 and then
                 State.backendToken = Completion.token
               then
                  Matched := True;
                  --  A certain refusal (3: nothing armed, the buffer is not
                  --  shown; 6: superseded before latching) drops this frame:
                  --  the targets stay as they were and the client redraws.
                  if Completion.valid and then Completion.status = COMPLETION_OK and then
                    presentAnswer (Completion) and then
                    Completion.msg.words (0) in GPU_Present_Refused | GPU_Present_Superseded
                  then
                     BT.Abandon (State.Targets, Completion.token);
                     --  The mailbox: a newer frame took this one's place
                     --  before the flip latched. Said so, not "failed".
                     if Completion.msg.words (0) = GPU_Present_Superseded then
                        State.backendFrame.Outcome := DSP.Superseded;
                     end if;
                     if BT.Current (State.Targets) = BT.Idle then
                        if droppedFrames < Unsigned_64'Last then droppedFrames := droppedFrames + 1; end if;
                        if droppedFrames = 1 or else droppedFrames mod 64 = 0 then
                           CuBit.Log.Warning ("display: GPU refused present status" &
                             Completion.msg.words (0)'Image & " target" &
                             State.backendTarget'Image & "; frame dropped, source released (" &
                             droppedFrames'Image & " dropped)");
                        end if;
                        finishFrame (Output, State.backendID, State.backendFrame,
                                     False, True, Response, Dropped => True);
                        State.backendToken := 0;
                        Ignored := answerFrame (Output, Response);
                        goto Next_Output;
                     end if;
                  end if;
                  Published := Completion.valid and then Completion.status = COMPLETION_OK and then
                    presentAnswer (Completion) and then
                    Completion.msg.words (0) = 0 and then
                    State.backendFrame.Session = State.activeSession and then
                    State.sessionOutput = State.currentOutput;
                  if not Published then
                     CuBit.Log.Warning ("display: present failed valid=" & Completion.valid'Image &
                       " ok=" & Boolean'Image (Completion.status = COMPLETION_OK) &
                       " label=" & Completion.msg.tag.label'Image &
                       " status=" & Completion.msg.words (0)'Image &
                       " session_match=" & Boolean'Image (State.backendFrame.Session = State.activeSession) &
                       " output_match=" & Boolean'Image (State.sessionOutput = State.currentOutput) &
                       "; output quarantined");
                  end if;
                  --  Validate the captured output directly, not selectedOutput.
                  Published := Published and then
                    State.sourceOutput = State.currentOutput and then
                    outputUsable (State.currentOutput);
                  BT.Complete (State.Targets, Completion.token,
                               Published and then State.backendTarget = BT.Target (State.Targets));
                  Published := Published and then BT.Current (State.Targets) = BT.Idle;
                  if Published then
                     declare
                        Flipped : constant Gpu_Buffer_Index := State.backendTarget;
                        Damage : constant Rect := State.backendDamage;
                        Timing : constant CuBit.Present_Timing.Timing := presentTiming (Completion);
                     begin
                        noteFlipped (State, Flipped, Damage);
                        --  The client learns when its frame reached the screen.
                        State.backendFrame.Timing := CuBit.Present_Timing.To_Compact (Timing);
                     end;
                  end if;
                  finishFrame (Output, State.backendID, State.backendFrame,
                               Published, Published, Response);
                  State.backendToken := 0;
                  Ignored := answerFrame (Output, Response);
               end if;
            end;
            <<Next_Output>>
         end loop;
         if not Matched then
            --  Never guess which buffer an unrecognized completion releases.
            for State of outputStates loop
               State.presentationFault := True;
               BT.Quarantine (State.Targets);
               PS.Close (State.frameState);
            end loop;
            debugPrint ("display: unrecognized GPU completion" & LF);
         end if;
      end loop;
   end collectFrames;

   --  A pool frame whose backend completion has not arrived within
   --  Present_Deadline_Ms fails, with a logged reason, and the output is
   --  quarantined (its buffers' state is uncertain). Never an endless wait.
   procedure checkPresentDeadlines is
      Now : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
      Response : Message;
      Ignored : Unsigned_64;
      pragma Unreferenced (Ignored);
   begin
      if Now = Unsigned_64'Last then return; end if;
      for Output in Output_Index loop
         declare
            State : Output_State renames outputStates (Output);
         begin
            if State.backendToken /= 0 and then State.backendStarted /= Unsigned_64'Last and then
              Now >= State.backendStarted and then
              Now - State.backendStarted >= Present_Deadline_Ms
            then
               CuBit.Log.Warning ("display: present stalled output" & Output'Image &
                 " token" & State.backendToken'Image & " age_ms" &
                 Unsigned_64'Image (Now - State.backendStarted) &
                 " (no GPU completion); frame failed, output quarantined");
               BT.Quarantine (State.Targets);
               finishFrame (Output, State.backendID, State.backendFrame,
                            False, False, Response);
               State.backendToken := 0;
               Ignored := answerFrame (Output, Response);
            end if;
         end;
      end loop;
   end checkPresentDeadlines;

   function presentPending return Boolean is
     (for some State of outputStates => State.backendToken /= 0);

   --  Epoch of the plan frame being decoded (Submit_Plan_Frame), else none.
   pendingFrameEpoch : DPL.Plan_Epoch := DPL.No_Epoch;

   procedure submitFrame (from : Process_ID; request : Message;
                          replyMsg : out Message) is
      decoded : DSP.Frame_Decoding;
      Pool_Decoded : Pool_Wire.Frame_Decoding;
      Slot : Natural range 0 .. 3 := 0;
      Source_Grant : MG.Grant_Reference;
      State : Output_State renames outputStates (selectedOutput);
      result : DSP.Frame_Result;
      admission : PS.Admission;
      id : PS.Submission_ID;
      mapped : System.Address;
      acquired, returned, applied, obligation : Boolean;
      r : Rect;
      published : Boolean := False;
      started : Unsigned_64;
      backendRequest : Message;
      token : Unsigned_64;
      Received_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      replyMsg := NULL_MESSAGE;
      replyMsg.tag := (label => request.tag.label, length => 1,
                       protocol => 0, ipc => <>, reserved => 0);
      replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
      if request.tag.label = Pool_Wire.Submit_Frame then
         Pool_Decoded := Pool_Wire.Decode_Frame (CuBit.Desktop_Messages.To_Wire (request));
         if not Pool_Decoded.Valid then return; end if;
         Slot := Pool_Decoded.Value.Buffer;
         decoded := (True, Pool_Decoded.Value.Request);
      else
         decoded := DSP.Decode_Frame (CuBit.Desktop_Messages.To_Wire (request));
      end if;
      if not decoded.Valid then return; end if;
      result := (decoded.Value.Session, decoded.Value.Frame,
                 DSP.Rejected, DSP.Not_Acquired, others => <>);
      r := (Natural (decoded.Value.Area.X), Natural (decoded.Value.Area.Y),
            Natural (decoded.Value.Area.Width), Natural (decoded.Value.Area.Height));
      if State.presentationFault or else not ownsDisplay (from) or else
        (if Slot = 0 then not State.srcAcquired
         else not Pool_Registry.Active (State.Pool) or else Pool_Registry.Faulted (State.Pool)) or else
        State.srcOwner /= from or else
        State.activeSession /= decoded.Value.Session or else
        State.sessionOutput /= State.currentOutput or else
        decoded.Value.Frame <= State.lastFrame or else
        PS.Current (State.frameState) /= PS.Idle or else
        isEmpty (r) or else r.x + r.w > State.srcWidth or else
        r.y + r.h > State.srcHeight or else
        backendSequence >= NO_COMPLETION_TOKEN - 1
      then
         replyMsg := frameResponse (result, Slot);
         return;
      end if;
      --  Attachment is not authority for new work after grant revocation.
      Source_Grant := (if Slot = 0 then State.srcGrant else
        Pool_Registry.Item (State.Pool, Pool_Wire.Buffer_Slot (Slot)).Grant);
      --  A pool buffer carries its frame's damage record after its rows.
      MG.Acquire (Source_Grant, from, 0,
                  Unsigned_64 (State.srcPitch) * Unsigned_64 (State.srcHeight) +
                    (if Slot = 0 then 0 else FD.Record_Bytes),
                  MG.Read_Access, mapped, acquired);
      if not acquired then
         replyMsg := frameResponse (result, Slot);
         return;
      end if;
      frameBoxes := Single_Box (r.x, r.y, r.w, r.h);
      --  A pooled frame's damage boxes: from its ring entry (Desktop's,
      --  already this process's copy), or, for a frame request message
      --  (display-check's), from the record after its rows, decoded from a
      --  snapshot read once (FD.Shared: the client can still write that
      --  page). Refused, the announced area stands (always correct). A lent
      --  frame is not copied: its area is what counts.
      if Slot /= 0 and then not State.lentPool then
         declare
            Area : constant FD.DP.Rectangle :=
              (FD.DP.Pixel_Coordinate (r.x), FD.DP.Pixel_Coordinate (r.y),
               FD.DP.Pixel_Extent (r.w), FD.DP.Pixel_Extent (r.h));
            Record_Item : constant FD.Decoding :=
              (if servingRing /= Frame_Rings.No_Ring
               then FD.Decode (servingDamage, decoded.Value.Frame, Area)
               else CuBit.Display_Frame_Damage.Shared.Decode_At
                 (mapped + Storage_Offset (State.srcPitch * State.srcHeight), decoded.Value.Frame, Area));
         begin
            if Record_Item.Valid then
               frameBoxes := Record_Item.Value;
            elsif not frameRecordWarned then
               frameRecordWarned := True;
               debugPrint ("display: a frame's damage boxes were refused; copying its area" & LF);
            end if;
         end;
      end if;
      PS.Submit (State.frameState, admission, id);
      if admission /= PS.Accepted then
         MG.Return_Acquisition (Source_Grant, returned);
         result.Buffer_State := (if returned then DSP.Released else DSP.Still_Held);
         State.presentationFault := True;
         replyMsg := frameResponse (result, Slot);
         return;
      end if;
      State.srcGrant := Source_Grant;
      State.Pool_Slot := Slot;
      State.lastFrame := decoded.Value.Frame;
      State.frameEpoch := pendingFrameEpoch;
      result.Buffer_State := DSP.Still_Held;
      result.Outcome := DSP.Failed;
      PS.Apply (State.frameState, id, PS.Begin_Read, applied, obligation);
      if not applied then
         finishFrame (selectedOutput, id, result, False, False, replyMsg);
         return;
      end if;
      State.srcAddr := mapped;
      started := CuBit.Published_Clock.Milliseconds;
      if Presentation_Test_Policy.Enabled and then
        not Presentation_Test_Policy.Verify_Buffer
          ((if Slot /= 0 and then State.lentPool then State.gpuScanoutAddr (Gpu_Buffer_Index (Slot - 1))
            else mapped), Unsigned_64 (State.srcPitch) * Unsigned_64 (State.srcHeight),
           decoded.Value.Frame)
      then
         State.presentationFault := True;
      elsif State.backend.Kind = Native_GPU then
         backendRequest :=
           (if Slot /= 0 and then State.lentPool
            then prepareLentRect (r, Pool_Wire.Buffer_Slot (Slot)) else prepareGpuRect (r));
         if backendRequest.tag.length /= 0 and then
           holdFrameAnswer (selectedOutput)
         then
            deferredReply := True;
            Compositor_Requests.Allocate (backendSequence, token);
            State.backendFrame := result;
            State.backendID := id;
            State.backendTarget := BT.Target (State.Targets);
            State.backendDamage := clampGpuRect (r);
            BT.Seal (State.Targets, token);
            if BT.Current (State.Targets) = BT.In_Flight and then
              capSubmit (GPU_Service, backendRequest, token)
            then
               State.backendToken := token;
               State.backendStarted := CuBit.Published_Clock.Milliseconds;
               declare
                  Forwarded_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
               begin
                  if Received_Us /= CuBit.Monotonic.Unavailable_Us and then
                    Forwarded_Us /= CuBit.Monotonic.Unavailable_Us and then Forwarded_Us >= Received_Us
                  then
                     statsForwardUs := statsForwardUs + (Forwarded_Us - Received_Us);
                     statsForwardMaxUs := Unsigned_64'Max (statsForwardMaxUs, Forwarded_Us - Received_Us);
                     statsForwarded := statsForwarded + 1;
                  end if;
               end;
               statsPresents := statsPresents + 1;
               --  A lent frame is not copied (copy px/frame counts copies).
               if not State.lentPool then
                  for I in 1 .. frameBoxes.Count loop
                     statsPixels := statsPixels + CuBit.Display_Box_Copy.Bytes (frameBoxes.Boxes (I)) / 4;
                  end loop;
               end if;
               statsCopyMs := statsCopyMs + CuBit.Published_Clock.Milliseconds - started;
               return;
            end if;
            --  No command accepted: safe to return the source acquisition.
            finishFrame (selectedOutput, id, result, False, True, replyMsg);
            declare
               Ignored : constant Unsigned_64 :=
                 answerFrame (selectedOutput, replyMsg);
            begin
               null;
            end;
            return;
         end if;
      elsif State.backend.Kind = Firmware_Framebuffer then
         declare
            Copy_Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
            Copied : Unsigned_64;
         begin
            for I in 1 .. frameBoxes.Count loop
               presentFirmwareRect (State.backend.Address,
                 Natural (frameBoxes.Boxes (I).X), Natural (frameBoxes.Boxes (I).Y),
                 Natural (frameBoxes.Boxes (I).Width), Natural (frameBoxes.Boxes (I).Height));
            end loop;
            Copied := CuBit.Monotonic.Microseconds;
            --  The firmware framebuffer scans out what was written: the copy's
            --  end stands for the latch (no refresh is known).
            if Copy_Started /= CuBit.Monotonic.Unavailable_Us and then
              Copied /= CuBit.Monotonic.Unavailable_Us and then Copy_Started <= Copied
            then
               result.Timing := CuBit.Present_Timing.To_Compact
                 ((CuBit.Present_Timing.Host_Fence, Copy_Started, Copied, Copied, 0));
            end if;
         end;
         published := True;
      end if;
      statsPresents := statsPresents + 1;
      for I in 1 .. frameBoxes.Count loop
         statsPixels := statsPixels + CuBit.Display_Box_Copy.Bytes (frameBoxes.Boxes (I)) / 4;
      end loop;
      statsCopyMs := statsCopyMs + CuBit.Published_Clock.Milliseconds - started;
      finishFrame (selectedOutput, id, result, published, True, replyMsg);
   end submitFrame;

   ---------------------------------------------------------------------------
   --  Output mode changes (CuBit.Output_Modes, docs/vm-guide.md). The GPU
   --  answers a watch when its host changed a preferred mode; display.svc
   --  re-reads the GPU catalog, publishes the new preference (Advertised) in
   --  its own catalog and answers Desktop's watch. Desktop releases its
   --  outputs and asks for the mode (Set_Mode); display.svc switches the GPU,
   --  remaps the scanout buffers and re-registers the output.
   ---------------------------------------------------------------------------
   --  "WxH" with no padding (log markers the tests match).
   function Dimensions (Width, Height : Natural) return String is
      W : constant String := Width'Image;
      H : constant String := Height'Image;
   begin
      return W (W'First + 1 .. W'Last) & "x" & H (H'First + 1 .. H'Last);
   end Dimensions;

   procedure answerOutputWatch is
      Ignored : Unsigned_64;
   begin
      if outputWatchArmed then
         outputWatchArmed := False;
         Ignored := answerSaved (outputWatch, CuBit.Desktop_Messages.From_Wire
           (OM.Encode_Watch (OD.Display_Broker, catalogRevision)));
      end if;
   end answerOutputWatch;

   procedure catalogChanged is
   begin
      if catalogRevision < OD.Catalog_Revision'Last then
         catalogRevision := catalogRevision + 1;
      end if;
      answerOutputWatch;
   end catalogChanged;

   procedure armGpuWatch is
      Token : Unsigned_64;
   begin
      if not gpuDetected or else not catalogReady or else gpuWatchToken /= 0 then
         return;
      end if;
      Compositor_Requests.Allocate (backendSequence, Token);
      if Token /= 0 and then capSubmit (GPU_Service, CuBit.Desktop_Messages.From_Wire
           (OM.Encode_Watch (OD.GPU_Backend, gpuRevision)), Token)
      then
         gpuWatchToken := Token;
      end if;
   end armGpuWatch;

   --  Copy the GPU's current preferences (Advertised) into the catalog.
   procedure refreshAdvertised is
      Saved : constant Output_Index := selectedOutput;
      Response : Message;
      Summary : OD.Summary_Decoding;
      Item : OD.Description_Decoding;
      Changed : Boolean := False;
   begin
      selectedOutput := 0;
      Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Catalog));
      Summary := OD.Decode_Summary (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
      if Summary.Valid and then Summary.Value.Count <= 16 then
         for Index in 1 .. Summary.Value.Count loop
            Response := callGpu
              (OD.Code (OD.GPU_Backend, OD.Get_Description),
               Summary.Value.Revision, Unsigned_64 (Index));
            Item := OD.Decode_Description
              (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
            exit when not Item.Valid or else Item.Value.Requested /= (Summary.Value.Revision, Index);
            for Known in 1 .. outputCatalog.Count loop
               declare
                  K : OD.Description renames outputCatalog.Items (Known);
               begin
                  if K.Source = OD.Virtio_GPU and then
                    K.Native_Number = Item.Value.Item.Native_Number and then
                    (K.Advertised_Width /= Item.Value.Item.Advertised_Width or else
                     K.Advertised_Height /= Item.Value.Item.Advertised_Height)
                  then
                     K.Advertised_Width := Item.Value.Item.Advertised_Width;
                     K.Advertised_Height := Item.Value.Item.Advertised_Height;
                     Changed := True;
                     debugPrint ("display: output" & K.Native_Number'Image & " host prefers" &
                       " " & Dimensions (K.Advertised_Width, K.Advertised_Height) & LF);
                  end if;
               end;
            end loop;
         end loop;
      end if;
      selectedOutput := Saved;
      if Changed then
         catalogChanged;
      end if;
   end refreshAdvertised;

   procedure collectGpuWatch (Completion : CompletionEntry) is
      Answer : constant OM.Revision_Decoding :=
        (if Completion.valid and then Completion.status = COMPLETION_OK
         then OM.Decode_Watch (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Completion.msg))
         else (Valid => False));
   begin
      if not Answer.Valid then
         --  A backend without mode changes (or a failed watch): outputs
         --  keep their modes; do not resubmit.
         debugPrint ("display: GPU mode watch unavailable" & LF);
         return;
      end if;
      gpuRevision := Answer.Value;
      if intelTakeover then
         --  DISP-009: a hot-plug re-detection changed intel-gpu's catalog:
         --  its attached-but-not-lit outputs come and go (one revision bump
         --  per change; the lit output is unchanged by detection).
         declare
            Saved : constant Output_Index := selectedOutput;
            Summary : OD.Summary_Decoding;
            Changed : Boolean := False;
         begin
            selectedOutput := 0;
            Summary := OD.Decode_Summary (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire
              (callGpu (OD.Code (OD.GPU_Backend, OD.Get_Catalog))));
            if Summary.Valid and then Summary.Value.Count >= 1 then
               readDetected (Summary.Value, outputCatalog, Changed);
            end if;
            selectedOutput := Saved;
            if Changed then
               debugPrint ("display: intel outputs changed (hot plug)" & LF);
               catalogChanged;
            end if;
         end;
      else
         refreshAdvertised;
      end if;
      armGpuWatch;
   end collectGpuWatch;

   --  The selected output switched to Mode: remap, clear, re-register and
   --  publish. Only with no client buffers, session or frame on it.
   procedure adoptMode (Mode : OM.Mode) is
      State : Output_State renames outputStates (selectedOutput);
      Returned : Boolean;
      Fresh_Targets : BT.State;
      Replacement : Outputs.Output_Reference;
      Changed : Outputs.Mutation_Result;
      Described : Outputs.Description;
   begin
      for Index in 0 .. BT.Count (State.Targets) - 1 loop
         if State.gpuScanoutAddr (Index) /= System.Null_Address then
            --  No copy uses the old mapping (no frame is in flight); the
            --  GPU's buffers stay where they are, only the length changes.
            MG.Return_Acquisition (State.gpuScanoutGrant (Index), Returned);
            if not Returned then
               State.presentationFault := True;
            end if;
            State.gpuScanoutAddr (Index) := System.Null_Address;
         end if;
         if State.lendParentLive (Index) then
            MG.Return_Acquisition (State.lendParent (Index), Returned);
            if not Returned then State.presentationFault := True; end if;
            State.lendParentLive (Index) := False;
         end if;
      end loop;
      State.fbWidth := Natural (Mode.Width);
      State.fbHeight := Natural (Mode.Height);
      --  Zero: take the stride the backend's mapping reports (Intel aligns
      --  rows to 64 bytes); ensureGpuScanout fills it in.
      State.fbPitch := 0;
      BT.Set_Count (Fresh_Targets, BT.Count (State.Targets));
      State.Targets := Fresh_Targets;
      State.gpuStale := [others => (others => 0)];
      if State.presentationFault or else not ensureGpuScanout or else
        not clearOutput (16#0013_1518#)
      then
         State.presentationFault := True;
         debugPrint ("display: output mode change failed" & LF);
         return;
      end if;
      Described := Outputs.Describe (outputRegistry, State.currentOutput);
      Described.Area.Width := Outputs.W.Extent (Mode.Width);
      Described.Area.Height := Outputs.W.Extent (Mode.Height);
      Outputs.Update (outputRegistry, State.currentOutput, Described, Replacement, Changed);
      if Changed /= Outputs.Applied then
         State.presentationFault := True;
         debugPrint ("display: output re-registration failed" & LF);
         return;
      end if;
      State.currentOutput := Replacement;
      for Index in 1 .. outputCatalog.Count loop
         declare
            K : OD.Description renames outputCatalog.Items (Index);
         begin
            if K.Source in OD.Virtio_GPU | OD.Intel_Display and then K.Role /= OD.Detected_Only and then
              K.Native_Number = Natural (selectedOutput)
            then
               if K.Role = OD.Selected_For_Desktop then
                  K := (OD.Selected_For_Desktop, K.Source, K.Native_Number,
                        K.Advertised_Width, K.Advertised_Height, K.Width_MM, K.Height_MM,
                        OD.Lit, K.Connector, Mode.Width, Mode.Height);
               else
                  K := (OD.Backend_Ready, K.Source, K.Native_Number,
                        K.Advertised_Width, K.Advertised_Height, K.Width_MM, K.Height_MM,
                        OD.Lit, K.Connector, Mode.Width, Mode.Height);
               end if;
            end if;
         end;
      end loop;
      planeOutputs (Output_Of (selectedOutput)).Width := DPL.Output_Extent (Mode.Width);
      planeOutputs (Output_Of (selectedOutput)).Height := DPL.Output_Extent (Mode.Height);
      if selectedOutput = 0 and then readyOutputs (1) then
         planeOutputs (Output_Of (1)).X := DPL.Space_Coordinate (Mode.Width);
      end if;
      replan;
      debugPrint ("display: output" & selectedOutput'Image & " mode" &
        " " & Dimensions (Mode.Width, Mode.Height) & LF);
      catalogChanged;
   end adoptMode;

   procedure handleSetMode (from : Process_ID; request : Message; replyMsg : out Message) is
      State : Output_State renames outputStates (selectedOutput);
      Wanted : constant OM.Request_Decoding :=
        OM.Decode_Set_Mode (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire (request));
      Current : constant OM.Mode := (OD.Extent (State.fbWidth), OD.Extent (State.fbHeight));
      Result : OM.Mode_Result := (DSP.DP.Bad_State, Current);
      Call : Message;
      Answer : OM.Result_Decoding;
   begin
      if not Wanted.Valid then
         Result.Status := DSP.DP.Invalid_Request;
      elsif State.backend.Kind /= Native_GPU then
         Result.Status := DSP.DP.Unsupported;
         debugPrint ("display: mode change refused: the output has no native GPU backend" & LF);
      elsif State.displayOwner /= No_Process and then State.displayOwner /= from then
         Result.Status := DSP.DP.Denied;
         debugPrint ("display: mode change refused: the caller does not own the output" & LF);
      elsif State.presentationFault or else State.srcAcquired or else
        Pool_Registry.Count (State.Pool) /= 0 or else State.activeSession /= 0 or else
        State.backendToken /= 0 or else PS.Current (State.frameState) /= PS.Idle or else
        BT.Current (State.Targets) not in BT.Unconfigured | BT.Idle
      then
         Result.Status := DSP.DP.Bad_State;
         --  Name what is still in use, so a refused mode change is
         --  explainable from Boot Logs.
         debugPrint ("display: mode change to " & Dimensions (Wanted.Value.Size.Width, Wanted.Value.Size.Height) &
           " refused, output busy:" &
           (if State.presentationFault then " presentation-fault" else "") &
           (if State.srcAcquired then " source-acquired" else "") &
           (if Pool_Registry.Count (State.Pool) /= 0 then " pools" &
              Pool_Registry.Count (State.Pool)'Image else "") &
           (if State.activeSession /= 0 then " session" else "") &
           (if State.backendToken /= 0 then " backend-call" else "") &
           (if PS.Current (State.frameState) /= PS.Idle then " frame-in-flight" else "") &
           (if BT.Current (State.Targets) not in BT.Unconfigured | BT.Idle then " targets-busy" else "") &
           LF);
      elsif Wanted.Value.Rate = OM.Best_Rate and then Wanted.Value.Size = Current then
         --  An exact rate goes through: the GPU knows the rate it runs.
         Result.Status := DSP.DP.Success;
      else
         Call := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
           (OM.Encode_Set_Mode (OD.GPU_Backend, Wanted.Value), selectedOutput));
         Call.tag := capCall (GPU_Service, Call,
                              CuBit.Messages.Deadline_After (GPU_Call_Deadline_Ms));
         Answer := OM.Decode_Result (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Call));
         if not Answer.Valid then
            debugPrint ("display: mode change to " & Dimensions (Wanted.Value.Size.Width, Wanted.Value.Size.Height) &
              ": no valid answer from the GPU" & LF);
         end if;
         if Answer.Valid then
            Result := Answer.Value;
            if Result.Status = DSP.DP.Success and then Result.Current /= Current then
               adoptMode (Result.Current);
               if State.presentationFault then
                  Result.Status := DSP.DP.Bad_State;
               end if;
            end if;
         end if;
      end if;
      replyMsg := CuBit.Desktop_Messages.From_Wire (OM.Encode_Result (OD.Display_Broker, Result));
   end handleSetMode;

   procedure handleRequest
      (from     : Process_ID;
       incoming : Message;
       replyMsg : out Message)
   is
      request : Message := incoming;
      Is_Ring_Open : Boolean;
   begin
      replyMsg := NULL_MESSAGE;
      statsRequests := statsRequests + 1;

      --  A frame ring's open (Desktop's presents and their answers).
      Frame_Rings.Handle_Open (from, request, Is_Ring_Open, replyMsg);
      if Is_Ring_Open then
         return;
      end if;

      if request.tag.label = OD.Code (OD.Display_Broker, OD.Get_Catalog) or else
         request.tag.label = OD.Code (OD.Display_Broker, OD.Get_Description)
      then
         if catalogReady and then outputUsable (outputStates (0).currentOutput) then
            replyMsg := CuBit.Desktop_Messages.From_Wire (OD.Respond
              (OD.Display_Broker, outputCatalog, catalogRevision,
               CuBit.Desktop_Messages.To_Wire (request)));
         else
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
         end if;
         return;
      end if;

      --  DISP-005/016: the output's monitor modes with the GPU's verdicts
      --  (List_Mode) and the monitor's EDID identity (Identify), read
      --  through from the output's GPU backend; other outputs answer
      --  Unsupported.
      if request.tag.label = OM.Code (OD.Display_Broker, OM.List_Mode) or else
        request.tag.label = OM.Code (OD.Display_Broker, OM.Identify)
      then
         declare
            Wire : constant CuBit.Display_Protocol.Wire_Message :=
              DSP.Without_Output (CuBit.Desktop_Messages.To_Wire (request));
            Output : constant DSP.Output_Number := DSP.Output_Of (CuBit.Desktop_Messages.To_Wire (request));
            Listing : constant Boolean := request.tag.label = OM.Code (OD.Display_Broker, OM.List_Mode);
            Wanted : constant OM.Index_Decoding :=
              (if Listing then OM.Decode_List_Request (OD.Display_Broker, Wire) else (Valid => False));
            Call : Message;
            Listed : OM.Listing_Decoding;
            Named : OM.Identity_Decoding;
         begin
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
            if (Wanted.Valid or else (not Listing and then Wire = OM.Identify_Request (OD.Display_Broker)))
              and then ((Output in Output_Index and then readyOutputs (Output) and then
                         outputStates (Output).backend.Kind = Native_GPU) or else
                        --  DISP-009/010: a monitor attached but not lit
                        --  (read-only: its name and modes for Settings).
                        (catalogReady and then
                         Detected.Find (outputCatalog, OD.Intel_Display, OD.Native_Output_Number (Output)) /= 0))
            then
               Call := CuBit.Desktop_Messages.From_Wire (DSP.With_Output
                 ((if Listing then OM.Encode_List_Request (OD.GPU_Backend, Wanted.Value)
                   else OM.Identify_Request (OD.GPU_Backend)), Output));
               Call.tag := capCall (GPU_Service, Call,
                                    CuBit.Messages.Deadline_After (GPU_Call_Deadline_Ms));
               if Listing then
                  Listed := OM.Decode_Listing (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Call));
                  if Listed.Valid then
                     replyMsg := CuBit.Desktop_Messages.From_Wire
                       (OM.Encode_Listing (OD.Display_Broker, Listed.Value));
                  else
                     replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                  end if;
               else
                  Named := OM.Decode_Identity (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Call));
                  if Named.Valid then
                     replyMsg := CuBit.Desktop_Messages.From_Wire
                       (OM.Encode_Identity (OD.Display_Broker, Named.Value));
                  end if;
               end if;
            end if;
         end;
         return;
      end if;

      if request.tag.label = OM.Code (OD.Display_Broker, OM.Watch) then
         declare
            Known : constant OM.Revision_Decoding :=
              OM.Decode_Watch (OD.Display_Broker, CuBit.Desktop_Messages.To_Wire (request));
         begin
            if not Known.Valid then
               replyMsg.tag := (label => request.tag.label, length => 1,
                                protocol => 0, ipc => <>, reserved => 0);
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif Known.Value /= catalogRevision then
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (OM.Encode_Watch (OD.Display_Broker, catalogRevision));
            else
               --  One watcher: a newer watch answers (supersedes) the older.
               answerOutputWatch;
               if saveInto (outputWatch) then
                  outputWatchArmed := True;
                  deferredReply := True;
               else
                  replyMsg.tag := (label => request.tag.label, length => 1,
                                   protocol => 0, ipc => <>, reserved => 0);
                  replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
               end if;
            end if;
         end;
         return;
      end if;

      if not DSP.Valid_Output (CuBit.Desktop_Messages.To_Wire (request)) or else
        request.tag.reserved > Unsigned_16 (Output_Index'Last) or else
        not readyOutputs (Output_Index (request.tag.reserved))
      then
         replyMsg.tag := (label => request.tag.label, length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
         return;
      end if;
      selectedOutput := Output_Index (request.tag.reserved);
      request.tag.reserved := 0;

      --  Admission is serialized, GPU completion is not. Preserve the source,
      --  lease and session until their captured continuation has retired.
      if PS.Current (outputStates (selectedOutput).frameState) /= PS.Idle and then
        request.tag.label not in OP_DISPLAY_GET_INFO | OP_DISPLAY_GET_STATUS |
          OP_SUBMIT_FRAME | Pool_Wire.Submit_Frame | PP.Submit_Plan_Frame and then
        --  Plane requests never touch the frame's buffers: pointer motion
        --  must not wait for presentation.
        not PP.Decode_Operation (request.tag.label).Valid
      then
         replyMsg.tag := (label => request.tag.label, length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
         return;
      end if;

      case request.tag.label is
         when Pool_Wire.Attach_Buffer =>
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            declare
               State : Output_State renames outputStates (selectedOutput);
               A : constant Pool_Wire.Attachment_Result :=
                 Pool_Wire.Decode_Attachment (CuBit.Desktop_Messages.To_Wire (request));
               Mapped : System.Address;
               Acquired, Accepted, Returned : Boolean;
            begin
               if not ownsDisplay (from) then
                  replyMsg.words (0) := DISPLAY_ERR_DENIED;
               elsif not A.Valid then
                  debugPrint ("display: pool attach refused: malformed request" & LF);
                  replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
               elsif State.presentationFault or else State.srcAcquired or else State.lentPool or else
                 State.activeSession /= 0 or else not Pool_Registry.Admits (State.Pool, A.Value)
               then null;
               elsif Natural (A.Value.Source.Layout.Width) > State.fbWidth or else
                 Natural (A.Value.Source.Layout.Height) > State.fbHeight
               then replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
               elsif State.backend.Kind = Native_GPU and then not ensureGpuScanout then
                  State.presentationFault := True;
               else
                  MG.Acquire (A.Value.Source.Grant, from, 0,
                    DSP.DP.Byte_Length (A.Value.Source.Layout), MG.Read_Access, Mapped, Acquired);
                  if not Acquired then
                     debugPrint ("display: pool attach refused: grant not acquired" & LF);
                     replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
                  else
                     Pool_Registry.Register (State.Pool, A.Value, Accepted);
                     if Accepted then
                        -- Registration retains one pin; each frame separately
                        -- reacquires live authority and returns that frame pin.
                        State.srcOwner := from;
                        State.sourceOutput := State.currentOutput;
                        State.srcWidth := Natural (A.Value.Source.Layout.Width);
                        State.srcHeight := Natural (A.Value.Source.Layout.Height);
                        State.srcPitch := A.Value.Source.Layout.Pitch;
                        replyMsg.words (0) := DISPLAY_OK;
                     else
                        MG.Return_Acquisition (A.Value.Source.Grant, Returned);
                        if not Returned then
                           State.presentationFault := True;
                           Pool_Registry.Quarantine (State.Pool);
                        end if;
                     end if;
                  end if;
               end if;
            end;

         when Pool_Wire.Lend_Buffer =>
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            declare
               State : Output_State renames outputStates (selectedOutput);
               A : constant Pool_Wire.Attachment_Result :=
                 Pool_Wire.Decode_Lend_Request (CuBit.Desktop_Messages.To_Wire (request));
               Mapped : System.Address;
               Acquired, Accepted, Returned : Boolean;
               use type DSP.DP.Buffer_Layout;
            begin
               if not ownsDisplay (from) then
                  replyMsg.words (0) := DISPLAY_ERR_DENIED;
               elsif not A.Valid then
                  debugPrint ("display: lend refused: malformed request" & LF);
                  replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
               elsif State.backend.Kind /= Native_GPU or else
                 BT.Count (State.Targets) /= Pool_Wire.Buffer_Slot'Last or else
                 A.Value.Source.Layout /= (DSP.DP.Positive_Extent (State.fbWidth),
                   DSP.DP.Positive_Extent (State.fbHeight), DSP.DP.Buffer_Pitch (State.fbPitch))
               then
                  --  The firmware framebuffer (or two buffers): copy instead.
                  replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
               elsif State.presentationFault or else State.srcAcquired or else
                 State.activeSession /= 0 or else
                 (Pool_Registry.Count (State.Pool) /= 0 and then not State.lentPool) or else
                 not Pool_Registry.Admits (State.Pool, A.Value)
               then null;
               elsif not ensureGpuScanout then
                  State.presentationFault := True;
               else
                  --  The client's own slot buffer (as Attach_Buffer): proof of
                  --  who asks, and its end tells display the owner ended
                  --  (ownerEnded). A lent frame reads none of it.
                  MG.Acquire (A.Value.Source.Grant, from, 0,
                              DSP.DP.Byte_Length (A.Value.Source.Layout) + FD.Record_Bytes,
                              MG.Read_Access, Mapped, Acquired);
                  if not Acquired then
                     debugPrint ("display: lend refused: slot buffer not acquired" & LF);
                     replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
                  else
                     lendSlot (State, from, A.Value, Accepted);
                     if Accepted then
                        Pool_Registry.Register (State.Pool, A.Value, Accepted);
                     end if;
                     if Accepted then
                        State.lentPool := True;
                        State.srcOwner := from;
                        State.sourceOutput := State.currentOutput;
                        State.srcWidth := State.fbWidth;
                        State.srcHeight := State.fbHeight;
                        State.srcPitch := State.fbPitch;
                        replyMsg := CuBit.Desktop_Messages.From_Wire (Pool_Wire.Encode
                          (Pool_Wire.Loan'(A.Value.Buffer, (State.lendChild (A.Value.Buffer), A.Value.Source.Layout),
                            Gpu_Buffer_Index (A.Value.Buffer - 1) = BT.Active (State.Targets))));
                     else
                        if State.lendChildLive (A.Value.Buffer) then
                           MG.Revoke (State.lendChild (A.Value.Buffer), Returned);
                           State.lendChildLive (A.Value.Buffer) := False;
                        end if;
                        if Pool_Registry.Count (State.Pool) = 0 then endLoans (State); end if;
                        MG.Return_Acquisition (A.Value.Source.Grant, Returned);
                        if not Returned then
                           State.presentationFault := True;
                           Pool_Registry.Quarantine (State.Pool);
                        end if;
                     end if;
                  end if;
               end if;
            end;

         when Pool_Wire.Open_Session =>
            replyMsg.tag := (label => request.tag.label, length => 4,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            declare
               State : Output_State renames outputStates (selectedOutput);
               Accepted : Boolean;
            begin
               if not Pool_Wire.Valid_Open (CuBit.Desktop_Messages.To_Wire (request)) then
                  replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
               elsif not ownsDisplay (from) or else State.srcOwner /= from then
                  replyMsg.words (0) := DISPLAY_ERR_DENIED;
               elsif not State.presentationFault and then sourceOnCurrentOutput and then
                 State.activeSession = 0 and then sessionSequence < Unsigned_64'Last
               then
                  Pool_Registry.Open (State.Pool, Accepted);
                  if Accepted then
                     sessionSequence := sessionSequence + 1;
                     State.activeSession := sessionSequence;
                     State.sessionOutput := State.sourceOutput;
                     State.lastFrame := 0;
                     replyMsg.words (0) := DISPLAY_OK;
                     replyMsg.words (1) := State.activeSession;
                     replyMsg.words (2) := 3;
                     replyMsg.words (3) := 1; -- contract version
                     --  Which path this session runs (UI-020).
                     debugPrint ((if State.lentPool
                       then "display: session lent scanout: the compositor composes into the scanout buffers"
                       else "display: session copies frames into the scanout") & LF);
                  end if;
               end if;
            end;

         when OP_OPEN_SESSION =>
            replyMsg.tag := (label => request.tag.label, length => 4,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            if not DSP.Valid_Open_Session (CuBit.Desktop_Messages.To_Wire (request)) then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif not ownsDisplay (from) or else outputStates (selectedOutput).srcOwner /= from then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif outputStates (selectedOutput).srcAcquired and then sourceOnCurrentOutput and then not outputStates (selectedOutput).presentationFault and then
              PS.Current (outputStates (selectedOutput).frameState) = PS.Idle and then sessionSequence < Unsigned_64'Last
            then
               declare
                  mapped : System.Address;
                  acquired, returned : Boolean;
               begin
                  MG.Acquire (outputStates (selectedOutput).srcGrant, from, 0,
                    Unsigned_64 (outputStates (selectedOutput).srcPitch) * Unsigned_64 (outputStates (selectedOutput).srcHeight), MG.Read_Access,
                    mapped, acquired);
                  if acquired then
                     MG.Return_Acquisition (outputStates (selectedOutput).srcGrant, returned);
                     if returned then
                        sessionSequence := sessionSequence + 1;
                        outputStates (selectedOutput).activeSession := sessionSequence;
                        outputStates (selectedOutput).sessionOutput := outputStates (selectedOutput).sourceOutput;
                        outputStates (selectedOutput).lastFrame := 0;
                        replyMsg.words (0) := DISPLAY_OK;
                        replyMsg.words (1) := outputStates (selectedOutput).activeSession;
                     else
                        outputStates (selectedOutput).presentationFault := True;
                     end if;
                  end if;
               end;
            end if;

         when OP_SUBMIT_FRAME | Pool_Wire.Submit_Frame =>
            submitFrame (from, request, replyMsg);

         when PP.Submit_Plan_Frame =>
            declare
               Tagged_Frame : constant PP.Plan_Frame_Decoding :=
                 PP.Decode_Plan_Frame (CuBit.Desktop_Messages.To_Wire (request));
            begin
               if Tagged_Frame.Valid then
                  pendingFrameEpoch := Tagged_Frame.Value.Epoch;
                  submitFrame (from, CuBit.Desktop_Messages.From_Wire
                    (Pool_Wire.Encode (Tagged_Frame.Value.Item)), replyMsg);
                  pendingFrameEpoch := DPL.No_Epoch;
               else
                  replyMsg.tag := (label => request.tag.label, length => 1,
                                   protocol => 0, ipc => <>, reserved => 0);
                  replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
               end if;
            end;

         when OP_DISPLAY_GET_INFO =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_INFO,
                             length => 4,
                             protocol  => 0, ipc => <>,
                             reserved  => 0);
            replyMsg.words (0) := Unsigned_64 (outputStates (selectedOutput).fbWidth);
            replyMsg.words (1) := Unsigned_64 (outputStates (selectedOutput).fbHeight);
            replyMsg.words (2) := Unsigned_64 (outputStates (selectedOutput).fbPitch);
            replyMsg.words (3) := 32; -- BGRA8888

         when OP_DISPLAY_GET_STATUS =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_STATUS,
                             length => 4,
                             protocol  => 0, ipc => <>,
                             reserved  => 0);
            replyMsg.words (0) := backendId;
            replyMsg.words (1) := backendCaps;
            replyMsg.words (2) := To_Word (outputStates (selectedOutput).displayOwner);
            replyMsg.words (3) := 0; -- reserved for backend-specific status

         when OP_DISPLAY_ACQUIRE =>
            replyMsg.tag := (label => OP_DISPLAY_ACQUIRE,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if not DSP.Valid_Lease_Request
              (CuBit.Desktop_Messages.To_Wire (request), DSP.Acquire_Display)
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif outputStates (selectedOutput).presentationFault or else
              not outputUsable (outputStates (selectedOutput).currentOutput)
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif outputStates (selectedOutput).displayOwner = No_Process or else outputStates (selectedOutput).displayOwner = from then
               outputStates (selectedOutput).displayOwner := from;
               outputStates (selectedOutput).leasedOutput := outputStates (selectedOutput).currentOutput;
               replyMsg.words (0) := DISPLAY_OK;
               CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.Display_Leased, Startup.Registration);
            elsif ownerEnded then
               --  The previous owner ended without releasing (BUG-001): its
               --  frames have finished (admission waits for that), so return
               --  what it lent and lease the output to the newcomer.
               declare
                  Ended : constant Process_ID := outputStates (selectedOutput).displayOwner;
               begin
                  detachOwnerBuffer;
                  if outputStates (selectedOutput).presentationFault then
                     replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                  else
                     if selectedOutput = 0 and then requestOwner = Ended then
                        resetRequests;
                     end if;
                     outputStates (selectedOutput).displayOwner := from;
                     outputStates (selectedOutput).leasedOutput := outputStates (selectedOutput).currentOutput;
                     debugPrint ("display: lease of ended owner pid" &
                                 CuBit.Process_IDs.Text.Image (Ended) &
                                 " released; output" & Output_Index'Image (selectedOutput) &
                                 " leased to pid" & CuBit.Process_IDs.Text.Image (from) & LF);
                     replyMsg.words (0) := DISPLAY_OK;
                  end if;
               end;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_RELEASE =>
            replyMsg.tag := (label => OP_DISPLAY_RELEASE,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if not DSP.Valid_Lease_Request
              (CuBit.Desktop_Messages.To_Wire (request), DSP.Release_Display)
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif outputStates (selectedOutput).displayOwner = from and then not outputStates (selectedOutput).presentationFault then
               detachOwnerBuffer;
               if outputStates (selectedOutput).presentationFault then
                  replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
               else
                  outputStates (selectedOutput).displayOwner := No_Process;
                  outputStates (selectedOutput).leasedOutput := Outputs.No_Output;
                  if selectedOutput = 0 and then requestOwner = from then
                     resetRequests;
                  end if;
                  replyMsg.words (0) := DISPLAY_OK;
               end if;
            elsif outputStates (selectedOutput).displayOwner = No_Process then
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_ATTACH_BUFFER =>
            replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif Pool_Registry.Count (outputStates (selectedOutput).Pool) /= 0 then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            else
               declare
                  decoded : constant DSP.Attachment_Decoding :=
                    DSP.Decode_Attachment
                      (CuBit.Desktop_Messages.To_Wire (request));
                  acquired : Boolean;
                  mapped : System.Address;
               begin
                  if not decoded.Valid then
                     replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
                  elsif Natural (decoded.Value.Layout.Width) > outputStates (selectedOutput).fbWidth or else
                    Natural (decoded.Value.Layout.Height) > outputStates (selectedOutput).fbHeight
                  then
                     replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
                  elsif outputStates (selectedOutput).backend.Kind = Native_GPU and then not ensureGpuScanout then
                     outputStates (selectedOutput).presentationFault := True;
                     replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                  else
                     MG.Acquire
                       (decoded.Value.Grant, from, 0,
                        DSP.DP.Byte_Length (decoded.Value.Layout),
                        MG.Read_Access, mapped, acquired);
                     if not acquired then
                        replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
                     else
                        --  Failed replacements preserve the current buffer.
                        --  Acquire first, even when replacing with the same
                        --  reference: the new pin survives returning the old.
                        detachOwnerBuffer;
                        outputStates (selectedOutput).srcGrant := decoded.Value.Grant;
                        outputStates (selectedOutput).srcAcquired := True;
                        outputStates (selectedOutput).srcAddr := mapped;
                        outputStates (selectedOutput).srcWidth := Natural (decoded.Value.Layout.Width);
                        outputStates (selectedOutput).srcHeight := Natural (decoded.Value.Layout.Height);
                        outputStates (selectedOutput).srcPitch := decoded.Value.Layout.Pitch;
                        outputStates (selectedOutput).srcOwner := from;
                        outputStates (selectedOutput).sourceOutput := outputStates (selectedOutput).currentOutput;
                        if outputStates (selectedOutput).backend.Kind = Native_GPU then
                           debugPrint ("display: gpu copy buffer attached" & LF);
                        end if;
                        replyMsg.words (0) := DISPLAY_OK;
                        debugPrint ("display: buffer attached" & LF);
                     end if;
                  end if;
               end;
            end if;

         when OP_DISPLAY_MAP_BACKBUFFER =>
            replyMsg.tag := (label => OP_DISPLAY_MAP_BACKBUFFER,
                             length => 4, protocol => 0, ipc => <>, reserved => 0);
            if not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            else
               --  A direct mapping would derive a client loan from the GPU's
               --  loan to display.svc. Generic grants deliberately reject
               --  that lifetime-unsafe operation. Keep the wire operation
               --  explicit but unavailable until a derived-loan primitive
               --  can preserve the parent range, rights, and revocation.
               replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
            end if;

         when OP_DISPLAY_PRESENT_RECT |
              OP_DISPLAY_PRESENT_IMMEDIATE_RECT =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else
              Pool_Registry.Count (outputStates (selectedOutput).Pool) /= 0 or else
              outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif outputStates (selectedOutput).srcOwner /= from or else not sourceOnCurrentOutput or else
              outputStates (selectedOutput).srcAddr = System.Null_Address then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            else
               if request.tag.length >= 4 then
                  queuePresent
                    ((x => Natural (request.words (0)),
                      y => Natural (request.words (1)),
                      w => Natural (request.words (2)),
                      h => Natural (request.words (3))));
                  --  Synchronous present form: copy/flush before replying so
                  --  single-buffer clients can safely draw their next frame.
                  --  Async packed presents remain queued for clients that have
                  --  their own buffering or can tolerate eventual scanout.
                  flushPendingPresent;
               else
                  --  Packed async form used by capSubmit: word0 = x/y,
                  --  word1 = w/h. This keeps fire-and-forget present within
                  --  the current three-word async submit ABI.
                  queuePresent
                    ((x => unpackLo32 (request.words (0)),
                      y => unpackHi32 (request.words (0)),
                      w => unpackLo32 (request.words (1)),
                      h => unpackHi32 (request.words (1))));
               end if;
               replyMsg.words (0) :=
                 (if outputStates (selectedOutput).presentationFault then
                    DISPLAY_ERR_BAD_STATE else DISPLAY_OK);
            end if;

         when OP_DISPLAY_PRESENT_REGION |
              OP_DISPLAY_PRESENT_IMMEDIATE_REGION =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else
              Pool_Registry.Count (outputStates (selectedOutput).Pool) /= 0 or else
              outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif outputStates (selectedOutput).srcOwner /= from or else not sourceOnCurrentOutput or else
              outputStates (selectedOutput).srcAddr = System.Null_Address then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif request.tag.length = 0 or else
              Natural (request.tag.length) > request.words'Length
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            else
               presentPackedRegion (request);
               replyMsg.words (0) :=
                 (if outputStates (selectedOutput).presentationFault then
                    DISPLAY_ERR_BAD_STATE else DISPLAY_OK);
            end if;

         when OP_DISPLAY_CLEAR =>
            replyMsg.tag := (label => OP_DISPLAY_CLEAR,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else
              Pool_Registry.Count (outputStates (selectedOutput).Pool) /= 0 or else
              outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif clearOutput (request.words (0)) then
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            end if;

         when others =>
            declare
               Plane_Kind : constant PP.Operation_Decoding :=
                 PP.Decode_Operation (request.tag.label);
            begin
               if request.tag.label = OM.Code (OD.Display_Broker, OM.Set_Mode) then
                  handleSetMode (from, request, replyMsg);
               elsif Plane_Kind.Valid then
                  handlePlaneRequest (from, Plane_Kind.Value, request, replyMsg);
               else
                  replyMsg.tag := (label  => request.tag.label,
                                   length => 1,
                                   protocol  => 0, ipc => <>,
                                   reserved  => 0);
                  replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
               end if;
            end;
      end case;

   end handleRequest;

   ret     : Unsigned_64;
   --  PWR-003: procmgr's Stop (shutdown). How long a present already
   --  submitted to the GPU driver may take to complete before display lets
   --  go of its buffers anyway: a flip lands at the next vertical blank
   --  (17 ms at 60 Hz), well inside display's 500 ms stop grace.
   Stop_Present_Wait_Ms : constant := 100;

   --  The Stop: every output lets go of what it holds, in order, and the
   --  GPU keeps scanning out the last frame (nothing is cleared: the screen
   --  stays as it was until S5).
   --    1. A present in flight completes (bounded by Stop_Present_Wait_Ms).
   --    2. The owner's session ends (detachOwnerBuffer): the loans display
   --       derived for Desktop are revoked (children before their parents),
   --       then the grants Desktop lent are returned. Desktop may have
   --       stopped already: its grants are then gone, and returning one
   --       that is gone is not an error here.
   --    3. The GPU driver's forwardable grants behind the loans are
   --       returned, then display's mappings of its scanout buffers.
   --  Serve one present from a frame ring as the frame request it carries,
   --  and answer it through the ring: now, or when its present completes.
   procedure serveFramePresent
     (Ring : Frame_Rings.Ring_Number; From : Process_ID; Tag : Display_Frame_Ring.RC.Token;
      Item : Display_Frame_Ring.Present; Valid : Boolean)
   is
      Taken_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
      Answer : Message := NULL_MESSAGE;
   begin
      --  Only frames travel here; anything else is answered as malformed.
      if not Valid or else
        (Item.Frame.Label /= Pool_Wire.Submit_Frame and then Item.Frame.Label /= PP.Submit_Plan_Frame)
      then
         Answer.tag := (label => (if Valid then Item.Frame.Label else 0), length => 1,
                        protocol => 0, ipc => <>, reserved => 0);
         Answer.words (0) := DISPLAY_ERR_BAD_OBJECT;
         Frame_Rings.Answer (Ring, Tag, CuBit.Desktop_Messages.To_Wire (Answer), Taken_Us);
         return;
      end if;
      servingRing := Ring;
      servingTag := Tag;
      servingTakenUs := Taken_Us;
      servingDamage := Item.Damage;
      deferredReply := False;
      handleRequest (From, CuBit.Desktop_Messages.From_Wire (Item.Frame), Answer);
      if outputStates (selectedOutput).pendingPresent then
         flushPendingPresent;
      end if;
      if not deferredReply then
         Frame_Rings.Answer (Ring, Tag, CuBit.Desktop_Messages.To_Wire (Answer), Taken_Us);
      end if;
      deferredReply := False;
      servingRing := Frame_Rings.No_Ring;
      servingDamage := [others => 0];
   end serveFramePresent;

   procedure stopOutputs is
      Saved : constant Output_Index := selectedOutput;
      Started : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
      Activity : Activity_Result;
      Returned : Boolean;
      Loans, Lent, Mapped, Lent_Held, Mapped_Held : Natural;
      pragma Unreferenced (Activity);
   begin
      --  1. Presents in flight, on every output.
      loop
         collectFrames;
         exit when (for all O in Output_Index =>
                      not readyOutputs (O) or else
                      (outputStates (O).backendToken = 0 and then outputStates (O).legacyToken = 0));
         exit when CuBit.Published_Clock.Milliseconds - Started >= Stop_Present_Wait_Ms;
         Activity := Wait_For_Activity_Until (Started + Stop_Present_Wait_Ms);
      end loop;
      for O in Output_Index loop
         if readyOutputs (O) then
            selectedOutput := O;
            declare
               State : Output_State renames outputStates (O);
            begin
               Loans := 0;
               for B in Pool_Wire.Buffer_Slot loop
                  if State.lendChildLive (B) then
                     Loans := Loans + 1;
                  end if;
               end loop;
               --  2. Loans revoked, Desktop's grants returned.
               detachOwnerBuffer;
               --  3. The GPU driver's grants, loans' parents first.
               Lent := 0;
               Mapped := 0;
               Lent_Held := 0;
               Mapped_Held := 0;
               for Index in 0 .. BT.Count (State.Targets) - 1 loop
                  if State.lendParentLive (Index) then
                     Lent_Held := Lent_Held + 1;
                     MG.Return_Acquisition (State.lendParent (Index), Returned);
                     State.lendParentLive (Index) := False;
                     if Returned then
                        Lent := Lent + 1;
                     end if;
                  end if;
               end loop;
               for Index in 0 .. BT.Count (State.Targets) - 1 loop
                  if State.gpuScanoutAddr (Index) /= System.Null_Address then
                     Mapped_Held := Mapped_Held + 1;
                     MG.Return_Acquisition (State.gpuScanoutGrant (Index), Returned);
                     State.gpuScanoutAddr (Index) := System.Null_Address;
                     if Returned then
                        Mapped := Mapped + 1;
                     end if;
                  end if;
               end loop;
               debugPrint ("display: Stop: output" & Output_Index'Image (O) & ":" &
                           Natural'Image (Loans) & " loans revoked; returned to the GPU driver" &
                           Natural'Image (Lent) & " of" & Natural'Image (Lent_Held) & " lent buffers and" &
                           Natural'Image (Mapped) & " of" & Natural'Image (Mapped_Held) & " scanout mappings" &
                           (if State.backendToken /= 0 or else State.legacyToken /= 0
                            then " (a present was still in flight)" else "") & LF);
            end;
         end if;
      end loop;
      selectedOutput := Saved;
   end stopOutputs;

   from    : Process_ID;
   msg     : Message;
   replyMsg : Message := NULL_MESSAGE;
   found : Boolean;
   eventMsg : aliased Message;
   ringsReady, served : Boolean;
   woke : Wake_Result;
   stopping : Boolean;

   --  A kernel notice: the Stop (let go of the buffers in order, then end
   --  with exit code 0: Stopping), a frame ring's wake or end.
   --  One event, if the readiness page says one may be queued.
   function pollEvent (Notice : out Message) return Boolean is
      Seen : constant CuBit.Readiness.Count := CuBit.Readiness.Produced (CuBit.Readiness.Events);
   begin
      if Poll_Event (Notice) then
         return True;
      end if;
      CuBit.Readiness.Caught_Up (displayReady, CuBit.Readiness.Events, Seen);
      return False;
   end pollEvent;

   procedure takeEvent (Notice : Message; Stopping : out Boolean) is
      Handled : Boolean;
      Ended : Frame_Rings.Ring_Number;
   begin
      Stopping := False;
      --  Only the kernel posts the Stop's label.
      if Is_Kernel_Stop (Notice.tag.label, Notice.tag.length, No_Process, Notice.words (0)) then
         stopOutputs;
         Stopping := True;
         return;
      end if;
      Frame_Rings.Take_Notice (Notice, Handled, Ended);
      if Ended /= Frame_Rings.No_Ring then
         forgetRing (Ended);
      end if;
   end takeEvent;
begin
   --  ABI-002: with logstore (the manifest declares it optional), Display's
   --  debug text goes into its logstore ring, not the kernel console.
   if Startup.Logstore.Present then
      CuBit.Console_Route.Route_To (Startup.Logstore.Value);
   end if;
   debugPrint ("display: starting" & LF);
   --  Its statistics say which calls it makes (display: stats ... calls).
   CuBit.Call_Census.Enabled := True;

   ret := setLatencyContract
      (LATENCY_REALTIME,
       16_667,  --  60 Hz scanout period in microseconds.
       2_000);  --  Budget hint for coalesced present/flush work.
   if ret = Unsigned_64'Last then
      debugPrint ("display: latency contract rejected" & LF);
   end if;

   if Registered_Driver (DRIVER_DISPLAY) /= No_Process then
      --  display.svc owns the visible scanout. A second copy would clear the
      --  screen and steal the well-known display role, so treat manual
      --  duplicate launches as harmless no-ops.
      debugPrint ("display: already running, exiting" & LF);
      ret := syscall (SYSCALL_EXIT, 0);
      return;
   end if;

   ret := registerDriver (DRIVER_DISPLAY, Startup.Registration);
   if ret = Unsigned_64'Last then
      debugPrint ("display: register failed" & LF);
   end if;

   setupBackend;
   if (outputStates (0).backend.Kind = Native_GPU and then not ensureGpuScanout) or else
     not clearOutput (16#0013_1518#) or else not registerOutput
   then
      debugPrint ("display: boot output initialization failed" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;
   readyOutputs (0) := True;
   debugPrint ("display: boot output registered" & LF);
   discoverOutputs;
   if catalogReady then
      debugPrint ("display: output catalog ready" & LF);
   else
      debugPrint ("display: output catalog unavailable" & LF);
   end if;
   --  Two independently sized native outputs.
   --  Firmware-only and independently detected PCI outputs keep their existing
   --  behavior. Logical arrangement and the primary role belong to Desktop.
   if outputStates (0).backend.Kind = Native_GPU and then catalogReady then
      for Item of outputCatalog.Items loop
         if Item.Source = OD.Virtio_GPU and then Item.Native_Number = 1 and then
           Item.Role = OD.Backend_Ready
         then
            selectedOutput := 1;
            outputStates (1).fbWidth := Item.Current_Width;
            outputStates (1).fbHeight := Item.Current_Height;
            outputStates (1).fbPitch := Item.Current_Width * 4;
            outputStates (1).backend := (Kind => Native_GPU);
            if ensureGpuScanout and then clearOutput (16#0013_1518#) and then registerOutput then
               readyOutputs (1) := True;
               debugPrint ("display: second output ready" & LF);
            end if;
         end if;
      end loop;
      selectedOutput := 0;
   end if;
   for Output in Output_Index loop
      if readyOutputs (Output) then
         discoverPlanes (Output);
      end if;
   end loop;
   replan;
   armGpuWatch;
   Frame_Rings.Configure (ringsReady);
   if not ringsReady then
      debugPrint ("display: frame rings unavailable (no memory for their table)" & LF);
   end if;
   debugPrint ("display: ready" & LF);

   loop
      collectFrames;
      checkPresentDeadlines;
      retryMoves;
      found := False;
      if CuBit.Readiness.Pending (displayReady, CuBit.Readiness.Requests) then
         declare
            Seen : constant CuBit.Readiness.Count := CuBit.Readiness.Produced (CuBit.Readiness.Requests);
         begin
            Poll_Service_Request (from, msg, found);
            if found then
               CuBit.Readiness.Took (displayReady, CuBit.Readiness.Requests);
            else
               CuBit.Readiness.Caught_Up (displayReady, CuBit.Readiness.Requests, Seen);
            end if;
         end;
      end if;
      served := False;
      if found then
         deferredReply := False;
         handleRequest (from, msg, replyMsg);
         if outputStates (selectedOutput).pendingPresent then
            flushPendingPresent;
         end if;
         if not deferredReply then ret := reply (from, replyMsg); end if;
      else
         --  The frame rings once the request messages are served, so a
         --  frame follows what its sender asked before it (a plane plan).
         declare
            Got, Valid : Boolean;
            Ring : Frame_Rings.Ring_Number;
            Sender : Process_ID;
            Tag : Display_Frame_Ring.RC.Token;
            Item : Display_Frame_Ring.Present;
         begin
            loop
               Frame_Rings.Next_Present (Got, Ring, Sender, Tag, Item, Valid);
               exit when not Got;
               served := True;
               serveFramePresent (Ring, Sender, Tag, Item, Valid);
            end loop;
         end;
      end if;
      maybePrintStats;
      if not found and then not served then
         if CuBit.Readiness.Pending (displayReady, CuBit.Readiness.Events) and then
           pollEvent (eventMsg)
         then
            takeEvent (eventMsg, stopping);
            if stopping then
               return;
            end if;
         elsif not Frame_Rings.Quiet then
            --  A present arrived while the rings were armed: serve it.
            null;
         else
            --  The notice that wakes Display (Desktop's ring rang) comes
            --  back in the same call: no poll to find it.
            Wait_For_Activity_Or_Event
              ((if presentPending then CuBit.Published_Clock.Milliseconds + Present_Check_Ms
                else Unsigned_64'Last), eventMsg, woke);
            if woke = Unavailable then
               debugPrint ("display: activity wait unavailable" & LF);
               return;
            elsif woke = Event_Taken then
               takeEvent (eventMsg, stopping);
               if stopping then
                  return;
               end if;
            end if;
         end if;
      end if;
   end loop;
end main;
