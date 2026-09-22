------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display scanout service prototype
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Display_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Memory_Grants;
with CuBit.Presentation_State;
with CuBit.Display_Outputs;
with CuBit.Output_Discovery;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with Presentation_Test_Policy;

procedure main is
   package DSP renames CuBit.Display_Protocol;
   use type DSP.Frame_Outcome;
   package MG renames CuBit.Memory_Grants;
   package PS is new CuBit.Presentation_State;
   use type DSP.Output_Number;
   package Outputs renames CuBit.Display_Outputs.Registry;
   package OD renames CuBit.Output_Discovery;
   package GM renames CuBit.Graphics_Metrics;
   backendCopies, repairCopies : GM.Counter;
   backendReporter, repairReporter : CuBit.Graphics_Metrics_IO.Reporter;
   use type OD.Output_Role, OD.Output_Source, OD.Query;
   use type Outputs.Mutation_Result, Outputs.Output_Reference;
   use type PS.Phase, PS.Admission;
   use ASCII;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;

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

   CAP_SLOT_GPU : constant CapabilitySlot := 9;

   type Rect is record
      x, y, w, h : Natural := 0;
   end record;
   subtype Gpu_Buffer_Index is Natural range 0 .. 1;
   type Gpu_Address_Array is
     array (Gpu_Buffer_Index) of System.Address;

   --  This is an owner-local routing number, not a primary-display role.
   --  Request handlers are serialized, but GPU replies are deferred. Their
   --  continuations carry captured output/session identity explicitly.
   subtype Output_Index is DSP.Output_Number range 0 .. 1;
   selectedOutput : Output_Index := 0;
   type Output_State is record
      fbWidth  : Natural := 0;
      fbHeight : Natural := 0;
      fbPitch  : Natural := 0;
      fbBpp    : Natural := 0;
      fbAddr   : System.Address := System.Null_Address;
      srcAddr   : System.Address := System.Null_Address;
      srcWidth  : Natural := 0;
      srcHeight : Natural := 0;
      srcPitch  : Natural := 0;
      srcOwner  : ProcessID := NO_PROCESS;
      srcGrant : MG.Grant_Reference;
      srcAcquired : Boolean := False;
      activeSession : Unsigned_64 := 0;
      lastFrame : Unsigned_64 := 0;
      frameState : PS.State;
      presentationFault : Boolean := False;
      gpuAvailable : Boolean := False;
      gpuCopyActive  : Boolean := False;
      gpuScanoutAddr : Gpu_Address_Array := [others => System.Null_Address];
      gpuScanoutWidth   : Natural := 0;
      gpuScanoutHeight  : Natural := 0;
      gpuScanoutPitch   : Natural := 0;
      gpuActiveBuffer : Gpu_Buffer_Index := 0;
      displayOwner : ProcessID := NO_PROCESS;
      currentOutput : Outputs.Output_Reference := Outputs.No_Output;
      leasedOutput : Outputs.Output_Reference := Outputs.No_Output;
      sourceOutput : Outputs.Output_Reference := Outputs.No_Output;
      sessionOutput : Outputs.Output_Reference := Outputs.No_Output;
      outputRebound : Boolean := False;
      gpuPreviousDamage : Rect;
      pendingPresent : Boolean := False;
      pendingRect : Rect;
      --  Captured continuation; never resolve a completion through whatever
      --  output the most recently received request happened to select.
      backendToken : Unsigned_64 := 0;
      backendFrame : DSP.Frame_Result;
      backendID : PS.Submission_ID := PS.No_Submission;
      backendTarget : Gpu_Buffer_Index := 0;
      backendDamage : Rect;
   end record;
   outputStates : array (Output_Index) of Output_State;
   readyOutputs : array (Output_Index) of Boolean := [others => False];
   sessionSequence : Unsigned_64 := 0;
   backendSequence : Unsigned_64 := 0;
   deferredReply : Boolean := False;
   function frameReplySlot (Output : Output_Index) return CapabilitySlot is
     (CapabilitySlot (32 + Natural (Output)));
   gpuDetected : Boolean := False;
   outputCatalog : OD.Catalog;
   catalogReady : Boolean := False;
   outputRegistry : Outputs.State (1);

   function outputUsable (item : Outputs.Output_Reference) return Boolean is
     (Outputs.Live (outputRegistry, item) and then
      Outputs.Presentable (Outputs.Describe (outputRegistry, item)));

   function sourceOnCurrentOutput return Boolean is
     (outputStates (selectedOutput).sourceOutput = outputStates (selectedOutput).currentOutput and then outputUsable (outputStates (selectedOutput).sourceOutput));

   function registerOutput return Boolean is
      result : Outputs.Mutation_Result;
   begin
      if outputStates (selectedOutput).fbWidth not in 1 .. Natural (Outputs.W.Extent'Last) or else
        outputStates (selectedOutput).fbHeight not in 1 .. Natural (Outputs.W.Extent'Last)
      then
         return False;
      end if;
      --  Provisional name, not an EDID identity or a saved monitor preference.
      --  Ready describes the selected mapped backend, not panel visibility.
      Outputs.Register
        (outputRegistry,
         (Backend => (Driver => 1, Number => Outputs.Output_Number (selectedOutput)),
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
   statsRequests : Unsigned_64 := 0;
   statsQueued   : Unsigned_64 := 0;
   statsPresents : Unsigned_64 := 0;
   statsWaitMs   : Unsigned_64 := 0;
   statsCopyMs   : Unsigned_64 := 0;
   statsPixels   : Unsigned_64 := 0;

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
      if outputStates (selectedOutput).gpuAvailable then
         return DISPLAY_BACKEND_VIRTIO_GPU;
      else
         return DISPLAY_BACKEND_LINEAR_FB;
      end if;
   end backendId;

   function backendCaps return Unsigned_64 is
   begin
      if outputStates (selectedOutput).gpuAvailable then
         --  Directly handing the GPU's received framebuffer grant to a
         --  client would be an untracked re-grant. Until CuBit has an
         --  explicit attenuating derived-loan operation, display.svc owns
         --  the scanout mapping and copies client damage into it.
         return DISPLAY_CAP_COPY_PRESENT or DISPLAY_CAP_GPU_PRESENT or
           DISPLAY_CAP_PAGE_FLIP;
      else
         --  A bootloader/firmware linear framebuffer does not provide a
         --  trustworthy vertical-blank event. In particular, polling the
         --  legacy VGA status port on modern Intel hardware can spin forever
         --  (and used to cost one syscall per poll). Present immediately until
         --  a hardware display backend supplies real flip completion events.
         return DISPLAY_CAP_COPY_PRESENT;
      end if;
   end backendCaps;

   function callGpu
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label => label, length => 4, flags => 0,
                     reserved => Unsigned_16 (selectedOutput)),
         authorityTag => 0,
         words    => [w0, w1, w2, w3]);
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_GPU, msg);
      msg.tag := tag;
      return msg;
   end callGpu;

   procedure setupBackend is
      status : Message;
      gpuPrimary : constant Boolean := getInfo (SYSINFO_GPU_IS_PRIMARY) /= 0;
   begin
      status := callGpu (OP_GPU_GET_STATUS);
      gpuDetected := status.tag.label = OP_GPU_GET_STATUS and then
        status.tag.length = 4 and then status.words (0) = 0 and then
        status.words (1) = 1;
      if status.tag.length >= 4 and then status.words (0) = 0 and then
         gpuPrimary and then
         status.words (2) = Unsigned_64 (outputStates (selectedOutput).fbWidth) and then
         status.words (3) = Unsigned_64 (outputStates (selectedOutput).fbHeight)
      then
         outputStates (selectedOutput).gpuAvailable := True;
         debugPrint ("display: backend virtio-gpu" & LF);
      elsif status.tag.length >= 1 and then status.words (0) = 0 then
         --  QEMU can expose a separate virtio-gpu-pci scanout while the
         --  visible console is still the bootloader framebuffer. In that
         --  shape the GPU service is real, but presenting the desktop through
         --  it makes the UI disappear from the window the user is watching.
         debugPrint ("display: gpu not primary, using linear-fb" & LF);
      else
         --  Keep the fallback path on the bootloader-provided linear
         --  framebuffer. This remains useful for hardware without virtio-gpu
         --  and for debugging the GPU service itself.
         debugPrint ("display: backend linear-fb" & LF);
      end if;
   end setupBackend;

   procedure discoverOutputs is
      Candidate : OD.Catalog;
      Seen : array (OD.Native_Output_Number) of Boolean := [others => False];
      Selected : Boolean := False;
      Response : Message;
      Summary : OD.Summary_Decoding;
      Item : OD.Description_Decoding;
   begin
      if not outputStates (selectedOutput).gpuAvailable then
         if outputStates (selectedOutput).fbWidth not in OD.Extent or else outputStates (selectedOutput).fbHeight not in OD.Extent then
            return;
         end if;
         Candidate.Count := 1;
         Candidate.Items (1) :=
           (OD.Selected_For_Desktop, OD.Boot_Framebuffer, 0,
            outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight, outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight);
         Selected := True;
      end if;
      if gpuDetected then
         Response := callGpu (OD.Code (OD.GPU_Backend, OD.Get_Catalog));
         Summary := OD.Decode_Summary
           (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (Response));
         if not Summary.Valid or else Summary.Value.Count > 16 then
            return;
         end if;
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
            if outputStates (selectedOutput).gpuAvailable and then Item.Value.Item.Native_Number = 0 then
               if Item.Value.Item.Role /= OD.Backend_Ready or else
                  Item.Value.Item.Current_Width /= outputStates (selectedOutput).fbWidth or else
                  Item.Value.Item.Current_Height /= outputStates (selectedOutput).fbHeight
               then
                  return;
               end if;
               Item.Value.Item :=
                 (OD.Selected_For_Desktop, OD.Virtio_GPU, 0,
                  Item.Value.Item.Advertised_Width,
                  Item.Value.Item.Advertised_Height, outputStates (selectedOutput).fbWidth, outputStates (selectedOutput).fbHeight);
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
         debugPrint ("" & LF);
      end if;

      statsStartMs := now;
      CuBit.Graphics_Metrics_IO.Publish
        (GM.Display_Backend, backendCopies, backendReporter);
      CuBit.Graphics_Metrics_IO.Publish
        (GM.Display_Repair, repairCopies, repairReporter);
      statsRequests := 0;
      statsQueued := 0;
      statsPresents := 0;
      statsWaitMs := 0;
      statsCopyMs := 0;
      statsPixels := 0;
   end maybePrintStats;

   function ownsDisplay (pid : ProcessID) return Boolean is
   begin
      return pid /= NO_PROCESS and then outputStates (selectedOutput).displayOwner = pid and then
        outputStates (selectedOutput).leasedOutput = outputStates (selectedOutput).currentOutput and then outputUsable (outputStates (selectedOutput).leasedOutput);
   end ownsDisplay;

   procedure detachOwnerBuffer is
      returned : Boolean;
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
      outputStates (selectedOutput).srcOwner := NO_PROCESS;
      outputStates (selectedOutput).pendingPresent := False;
      outputStates (selectedOutput).pendingRect := (others => 0);
      outputStates (selectedOutput).gpuCopyActive := False;
      outputStates (selectedOutput).gpuPreviousDamage := (others => 0);
      if outputStates (selectedOutput).srcAcquired then
         MG.Return_Acquisition (outputStates (selectedOutput).srcGrant, returned);
         if not returned then
            outputStates (selectedOutput).presentationFault := True;
            debugPrint ("display: buffer acquisition return failed" & LF);
         end if;
         outputStates (selectedOutput).srcAcquired := False;
      end if;
   end detachOwnerBuffer;

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

   procedure clear (color : Unsigned_32) is
      line : array (Natural range 0 .. 1023) of Unsigned_32;
      ignore : System.Address;
   begin
      if outputStates (selectedOutput).fbBpp /= 32 then
         return;
      end if;

      --  The current QEMU mode is 1024 pixels wide. Keep this conservative so
      --  clear never writes beyond the stack buffer if a later mode changes.
      if outputStates (selectedOutput).fbWidth <= line'Length then
         for x in 0 .. outputStates (selectedOutput).fbWidth - 1 loop
            line (x) := color;
         end loop;

         for y in 0 .. outputStates (selectedOutput).fbHeight - 1 loop
            ignore := memcpy
              (outputStates (selectedOutput).fbAddr + Storage_Offset (y * outputStates (selectedOutput).fbPitch),
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
                     outputStates (selectedOutput).fbAddr + Storage_Offset (y * outputStates (selectedOutput).fbPitch + x * 4);
            begin
               pixel := color;
            end;
         end loop;
      end loop;
   end clear;

   function clearGpu (color : Unsigned_64) return Boolean is
      reply : Message;
   begin
      if not outputStates (selectedOutput).gpuAvailable then
         return False;
      end if;

      reply := callGpu (OP_GPU_CLEAR, color, 0, 0, 0);
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         outputStates (selectedOutput).gpuActiveBuffer := 0;
         outputStates (selectedOutput).gpuPreviousDamage := (others => 0);
         return True;
      end if;

      debugPrint ("display: gpu clear failed" & LF);
      outputStates (selectedOutput).gpuAvailable := False;
      return False;
   end clearGpu;

   procedure presentRect (x, y, w, h : Natural) is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      ignore : System.Address;
   begin
      if outputStates (selectedOutput).fbBpp /= 32 or else outputStates (selectedOutput).srcAddr = System.Null_Address then
         return;
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
           (outputStates (selectedOutput).fbAddr + Storage_Offset (y * outputStates (selectedOutput).fbPitch),
            outputStates (selectedOutput).srcAddr + Storage_Offset (y * outputStates (selectedOutput).srcPitch),
            Storage_Count ((maxY - y) * outputStates (selectedOutput).fbPitch));
         GM.Add (backendCopies, Unsigned_64 (maxY - y) * Unsigned_64 (outputStates (selectedOutput).fbPitch));
         return;
      end if;

      for row in y .. maxY - 1 loop
         ignore := memcpy
           (outputStates (selectedOutput).fbAddr + Storage_Offset (row * outputStates (selectedOutput).fbPitch + x * 4),
            outputStates (selectedOutput).srcAddr + Storage_Offset (row * outputStates (selectedOutput).srcPitch + x * 4),
            Storage_Count ((maxX - x) * 4));
      end loop;
      GM.Add (backendCopies,
              Unsigned_64 (maxX - x) * Unsigned_64 (maxY - y) * 4);
   end presentRect;

   function ensureGpuScanout return Boolean is
      gpuMap : Message;
      wire : DSP.Wire_Message;
      mapped : System.Address;
      acquired : Boolean;
   begin
      if not outputStates (selectedOutput).gpuAvailable then return False; end if;
      for index in Gpu_Buffer_Index loop
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
               if index /= Gpu_Buffer_Index'First and then
                 (outputStates (selectedOutput).gpuScanoutWidth /= Natural (decoded.Value.Layout.Width) or else
                  outputStates (selectedOutput).gpuScanoutHeight /= Natural (decoded.Value.Layout.Height) or else
                  outputStates (selectedOutput).gpuScanoutPitch /= decoded.Value.Layout.Pitch)
               then
                  debugPrint ("display: gpu swapchain geometry mismatch" & LF);
                  return False;
               end if;
               MG.Acquire_Via_Capability
                 (CAP_SLOT_GPU, decoded.Value.Grant, 0,
                  DSP.DP.Byte_Length (decoded.Value.Layout), MG.Write_Access, mapped, acquired);
               if not acquired then
                  debugPrint ("display: GPU mapping acquisition rejected" & LF);
                  return False;
               end if;
               -- Retain both acquisitions for this display instance. GPU death
               -- or grant revocation cannot recycle memory beneath a CPU copy.
               outputStates (selectedOutput).gpuScanoutAddr (index) := mapped;
               if index = Gpu_Buffer_Index'First then
                  outputStates (selectedOutput).gpuScanoutWidth := Natural (decoded.Value.Layout.Width);
                  outputStates (selectedOutput).gpuScanoutHeight := Natural (decoded.Value.Layout.Height);
                  outputStates (selectedOutput).gpuScanoutPitch := decoded.Value.Layout.Pitch;
               end if;
            end;
         end if;
      end loop;
      return True;
   end ensureGpuScanout;

   function clampGpuRect (r : Rect) return Rect is
      limitW : constant Natural := Natural'Min (outputStates (selectedOutput).srcWidth, outputStates (selectedOutput).gpuScanoutWidth);
      limitH : constant Natural := Natural'Min (outputStates (selectedOutput).srcHeight, outputStates (selectedOutput).gpuScanoutHeight);
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

      if r.x = 0 and then r.w = outputStates (selectedOutput).gpuScanoutWidth and then
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
      previous : constant Rect := clampGpuRect (outputStates (selectedOutput).gpuPreviousDamage);
      target : constant Gpu_Buffer_Index := 1 - outputStates (selectedOutput).gpuActiveBuffer;
      transfer : Rect := r;
      packedXY : Unsigned_64;
      packedWH : Unsigned_64;
   begin
      if not outputStates (selectedOutput).gpuCopyActive or else outputStates (selectedOutput).srcAddr = System.Null_Address or else
        isEmpty (r)
      then
         return NULL_MESSAGE;
      end if;

      --  The inactive resource is one frame old. Bring forward precisely the
      --  damage written while it was inactive, then apply this frame's new
      --  pixels. This is buffer-age tracking: unchanged pixels never need a
      --  full-screen copy merely because the scanout resource alternates.
      if not isEmpty (previous) then
         copyGpuRect
           (outputStates (selectedOutput).gpuScanoutAddr (target), outputStates (selectedOutput).gpuScanoutPitch,
            outputStates (selectedOutput).gpuScanoutAddr (outputStates (selectedOutput).gpuActiveBuffer), outputStates (selectedOutput).gpuScanoutPitch,
            previous, repairCopies);
         transfer := unionRect (previous, r);
      end if;

      copyGpuRect
        (outputStates (selectedOutput).gpuScanoutAddr (target), outputStates (selectedOutput).gpuScanoutPitch,
         outputStates (selectedOutput).srcAddr, outputStates (selectedOutput).srcPitch, r, backendCopies);

      packedXY := Unsigned_64 (transfer.x) or
        Shift_Left (Unsigned_64 (transfer.y), 32);
      packedWH := Unsigned_64 (transfer.w) or
        Shift_Left (Unsigned_64 (transfer.h), 32);

      return (tag => (OP_GPU_PRESENT_BUFFER, 4, 0,
                      Unsigned_16 (selectedOutput)),
              authorityTag => 0,
              words => [Unsigned_64 (target), packedXY, packedWH, 0]);
   end prepareGpuRect;

   function copyAndFlipGpuRect (damage : Rect) return Boolean is
      request : Message := prepareGpuRect (damage);
      tag : MessageTag;
   begin
      if request.tag.length = 0 then return False; end if;
      tag := capCall (CAP_SLOT_GPU, request);
      request.tag := tag;
      if request.tag.length = 1 and then request.words (0) = 0 then
         outputStates (selectedOutput).gpuActiveBuffer :=
           1 - outputStates (selectedOutput).gpuActiveBuffer;
         outputStates (selectedOutput).gpuPreviousDamage := clampGpuRect (damage);
         return True;
      end if;

      debugPrint ("display: gpu page flip failed" & LF);
      outputStates (selectedOutput).gpuCopyActive := False;
      return False;
   end copyAndFlipGpuRect;

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

      --  All scanout timing lives here. Clients submit damage and keep
      --  rendering; display.svc coalesces pending rectangles and copies one
      --  display-owned frame during vblank. The backend case is deliberately
      --  centralized so a future VirtIO/real-GPU backend can turn this
      --  operation into a page flip or command submission without changing
      --  desktop.svc.
      waitStart := syscall (SYSCALL_GETTIME);
      if outputStates (selectedOutput).gpuCopyActive then
         copyStart := syscall (SYSCALL_GETTIME);
         if not copyAndFlipGpuRect (r) then
            presentRect (r.x, r.y, r.w, r.h);
         end if;
      else
         copyStart := syscall (SYSCALL_GETTIME);
         presentRect (r.x, r.y, r.w, r.h);
      end if;
      copyEnd := syscall (SYSCALL_GETTIME);

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
      waitStart := syscall (SYSCALL_GETTIME);
      copyStart := syscall (SYSCALL_GETTIME);

      if outputStates (selectedOutput).gpuCopyActive then
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
            if not copyAndFlipGpuRect (r) then
               presentRect (r.x, r.y, r.w, r.h);
            end if;
         end if;
         copyEnd := syscall (SYSCALL_GETTIME);
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
            presentRect (r.x, r.y, r.w, r.h);
            pixels := pixels + Unsigned_64 (r.w) * Unsigned_64 (r.h);
         end if;
      end loop;
      copyEnd := syscall (SYSCALL_GETTIME);

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

   procedure finishFrame
     (Output : Output_Index; ID : PS.Submission_ID;
      Result : in out DSP.Frame_Result; Published, Certain : Boolean;
      Response : out Message)
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
      if not Published then State.presentationFault := True; end if;
      if State.presentationFault then
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
      Response := CuBit.Desktop_Messages.From_Wire
        (DSP.Encode_Frame_Result (Result));
   end finishFrame;

   procedure collectFrames is
      Completion : aliased CompletionEntry;
      Response : Message;
      Ignored : Unsigned_64;
      Published : Boolean;
      Matched : Boolean;
   begin
      --  At most two backend operations are outstanding; do not let a faulty
      --  producer monopolize this loop with unbounded completion traffic.
      for Count in 1 .. COMPLETION_QUEUE_SIZE loop
         exit when Poll_Completion (Completion'Address) /= 1;
         Matched := False;
         for Output in Output_Index loop
            declare
               State : Output_State renames outputStates (Output);
            begin
               if State.backendToken /= 0 and then
                 State.backendToken = Completion.token
               then
                  Matched := True;
                  Published := Completion.valid and then Completion.status = COMPLETION_OK and then
                    Completion.msg.tag = (OP_GPU_PRESENT_BUFFER, 1, 0, 0) and then
                    Completion.msg.words (0) = 0 and then
                    State.backendFrame.Session = State.activeSession and then
                    State.sessionOutput = State.currentOutput;
                  --  Validate the captured output directly, not selectedOutput.
                  Published := Published and then
                    State.sourceOutput = State.currentOutput and then
                    outputUsable (State.currentOutput);
                  if Published then
                     State.gpuActiveBuffer := State.backendTarget;
                     State.gpuPreviousDamage := State.backendDamage;
                  end if;
                  finishFrame (Output, State.backendID, State.backendFrame,
                               Published, Published, Response);
                  State.backendToken := 0;
                  Ignored := replyCap (frameReplySlot (Output), Response);
               end if;
            end;
         end loop;
         if not Matched then
            --  Never guess which buffer an unrecognized completion releases.
            for State of outputStates loop
               State.presentationFault := True;
               PS.Close (State.frameState);
            end loop;
            debugPrint ("display: unrecognized GPU completion" & LF);
         end if;
      end loop;
   end collectFrames;

   procedure submitFrame (from : ProcessID; request : Message;
                          replyMsg : out Message) is
      decoded : constant DSP.Frame_Decoding := DSP.Decode_Frame
        (CuBit.Desktop_Messages.To_Wire (request));
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
   begin
      replyMsg := NULL_MESSAGE;
      replyMsg.tag := (DSP.Code (DSP.Submit_Frame), 1, 0, 0);
      replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
      if not decoded.Valid then return; end if;
      result := (decoded.Value.Session, decoded.Value.Frame,
                 DSP.Rejected, DSP.Not_Acquired);
      r := (Natural (decoded.Value.Area.X), Natural (decoded.Value.Area.Y),
            Natural (decoded.Value.Area.Width), Natural (decoded.Value.Area.Height));
      if State.presentationFault or else not ownsDisplay (from) or else
        not State.srcAcquired or else State.srcOwner /= from or else
        State.activeSession /= decoded.Value.Session or else
        State.sessionOutput /= State.currentOutput or else
        decoded.Value.Frame <= State.lastFrame or else
        PS.Current (State.frameState) /= PS.Idle or else
        isEmpty (r) or else r.x + r.w > State.srcWidth or else
        r.y + r.h > State.srcHeight or else
        backendSequence >= NO_COMPLETION_TOKEN - 1
      then
         replyMsg := CuBit.Desktop_Messages.From_Wire (DSP.Encode_Frame_Result (result));
         return;
      end if;
      --  Attachment is not authority for new work after grant revocation.
      MG.Acquire (State.srcGrant, from, 0,
                  Unsigned_64 (State.srcPitch) * Unsigned_64 (State.srcHeight),
                  MG.Read_Access, mapped, acquired);
      if not acquired then
         replyMsg := CuBit.Desktop_Messages.From_Wire (DSP.Encode_Frame_Result (result));
         return;
      end if;
      PS.Submit (State.frameState, admission, id);
      if admission /= PS.Accepted then
         MG.Return_Acquisition (State.srcGrant, returned);
         result.Buffer_State := (if returned then DSP.Released else DSP.Still_Held);
         State.presentationFault := True;
         replyMsg := CuBit.Desktop_Messages.From_Wire (DSP.Encode_Frame_Result (result));
         return;
      end if;
      State.lastFrame := decoded.Value.Frame;
      result.Buffer_State := DSP.Still_Held;
      result.Outcome := DSP.Failed;
      PS.Apply (State.frameState, id, PS.Begin_Read, applied, obligation);
      if not applied then
         finishFrame (selectedOutput, id, result, False, False, replyMsg);
         return;
      end if;
      State.srcAddr := mapped;
      started := syscall (SYSCALL_GETTIME);
      if Presentation_Test_Policy.Enabled and then
        not Presentation_Test_Policy.Verify_Buffer
          (mapped, Unsigned_64 (State.srcPitch) * Unsigned_64 (State.srcHeight),
           decoded.Value.Frame)
      then
         State.presentationFault := True;
      elsif State.gpuCopyActive then
         backendRequest := prepareGpuRect (r);
         if backendRequest.tag.length /= 0 and then
           saveReplyCap (Unsigned_64 (frameReplySlot (selectedOutput))) = 1
         then
            deferredReply := True;
            backendSequence := backendSequence + 1;
            State.backendFrame := result;
            State.backendID := id;
            State.backendTarget := 1 - State.gpuActiveBuffer;
            State.backendDamage := clampGpuRect (r);
            if capSubmit (CAP_SLOT_GPU, backendRequest, backendSequence) then
               State.backendToken := backendSequence;
               statsPresents := statsPresents + 1;
               statsPixels := statsPixels + Unsigned_64 (r.w) * Unsigned_64 (r.h);
               statsCopyMs := statsCopyMs + syscall (SYSCALL_GETTIME) - started;
               return;
            end if;
            --  No command accepted: safe to return the source acquisition.
            finishFrame (selectedOutput, id, result, False, True, replyMsg);
            declare
               Ignored : constant Unsigned_64 :=
                 replyCap (frameReplySlot (selectedOutput), replyMsg);
            begin
               null;
            end;
            return;
         end if;
      else
         presentRect (r.x, r.y, r.w, r.h);
         published := True;
      end if;
      statsPresents := statsPresents + 1;
      statsPixels := statsPixels + Unsigned_64 (r.w) * Unsigned_64 (r.h);
      statsCopyMs := statsCopyMs + syscall (SYSCALL_GETTIME) - started;
      finishFrame (selectedOutput, id, result, published, True, replyMsg);
   end submitFrame;

   procedure handleRequest
      (from     : ProcessID;
       incoming : Message;
       replyMsg : out Message)
   is
      request : Message := incoming;
   begin
      replyMsg := NULL_MESSAGE;
      statsRequests := statsRequests + 1;

      if request.tag.label = OD.Code (OD.Display_Broker, OD.Get_Catalog) or else
         request.tag.label = OD.Code (OD.Display_Broker, OD.Get_Description)
      then
         if catalogReady and then outputUsable (outputStates (0).currentOutput) then
            replyMsg := CuBit.Desktop_Messages.From_Wire (OD.Respond
              (OD.Display_Broker, outputCatalog,
               OD.Catalog_Revision (Outputs.Version (outputRegistry)),
               CuBit.Desktop_Messages.To_Wire (request)));
         else
            replyMsg.tag := (request.tag.label, 1, 0, 0);
            replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
         end if;
         return;
      end if;

      if not DSP.Valid_Output (CuBit.Desktop_Messages.To_Wire (request)) or else
        request.tag.reserved > Unsigned_16 (Output_Index'Last) or else
        not readyOutputs (Output_Index (request.tag.reserved))
      then
         replyMsg.tag := (request.tag.label, 1, 0, 0);
         replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
         return;
      end if;
      selectedOutput := Output_Index (request.tag.reserved);
      request.tag.reserved := 0;

      --  Admission is serialized, GPU completion is not. Preserve the source,
      --  lease and session until their captured continuation has retired.
      if PS.Current (outputStates (selectedOutput).frameState) /= PS.Idle and then
        request.tag.label not in OP_DISPLAY_GET_INFO | OP_DISPLAY_GET_STATUS |
          OP_SUBMIT_FRAME
      then
         replyMsg.tag := (request.tag.label, 1, 0, 0);
         replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
         return;
      end if;

      case request.tag.label is
         when OP_OPEN_SESSION =>
            replyMsg.tag := (request.tag.label, 4, 0, 0);
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

         when OP_SUBMIT_FRAME =>
            submitFrame (from, request, replyMsg);

         when OP_DISPLAY_GET_INFO =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_INFO,
                             length => 4,
                             flags  => 0,
                             reserved  => 0);
            replyMsg.words (0) := Unsigned_64 (outputStates (selectedOutput).fbWidth);
            replyMsg.words (1) := Unsigned_64 (outputStates (selectedOutput).fbHeight);
            replyMsg.words (2) := Unsigned_64 (outputStates (selectedOutput).fbPitch);
            replyMsg.words (3) := 32; -- BGRA8888

         when OP_DISPLAY_GET_STATUS =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_STATUS,
                             length => 4,
                             flags  => 0,
                             reserved  => 0);
            replyMsg.words (0) := backendId;
            replyMsg.words (1) := backendCaps;
            replyMsg.words (2) := Unsigned_64 (outputStates (selectedOutput).displayOwner);
            replyMsg.words (3) := 0; -- reserved for backend-specific status

         when OP_DISPLAY_ACQUIRE =>
            replyMsg.tag := (label => OP_DISPLAY_ACQUIRE,
                             length => 1, flags => 0, reserved => 0);
            if not DSP.Valid_Lease_Request
              (CuBit.Desktop_Messages.To_Wire (request), DSP.Acquire_Display)
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif not outputUsable (outputStates (selectedOutput).currentOutput) then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif outputStates (selectedOutput).displayOwner = NO_PROCESS or else outputStates (selectedOutput).displayOwner = from then
               outputStates (selectedOutput).displayOwner := from;
               outputStates (selectedOutput).leasedOutput := outputStates (selectedOutput).currentOutput;
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_RELEASE =>
            replyMsg.tag := (label => OP_DISPLAY_RELEASE,
                             length => 1, flags => 0, reserved => 0);
            if not DSP.Valid_Lease_Request
              (CuBit.Desktop_Messages.To_Wire (request), DSP.Release_Display)
            then
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif outputStates (selectedOutput).displayOwner = from and then not outputStates (selectedOutput).presentationFault then
               detachOwnerBuffer;
               outputStates (selectedOutput).displayOwner := NO_PROCESS;
               outputStates (selectedOutput).leasedOutput := Outputs.No_Output;
               replyMsg.words (0) := DISPLAY_OK;
            elsif outputStates (selectedOutput).displayOwner = NO_PROCESS then
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_ATTACH_BUFFER =>
            replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                             length => 1, flags => 0, reserved => 0);
            if outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
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
                  elsif outputStates (selectedOutput).gpuAvailable and then not ensureGpuScanout then
                     outputStates (selectedOutput).presentationFault := True;
                     replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                  elsif outputStates (selectedOutput).gpuAvailable and then
                    (Natural (decoded.Value.Layout.Width) > outputStates (selectedOutput).gpuScanoutWidth or else
                     Natural (decoded.Value.Layout.Height) > outputStates (selectedOutput).gpuScanoutHeight)
                  then
                     replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
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
                        if outputStates (selectedOutput).gpuAvailable then
                           outputStates (selectedOutput).gpuCopyActive := True;
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
                             length => 4, flags => 0, reserved => 0);
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
                             length => 1, flags => 0, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else outputStates (selectedOutput).presentationFault then
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
               replyMsg.words (0) := DISPLAY_OK;
            end if;

         when OP_DISPLAY_PRESENT_REGION |
              OP_DISPLAY_PRESENT_IMMEDIATE_REGION =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, flags => 0, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else outputStates (selectedOutput).presentationFault then
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
               replyMsg.words (0) := DISPLAY_OK;
            end if;

         when OP_DISPLAY_CLEAR =>
            replyMsg.tag := (label => OP_DISPLAY_CLEAR,
                             length => 1, flags => 0, reserved => 0);
            if outputStates (selectedOutput).activeSession /= 0 or else outputStates (selectedOutput).presentationFault then
               replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
            elsif not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif not clearGpu (request.words (0)) then
               if outputStates (selectedOutput).fbAddr /= System.Null_Address then
                  clear (Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
                  replyMsg.words (0) := DISPLAY_OK;
               else
                  -- A secondary GPU output has no firmware framebuffer to
                  -- fall back to. Do not turn backend failure into a null write.
                  outputStates (selectedOutput).presentationFault := True;
                  replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
               end if;
            else
               replyMsg.words (0) := DISPLAY_OK;
            end if;

         when others =>
            replyMsg.tag := (label  => request.tag.label,
                             length => 1,
                             flags  => 0,
                             reserved  => 0);
            replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
      end case;

   end handleRequest;

   ret     : Unsigned_64;
   from    : ProcessID;
   msg     : Message;
   replyMsg : Message := NULL_MESSAGE;
   found : Boolean;
   eventMsg : Message;
begin
   debugPrint ("display: starting" & LF);

   ret := setLatencyContract
      (LATENCY_REALTIME,
       16_667,  --  60 Hz scanout period in microseconds.
       2_000);  --  Budget hint for coalesced present/flush work.
   if ret = Unsigned_64'Last then
      debugPrint ("display: latency contract rejected" & LF);
   end if;

   ret := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DISPLAY);
   if ret /= 0 and then ret /= Unsigned_64'Last then
      --  display.svc owns the visible scanout. A second copy would clear the
      --  screen and steal the well-known display role, so treat manual
      --  duplicate launches as harmless no-ops.
      debugPrint ("display: already running, exiting" & LF);
      ret := syscall (SYSCALL_EXIT, 0);
      return;
   end if;

   ret := registerDriver (DRIVER_DISPLAY);
   if ret = Unsigned_64'Last then
      debugPrint ("display: register failed" & LF);
   end if;

   ret := syscall (SYSCALL_MAPFB);
   if ret = Unsigned_64'Last then
      debugPrint ("display: MAPFB failed" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;

   outputStates (selectedOutput).fbAddr   := To_Address (Integer_Address (ret));
   outputStates (selectedOutput).fbWidth  := Natural (getInfo (SYSINFO_FB_WIDTH));
   outputStates (selectedOutput).fbHeight := Natural (getInfo (SYSINFO_FB_HEIGHT));
   outputStates (selectedOutput).fbPitch  := Natural (getInfo (SYSINFO_FB_PITCH));
   outputStates (selectedOutput).fbBpp    := Natural (getInfo (SYSINFO_FB_BPP));

   setupBackend;
   if not registerOutput then
      debugPrint ("display: boot output registration failed" & LF);
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
   if clearGpu (16#0013_1518#) then
      debugPrint ("display: gpu scanout cleared" & LF);
   else
      clear (16#0013_1518#);
   end if;
   --  Initial native multi-output milestone: two equal-mode virtio outputs.
   --  Firmware-only and independently detected PCI outputs keep their existing
   --  behavior. Logical arrangement and the primary role belong to Desktop.
   if outputStates (0).gpuAvailable and then catalogReady then
      for Item of outputCatalog.Items loop
         if Item.Source = OD.Virtio_GPU and then Item.Native_Number = 1 and then
           Item.Role = OD.Backend_Ready
         then
            selectedOutput := 1;
            outputStates (1).fbWidth := Item.Current_Width;
            outputStates (1).fbHeight := Item.Current_Height;
            outputStates (1).fbPitch := Item.Current_Width * 4;
            outputStates (1).fbBpp := 32;
            outputStates (1).gpuAvailable := True;
            if clearGpu (16#0013_1518#) and then registerOutput then
               readyOutputs (1) := True;
               debugPrint ("display: second output ready" & LF);
            end if;
         end if;
      end loop;
      selectedOutput := 0;
   end if;
   debugPrint ("display: ready" & LF);

   loop
      collectFrames;
      Poll_Service_Request (from, msg, found);
      if found then
         deferredReply := False;
         handleRequest (from, msg, replyMsg);
         if outputStates (selectedOutput).pendingPresent then
            flushPendingPresent;
         end if;
         if not deferredReply then ret := reply (from, replyMsg); end if;
      end if;
      maybePrintStats;
      if not found and then not Poll_Event (eventMsg) then
         if Wait_For_Activity_Until (Unsigned_64'Last) = Unavailable then
            debugPrint ("display: activity wait unavailable" & LF);
            return;
         end if;
      end if;
   end loop;
end main;
