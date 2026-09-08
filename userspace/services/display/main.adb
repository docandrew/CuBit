------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display scanout service prototype
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;

   OP_DISPLAY_GET_INFO      : constant Unsigned_32 := 16#0900#;
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := 16#0901#;
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := 16#0902#;
   OP_DISPLAY_CLEAR         : constant Unsigned_32 := 16#0903#;
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := 16#0904#;
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := 16#0905#;
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := 16#0906#;
   OP_DISPLAY_MAP_BACKBUFFER : constant Unsigned_32 := 16#0907#;
   OP_DISPLAY_PRESENT_IMMEDIATE_RECT : constant Unsigned_32 := 16#0908#;
   OP_DISPLAY_PRESENT_REGION : constant Unsigned_32 := 16#0909#;
   OP_DISPLAY_PRESENT_IMMEDIATE_REGION : constant Unsigned_32 := 16#090A#;

   OP_GPU_CLEAR         : constant Unsigned_32 := 16#0A03#;
   OP_GPU_GET_STATUS    : constant Unsigned_32 := 16#0A04#;
   OP_GPU_MAP_FRAMEBUFFER : constant Unsigned_32 := 16#0A05#;
   OP_GPU_PRESENT_BUFFER : constant Unsigned_32 := 16#0A07#;

   DISPLAY_OK              : constant Unsigned_64 := 0;
   DISPLAY_ERR_DENIED      : constant Unsigned_64 := 1;
   DISPLAY_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   DISPLAY_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   DISPLAY_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB
   DISPLAY_BACKEND_LINEAR_FB : constant Unsigned_64 := 1;
   DISPLAY_BACKEND_VIRTIO_GPU : constant Unsigned_64 := 3;

   DISPLAY_CAP_COPY_PRESENT : constant Unsigned_64 := 16#0001#;
   DISPLAY_CAP_GPU_PRESENT  : constant Unsigned_64 := 16#0004#;
   DISPLAY_CAP_PAGE_FLIP    : constant Unsigned_64 := 16#0008#;

   CAP_SLOT_GPU : constant CapabilitySlot := 9;

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
   gpuAvailable : Boolean := False;
   gpuCopyActive  : Boolean := False;
   subtype Gpu_Buffer_Index is Natural range 0 .. 1;
   type Gpu_Address_Array is
     array (Gpu_Buffer_Index) of System.Address;
   type Gpu_Grant_Array is
     array (Gpu_Buffer_Index) of Unsigned_64;
   gpuScanoutAddr : Gpu_Address_Array := (others => System.Null_Address);
   gpuScanoutGrantId : Gpu_Grant_Array := (others => 0);
   gpuScanoutWidth   : Natural := 0;
   gpuScanoutHeight  : Natural := 0;
   gpuScanoutPitch   : Natural := 0;
   gpuActiveBuffer : Gpu_Buffer_Index := 0;
   displayOwner : ProcessID := NO_PROCESS;

   type Rect is record
      x : Natural := 0;
      y : Natural := 0;
      w : Natural := 0;
      h : Natural := 0;
   end record;

   gpuPreviousDamage : Rect := (others => 0);

   pendingPresent : Boolean := False;
   pendingRect    : Rect;

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
      if gpuAvailable then
         return DISPLAY_BACKEND_VIRTIO_GPU;
      else
         return DISPLAY_BACKEND_LINEAR_FB;
      end if;
   end backendId;

   function backendCaps return Unsigned_64 is
   begin
      if gpuAvailable then
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
        (tag      => (label => label, length => 4, flags => 0, reserved => 0),
         authorityTag => 0,
         words    => (w0, w1, w2, w3));
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
      if status.tag.length >= 4 and then status.words (0) = 0 and then
         gpuPrimary and then
         status.words (2) = Unsigned_64 (fbWidth) and then
         status.words (3) = Unsigned_64 (fbHeight)
      then
         gpuAvailable := True;
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
         debugPrint (" copy_ms=");
         printDec (statsCopyMs);
         debugPrint (" px=");
         printDec (statsPixels);
         debugPrint ("" & LF);
      end if;

      statsStartMs := now;
      statsRequests := 0;
      statsQueued := 0;
      statsPresents := 0;
      statsWaitMs := 0;
      statsCopyMs := 0;
      statsPixels := 0;
   end maybePrintStats;

   function toAddr (x : Unsigned_64) return System.Address is
   begin
      return To_Address (Integer_Address (x));
   end toAddr;

   function ownsDisplay (pid : ProcessID) return Boolean is
   begin
      return displayOwner = pid;
   end ownsDisplay;

   procedure detachOwnerBuffer is
   begin
      srcAddr := System.Null_Address;
      srcWidth := 0;
      srcHeight := 0;
      srcPitch := 0;
      srcOwner := NO_PROCESS;
      pendingPresent := False;
      pendingRect := (others => 0);
      gpuCopyActive := False;
      gpuPreviousDamage := (others => 0);
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

      if pendingPresent then
         pendingRect := unionRect (pendingRect, r);
      else
         pendingRect := r;
         pendingPresent := True;
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
      if fbBpp /= 32 then
         return;
      end if;

      --  The current QEMU mode is 1024 pixels wide. Keep this conservative so
      --  clear never writes beyond the stack buffer if a later mode changes.
      if fbWidth <= line'Length then
         for x in 0 .. fbWidth - 1 loop
            line (x) := color;
         end loop;

         for y in 0 .. fbHeight - 1 loop
            ignore := memcpy
              (fbAddr + Storage_Offset (y * fbPitch),
               line'Address,
               Storage_Count (fbWidth * 4));
         end loop;
         return;
      end if;

      for y in 0 .. fbHeight - 1 loop
         for x in 0 .. fbWidth - 1 loop
            declare
               pixel : Unsigned_32 with
                  Import, Address =>
                     fbAddr + Storage_Offset (y * fbPitch + x * 4);
            begin
               pixel := color;
            end;
         end loop;
      end loop;
   end clear;

   function clearGpu (color : Unsigned_64) return Boolean is
      reply : Message;
   begin
      if not gpuAvailable then
         return False;
      end if;

      reply := callGpu (OP_GPU_CLEAR, color, 0, 0, 0);
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         gpuActiveBuffer := 0;
         gpuPreviousDamage := (others => 0);
         return True;
      end if;

      debugPrint ("display: gpu clear failed" & LF);
      gpuAvailable := False;
      return False;
   end clearGpu;

   procedure presentRect (x, y, w, h : Natural) is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      ignore : System.Address;
   begin
      if fbBpp /= 32 or else srcAddr = System.Null_Address then
         return;
      end if;
      if w = 0 or else h = 0 or else x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      if maxX > fbWidth then
         maxX := fbWidth;
      end if;
      if maxY > fbHeight then
         maxY := fbHeight;
      end if;
      if maxX > srcWidth then
         maxX := srcWidth;
      end if;
      if maxY > srcHeight then
         maxY := srcHeight;
      end if;
      if x >= maxX or else y >= maxY then
         return;
      end if;

      --  Full-width damage is contiguous when source and scanout pitches
      --  match. Copy it as one span instead of issuing one memcpy per row;
      --  full-frame presents and large vertical bands are common during
      --  startup, shell switches, and simple compositor redraws.
      if x = 0 and then maxX = fbWidth and then srcPitch = fbPitch then
         ignore := memcpy
           (fbAddr + Storage_Offset (y * fbPitch),
            srcAddr + Storage_Offset (y * srcPitch),
            Storage_Count ((maxY - y) * fbPitch));
         return;
      end if;

      for row in y .. maxY - 1 loop
         ignore := memcpy
           (fbAddr + Storage_Offset (row * fbPitch + x * 4),
            srcAddr + Storage_Offset (row * srcPitch + x * 4),
            Storage_Count ((maxX - x) * 4));
      end loop;
   end presentRect;

   function ensureGpuScanout return Boolean is
      gpuMap : Message;
   begin
      if not gpuAvailable then
         return False;
      end if;

      for index in Gpu_Buffer_Index loop
         if gpuScanoutAddr (index) = System.Null_Address then
            gpuMap := callGpu
              (OP_GPU_MAP_FRAMEBUFFER, Unsigned_64 (index), 0, 0, 0);
            if gpuMap.tag.length < 4 or else gpuMap.words (0) /= 0 then
               debugPrint ("display: gpu scanout map failed" & LF);
               return False;
            end if;

            gpuScanoutGrantId (index) := gpuMap.words (1);
            gpuScanoutAddr (index) := toAddr
              (GRANT_REGION_BASE +
               gpuScanoutGrantId (index) * GRANT_SLOT_SIZE);
            if index = Gpu_Buffer_Index'First then
               gpuScanoutWidth := unpackLo32 (gpuMap.words (2));
               gpuScanoutHeight := unpackHi32 (gpuMap.words (2));
               gpuScanoutPitch := Natural (gpuMap.words (3));
            elsif gpuScanoutWidth /= unpackLo32 (gpuMap.words (2)) or else
              gpuScanoutHeight /= unpackHi32 (gpuMap.words (2)) or else
              gpuScanoutPitch /= Natural (gpuMap.words (3))
            then
               debugPrint ("display: gpu swapchain geometry mismatch" & LF);
               return False;
            end if;
         end if;
      end loop;

      return
        (for all index in Gpu_Buffer_Index =>
           gpuScanoutAddr (index) /= System.Null_Address) and then
        gpuScanoutWidth > 0 and then gpuScanoutHeight > 0 and then
        gpuScanoutPitch >= gpuScanoutWidth * 4;
   end ensureGpuScanout;

   function clampGpuRect (r : Rect) return Rect is
      limitW : constant Natural := Natural'Min (srcWidth, gpuScanoutWidth);
      limitH : constant Natural := Natural'Min (srcHeight, gpuScanoutHeight);
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
      r         : Rect)
   is
      ignore : System.Address;
   begin
      if isEmpty (r) then
         return;
      end if;

      if r.x = 0 and then r.w = gpuScanoutWidth and then
         sourcePitch = destPitch
      then
         ignore := memcpy
           (dest + Storage_Offset (r.y * destPitch),
            source + Storage_Offset (r.y * sourcePitch),
            Storage_Count (r.h * destPitch));
      else
         for row in r.y .. r.y + r.h - 1 loop
            ignore := memcpy
              (dest + Storage_Offset (row * destPitch + r.x * 4),
               source + Storage_Offset (row * sourcePitch + r.x * 4),
               Storage_Count (r.w * 4));
         end loop;
      end if;
   end copyGpuRect;

   function copyAndFlipGpuRect (damage : Rect) return Boolean is
      r : constant Rect := clampGpuRect (damage);
      previous : constant Rect := clampGpuRect (gpuPreviousDamage);
      target : constant Gpu_Buffer_Index := 1 - gpuActiveBuffer;
      transfer : Rect := r;
      reply : Message;
      packedXY : Unsigned_64;
      packedWH : Unsigned_64;
   begin
      if not gpuCopyActive or else srcAddr = System.Null_Address or else
        isEmpty (r)
      then
         return False;
      end if;

      --  The inactive resource is one frame old. Bring forward precisely the
      --  damage written while it was inactive, then apply this frame's new
      --  pixels. This is buffer-age tracking: unchanged pixels never need a
      --  full-screen copy merely because the scanout resource alternates.
      if not isEmpty (previous) then
         copyGpuRect
           (gpuScanoutAddr (target), gpuScanoutPitch,
            gpuScanoutAddr (gpuActiveBuffer), gpuScanoutPitch,
            previous);
         transfer := unionRect (previous, r);
      end if;

      copyGpuRect
        (gpuScanoutAddr (target), gpuScanoutPitch,
         srcAddr, srcPitch, r);

      packedXY := Unsigned_64 (transfer.x) or
        Shift_Left (Unsigned_64 (transfer.y), 32);
      packedWH := Unsigned_64 (transfer.w) or
        Shift_Left (Unsigned_64 (transfer.h), 32);

      reply := callGpu
        (OP_GPU_PRESENT_BUFFER,
         Unsigned_64 (target), packedXY, packedWH, 0);
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         gpuActiveBuffer := target;
         gpuPreviousDamage := r;
         return True;
      end if;

      debugPrint ("display: gpu page flip failed" & LF);
      gpuCopyActive := False;
      return False;
   end copyAndFlipGpuRect;

   procedure flushPendingPresent is
      r : constant Rect := pendingRect;
      waitStart : Unsigned_64;
      copyStart : Unsigned_64;
      copyEnd   : Unsigned_64;
   begin
      if not pendingPresent then
         return;
      end if;

      pendingPresent := False;
      pendingRect := (others => 0);

      --  All scanout timing lives here. Clients submit damage and keep
      --  rendering; display.svc coalesces pending rectangles and copies one
      --  display-owned frame during vblank. The backend case is deliberately
      --  centralized so a future VirtIO/real-GPU backend can turn this
      --  operation into a page flip or command submission without changing
      --  desktop.svc.
      waitStart := syscall (SYSCALL_GETTIME);
      if gpuCopyActive then
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

      if gpuCopyActive then
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

   procedure handleRequest
      (from     : ProcessID;
       request  : Message;
       replyMsg : out Message)
   is
   begin
      replyMsg := NULL_MESSAGE;
      statsRequests := statsRequests + 1;

      case request.tag.label is
         when OP_DISPLAY_GET_INFO =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_INFO,
                             length => 4,
                             flags  => 0,
                             reserved  => 0);
            replyMsg.words (0) := Unsigned_64 (fbWidth);
            replyMsg.words (1) := Unsigned_64 (fbHeight);
            replyMsg.words (2) := Unsigned_64 (fbPitch);
            replyMsg.words (3) := 32; -- BGRA8888

         when OP_DISPLAY_GET_STATUS =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_STATUS,
                             length => 4,
                             flags  => 0,
                             reserved  => 0);
            replyMsg.words (0) := backendId;
            replyMsg.words (1) := backendCaps;
            replyMsg.words (2) := Unsigned_64 (displayOwner);
            replyMsg.words (3) := 0; -- reserved for backend-specific status

         when OP_DISPLAY_ACQUIRE =>
            replyMsg.tag := (label => OP_DISPLAY_ACQUIRE,
                             length => 1, flags => 0, reserved => 0);
            if displayOwner = NO_PROCESS or else displayOwner = from then
               displayOwner := from;
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_RELEASE =>
            replyMsg.tag := (label => OP_DISPLAY_RELEASE,
                             length => 1, flags => 0, reserved => 0);
            if displayOwner = from then
               detachOwnerBuffer;
               displayOwner := NO_PROCESS;
               replyMsg.words (0) := DISPLAY_OK;
            elsif displayOwner = NO_PROCESS then
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_ATTACH_BUFFER =>
            if not ownsDisplay (from) then
               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, reserved => 0);
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif request.words (1) = 0 or else request.words (2) = 0 then
               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, reserved => 0);
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif request.words (1) > Unsigned_64 (fbWidth) or else
                  request.words (2) > Unsigned_64 (fbHeight) or else
                  request.words (3) < request.words (1) * 4
            then
               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, reserved => 0);
               replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
            else
               srcAddr :=
                  toAddr (GRANT_REGION_BASE +
                          request.words (0) * GRANT_SLOT_SIZE);
               srcWidth  := Natural (request.words (1));
               srcHeight := Natural (request.words (2));
               srcPitch  := Natural (request.words (3));
               srcOwner  := from;
               if gpuAvailable then
                  gpuCopyActive := ensureGpuScanout;
                  if gpuCopyActive then
                     debugPrint ("display: gpu copy buffer attached" & LF);
                  end if;
               end if;

               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, reserved => 0);
               replyMsg.words (0) := DISPLAY_OK;
               debugPrint ("display: buffer attached" & LF);
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
            if not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif srcOwner /= from or else srcAddr = System.Null_Address then
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
            if not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif srcOwner /= from or else srcAddr = System.Null_Address then
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
            if not ownsDisplay (from) then
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif not clearGpu (request.words (0)) then
               clear (Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
               replyMsg.words (0) := DISPLAY_OK;
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

   fbAddr   := To_Address (Integer_Address (ret));
   fbWidth  := Natural (getInfo (SYSINFO_FB_WIDTH));
   fbHeight := Natural (getInfo (SYSINFO_FB_HEIGHT));
   fbPitch  := Natural (getInfo (SYSINFO_FB_PITCH));
   fbBpp    := Natural (getInfo (SYSINFO_FB_BPP));

   setupBackend;
   if clearGpu (16#0013_1518#) then
      debugPrint ("display: gpu scanout cleared" & LF);
   else
      clear (16#0013_1518#);
   end if;
   debugPrint ("display: ready" & LF);

   receive (from, msg);

   loop
      handleRequest (from, msg, replyMsg);

      --  Async/no-completion presents are queued by handleRequest. Flush them
      --  before replyWait can block the display service waiting for more work.
      if pendingPresent then
         flushPendingPresent;
      end if;
      maybePrintStats;

      replyWait (from, replyMsg, from, msg);
   end loop;
end main;
