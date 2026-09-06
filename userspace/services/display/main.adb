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

   OP_GPU_CLEAR         : constant Unsigned_32 := 16#0A03#;
   OP_GPU_GET_STATUS    : constant Unsigned_32 := 16#0A04#;
   OP_GPU_MAP_FRAMEBUFFER : constant Unsigned_32 := 16#0A05#;
   OP_GPU_FLUSH_RECT    : constant Unsigned_32 := 16#0A06#;

   DISPLAY_OK              : constant Unsigned_64 := 0;
   DISPLAY_ERR_DENIED      : constant Unsigned_64 := 1;
   DISPLAY_ERR_BAD_OBJECT  : constant Unsigned_64 := 2;
   DISPLAY_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   DISPLAY_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB
   VGA_STATUS_PORT   : constant Unsigned_16 := 16#03DA#;
   VGA_VBLANK_BIT    : constant Unsigned_64 := 16#08#;

   DISPLAY_BACKEND_LINEAR_FB : constant Unsigned_64 := 1;
   DISPLAY_BACKEND_VIRTIO_GPU : constant Unsigned_64 := 3;

   DISPLAY_CAP_COPY_PRESENT : constant Unsigned_64 := 16#0001#;
   DISPLAY_CAP_VBLANK_WAIT  : constant Unsigned_64 := 16#0002#;
   DISPLAY_CAP_GPU_PRESENT  : constant Unsigned_64 := 16#0004#;

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
   gpuScanoutAddr : System.Address := System.Null_Address;
   gpuScanoutGrantId : Unsigned_64 := 0;
   gpuScanoutWidth   : Natural := 0;
   gpuScanoutHeight  : Natural := 0;
   gpuScanoutPitch   : Natural := 0;
   displayOwner : ProcessID := NO_PROCESS;

   type Rect is record
      x : Natural := 0;
      y : Natural := 0;
      w : Natural := 0;
      h : Natural := 0;
   end record;

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
         return DISPLAY_CAP_COPY_PRESENT or DISPLAY_CAP_GPU_PRESENT;
      else
         return DISPLAY_CAP_COPY_PRESENT or DISPLAY_CAP_VBLANK_WAIT;
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
        (tag      => (label => label, length => 4, flags => 0, badge => 0),
         capBadge => 0,
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

   procedure waitForVBlank is
      val : Unsigned_64;
      MAX_POLLS : constant Natural := 200_000;
   begin
      --  QEMU's VESA framebuffer is still backed by VGA-compatible scanout
      --  state. Waiting for vertical blank before touching the live scanout
      --  buffer reduces tearing until display.svc grows real page flipping.
      --
      --  This is deliberately bounded: if the platform does not expose the
      --  VGA status bit, or the I/O-port cap was not granted, present should
      --  degrade to immediate copy rather than wedging the display service.
      for i in 1 .. MAX_POLLS loop
         val := portInp8 (VGA_STATUS_PORT);
         if val = Unsigned_64'Last then
            return;
         end if;
         exit when (val and VGA_VBLANK_BIT) = 0;
      end loop;

      for i in 1 .. MAX_POLLS loop
         val := portInp8 (VGA_STATUS_PORT);
         if val = Unsigned_64'Last then
            return;
         end if;
         exit when (val and VGA_VBLANK_BIT) /= 0;
      end loop;
   end waitForVBlank;

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

      if gpuScanoutAddr = System.Null_Address then
         gpuMap := callGpu (OP_GPU_MAP_FRAMEBUFFER);
         if gpuMap.tag.length < 4 or else gpuMap.words (0) /= 0 then
            debugPrint ("display: gpu scanout map failed" & LF);
            return False;
         end if;

         gpuScanoutGrantId := gpuMap.words (1);
         gpuScanoutAddr := toAddr
           (GRANT_REGION_BASE + gpuScanoutGrantId * GRANT_SLOT_SIZE);
         gpuScanoutWidth := unpackLo32 (gpuMap.words (2));
         gpuScanoutHeight := unpackHi32 (gpuMap.words (2));
         gpuScanoutPitch := Natural (gpuMap.words (3));
      end if;

      return gpuScanoutAddr /= System.Null_Address and then
        gpuScanoutWidth > 0 and then gpuScanoutHeight > 0 and then
        gpuScanoutPitch >= gpuScanoutWidth * 4;
   end ensureGpuScanout;

   function copyAndFlushGpuRect (r : Rect) return Boolean is
      maxX : Natural := r.x + r.w;
      maxY : Natural := r.y + r.h;
      ignore : System.Address;
      reply : Message;
   begin
      if not gpuCopyActive or else srcAddr = System.Null_Address or else
         r.w = 0 or else r.h = 0 or else
         r.x >= srcWidth or else r.y >= srcHeight or else
         r.x >= gpuScanoutWidth or else r.y >= gpuScanoutHeight
      then
         return False;
      end if;

      maxX := Natural'Min (maxX, srcWidth);
      maxX := Natural'Min (maxX, gpuScanoutWidth);
      maxY := Natural'Min (maxY, srcHeight);
      maxY := Natural'Min (maxY, gpuScanoutHeight);
      if r.x >= maxX or else r.y >= maxY then
         return False;
      end if;

      if r.x = 0 and then maxX = gpuScanoutWidth and then
         srcPitch = gpuScanoutPitch
      then
         ignore := memcpy
           (gpuScanoutAddr + Storage_Offset (r.y * gpuScanoutPitch),
            srcAddr + Storage_Offset (r.y * srcPitch),
            Storage_Count ((maxY - r.y) * gpuScanoutPitch));
      else
         for row in r.y .. maxY - 1 loop
            ignore := memcpy
              (gpuScanoutAddr +
                 Storage_Offset (row * gpuScanoutPitch + r.x * 4),
               srcAddr + Storage_Offset (row * srcPitch + r.x * 4),
               Storage_Count ((maxX - r.x) * 4));
         end loop;
      end if;

      reply := callGpu
        (OP_GPU_FLUSH_RECT,
         Unsigned_64 (r.x),
         Unsigned_64 (r.y),
         Unsigned_64 (maxX - r.x),
         Unsigned_64 (maxY - r.y));
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         return True;
      end if;

      debugPrint ("display: gpu copy flush failed" & LF);
      gpuCopyActive := False;
      return False;
   end copyAndFlushGpuRect;

   procedure flushPendingPresent (waitForScanout : Boolean := True) is
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
         if not copyAndFlushGpuRect (r) then
            if waitForScanout then
               waitForVBlank;
            end if;
            presentRect (r.x, r.y, r.w, r.h);
         end if;
      else
         if waitForScanout then
            waitForVBlank;
         end if;
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
                             badge  => 0);
            replyMsg.words (0) := Unsigned_64 (fbWidth);
            replyMsg.words (1) := Unsigned_64 (fbHeight);
            replyMsg.words (2) := Unsigned_64 (fbPitch);
            replyMsg.words (3) := 32; -- BGRA8888

         when OP_DISPLAY_GET_STATUS =>
            replyMsg.tag := (label  => OP_DISPLAY_GET_STATUS,
                             length => 4,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := backendId;
            replyMsg.words (1) := backendCaps;
            replyMsg.words (2) := Unsigned_64 (displayOwner);
            replyMsg.words (3) := 0; -- reserved for backend-specific status

         when OP_DISPLAY_ACQUIRE =>
            replyMsg.tag := (label => OP_DISPLAY_ACQUIRE,
                             length => 1, flags => 0, badge => 0);
            if displayOwner = NO_PROCESS or else displayOwner = from then
               displayOwner := from;
               replyMsg.words (0) := DISPLAY_OK;
            else
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            end if;

         when OP_DISPLAY_RELEASE =>
            replyMsg.tag := (label => OP_DISPLAY_RELEASE,
                             length => 1, flags => 0, badge => 0);
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
                                length => 1, flags => 0, badge => 0);
               replyMsg.words (0) := DISPLAY_ERR_DENIED;
            elsif request.words (1) = 0 or else request.words (2) = 0 then
               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, badge => 0);
               replyMsg.words (0) := DISPLAY_ERR_BAD_OBJECT;
            elsif request.words (1) > Unsigned_64 (fbWidth) or else
                  request.words (2) > Unsigned_64 (fbHeight) or else
                  request.words (3) < request.words (1) * 4
            then
               replyMsg.tag := (label => OP_DISPLAY_ATTACH_BUFFER,
                                length => 1, flags => 0, badge => 0);
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
                                length => 1, flags => 0, badge => 0);
               replyMsg.words (0) := DISPLAY_OK;
               debugPrint ("display: buffer attached" & LF);
            end if;

         when OP_DISPLAY_MAP_BACKBUFFER =>
            replyMsg.tag := (label => OP_DISPLAY_MAP_BACKBUFFER,
                             length => 4, flags => 0, badge => 0);
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
                             length => 1, flags => 0, badge => 0);
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
                  flushPendingPresent
                    (waitForScanout =>
                       request.tag.label = OP_DISPLAY_PRESENT_RECT);
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

         when OP_DISPLAY_CLEAR =>
            replyMsg.tag := (label => OP_DISPLAY_CLEAR,
                             length => 1, flags => 0, badge => 0);
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
                             badge  => 0);
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
