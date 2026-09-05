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

   OP_GPU_ATTACH_BUFFER : constant Unsigned_32 := 16#0A01#;
   OP_GPU_PRESENT_RECT  : constant Unsigned_32 := 16#0A02#;
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
   DISPLAY_CAP_DIRECT_BACKBUFFER : constant Unsigned_64 := 16#0008#;

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
   gpuActive    : Boolean := False;
   gpuGrantId   : Unsigned_64 := 0;
   directActive  : Boolean := False;
   directAddr    : System.Address := System.Null_Address;
   directGrantId : Unsigned_64 := 0;
   directWidth   : Natural := 0;
   directHeight  : Natural := 0;
   directPitch   : Natural := 0;
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
         return DISPLAY_CAP_COPY_PRESENT or DISPLAY_CAP_GPU_PRESENT or
            DISPLAY_CAP_DIRECT_BACKBUFFER;
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
      gpuActive := False;
      gpuGrantId := 0;
      directActive := False;
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

   function packU32 (lo, hi : Unsigned_64) return Unsigned_64 is
   begin
      return (lo and 16#FFFF_FFFF#) or Shift_Left (hi and 16#FFFF_FFFF#, 32);
   end packU32;

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
      gpuActive := False;
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

   function attachGpuBuffer return Boolean is
      pages : Natural;
      bytes : Unsigned_64;
      ok : Boolean;
      reply : Message;
   begin
      if not gpuAvailable or else srcAddr = System.Null_Address then
         return False;
      end if;

      bytes := Unsigned_64 (srcPitch) * Unsigned_64 (srcHeight);
      pages := Natural ((bytes + 4095) / 4096);
      if pages = 0 then
         return False;
      end if;

      createGrantViaCap
        (slot      => CAP_SLOT_GPU,
         localAddr => srcAddr,
         numPages  => pages,
         readWrite => False,
         grantId   => gpuGrantId,
         success   => ok);
      if not ok then
         debugPrint ("display: gpu grant failed" & LF);
         return False;
      end if;

      reply := callGpu
        (OP_GPU_ATTACH_BUFFER,
         gpuGrantId,
         Unsigned_64 (srcWidth),
         Unsigned_64 (srcHeight),
         Unsigned_64 (srcPitch));
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         gpuActive := True;
         debugPrint ("display: gpu buffer attached" & LF);
         return True;
      end if;

      debugPrint ("display: gpu attach failed" & LF);
      gpuActive := False;
      return False;
   end attachGpuBuffer;

   function presentGpuRect (r : Rect) return Boolean is
      reply : Message;
   begin
      if not gpuActive then
         return False;
      end if;

      reply := callGpu
        (OP_GPU_PRESENT_RECT,
         Unsigned_64 (r.x),
         Unsigned_64 (r.y),
         Unsigned_64 (r.w),
         Unsigned_64 (r.h));
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         return True;
      end if;

      debugPrint ("display: gpu present failed, falling back" & LF);
      gpuActive := False;
      return False;
   end presentGpuRect;

   function flushGpuRect (r : Rect) return Boolean is
      reply : Message;
   begin
      if not directActive then
         return False;
      end if;

      reply := callGpu
        (OP_GPU_FLUSH_RECT,
         Unsigned_64 (r.x),
         Unsigned_64 (r.y),
         Unsigned_64 (r.w),
         Unsigned_64 (r.h));
      if reply.tag.length >= 1 and then reply.words (0) = 0 then
         return True;
      end if;

      debugPrint ("display: gpu direct flush failed" & LF);
      directActive := False;
      return False;
   end flushGpuRect;

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
      if directActive then
         copyStart := syscall (SYSCALL_GETTIME);
         if not flushGpuRect (r) then
            if waitForScanout then
               waitForVBlank;
            end if;
            presentRect (r.x, r.y, r.w, r.h);
         end if;
      elsif gpuActive then
         copyStart := syscall (SYSCALL_GETTIME);
         if not presentGpuRect (r) then
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
               directActive := False;
               if gpuAvailable then
                  if not attachGpuBuffer then
                     gpuActive := False;
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
            elsif not gpuAvailable then
               replyMsg.words (0) := DISPLAY_ERR_UNSUPPORTED;
            else
               declare
                  gpuMap : Message;
                  desktopGrant : Unsigned_64;
                  grantOk : Boolean;
                  pages : Natural;
               begin
                  if directAddr = System.Null_Address then
                     gpuMap := callGpu (OP_GPU_MAP_FRAMEBUFFER);
                     if gpuMap.tag.length >= 4 and then gpuMap.words (0) = 0
                     then
                        directGrantId := gpuMap.words (1);
                        directAddr := toAddr
                          (GRANT_REGION_BASE +
                           directGrantId * GRANT_SLOT_SIZE);
                        directWidth := unpackLo32 (gpuMap.words (2));
                        directHeight := unpackHi32 (gpuMap.words (2));
                        directPitch := Natural (gpuMap.words (3));
                     end if;
                  end if;

                  if directAddr = System.Null_Address or else
                     directWidth = 0 or else directHeight = 0 or else
                     directPitch < directWidth * 4
                  then
                     replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                  else
                     pages := Natural
                       ((Unsigned_64 (directPitch) *
                         Unsigned_64 (directHeight) + 4095) / 4096);
                     createGrant
                       (grantee   => from,
                        localAddr => directAddr,
                        numPages  => pages,
                        readWrite => True,
                        grantId   => desktopGrant,
                        success   => grantOk);
                     if grantOk then
                        srcAddr := directAddr;
                        srcWidth := directWidth;
                        srcHeight := directHeight;
                        srcPitch := directPitch;
                        srcOwner := from;
                        gpuActive := False;
                        directActive := True;

                        replyMsg.words (0) := DISPLAY_OK;
                        replyMsg.words (1) := desktopGrant;
                        replyMsg.words (2) :=
                           packU32 (Unsigned_64 (directWidth),
                                    Unsigned_64 (directHeight));
                        replyMsg.words (3) := Unsigned_64 (directPitch);
                        debugPrint ("display: direct gpu backbuffer mapped" &
                                    LF);
                     else
                        replyMsg.words (0) := DISPLAY_ERR_BAD_STATE;
                     end if;
                  end if;
               end;
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
