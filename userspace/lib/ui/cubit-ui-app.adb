------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Minimal desktop-surface harness for native CuBit UI applications
------------------------------------------------------------------------------
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body CuBit.UI.App is
   use ASCII;

   OP_DESKTOP_HELLO    : constant Unsigned_32 := 16#0800#;
   OP_DESKTOP_BYE      : constant Unsigned_32 := 16#0801#;
   OP_DESKTOP_GET_INFO : constant Unsigned_32 := 16#0802#;
   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;
   OP_SURFACE_PRESENT  : constant Unsigned_32 := 16#0812#;
   OP_SURFACE_ATTACH_BUFFER : constant Unsigned_32 := 16#0814#;
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := 16#0841#;
   OP_WINDOW_SET_TITLE  : constant Unsigned_32 := 16#0842#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;

   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;
   PIXEL_FORMAT_BGRA8888 : constant Unsigned_64 := 1;
   WINDOW_CHROME_W : constant Natural := 8;
   WINDOW_CHROME_H : constant Natural := 34;
   PROTOCOL_VERSION : constant Unsigned_64 :=
      0 or Shift_Left (Unsigned_64'(1), 32);

   function Call_Desktop
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label  => label,
                      length => 4,
                      flags  => 0,
                      badge  => 0),
         capBadge => 0,
         words    => (w0, w1, w2, w3));
      tag : MessageTag;
   begin
      tag := capCall (CAP_SLOT_DESKTOP, msg);
      msg.tag := tag;
      return msg;
   end Call_Desktop;

   function Pack_U32_Pair (lo, hi : Unsigned_64) return Unsigned_64 is
   begin
      return (lo and 16#FFFF_FFFF#) or Shift_Left (hi and 16#FFFF_FFFF#, 32);
   end Pack_U32_Pair;

   function Align_Up_Page (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end Align_Up_Page;

   function Is_Open (win : Window) return Boolean is
   begin
      return win.surfaceId /= 0;
   end Is_Open;

   function Surface_ID (win : Window) return Unsigned_64 is
   begin
      return win.surfaceId;
   end Surface_ID;

   function Width (win : Window) return Natural is
   begin
      return win.width;
   end Width;

   function Height (win : Window) return Natural is
   begin
      return win.height;
   end Height;

   function Full_Rect (win : Window) return CuBit.UI.Rect is
   begin
      return (x => 0, y => 0, w => win.width, h => win.height);
   end Full_Rect;

   function Canvas (win : Window) return CuBit.UI.Canvas is
   begin
      return
        (addr        => win.bufferAddr,
         width       => win.width,
         height      => win.height,
         pitch       => win.pitch,
         clipEnabled => False,
         clip        => (others => 0));
   end Canvas;

   function Canvas
      (win : Window; clip : CuBit.UI.Rect) return CuBit.UI.Canvas
   is
      base : constant CuBit.UI.Canvas := Canvas (win);
   begin
      if CuBit.UI.Is_Empty (clip) then
         return base;
      elsif clip.x = 0 and then clip.y = 0 and then
            clip.w = win.width and then clip.h = win.height
      then
         return base;
      else
         return
           (addr        => win.bufferAddr,
            width       => win.width,
            height      => win.height,
            pitch       => win.pitch,
            clipEnabled => True,
            clip        => CuBit.UI.Clamp_Rect (base, clip));
      end if;
   end Canvas;

   function Horizontal_Chrome (win : Window) return Natural is
   begin
      if (win.flags and WINDOW_FLAG_DECORATED) /= 0 then
         return WINDOW_CHROME_W;
      end if;
      return 0;
   end Horizontal_Chrome;

   function Vertical_Chrome (win : Window) return Natural is
   begin
      if (win.flags and WINDOW_FLAG_DECORATED) /= 0 then
         return WINDOW_CHROME_H;
      end if;
      return 0;
   end Vertical_Chrome;

   function Content_Size_From_Surface
      (win : Window; surfaceSize : Unsigned_64; horizontal : Boolean)
      return Natural
   is
      chrome : constant Natural :=
         (if horizontal then Horizontal_Chrome (win) else Vertical_Chrome (win));
      value : Natural;
   begin
      if surfaceSize <= Unsigned_64 (chrome) then
         return 1;
      end if;
      value := Natural (surfaceSize - Unsigned_64 (chrome));
      if value = 0 then
         return 1;
      end if;
      return value;
   end Content_Size_From_Surface;

   procedure Attach_Buffer
      (win : in out Window;
       width, height : Natural;
       ok : out Boolean)
   is
      reply : Message;
   begin
      ok := False;
      if win.surfaceId = 0 or else win.bufferGrant = 0 or else
         width = 0 or else height = 0
      then
         return;
      end if;

      win.width := width;
      win.height := height;
      win.pitch := width * 4;

      reply := Call_Desktop
        (OP_SURFACE_ATTACH_BUFFER,
         win.surfaceId,
         win.bufferGrant,
         Pack_U32_Pair (Unsigned_64 (win.width), Unsigned_64 (win.height)),
         Unsigned_64 (win.pitch) or
            Shift_Left (PIXEL_FORMAT_BGRA8888, 32));
      ok := reply.words (0) = 0;
   end Attach_Buffer;

   procedure Ensure_Buffer
      (win : in out Window;
       width, height : Natural;
       ok : out Boolean)
   is
      raw : Unsigned_64;
      pages : Unsigned_64;
      grantOk : Boolean;
   begin
      ok := False;
      if width = 0 or else height = 0 then
         return;
      end if;

      pages := (Unsigned_64 (width * 4) * Unsigned_64 (height) + 4095) / 4096;
      if pages = 0 then
         pages := 1;
      end if;

      if win.bufferAddr = System.Null_Address or else pages > win.bufferPages
      then
         raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
         if raw = Unsigned_64'Last then
            return;
         end if;

         win.bufferAddr := To_Address (Integer_Address (Align_Up_Page (raw)));
         createGrantViaCap
           (slot      => CAP_SLOT_DESKTOP,
            localAddr => win.bufferAddr,
            numPages  => Natural (pages),
            readWrite => False,
            grantId   => win.bufferGrant,
            success   => grantOk);
         if not grantOk then
            win.bufferGrant := 0;
            return;
         end if;
         win.bufferPages := pages;
      end if;

      Attach_Buffer (win, width, height, ok);
   end Ensure_Buffer;

   procedure Open
      (win : in out Window;
       width, height : Natural;
       flags : Unsigned_64;
       ok : out Boolean;
       maximum_width : Natural := 0;
       maximum_height : Natural := 0;
       title : String := "Application")
   is
      hello : Message;
      info : Message;
      created : Message;
      reply : Message;
      minW : constant Unsigned_64 :=
         Unsigned_64 (width + WINDOW_CHROME_W);
      minH : constant Unsigned_64 :=
         Unsigned_64 (height + WINDOW_CHROME_H);
      maxW : Unsigned_64 := 0;
      maxH : Unsigned_64 := 0;
      attached : Boolean;
   begin
      ok := False;
      win := (others => <>);
      win.flags := flags;

      if (flags and WINDOW_FLAG_FIXED_SIZE) /= 0 then
         maxW := minW;
         maxH := minH;
      elsif maximum_width > 0 and then maximum_height > 0 then
         maxW := Unsigned_64 (Natural'Max (width, maximum_width) +
                              WINDOW_CHROME_W);
         maxH := Unsigned_64 (Natural'Max (height, maximum_height) +
                              WINDOW_CHROME_H);
      end if;

      hello := Call_Desktop (OP_DESKTOP_HELLO, PROTOCOL_VERSION, 0, 0, 0);
      if hello.words (0) = 0 then
         debugPrint ("ui-app: desktop hello failed" & LF);
         return;
      end if;

      info := Call_Desktop (OP_DESKTOP_GET_INFO);
      if info.words (0) = 0 then
         debugPrint ("ui-app: desktop info failed" & LF);
         return;
      end if;

      created :=
         Call_Desktop
           (OP_SURFACE_CREATE,
            minW,
            minH,
            SURFACE_FLAG_WINDOW,
            0);
      win.surfaceId := created.words (0);
      if win.surfaceId = 0 then
         debugPrint ("ui-app: surface create failed" & LF);
         Close (win);
         return;
      end if;

      reply := Call_Desktop
        (OP_WINDOW_SET_LIMITS,
         win.surfaceId,
         Pack_U32_Pair (minW, minH),
         Pack_U32_Pair (maxW, maxH),
         flags);
      if reply.words (0) = 0 then
         null;
      end if;

      Set_Title (win, title);

      Ensure_Buffer (win, width, height, attached);
      if not attached then
         debugPrint ("ui-app: buffer attach failed" & LF);
         Close (win);
         return;
      end if;

      ok := True;
   end Open;

   procedure Set_Title (win : Window; title : String) is
      --  Three inline IPC words leave 23 UTF-8/ASCII bytes plus an explicit
      --  length byte. Longer titles are clipped deterministically for now;
      --  a future string grant can lift the transport bound without changing
      --  window ownership semantics.
      MAX_INLINE_TITLE : constant Natural := 23;
      titleLength : constant Natural := Natural'Min
        (title'Length, MAX_INLINE_TITLE);
      packed0, packed1, packed2 : Unsigned_64 := 0;
      value : Unsigned_64;
      byteIndex : Natural;
   begin
      if win.surfaceId = 0 then
         return;
      end if;
      if titleLength > 0 then
         for index in 0 .. titleLength - 1 loop
            byteIndex := index mod 8;
            value := Shift_Left
              (Unsigned_64 (Character'Pos (title (title'First + index))),
               byteIndex * 8);
            case index / 8 is
               when 0 => packed0 := packed0 or value;
               when 1 => packed1 := packed1 or value;
               when others => packed2 := packed2 or value;
            end case;
         end loop;
      end if;
      packed2 := packed2 or Shift_Left (Unsigned_64 (titleLength), 56);
      declare
         reply : constant Message := Call_Desktop
           (OP_WINDOW_SET_TITLE, win.surfaceId, packed0, packed1, packed2);
      begin
         if reply.words (0) = 0 then
            null;
         end if;
      end;
   end Set_Title;

   procedure Poll_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
      reply : Message;
   begin
      event := (others => <>);
      found := False;
      if win.surfaceId = 0 then
         return;
      end if;

      reply := Call_Desktop (OP_INPUT_POLL, win.surfaceId, win.lastEvent, 0, 0);
      if reply.words (0) = INPUT_NONE then
         win.lastEvent := reply.words (1);
         return;
      end if;

      win.lastEvent := reply.words (1);
      event :=
        (kind     => reply.words (0),
         serial   => reply.words (1),
         payload0 => reply.words (2),
         payload1 => reply.words (3));
      if event.kind = INPUT_CONFIGURE then
         declare
            newW : constant Natural :=
               Content_Size_From_Surface (win, event.payload0, True);
            newH : constant Natural :=
               Content_Size_From_Surface (win, event.payload1, False);
            resized : Boolean;
         begin
            Ensure_Buffer (win, newW, newH, resized);
            event.payload0 := Unsigned_64 (win.width);
            event.payload1 := Unsigned_64 (win.height);
         end;
      end if;
      found := True;
   end Poll_Input;

   procedure Present
      (win : Window; damage : CuBit.UI.Rect)
   is
      reply : Message;
      r : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (Canvas (win), damage);
   begin
      if win.surfaceId = 0 or else CuBit.UI.Is_Empty (r) then
         return;
      end if;

      reply := Call_Desktop
        (OP_SURFACE_PRESENT,
         win.surfaceId,
         Pack_U32_Pair (Unsigned_64 (r.x), Unsigned_64 (r.y)),
         Pack_U32_Pair (Unsigned_64 (r.w), Unsigned_64 (r.h)),
         0);
   end Present;

   procedure Apply_Pointer_Event
      (interaction : in out Pointer_Interaction;
       ui : in out CuBit.UI.State.UI_State;
       controls : CuBit.UI.Controls.Control_Map;
       win : Window;
       event : Input_Event;
       dirty : in out CuBit.UI.Rect;
       repaint : Pointer_Repaint_Policy := Repaint_Changed_Controls)
   is
      x, y : Natural;
      hit : CuBit.UI.Controls.Control_ID;
      down : Boolean;

      procedure Mark (id : CuBit.UI.Controls.Control_ID) is
      begin
         CuBit.UI.Controls.Mark_Dirty (dirty, controls, id);
      end Mark;

      procedure Mark_Hover_Transition
         (next : CuBit.UI.Controls.Control_ID)
      is
      begin
         if next /= interaction.hovered then
            Mark (interaction.hovered);
            Mark (next);
         end if;
         interaction.hovered := next;
      end Mark_Hover_Transition;
   begin
      if event.kind /= INPUT_POINTER_MOVE and then
         event.kind /= INPUT_POINTER_DOWN and then
         event.kind /= INPUT_POINTER_UP and then
         event.kind /= INPUT_POINTER_WHEEL
      then
         return;
      end if;

      x := Natural (event.payload0 and 16#FFFF_FFFF#);
      y := Natural (Shift_Right (event.payload0, 32));
      hit := CuBit.UI.Controls.Hit (controls, x, y);

      if event.kind = INPUT_POINTER_MOVE then
         down := (event.payload1 and 1) /= 0;
         CuBit.UI.State.Set_Pointer (ui, x, y, down);
         if repaint = Repaint_Every_Motion then
            dirty := CuBit.UI.Union_Rect (dirty, Full_Rect (win));
            interaction.hovered := hit;
         else
            Mark_Hover_Transition (hit);
            if down then
               --  Captured controls declare their own drag-damage region.
               --  A splitter can therefore invalidate a pane while an
               --  ordinary button repaints only its face.
               Mark (interaction.captured);
            end if;
         end if;
      elsif event.kind = INPUT_POINTER_DOWN then
         Mark_Hover_Transition (hit);
         interaction.captured := hit;
         CuBit.UI.State.Set_Pointer
           (ui, x, y, True, pressed => True);
         Mark (hit);
      elsif event.kind = INPUT_POINTER_UP then
         Mark (interaction.captured);
         Mark_Hover_Transition (hit);
         CuBit.UI.State.Set_Pointer
           (ui, x, y, False, released => True);
         interaction.captured := CuBit.UI.Controls.NO_CONTROL;
      else
         --  Wheel payload1 is a signed delta, not a button mask. Keep the
         --  existing button state while updating hover for wheel-at-pointer.
         CuBit.UI.State.Set_Pointer (ui, x, y, ui.pointer.down);
         Mark_Hover_Transition (hit);
      end if;
   end Apply_Pointer_Event;

   procedure Run (win : in out Window)
   is
      running : Boolean := True;
      ignore : Unsigned_64;
      drainLimit : constant Natural := 32;
      dirtyBatchLimit : constant Natural := 4;
      pointer : Pointer_Interaction;
   begin
      if not Is_Open (win) then
         return;
      end if;

      Render (win, Full_Rect (win));
      Present (win, Full_Rect (win));

      while running loop
         declare
            dirty : CuBit.UI.Rect := (others => 0);
            sawInput : Boolean := False;
            dirtyEvents : Natural := 0;
         begin
            for i in 1 .. drainLimit loop
               declare
                  event : Input_Event;
                  found : Boolean;
               begin
                  Poll_Input (win, event, found);
                  exit when not found;

                  sawInput := True;
                  if not CuBit.UI.Is_Empty (dirty) then
                     dirtyEvents := dirtyEvents + 1;
                  end if;

                  Apply_Pointer_Event
                    (pointer, ui, controls, win, event, dirty,
                     pointerRepaint);
                  Handle_Event (win, event, dirty, running);
                  exit when not running;
                  --  Immediate-mode controls must observe a pressed frame
                  --  before release. This also makes depressed feedback
                  --  deterministic even when the input queue is busy.
                  exit when
                    (event.kind = INPUT_POINTER_DOWN or else
                     event.kind = INPUT_POINTER_UP) and then
                    not CuBit.UI.Is_Empty (dirty);
                  exit when not CuBit.UI.Is_Empty (dirty) and then
                            dirtyEvents >= dirtyBatchLimit;
               end;
            end loop;

            if not CuBit.UI.Is_Empty (dirty) then
               Render (win, dirty);
               Present (win, dirty);
            end if;

            if running then
               if sawInput then
                  ignore := syscall (SYSCALL_SLEEP, 1);
               else
                  ignore := syscall (SYSCALL_SLEEP, 10);
               end if;
            end if;
         end;
      end loop;
   end Run;

   procedure Close (win : in out Window) is
      reply : Message;
   begin
      if win.sentBye then
         return;
      end if;

      reply := Call_Desktop (OP_DESKTOP_BYE);
      win.sentBye := True;
   end Close;
end CuBit.UI.App;
