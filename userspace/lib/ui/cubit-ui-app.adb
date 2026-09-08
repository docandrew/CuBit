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
   OP_SURFACE_SET_POINTER_CURSOR : constant Unsigned_32 := 16#0815#;
   OP_WINDOW_SET_LIMITS : constant Unsigned_32 := 16#0841#;
   OP_WINDOW_SET_TITLE  : constant Unsigned_32 := 16#0842#;
   OP_INPUT_POLL       : constant Unsigned_32 := 16#0821#;
   OP_INPUT_WAIT       : constant Unsigned_32 := 16#0822#;
   INPUT_REPLY_MORE_PENDING : constant Unsigned_8 := 1;

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
                      reserved  => 0),
         authorityTag => 0,
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
      result : CuBit.UI.Canvas := Canvas (win);
   begin
      if CuBit.UI.Is_Empty (clip) then
         return result;
      elsif clip.x = 0 and then clip.y = 0 and then
            clip.w = win.width and then clip.h = win.height
      then
         return result;
      else
         result.clip := CuBit.UI.Clamp_Rect (result, clip);
         result.clipEnabled := True;
         return result;
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

   procedure Receive_Input
      (win : in out Window;
       operation : Unsigned_32;
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

      reply := Call_Desktop (operation, win.surfaceId, win.lastEvent, 0, 0);
      win.inputMayRemain :=
        (reply.tag.flags and INPUT_REPLY_MORE_PENDING) /= 0;
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
   end Receive_Input;

   procedure Poll_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Receive_Input (win, OP_INPUT_POLL, event, found);
   end Poll_Input;

   procedure Wait_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Receive_Input (win, OP_INPUT_WAIT, event, found);
   end Wait_Input;

   function Input_May_Remain (win : Window) return Boolean is
     (win.inputMayRemain);

   function Pointer_Wheel_Delta (event : Input_Event) return Integer is
      raw : constant Unsigned_32 :=
        Unsigned_32 (event.payload1 and 16#FFFF_FFFF#);
      negativeMagnitude : Unsigned_64;
   begin
      if raw <= Unsigned_32 (Integer'Last) then
         return Integer (raw);
      elsif raw = 16#8000_0000# then
         return Integer'First;
      end if;

      negativeMagnitude := 16#1_0000_0000# - Unsigned_64 (raw);
      return -Integer (negativeMagnitude);
   end Pointer_Wheel_Delta;

   procedure Set_Pointer_Cursor
      (win : Window; cursor : CuBit.UI.Pointer_Cursor_Style)
   is
      reply : Message;
   begin
      if win.surfaceId = 0 then
         return;
      end if;
      reply := Call_Desktop
        (OP_SURFACE_SET_POINTER_CURSOR,
         win.surfaceId,
         Unsigned_64 (CuBit.UI.Pointer_Cursor_Style'Enum_Rep (cursor)));
   end Set_Pointer_Cursor;

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
       controls : in out CuBit.UI.Controls.Control_Map;
       win : Window;
       event : Input_Event;
       dirty : in out CuBit.UI.Rect;
       repaint : Pointer_Repaint_Policy := Repaint_Changed_Controls)
   is
      x, y : Natural;
      hit : CuBit.UI.Controls.Control_ID;
      down : Boolean;
      retainedChanged : Boolean := False;
      retainedHandled : Boolean := False;

      procedure Mark_Visual (id : CuBit.UI.Controls.Control_ID) is
      begin
         CuBit.UI.Controls.Mark_Visual_Dirty (dirty, controls, id);
      end Mark_Visual;

      procedure Mark_Action (id : CuBit.UI.Controls.Control_ID) is
      begin
         CuBit.UI.Controls.Mark_Action_Dirty (dirty, controls, id);
      end Mark_Action;

      procedure Mark_Hover_Transition
         (next : CuBit.UI.Controls.Control_ID)
      is
      begin
         if next /= interaction.hovered then
            Mark_Visual (interaction.hovered);
            Mark_Visual (next);
         end if;
         interaction.hovered := next;
      end Mark_Hover_Transition;

      procedure Update_Cursor
         (control : CuBit.UI.Controls.Control_ID)
      is
         nextCursor : constant CuBit.UI.Pointer_Cursor_Style :=
           CuBit.UI.Controls.Cursor (controls, control);
      begin
         if nextCursor /= interaction.cursor then
            Set_Pointer_Cursor (win, nextCursor);
            interaction.cursor := nextCursor;
         end if;
      end Update_Cursor;
   begin
      if event.kind = INPUT_RESYNC then
         x := Natural (event.payload0 and 16#FFFF_FFFF#);
         y := Natural (Shift_Right (event.payload0, 32));
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         down := (event.payload1 and 1) /= 0;
         CuBit.UI.Controls.Dispatch_Pointer
           (controls, interaction.captured,
            CuBit.UI.Controls.Pointer_Cancel, x, y,
            retainedChanged, retainedHandled);
         interaction.captured := CuBit.UI.Controls.NO_CONTROL;
         interaction.hovered := hit;
         CuBit.UI.State.Resynchronize_Pointer (ui, x, y, down);
         Update_Cursor (hit);
         dirty := CuBit.UI.Union_Rect (dirty, Full_Rect (win));
         return;
      end if;

      if event.kind /= INPUT_POINTER_MOVE and then
         event.kind /= INPUT_POINTER_DOWN and then
         event.kind /= INPUT_POINTER_UP and then
         event.kind /= INPUT_POINTER_WHEEL
      then
         return;
      end if;

      x := Natural (event.payload0 and 16#FFFF_FFFF#);
      y := Natural (Shift_Right (event.payload0, 32));

      if not CuBit.UI.Controls.Is_Valid (controls) then
         if interaction.controlsValid then
            debugPrint
              ("ui-app: invalid control map; input disabled" & LF);
         end if;
         interaction.controlsValid := False;
         interaction.captured := CuBit.UI.Controls.NO_CONTROL;
         interaction.hovered := CuBit.UI.Controls.NO_CONTROL;
         CuBit.UI.State.Resynchronize_Pointer
           (ui, x, y,
            (if event.kind = INPUT_POINTER_MOVE
             then (event.payload1 and 1) /= 0
             else event.kind = INPUT_POINTER_DOWN));
         ui.pointer.enabled := False;
         Update_Cursor (CuBit.UI.Controls.NO_CONTROL);
         dirty := CuBit.UI.Union_Rect (dirty, Full_Rect (win));
         return;
      end if;
      interaction.controlsValid := True;
      hit := CuBit.UI.Controls.Hit (controls, x, y);

      if event.kind = INPUT_POINTER_MOVE then
         down := (event.payload1 and 1) /= 0;
         CuBit.UI.State.Set_Pointer (ui, x, y, down);
         if down then
            CuBit.UI.Controls.Dispatch_Pointer
              (controls, interaction.captured,
               CuBit.UI.Controls.Pointer_Move, x, y,
               retainedChanged, retainedHandled);
         end if;
         if repaint = Repaint_Every_Motion then
            dirty := CuBit.UI.Union_Rect (dirty, Full_Rect (win));
            interaction.hovered := hit;
            Update_Cursor
              ((if down then interaction.captured else hit));
         else
            Mark_Hover_Transition (hit);
            if down then
               if retainedHandled then
                  if retainedChanged then
                     Mark_Action (interaction.captured);
                  end if;
               elsif CuBit.UI.Controls.Has_Continuous_Action
                    (controls, interaction.captured)
               then
                  --  Splitters, sliders, and scrollbar thumbs can change
                  --  continuously while captured. Click controls cannot;
                  --  repainting their containing view for held motion only
                  --  adds latency and presentation traffic.
                  Mark_Action (interaction.captured);
               end if;
               Update_Cursor (interaction.captured);
            else
               Update_Cursor (hit);
            end if;
         end if;
      elsif event.kind = INPUT_POINTER_DOWN then
         if interaction.captured /= CuBit.UI.Controls.NO_CONTROL then
            CuBit.UI.Controls.Dispatch_Pointer
              (controls, interaction.captured,
               CuBit.UI.Controls.Pointer_Cancel, x, y,
               retainedChanged, retainedHandled);
            Mark_Visual (interaction.captured);
         end if;
         Mark_Hover_Transition (hit);
         interaction.captured := hit;
         Update_Cursor (hit);
         CuBit.UI.State.Set_Pointer
           (ui, x, y, True, pressed => True);
         CuBit.UI.Controls.Dispatch_Pointer
           (controls, hit, CuBit.UI.Controls.Pointer_Press, x, y,
            retainedChanged, retainedHandled);
         --  Sliders, scrollbars, and splitters can change their value on the
         --  pressed frame.  Their registered action region must therefore be
         --  rendered immediately; buttons with local actions simply register
         --  their own bounds.
         if retainedHandled and then retainedChanged then
            Mark_Action (hit);
         elsif retainedHandled then
            Mark_Visual (hit);
         elsif CuBit.UI.Controls.Has_Continuous_Action (controls, hit) then
            Mark_Action (hit);
         else
            Mark_Visual (hit);
         end if;
      elsif event.kind = INPUT_POINTER_UP then
         CuBit.UI.Controls.Dispatch_Pointer
           (controls, interaction.captured,
            CuBit.UI.Controls.Pointer_Release, x, y,
            retainedChanged, retainedHandled);
         if retainedHandled and then retainedChanged then
            Mark_Action (interaction.captured);
         elsif retainedHandled then
            Mark_Visual (interaction.captured);
         elsif CuBit.UI.Controls.Has_Continuous_Action
              (controls, interaction.captured)
         then
            Mark_Action (interaction.captured);
         else
            Mark_Visual (interaction.captured);
         end if;
         Mark_Hover_Transition (hit);
         CuBit.UI.State.Set_Pointer
           (ui, x, y, False, released => True);
         interaction.captured := CuBit.UI.Controls.NO_CONTROL;
         Update_Cursor (hit);
      else
         --  Wheel payload1 is a signed delta, not a button mask. Keep the
         --  existing button state while updating hover for wheel-at-pointer.
         CuBit.UI.State.Set_Pointer (ui, x, y, ui.pointer.down);
         Mark_Hover_Transition (hit);
         Update_Cursor (hit);
      end if;
   end Apply_Pointer_Event;

   procedure Run (win : in out Window)
   is
      running : Boolean := True;
      drainLimit : constant Natural := 32;
      dirtyBatchLimit : constant Natural := 4;
      pointer : Pointer_Interaction;
      pendingEvent : Input_Event;
      hasPendingEvent : Boolean := False;
   begin
      if not Is_Open (win) then
         return;
      end if;

      Render (win, Full_Rect (win));
      if CuBit.UI.State.Followup_Render_Requested (ui) then
         Render (win, Full_Rect (win));
      end if;
      Present (win, Full_Rect (win));

      while running loop
         declare
            dirty : CuBit.UI.Rect := (others => 0);
            dirtyEvents : Natural := 0;
         begin
            for i in 1 .. drainLimit loop
               declare
                  event : Input_Event;
                  found : Boolean;
                  fromWait : constant Boolean := hasPendingEvent;
               begin
                  if hasPendingEvent then
                     event := pendingEvent;
                     found := True;
                     hasPendingEvent := False;
                  else
                     Poll_Input (win, event, found);
                  end if;
                  exit when not found;

                  --  Press/release/wheel events are ordering barriers for
                  --  immediate-mode widgets.  Render accumulated motion while
                  --  the prior button state is still active; otherwise a
                  --  move followed by release in one drain batch makes a
                  --  splitter lose its final position and visual update.
                  if not CuBit.UI.Is_Empty (dirty) and then
                    (event.kind = INPUT_POINTER_DOWN or else
                     event.kind = INPUT_POINTER_UP or else
                     event.kind = INPUT_POINTER_WHEEL)
                  then
                     pendingEvent := event;
                     hasPendingEvent := True;
                     exit;
                  end if;

                  if not CuBit.UI.Is_Empty (dirty) then
                     dirtyEvents := dirtyEvents + 1;
                  end if;

                  Apply_Pointer_Event
                    (pointer, ui, controls, win, event, dirty,
                     pointerRepaint);
                  Handle_Event (win, event, dirty, running);
                  exit when not running;
                  --  Keyboard, wheel, configuration, and application-defined
                  --  events may change which controls exist or where they are
                  --  placed.  Rebuild the immediate-mode control map before
                  --  dispatching another queued event against it. Pointer
                  --  motion is the only event class safe to coalesce here.
                  exit when event.kind /= INPUT_POINTER_MOVE and then
                            not CuBit.UI.Is_Empty (dirty);
                  --  A blocking wait reply tells us whether another event was
                  --  already queued. If not, avoid an immediately-following
                  --  empty poll and return to the atomic wait path. An event
                  --  arriving after the reply will wake that wait normally.
                  exit when fromWait and then not Input_May_Remain (win);
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
               if CuBit.UI.State.Followup_Render_Requested (ui) then
                  --  A grouped selection may be changed by an item rendered
                  --  after the previously selected item. Repaint the stable
                  --  post-action tree once before presentation so both old
                  --  and new selection visuals cannot survive together.
                  dirty := Full_Rect (win);
                  Render (win, dirty);
               end if;
               Present (win, dirty);
            end if;

            if running and then not hasPendingEvent then
               --  Park on a deferred one-use reply capability. Reducing the
               --  old polling interval would still add avoidable latency and
               --  burn CPU; this wakes directly when input is queued. It is
               --  also safe immediately after a drained batch: already-queued
               --  input replies at once, while a later arrival resolves the
               --  installed one-use waiter.
               Wait_Input (win, pendingEvent, hasPendingEvent);
               if not hasPendingEvent then
                  --  INPUT_WAIT returns no event only if the surface is no
                  --  longer owned by this process. Avoid a failed-wait spin.
                  running := False;
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
