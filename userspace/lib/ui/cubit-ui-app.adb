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
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Memory_Grants;

package body CuBit.UI.App is
   use ASCII;
   package DP renames CuBit.Desktop_Protocol;
   package MG renames CuBit.Memory_Grants;
   use type DP.Status_Code;

   use type DP.Operation;
   use type DP.Input_Event_Kind;

   WINDOW_CHROME_W : constant Natural := 8;
   WINDOW_CHROME_H : constant Natural := 34;

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
      request : Message;
      layout : constant DP.Buffer_Layout :=
        (DP.Positive_Extent (width), DP.Positive_Extent (height), width * 4);
   begin
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Attachment
           ((DP.Live_Surface_Name (win.surfaceId), win.bufferGrant, layout)));
      request.tag := capCall (CAP_SLOT_DESKTOP, request);
      ok := request.tag.label = DP.Code (DP.Attach_Buffer) and then
        request.tag.length = 1 and then request.words (0) = 0;
      if ok then
         win.width := width;
         win.height := height;
         win.pitch := layout.Pitch;
      end if;
   end Attach_Buffer;

   procedure Ensure_Buffer
      (win : in out Window;
       width, height : Natural;
       ok : out Boolean)
   is
      raw : Unsigned_64;
      pages : Unsigned_64;
      created, revoked : Boolean;
      candidate : Window := win;
      replacing : Boolean := False;
   begin
      ok := False;
      if win.surfaceId = 0 or else
        width not in 1 .. Natural (DP.Positive_Extent'Last) or else
        height not in 1 .. Natural (DP.Positive_Extent'Last)
      then
         return;
      end if;
      -- Validate before allocation or mutation, using the same wire policy.
      if not DP.Valid_Layout
        ((DP.Positive_Extent (width), DP.Positive_Extent (height), width * 4))
      then
         return;
      end if;
      pages := (Unsigned_64 (width) * 4 * Unsigned_64 (height) + 4095) / 4096;

      if win.bufferPages = 0 or else pages > win.bufferPages then
         raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
         if raw = Unsigned_64'Last then
            return;
         end if;
         candidate.bufferAddr := To_Address (Integer_Address (Align_Up_Page (raw)));
         MG.Create_Via_Capability
           (CAP_SLOT_DESKTOP, candidate.bufferAddr, Natural (pages), False,
            candidate.bufferGrant, created);
         if not created then
            return;
         end if;
         candidate.bufferPages := pages;
         replacing := True;
      end if;

      Attach_Buffer (candidate, width, height, ok);
      if ok then
         if replacing and then win.bufferPages /= 0 then
            -- The accepted replacement released the compositor's old loan.
            MG.Revoke (win.bufferGrant, revoked);
         end if;
         win := candidate;
      elsif replacing then
         MG.Revoke (candidate.bufferGrant, revoked);
      end if;
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
      creation : DP.Creation_Result;
      limits : DP.Limits_Request;
      reply : Message;
      minW, minH : Unsigned_64;
      maxW : Unsigned_64 := 0;
      maxH : Unsigned_64 := 0;
      attached : Boolean;
   begin
      ok := False;
      win := (others => <>);
      win.flags := flags;
      -- Validate the public API before adding chrome or converting into the
      -- wire's bounded geometry. Bad caller input leaves the window unopened.
      if width not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_W or else
        height not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_H or else
        maximum_width > Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_W or else
        maximum_height > Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_H or else
        flags > DP.Feature_Bits ([others => True])
      then
         return;
      end if;
      minW := Unsigned_64 (width + WINDOW_CHROME_W);
      minH := Unsigned_64 (height + WINDOW_CHROME_H);

      if (flags and WINDOW_FLAG_FIXED_SIZE) /= 0 then
         maxW := minW;
         maxH := minH;
      elsif maximum_width > 0 and then maximum_height > 0 then
         maxW := Unsigned_64 (Natural'Max (width, maximum_width) +
                              WINDOW_CHROME_W);
         maxH := Unsigned_64 (Natural'Max (height, maximum_height) +
                              WINDOW_CHROME_H);
      end if;

      hello := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Hello (DP.Current_Revision));
      hello.tag := capCall (CAP_SLOT_DESKTOP, hello);
      if DP.Decode_Hello_Result (CuBit.Desktop_Messages.To_Wire (hello)).Status /= DP.Success then
         debugPrint ("ui-app: desktop hello failed" & LF);
         return;
      end if;

      info := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Get_Information));
      info.tag := capCall (CAP_SLOT_DESKTOP, info);
      if DP.Decode_Information_Result (CuBit.Desktop_Messages.To_Wire (info)).Status /= DP.Success then
         debugPrint ("ui-app: desktop info failed" & LF);
         return;
      end if;

      created := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Create ((DP.Pixel_Extent (minW), DP.Pixel_Extent (minH), DP.Window_Surface)));
      created.tag := capCall (CAP_SLOT_DESKTOP, created);
      creation := DP.Decode_Creation_Result (CuBit.Desktop_Messages.To_Wire (created));
      if creation.Status /= DP.Success then
         debugPrint ("ui-app: surface create failed" & LF);
         Close (win);
         return;
      end if;
      win.surfaceId := Unsigned_64 (creation.Surface);

      limits.Surface := creation.Surface;
      limits.Bounds :=
        (DP.Pixel_Extent (minW), DP.Pixel_Extent (minH),
         DP.Pixel_Extent (maxW), DP.Pixel_Extent (maxH));
      for feature in DP.Window_Feature loop
         limits.Features (feature) :=
           (flags and DP.Window_Feature'Enum_Rep (feature)) /= 0;
      end loop;
      reply := CuBit.Desktop_Messages.From_Wire (DP.Encode_Limits (limits));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply);
      if DP.Decode_Limits_Result
        (CuBit.Desktop_Messages.To_Wire (reply)).Status /= DP.Success
      then
         debugPrint ("ui-app: window limits failed" & LF);
         Close (win);
         return;
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
      request : Message;
   begin
      if win.surfaceId = 0 then
         return;
      end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Title
           ((DP.Live_Surface_Name (win.surfaceId), DP.Make_Title (title))));
      request.tag := capCall (CAP_SLOT_DESKTOP, request);
      if DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (request),
                           DP.Set_Window_Title) /= DP.Success
      then
         debugPrint ("ui-app: window title request failed" & LF);
      end if;
   end Set_Title;

   procedure Receive_Input
      (win : in out Window;
       operation : DP.Input_Operation;
       event : out Input_Event;
       found : out Boolean;
       deadline : Unsigned_64 := 0)
   is
      reply : Message;
      decoded : DP.Input_Result;
   begin
      event := (others => <>);
      found := False;
      if win.surfaceId = 0 then
         return;
      end if;

      reply := CuBit.Desktop_Messages.From_Wire
        ((if operation = DP.Poll_Input then
            DP.Encode_Input_Request ((DP.Poll_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent))
          else DP.Encode_Input_Request ((DP.Wait_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent, deadline))));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply);
      decoded := DP.Decode_Input_Result (CuBit.Desktop_Messages.To_Wire (reply), operation);
      win.inputMayRemain := False;
      if decoded.Status /= DP.Success then
         debugPrint ("ui-app: input request or reply rejected" & LF);
         return;
      end if;
      win.inputMayRemain := decoded.Value.More_Pending;
      win.lastEvent := decoded.Value.Serial;
      if decoded.Value.Kind = DP.No_Input then
         return;
      end if;

      event :=
        (kind     => DP.Input_Event_Kind'Enum_Rep (decoded.Value.Kind),
         serial   => decoded.Value.Serial,
         payload0 => decoded.Value.Payload0,
         payload1 => decoded.Value.Payload1);
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
      Receive_Input (win, DP.Poll_Input, event, found);
   end Poll_Input;

   procedure Wait_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Receive_Input (win, DP.Wait_Input, event, found);
   end Wait_Input;

   procedure Wait_Input_Until
      (win : in out Window;
       deadline : Unsigned_64;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Receive_Input (win, DP.Wait_Input, event, found, deadline);
   end Wait_Input_Until;

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

   function Request_Pointer_Cursor
      (win : Window; cursor : CuBit.UI.Pointer_Cursor_Style)
      return Boolean
   is
      reply : Message;
   begin
      if win.surfaceId = 0 then
         return False;
      end if;
      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Cursor
           ((DP.Live_Surface_Name (win.surfaceId),
             (case cursor is
                when Pointer_Default => DP.Default_Cursor,
                when Pointer_Text => DP.Text_Cursor,
                when Pointer_Resize_Horizontal => DP.Horizontal_Resize_Cursor,
                when Pointer_Resize_Vertical => DP.Vertical_Resize_Cursor,
                when Pointer_Resize_Diagonal => DP.Diagonal_Resize_Cursor))));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply);
      return DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (reply),
                               DP.Set_Pointer_Cursor) = DP.Success;
   end Request_Pointer_Cursor;

   procedure Set_Pointer_Cursor
      (win : Window; cursor : CuBit.UI.Pointer_Cursor_Style) is
   begin
      if not Request_Pointer_Cursor (win, cursor) then
         debugPrint ("ui-app: pointer cursor request failed" & LF);
      end if;
   end Set_Pointer_Cursor;

   procedure Present
      (win : Window; damage : CuBit.UI.Rect)
   is
      request : Message;
      tag : MessageTag;
      r : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (Canvas (win), damage);
   begin
      if win.surfaceId = 0 or else CuBit.UI.Is_Empty (r) then
         return;
      end if;

      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Present
           ((DP.Live_Surface_Name (win.surfaceId),
             (DP.Pixel_Coordinate (r.x), DP.Pixel_Coordinate (r.y),
              DP.Pixel_Extent (r.w), DP.Pixel_Extent (r.h)))));
      tag := capCall (CAP_SLOT_DESKTOP, request);
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
         if nextCursor /= interaction.cursor and then
           Request_Pointer_Cursor (win, nextCursor)
         then
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
      revoked : Boolean;
   begin
      if win.sentBye then
         return;
      end if;

      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Goodbye));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply);
      if DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (reply), DP.Goodbye) /= DP.Success then
         debugPrint ("ui-app: desktop goodbye failed; retaining window state" & LF);
         return;
      end if;
      if win.bufferPages /= 0 then
         MG.Revoke (win.bufferGrant, revoked);
      end if;
      win.bufferPages := 0;
      win.bufferAddr := System.Null_Address;
      win.surfaceId := 0;
      win.sentBye := True;
   end Close;
end CuBit.UI.App;
