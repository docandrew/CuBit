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
with CuBit.UI.Theme_Data;
with Client_Frame_Wakeup;
with Client_Input_Channel;

package body CuBit.UI.App is
   use ASCII;
   package DP renames CuBit.Desktop_Protocol;
   package MG renames CuBit.Memory_Grants;
   package FP renames Client_Frame_Pair;
   package IP renames Client_Input_Provenance;
   use type DP.Status_Code;

   use type DP.Operation;
   use type DP.Input_Event_Kind;

   WINDOW_CHROME_W : constant Natural := 8;
   WINDOW_CHROME_H : constant Natural := 34;

   function Align_Up_Page (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end Align_Up_Page;

   function Input_Stopped (win : Window) return Boolean is (win.inputStopped);

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
        (addr        => (if win.protectedFrames then FP.Address (win.frames) else win.bufferAddr),
         width       => win.width,
         height      => win.height,
         pitch       => win.pitch,
         densityNumerator => win.densityNumerator,
         densityDenominator => win.densityDenominator,
         clipEnabled => False,
         clip        => (others => 0), others => <>);
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
         return CuBit.UI.With_Repair_Clip (result, clip);
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

   procedure Ensure_Buffer
      (win : in out Window; width, height : Natural; ok : out Boolean)
   is
      raw, pages : Unsigned_64;
      created, revoked : Boolean;
      candidateAddr : System.Address := win.bufferAddr;
      candidateGrant : MG.Grant_Reference := win.bufferGrant;
      replacing : Boolean := False;
      request : Message;
      config : FP.Pub.Configuration_Result;
   begin
      ok := False;
      if win.surfaceId = 0 then return; end if;
      if win.protectedFrames then
         FP.Configure (win.frames, DP.Live_Surface_Name (win.surfaceId), ok);
         if ok then
            config := FP.Configuration (win.frames);
            win.width := Natural (config.Value.Width);
            win.height := Natural (config.Value.Height);
            win.pitch := config.Value.Layout.Pitch;
            win.densityNumerator := config.Value.Numerator;
            win.densityDenominator := config.Value.Denominator;
         end if;
         return;
      end if;
      if width not in 1 .. Natural (DP.Positive_Extent'Last) or else
         height not in 1 .. Natural (DP.Positive_Extent'Last) or else
         not DP.Valid_Layout ((DP.Positive_Extent (width), DP.Positive_Extent (height), width * 4))
      then return; end if;
      pages := (Unsigned_64 (width) * 4 * Unsigned_64 (height) + 4095) / 4096;
      if win.bufferPages = 0 or else pages > win.bufferPages then
         raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
         if raw = Unsigned_64'Last then return; end if;
         candidateAddr := To_Address (Integer_Address (Align_Up_Page (raw)));
         MG.Create_Via_Capability (CAP_SLOT_DESKTOP, candidateAddr, Natural (pages), False,
                                   candidateGrant, created);
         if not created then return; end if;
         replacing := True;
      end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Attachment ((DP.Live_Surface_Name (win.surfaceId), candidateGrant,
           (DP.Positive_Extent (width), DP.Positive_Extent (height), width * 4))));
      request.tag := capCall (CAP_SLOT_DESKTOP, request, CuBit.Messages.Wait_Forever);
      ok := DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (request), DP.Attach_Buffer) = DP.Success;
      if ok then
         if replacing and win.bufferPages /= 0 then MG.Revoke (win.bufferGrant, revoked); end if;
         win.bufferAddr := candidateAddr; win.bufferGrant := candidateGrant;
         if replacing then win.bufferPages := pages; end if;
         win.width := width; win.height := height; win.pitch := width * 4;
      elsif replacing then MG.Revoke (candidateGrant, revoked);
      end if;
   end Ensure_Buffer;

   function Frame_Pending (win : Window) return Boolean is
     (win.protectedFrames and then
       (FP.Pending (win.frames) or else not CuBit.UI.Is_Empty (win.deferredDamage)));
   procedure Begin_Input_Event (win : in out Window; event : Input_Event) is
      Accepted : Boolean;
   begin
      if not win.protectedFrames or else not win.provenanceHealthy then return; end if;
      if event.serial = 0 or else event.serial > win.lastEvent then
         win.provenanceHealthy := False; return;
      end if;
      IP.Begin_Event (win.provenance, event.serial, Accepted);
      win.provenanceHealthy := Accepted;
   end Begin_Input_Event;
   procedure Finish_Input_Event (win : in out Window; event : Input_Event) is
      Accepted : Boolean;
   begin
      if not win.protectedFrames or else not win.provenanceHealthy then return; end if;
      IP.Finish_Event (win.provenance, event.serial, Accepted);
      win.provenanceHealthy := Accepted;
   end Finish_Input_Event;
   procedure Cancel_Paint (win : in out Window) is
      Ignored : Unsigned_64;
   begin
      if win.protectedFrames then FP.Cancel_Paint (win.frames); end if;
      IP.End_Paint (win.provenance, False, Ignored);
   end Cancel_Paint;

   procedure Begin_Paint
     (win : in out Window; changed : CuBit.UI.Rect;
      repair : out CuBit.UI.Rect; ready : out Boolean)
   is
      clipped : CuBit.UI.Rect;
      debt, changedDebt : FP.Debt.Box;
      Captured : Boolean;
   begin
      ready := False; repair := (others => 0);
      if not Is_Open (win) or else win.inputStopped then return; end if;
      if not win.protectedFrames then
         repair := CuBit.UI.Clamp_Rect (Canvas (win), changed);
         ready := not CuBit.UI.Is_Empty (repair);
         return;
      end if;
      clipped := CuBit.UI.Clamp_Rect (Canvas (win), changed);
      win.deferredDamage := CuBit.UI.Union_Rect (win.deferredDamage, clipped);
      Ensure_Buffer (win, win.width, win.height, ready);
      if not ready then return; end if;
      clipped := CuBit.UI.Clamp_Rect (Canvas (win), win.deferredDamage);
      changedDebt := (if CuBit.UI.Is_Empty (clipped) then FP.Debt.Empty else
        (clipped.x, clipped.y, clipped.x + clipped.w, clipped.y + clipped.h));
      FP.Begin_Paint (win.frames, changedDebt, debt, ready);
      win.deferredDamage := (others => 0);
      if ready then
         repair := (debt.Left, debt.Top, debt.Right - debt.Left, debt.Bottom - debt.Top);
         IP.Begin_Paint (win.provenance, Captured);
         win.provenanceHealthy := win.provenanceHealthy and Captured;
      end if;
   end Begin_Paint;

   procedure Refresh_Theme;

   procedure Open
      (win : in out Window;
       width, height : Natural;
       flags : Unsigned_64;
       ok : out Boolean;
       maximum_width : Natural := 0;
       maximum_height : Natural := 0;
       title : String := "Application";
       protected_frames : Boolean := False;
       batched_input : Boolean := False;
       minimum_width : Natural := 0;
       minimum_height : Natural := 0)
   is
      hello : Message;
      info : Message;
      created : Message;
      creation : DP.Creation_Result;
      limits : DP.Limits_Request;
      reply : Message;
      initialW, initialH, minW, minH : Unsigned_64;
      maxW : Unsigned_64 := 0;
      maxH : Unsigned_64 := 0;
      attached : Boolean;
      Fresh_Provenance : IP.State;
   begin
      ok := False;
      if Input_Wait_Pending (win) then return; end if;
      if Is_Open (win) then Close (win); end if;
      if Is_Open (win) then return; end if;
      FP.Reset (win.frames, attached);
      if not attached then return; end if;
      win.protectedFrames := protected_frames;
      win.sentBye := False;
      win.inputStopped := False;
      win.inputErrorReported := False;
      win.width := 0; win.height := 0; win.pitch := 0;
      win.densityNumerator := 1; win.densityDenominator := 1;
      win.lastEvent := 0; win.inputMayRemain := False;
      win.batchedInput := batched_input;
      win.inputStats := (Batch_Enabled => batched_input, others => <>);
      Client_Input_Batch_Cache.Clear (win.inputCache);
      win.deferredDamage := (others => 0);
      win.firstManagedFrame := False;
      win.provenance := Fresh_Provenance; win.provenanceHealthy := True;
      win.flags := flags;
      -- Validate the public API before adding chrome or converting into the
      -- wire's bounded geometry. Bad caller input leaves the window unopened.
      if width not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_W or else
        height not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_H or else
        minimum_width > width or else minimum_height > height or else
        maximum_width > Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_W or else
        maximum_height > Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_H or else
        flags > DP.Feature_Bits ([others => True])
      then
         return;
      end if;
      initialW := Unsigned_64 (width + WINDOW_CHROME_W);
      initialH := Unsigned_64 (height + WINDOW_CHROME_H);
      minW := (if minimum_width = 0 then initialW else
                 Unsigned_64 (minimum_width + WINDOW_CHROME_W));
      minH := (if minimum_height = 0 then initialH else
                 Unsigned_64 (minimum_height + WINDOW_CHROME_H));

      if (flags and WINDOW_FLAG_FIXED_SIZE) /= 0 then
         minW := initialW; minH := initialH;
         maxW := initialW;
         maxH := initialH;
      elsif maximum_width > 0 and then maximum_height > 0 then
         maxW := Unsigned_64 (Natural'Max (width, maximum_width) +
                              WINDOW_CHROME_W);
         maxH := Unsigned_64 (Natural'Max (height, maximum_height) +
                              WINDOW_CHROME_H);
      end if;

      hello := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Hello (DP.Current_Revision));
      hello.tag := capCall (CAP_SLOT_DESKTOP, hello, CuBit.Messages.Wait_Forever);
      if DP.Decode_Hello_Result (CuBit.Desktop_Messages.To_Wire (hello)).Status /= DP.Success then
         debugPrint ("ui-app: desktop hello failed" & LF);
         return;
      end if;

      Refresh_Theme;
      info := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Get_Information));
      info.tag := capCall (CAP_SLOT_DESKTOP, info, CuBit.Messages.Wait_Forever);
      if DP.Decode_Information_Result (CuBit.Desktop_Messages.To_Wire (info)).Status /= DP.Success then
         debugPrint ("ui-app: desktop info failed" & LF);
         return;
      end if;

      created := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Create ((DP.Pixel_Extent (initialW), DP.Pixel_Extent (initialH), DP.Window_Surface)));
      created.tag := capCall (CAP_SLOT_DESKTOP, created, CuBit.Messages.Wait_Forever);
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
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply, CuBit.Messages.Wait_Forever);
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
      request.tag := capCall (CAP_SLOT_DESKTOP, request, CuBit.Messages.Wait_Forever);
      if DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (request),
                           DP.Set_Window_Title) /= DP.Success
      then
         debugPrint ("ui-app: window title request failed" & LF);
      end if;
   end Set_Title;

   procedure Refresh_Theme is
      Response : Message;
      Candidate : Theme_Data.Palette_Colors := Theme_Data.Colors (Current_Theme);
      Revision : Unsigned_64 := 0;
      Valid : Boolean;
   begin
      for Index in Theme_Data.Chunk_Index loop
         Response := NULL_MESSAGE;
         Response.tag := (DP.Code (DP.Get_Appearance), 1, 0, 0);
         Response.words (0) := Unsigned_64 (Index);
         Response.tag := capCall (CAP_SLOT_DESKTOP, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= DP.Code (DP.Get_Appearance) or else
           Response.tag.length /= 4 or else Response.tag.flags /= 0 or else
           Response.tag.reserved /= 0 or else Response.words (0) = 0
         then return; end if;
         if Index = 0 then Revision := Response.words (0);
         elsif Revision /= Response.words (0) then return;
         end if;
         Theme_Data.Merge (Candidate, Index,
           [Response.words (1), Response.words (2), Response.words (3)], Valid);
         if not Valid then return; end if;
      end loop;
      --  Publish only a complete, same-generation snapshot. A later change
      --  already has another notification queued; never install mixed colors.
      Set_Theme (Theme_Data.To_Theme (Candidate));
   end Refresh_Theme;

   procedure Apply_Input_Result
      (win : in out Window;
       decoded : DP.Input_Result;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      event := (others => <>);
      found := False;
      if win.surfaceId = 0 or else win.inputStopped then
         return;
      end if;

      win.inputMayRemain := False;
      if decoded.Status /= DP.Success then
         if not win.inputErrorReported then
            debugPrint ("ui-app: input request or reply rejected" & LF);
            win.inputErrorReported := True;
         end if;
         if decoded.Status = DP.Bad_Object then
            -- The decoder accepts this only from a valid status response.
            -- Do not clear the surface identity or free uncertain frame loans.
            win.inputStopped := True;
            Client_Input_Batch_Cache.Clear (win.inputCache);
         end if;
         return;
      end if;
      win.inputErrorReported := False;
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
         Refresh_Theme;
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
      elsif event.kind = INPUT_RESYNC then
         Refresh_Theme;
      end if;
      found := True;
   end Apply_Input_Result;

   function Input_Wait_Pending (win : Window) return Boolean is
      use type CuBit.Async_Requests.Phase;
   begin
      return CuBit.Async_Requests.State (win.inputRequest) /= CuBit.Async_Requests.Idle;
   end Input_Wait_Pending;

   procedure Submit_Input_Wait
     (win : in out Window; token : Unsigned_64; accepted : out Boolean;
      deadline : Unsigned_64 := 0)
   is
      request : Message;
   begin
      accepted := False;
      if win.surfaceId = 0 or else win.inputStopped or else win.sentBye or else win.batchedInput or else
        not CuBit.Async_Requests.Can_Reserve (win.inputRequest, token)
      then return; end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Input_Request ((DP.Wait_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent, deadline)));
      CuBit.Async_Requests.Reserve (win.inputRequest, token, accepted);
      if not accepted then return; end if;
      accepted := capSubmit (CAP_SLOT_DESKTOP, request, token);
      CuBit.Async_Requests.Submitted (win.inputRequest, accepted);
   end Submit_Input_Wait;

   procedure Complete_Input_Wait
     (win : in out Window; receipt : CompletionEntry;
      event : out Input_Event; found, consumed, healthy : out Boolean)
   is
      decoded : DP.Input_Result;
   begin
      event := (others => <>); found := False; healthy := True;
      CuBit.Async_Requests.Capture (win.inputRequest, receipt.token, receipt.valid, consumed);
      if not consumed then return; end if;
      CuBit.Async_Requests.Release (win.inputRequest);
      healthy := receipt.status = COMPLETION_OK and not win.sentBye;
      if not healthy then return; end if;
      decoded := DP.Decode_Input_Result (CuBit.Desktop_Messages.To_Wire (receipt.msg), DP.Wait_Input);
      healthy := decoded.Status = DP.Success;
      Apply_Input_Result (win, decoded, event, found);
   end Complete_Input_Wait;

   procedure Add_Input_Count (Value : in out Unsigned_64; Amount : Unsigned_64 := 1) is
   begin
      Value := (if Amount > Unsigned_64'Last - Value then Unsigned_64'Last else Value + Amount);
   end Add_Input_Count;

   function Input_Statistics (win : Window) return Input_Diagnostics is
      Result : Input_Diagnostics := win.inputStats;
   begin
      Result.Channel_Disabled := Client_Input_Channel.Is_Disabled;
      return Result;
   end Input_Statistics;

   procedure Take_Cached_Input
     (win : in out Window; event : out Input_Event; found, valid : out Boolean)
   is
      package Cache renames Client_Input_Batch_Cache;
      Decoded : DP.Input_Result;
   begin
      event := (others => <>); found := False; valid := False;
      if win.surfaceId = 0 or else win.inputStopped or else Input_Wait_Pending (win) or else
        not win.batchedInput or else Cache.Remaining (win.inputCache) = 0
      then return; end if;
      Cache.Take (win.inputCache, win.surfaceId, win.lastEvent, Decoded);
      if Decoded.Status = DP.Success then
         valid := True;
         Add_Input_Count (win.inputStats.Delivered_Events);
         Apply_Input_Result (win, Decoded, event, found);
      else
         Add_Input_Count (win.inputStats.Cache_Rejections);
         Cache.Clear (win.inputCache);
      end if;
   end Take_Cached_Input;

   procedure Poll_Cached_Input
     (win : in out Window; event : out Input_Event; found : out Boolean)
   is
      Valid : Boolean;
   begin
      Take_Cached_Input (win, event, found, Valid);
   end Poll_Cached_Input;

   procedure Receive_Input
      (win : in out Window;
       operation : DP.Input_Operation;
       event : out Input_Event;
       found : out Boolean;
       deadline : Unsigned_64 := 0)
   is
      reply : Message;
   begin
      event := (others => <>); found := False;
      if win.surfaceId = 0 or else win.inputStopped or else Input_Wait_Pending (win) then return; end if;
      if win.batchedInput then
         declare
            package Cache renames Client_Input_Batch_Cache;
            Loaded, Valid : Boolean;
         begin
            if Cache.Remaining (win.inputCache) = 0 and then operation = DP.Poll_Input then
               Client_Input_Channel.Fetch (win.inputCache, win.surfaceId, win.lastEvent, Loaded);
               if Loaded then
                  Add_Input_Count (win.inputStats.Successful_Fetches);
                  Add_Input_Count (win.inputStats.Fetched_Events, Unsigned_64 (Cache.Remaining (win.inputCache)));
               else
                  Add_Input_Count (win.inputStats.Fallback_Polls);
               end if;
               if Loaded and then Cache.Remaining (win.inputCache) = 0 then
                  Apply_Input_Result (win, (DP.Success,
                    (DP.No_Input, win.lastEvent, 0, 0, False)), event, found);
                  return;
               end if;
            end if;
            if Cache.Remaining (win.inputCache) > 0 then
               Take_Cached_Input (win, event, found, Valid);
               if Valid then return; end if;
               -- Failed cache validation leaves the acknowledged serial intact.
               -- Ordinary polling retains its existing fallback recovery below.
            end if;
         end;
      end if;
      reply := CuBit.Desktop_Messages.From_Wire
        ((if operation = DP.Poll_Input then
            DP.Encode_Input_Request ((DP.Poll_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent))
          else DP.Encode_Input_Request ((DP.Wait_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent, deadline))));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply, CuBit.Messages.Wait_Forever);
      Apply_Input_Result (win,
        DP.Decode_Input_Result (CuBit.Desktop_Messages.To_Wire (reply), operation), event, found);
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

   function Cached_Input_Count (win : Window) return Natural is
     (Client_Input_Batch_Cache.Remaining (win.inputCache));

   function Input_May_Remain (win : Window) return Boolean is
     (win.inputMayRemain or else Client_Input_Batch_Cache.Remaining (win.inputCache) > 0);


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
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply, CuBit.Messages.Wait_Forever);
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
      (win : in out Window; damage : CuBit.UI.Rect)
   is
      request : Message;
      tag : MessageTag;
      r : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (Canvas (win), damage);
      accepted : Boolean;
      Watermark : Unsigned_64;
   begin
      if win.surfaceId = 0 or else win.inputStopped or else CuBit.UI.Is_Empty (r) then
         Cancel_Paint (win);
         return;
      end if;

      if win.protectedFrames then
         Watermark := (if win.provenanceHealthy and IP.Painting (win.provenance)
                       then IP.Frozen (win.provenance) else 0);
         FP.Publish (win.frames, (r.x, r.y, r.x + r.w, r.y + r.h), accepted, Watermark);
         IP.End_Paint (win.provenance, accepted, Watermark);
         if accepted and then not win.firstManagedFrame then
            win.firstManagedFrame := True;
            debugPrint ("ui-app: protected frame published" & LF);
         end if;
         return;
      end if;

      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Present
           ((DP.Live_Surface_Name (win.surfaceId),
             (DP.Pixel_Coordinate (r.x), DP.Pixel_Coordinate (r.y),
              DP.Pixel_Extent (r.w), DP.Pixel_Extent (r.h)))));
      tag := capCall (CAP_SLOT_DESKTOP, request, CuBit.Messages.Wait_Forever);
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

   procedure No_Completion
      (win : in out Window;
       receipt : CuBit.Messages.CompletionEntry;
       consumed : out Boolean;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean)
   is
      pragma Unreferenced (win, receipt, dirty, running);
   begin
      consumed := False;
   end No_Completion;

   --  Input wait tokens (Activity_Wait), process-wide and increasing.
   Last_Input_Token : Unsigned_64 := 0;

   procedure Run (win : in out Window)
   is
      running : Boolean := True;
      drainLimit : constant Natural := 32;
      dirtyBatchLimit : constant Natural := 4;
      pointer : Pointer_Interaction;
      pendingEvent : Input_Event;
      hasPendingEvent : Boolean := False;
      procedure Paint (Damage : CuBit.UI.Rect) is
         Repair : CuBit.UI.Rect;
         Ready : Boolean;
      begin
         Begin_Paint (win, Damage, Repair, Ready);
         if not Ready then return; end if;
         Render (win, Repair);
         if CuBit.UI.State.Followup_Render_Requested (ui) then
            Cancel_Paint (win);
            Begin_Paint (win, Full_Rect (win), Repair, Ready);
            if not Ready then return; end if;
            Render (win, Repair);
         end if;
         Present (win, Repair);
      end Paint;
   begin
      if not Is_Open (win) then
         return;
      end if;

      Paint (Full_Rect (win));

      while running and then not win.inputStopped loop
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

                  Begin_Input_Event (win, event);
                  Apply_Pointer_Event
                    (pointer, ui, controls, win, event, dirty,
                     pointerRepaint);
                  Handle_Event (win, event, dirty, running);
                  Finish_Input_Event (win, event);
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

            if not CuBit.UI.Is_Empty (dirty) or else
              (win.protectedFrames and then (FP.Pending (win.frames) or else not CuBit.UI.Is_Empty (win.deferredDamage)))
            then
               Paint (dirty);
            end if;

            if running and then not win.inputStopped and then not hasPendingEvent then
               --  Park on a deferred one-use reply capability. Reducing the
               --  old polling interval would still add avoidable latency and
               --  burn CPU; this wakes directly when input is queued. It is
               --  also safe immediately after a drained batch: already-queued
               --  input replies at once, while a later arrival resolves the
               --  installed one-use waiter.
               declare
                  appDeadline : constant Unsigned_64 := Next_Deadline;
                  now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
                  deadline : constant Unsigned_64 := Client_Frame_Wakeup.Deadline
                    (now, appDeadline, win.protectedFrames and then (FP.Pending (win.frames) or else not CuBit.UI.Is_Empty (win.deferredDamage)));
                  timerDirty : CuBit.UI.Rect := (others => 0);
               begin
                  if appDeadline /= 0 and then now >= appDeadline then
                     On_Deadline (win, timerDirty, running);
                     if running and then not CuBit.UI.Is_Empty (timerDirty) then Paint (timerDirty); end if;
                  elsif Activity_Wait and then (Input_Wait_Pending (win) or else not win.batchedInput) then
                     --  One wait for input, other completions and the
                     --  deadline; no blocking call.
                     declare
                        accepted, consumed, found, healthy : Boolean;
                        activity : Activity_Result;
                        receipt : CompletionEntry;
                        event : Input_Event;
                        pragma Unreferenced (activity);
                     begin
                        if not Input_Wait_Pending (win) and then Last_Input_Token < APPLICATION_TOKEN_FIRST - 1 then
                           Last_Input_Token := Last_Input_Token + 1;
                           Submit_Input_Wait (win, Last_Input_Token, accepted);
                        end if;
                        activity := Wait_For_Activity_Until (if deadline = 0 then Unsigned_64'Last else deadline);
                        while Poll_Completion (receipt'Address) /= 0 loop
                           Complete_Input_Wait (win, receipt, event, found, consumed, healthy);
                           if consumed then
                              if not healthy then
                                 win.inputStopped := True;
                              elsif found then
                                 pendingEvent := event;
                                 hasPendingEvent := True;
                              end if;
                           else
                              On_Completion (win, receipt, consumed, timerDirty, running);
                           end if;
                        end loop;
                        if win.inputStopped then
                           running := False;
                        elsif not hasPendingEvent and then appDeadline /= 0 and then
                          syscall (SYSCALL_GETTIME) >= appDeadline
                        then
                           On_Deadline (win, timerDirty, running);
                        end if;
                        if running and then not CuBit.UI.Is_Empty (timerDirty) then Paint (timerDirty); end if;
                     end;
                  elsif deadline = 0 then
                     Wait_Input (win, pendingEvent, hasPendingEvent);
                     if not hasPendingEvent then running := False; end if;
                  else
                     Wait_Input_Until (win, deadline, pendingEvent, hasPendingEvent);
                     if win.inputStopped then
                        running := False;
                     elsif not hasPendingEvent and then appDeadline /= 0 and then
                       syscall (SYSCALL_GETTIME) >= appDeadline
                     then
                        On_Deadline (win, timerDirty, running);
                        if running and then not CuBit.UI.Is_Empty (timerDirty) then Paint (timerDirty); end if;
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Run;

   procedure Close (win : in out Window) is
      reply : Message;
      revoked : Boolean;
   begin
      Cancel_Paint (win);
      if win.sentBye then
         if win.protectedFrames then FP.Close (win.frames, revoked); end if;
         return;
      end if;

      -- Close this surface only; sibling windows share the process endpoint.
      -- Failure retains the window and every uncertain frame loan for retry.
      if win.surfaceId = 0 then return; end if;
      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Destroy ((Surface => DP.Surface_Name (win.surfaceId))));
      reply.tag := capCall (CAP_SLOT_DESKTOP, reply, CuBit.Messages.Wait_Forever);
      if DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (reply), DP.Destroy_Surface) /= DP.Success then
         debugPrint ("ui-app: surface destroy failed; retaining window state" & LF);
         return;
      end if;
      if win.protectedFrames then FP.Close (win.frames, revoked);
      elsif win.bufferPages /= 0 then MG.Revoke (win.bufferGrant, revoked);
      end if;
      win.bufferPages := 0;
      win.bufferAddr := System.Null_Address;
      win.surfaceId := 0;
      win.sentBye := True;
      win.batchedInput := False;
      Client_Input_Batch_Cache.Clear (win.inputCache);
   end Close;
end CuBit.UI.App;
