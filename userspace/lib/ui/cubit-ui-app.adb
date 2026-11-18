------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Minimal desktop-surface harness for native CuBit UI applications
------------------------------------------------------------------------------
with CuBit.Readiness;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Control_Events;
with CuBit.Process_Events;
with CuBit.Process_IDs;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Memory_Grants;
with CuBit.UI.Theme_Data;
with CuBit.UI.Window_Chrome;
with CuBit.Deadlines;
with CuBit.Channels;
with CuBit.Channel_Protocol;
with CuBit.Published_Clock;
with Client_Frame_Wakeup;
with UI_Desktop_Service;
with CuBit.Log;
with CuBit.Handles;
with CuBit.Monotonic;

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
   WINDOW_CHROME_H : constant Natural := CuBit.UI.Window_Chrome.Client_Top;

   package Ring renames Client_Surface_Ring;
   package Wire renames Compositor_Surface_Wire;
   use type Wire.Request_Kind;
   use type Ring.Submit_Result;
   --  Opening a window's ring is one call to Desktop, at most this long.
   RING_OPEN_LIMIT_MS : constant := 2_000;
   --  How long Open waits for each of Desktop's answers. A window asked for
   --  while Desktop is still starting is answered when its start is done
   --  (LOG-011): the wait is for that answer, not a retry.
   OPEN_ANSWER_LIMIT_MS : constant := 30_000;

   function Align_Up_Page (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end Align_Up_Page;

   function Input_Stopped (win : Window) return Boolean is (win.inputStopped);

   --  A window the toolkit stops, or whose input stalls, without the user
   --  closing it says why (UI-039), through the program's logstore binding
   --  (CuBit.Log, echoed to the debug console) so `logs` shows it.
   type Stop_Reason is
     (Reply_Failed,          --  the input wait completed with a failure status
      Reply_Rejected,        --  its reply did not decode as a Wait_Input result
      Surface_Ended,         --  Desktop answered Bad_Object: the surface is gone
      Ring_Ended,            --  the window's surface ring is no longer open
      Activity_Unavailable,  --  the process's activity wait failed
      Wait_Not_Reserved,     --  no input wait could be reserved (one is pending)
      Wait_Not_Submitted);   --  the input request could not be submitted

   function Completion_Image (Status : Unsigned_64) return String is
     (case Status is
        when COMPLETION_OK => "ok",
        when COMPLETION_TARGET_DIED => "target-died",
        when COMPLETION_CANCELLED => "cancelled",
        when COMPLETION_QUEUE_OVERFLOW => "queue-overflow",
        when others => "code" & Unsigned_64'Image (Status));

   procedure Report_Stop
     (win : in out Window; Reason : Stop_Reason; Detail : String := "")
   is
      Window_Text : constant String := "ui-app: window" & Unsigned_64'Image (win.surfaceId);
      Tail : constant String := (if Detail = "" then "" else " " & Detail);
   begin
      --  A stall repeats on every loop turn: report it once until a wait
      --  is submitted again.
      if Reason in Wait_Not_Reserved | Wait_Not_Submitted then
         if win.inputStallReported then return; end if;
         win.inputStallReported := True;
      end if;
      case Reason is
         when Reply_Failed =>
            CuBit.Log.Warning (Window_Text & " stopped: input reply failed" & Tail);
         when Reply_Rejected =>
            CuBit.Log.Warning (Window_Text & " stopped: input reply rejected" & Tail);
         when Surface_Ended =>
            CuBit.Log.Warning (Window_Text & " stopped: Desktop ended the surface" & Tail);
         when Ring_Ended =>
            CuBit.Log.Warning (Window_Text & " stopped: surface ring ended" & Tail);
         when Activity_Unavailable =>
            CuBit.Log.Warning (Window_Text & " stopped: activity wait unavailable" & Tail);
         when Wait_Not_Reserved =>
            CuBit.Log.Warning (Window_Text & " input stalled: wait not reserved" & Tail);
         when Wait_Not_Submitted =>
            CuBit.Log.Warning (Window_Text & " input stalled: wait not submitted" & Tail);
      end case;
   end Report_Stop;

   Wake_Trace : Wake_Record;
   Publish_Trace : Publish_Record;
   function Last_Wake return Wake_Record is (Wake_Trace);
   function Last_Publish return Publish_Record is (Publish_Trace);

   function Image (Item : Open_Result) return String is
     (case Item is
        when Opened => "opened",
        when Invalid_Request => "invalid request (size, minima or flags)",
        when Window_Busy => "the window is still waiting or closing",
        when Desktop_Unavailable => "no desktop to ask (no endpoint, or no answer)",
        when Desktop_Refused => "the desktop refused the hello or its information",
        when Surface_Refused => "the desktop refused the surface",
        when Limits_Refused => "the desktop refused the window's limits",
        when Buffer_Not_Attached => "its frame buffers were not attached",
        when Ring_Not_Opened => "its surface ring did not open");

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
         if not win.configStale then
            ok := FP.Configuration (win.frames).Status = DP.Success;
            return;
         end if;
         FP.Configure (win.frames, DP.Live_Surface_Name (win.surfaceId), ok);
         win.configStale := not ok;
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
         MG.Create_Via_Capability (UI_Desktop_Service.Endpoint, candidateAddr, Natural (pages), False,
                                   candidateGrant, created);
         if not created then return; end if;
         replacing := True;
      end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Attachment ((DP.Live_Surface_Name (win.surfaceId), candidateGrant,
           (DP.Positive_Extent (width), DP.Positive_Extent (height), width * 4))));
      request.tag := capCall (UI_Desktop_Service.Endpoint, request, CuBit.Messages.Wait_Forever);
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
     ((win.protectedFrames and then
        (FP.Pending (win.frames) or else not CuBit.UI.Is_Empty (win.deferredDamage)))
      or else not CuBit.UI.Is_Empty (win.presentBacklog));

   procedure Ring_Pump (win : in out Window);
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
      Ring_Pump (win);
      Ensure_Buffer (win, win.width, win.height, ready);
      if not ready then return; end if;
      clipped := CuBit.UI.Clamp_Rect (Canvas (win), win.deferredDamage);
      changedDebt := (if CuBit.UI.Is_Empty (clipped) then FP.Debt.Empty else
        (clipped.x, clipped.y, clipped.x + clipped.w, clipped.y + clipped.h));
      FP.Begin_Paint (win.frames, win.ring, changedDebt, debt, ready);
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
       Result : out Open_Result;
       title : String := "Application";
       protected_frames : Boolean := False;
       minimum_width : Natural := 0;
       minimum_height : Natural := 0;
       preferred_size : Boolean := False)
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
      procedure Fail (Why : Open_Result) is
      begin
         Result := Why;
         debugPrint ("ui-app: window '" & title & "' not opened: " & Image (Why) & LF);
         CuBit.Log.Warning ("ui-app: window '" & title & "' not opened: " & Image (Why));
      end Fail;
   begin
      Result := Window_Busy;
      if Input_Wait_Pending (win) then Fail (Window_Busy); return; end if;
      if Is_Open (win) then Close (win); end if;
      if Is_Open (win) then Fail (Window_Busy); return; end if;
      FP.Reset (win.frames, attached);
      if not attached then Fail (Window_Busy); return; end if;
      win.protectedFrames := protected_frames;
      win.sentBye := False;
      win.inputStopped := False;
      win.inputErrorReported := False;
      win.width := 0; win.height := 0; win.pitch := 0;
      win.densityNumerator := 1; win.densityDenominator := 1;
      win.lastEvent := 0; win.inputMayRemain := False;
      win.inputStats := (Batch_Enabled => True, others => <>);
      win.ringCount := 0; win.ringNext := 0; win.ringMore := False; win.ringAcked := 0;
      win.inputGeneration := 0; win.inputRequested := False;
      win.presentBacklog := (others => 0);
      win.configStale := True;
      win.deferredDamage := (others => 0);
      win.firstManagedFrame := False;
      win.provenance := Fresh_Provenance; win.provenanceHealthy := True;
      win.flags := flags;
      -- Validate the public API before adding chrome or converting into the
      -- wire's bounded geometry. Bad caller input leaves the window unopened.
      if width not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_W or else
        height not in 1 .. Natural (DP.Pixel_Extent'Last) - WINDOW_CHROME_H or else
        minimum_width > width or else minimum_height > height or else
        flags > DP.Feature_Bits ([others => True])
      then
         Fail (Invalid_Request);
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
      end if;

      hello := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Hello (DP.Current_Revision));
      if not CuBit.Handles.Valid (UI_Desktop_Service.Endpoint) then
         Fail (Desktop_Unavailable);
         return;
      end if;
      hello.tag := capCall (UI_Desktop_Service.Endpoint, hello, CuBit.Messages.Deadline_After (OPEN_ANSWER_LIMIT_MS));
      if hello.tag.label = CuBit.Messages.REPLY_TIMEOUT then
         Fail (Desktop_Unavailable);
         return;
      end if;
      if DP.Decode_Hello_Result (CuBit.Desktop_Messages.To_Wire (hello)).Status /= DP.Success then
         Fail (Desktop_Refused);
         return;
      end if;

      Refresh_Theme;
      info := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Get_Information));
      info.tag := capCall (UI_Desktop_Service.Endpoint, info, CuBit.Messages.Deadline_After (OPEN_ANSWER_LIMIT_MS));
      if info.tag.label = CuBit.Messages.REPLY_TIMEOUT then
         Fail (Desktop_Unavailable);
         return;
      end if;
      if DP.Decode_Information_Result (CuBit.Desktop_Messages.To_Wire (info)).Status /= DP.Success then
         Fail (Desktop_Refused);
         return;
      end if;

      created := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Create ((DP.Pixel_Extent (initialW), DP.Pixel_Extent (initialH), DP.Window_Surface,
                            (if preferred_size then DP.Preferred_Size else DP.Desktop_Sized))));
      created.tag := capCall (UI_Desktop_Service.Endpoint, created, CuBit.Messages.Deadline_After (OPEN_ANSWER_LIMIT_MS));
      if created.tag.label = CuBit.Messages.REPLY_TIMEOUT then
         Fail (Desktop_Unavailable);
         return;
      end if;
      creation := DP.Decode_Creation_Result (CuBit.Desktop_Messages.To_Wire (created));
      if creation.Status /= DP.Success then
         Close (win);
         Fail (Surface_Refused);
         return;
      end if;
      win.surfaceId := Unsigned_64 (creation.Surface);

      win.maximumW := maxW;
      win.maximumH := maxH;
      limits.Surface := creation.Surface;
      limits.Bounds :=
        (DP.Pixel_Extent (minW), DP.Pixel_Extent (minH),
         DP.Pixel_Extent (maxW), DP.Pixel_Extent (maxH));
      for feature in DP.Window_Feature loop
         limits.Features (feature) :=
           (flags and DP.Window_Feature'Enum_Rep (feature)) /= 0;
      end loop;
      reply := CuBit.Desktop_Messages.From_Wire (DP.Encode_Limits (limits));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Deadline_After (OPEN_ANSWER_LIMIT_MS));
      if DP.Decode_Limits_Result
        (CuBit.Desktop_Messages.To_Wire (reply)).Status /= DP.Success
      then
         Close (win);
         Fail (Limits_Refused);
         return;
      end if;

      Set_Title (win, title);

      Ensure_Buffer (win, width, height, attached);
      if not attached then
         Close (win);
         Fail (Buffer_Not_Attached);
         return;
      end if;

      --  The window's ring: from here on, frames and input take no call.
      --  There is no synchronous fallback: without its ring the window
      --  does not open, and says why.
      declare
         use type CuBit.Channels.Open_Result;
         Result : CuBit.Channels.Open_Result;
         Refusal : CuBit.Channel_Protocol.Open_Refusal;
      begin
         Ring.Open (win.ring, UI_Desktop_Service.Endpoint,
                    CuBit.Deadlines.At_Monotonic
                      (CuBit.Published_Clock.Milliseconds + RING_OPEN_LIMIT_MS), attached,
                    Result, Refusal);
         if not attached then
            CuBit.Log.Warning
              ("ui-app: window" & Unsigned_64'Image (win.surfaceId) & " not opened: surface ring " &
               (case Result is
                  when CuBit.Channels.Refused =>
                    "refused reason=" & CuBit.Channel_Protocol.Open_Refusal'Image (Refusal),
                  when CuBit.Channels.Failed => "open failed (no answer by" &
                    Natural'Image (RING_OPEN_LIMIT_MS) & " ms, or no grant)",
                  when CuBit.Channels.Opened => "attach failed"));
            Close (win);
            Fail (Ring_Not_Opened);
            return;
         end if;
      end;

      Result := Opened;
   end Open;

   procedure Request_Size
      (win : in out Window;
       width, height : Natural;
       minimum_width, minimum_height : Natural;
       ok : out Boolean)
   is
      limits : DP.Limits_Request;
      reply : Message;
      resized : DP.Resize_Result;
      chromeW : constant Natural := Horizontal_Chrome (win);
      chromeH : constant Natural := Vertical_Chrome (win);
   begin
      ok := False;
      if win.surfaceId = 0 or else
        width not in 1 .. Natural (DP.Pixel_Extent'Last) - chromeW or else
        height not in 1 .. Natural (DP.Pixel_Extent'Last) - chromeH or else
        minimum_width > width or else minimum_height > height or else
        (win.maximumW > 0 and then Unsigned_64 (width + chromeW) > win.maximumW) or else
        (win.maximumH > 0 and then Unsigned_64 (height + chromeH) > win.maximumH)
      then
         return;
      end if;
      limits.Surface := DP.Live_Surface_Name (win.surfaceId);
      limits.Bounds :=
        (DP.Pixel_Extent (minimum_width + chromeW), DP.Pixel_Extent (minimum_height + chromeH),
         DP.Pixel_Extent (win.maximumW), DP.Pixel_Extent (win.maximumH));
      for feature in DP.Window_Feature loop
         limits.Features (feature) :=
           (win.flags and DP.Window_Feature'Enum_Rep (feature)) /= 0;
      end loop;
      reply := CuBit.Desktop_Messages.From_Wire (DP.Encode_Limits (limits));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
      if DP.Decode_Limits_Result
        (CuBit.Desktop_Messages.To_Wire (reply)).Status /= DP.Success
      then
         debugPrint ("ui-app: window limits failed" & LF);
         return;
      end if;
      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Resize ((DP.Live_Surface_Name (win.surfaceId),
                            DP.Pixel_Extent (width + chromeW), DP.Pixel_Extent (height + chromeH))));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
      resized := DP.Decode_Resize_Result (CuBit.Desktop_Messages.To_Wire (reply));
      ok := resized.Status = DP.Success;
   end Request_Size;

   procedure Set_Title (win : Window; title : String) is
      request : Message;
   begin
      if win.surfaceId = 0 then
         return;
      end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Title
           ((DP.Live_Surface_Name (win.surfaceId), DP.Make_Title (title))));
      request.tag := capCall (UI_Desktop_Service.Endpoint, request, CuBit.Messages.Wait_Forever);
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
         Response.tag := (label => DP.Code (DP.Get_Appearance), length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         Response.words (0) := Unsigned_64 (Index);
         Response.tag := capCall (UI_Desktop_Service.Endpoint, Response, CuBit.Messages.Wait_Forever);
         if Response.tag.label /= DP.Code (DP.Get_Appearance) or else
           Response.tag.length /= 4 or else Response.tag.protocol /= 0 or else
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
            Report_Stop (win, Surface_Ended, "verdict=" & DP.Status_Code'Image (decoded.Status));
            win.inputStopped := True;
            win.ringCount := 0; win.ringNext := 0;
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
         win.configStale := True;
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
         win.configStale := True;
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
      if win.surfaceId = 0 or else win.inputStopped or else win.sentBye or else
        Ring.Outstanding (win.ring, Wire.Receive_Input) > 0 or else win.ringNext < win.ringCount
      then return; end if;
      if not CuBit.Async_Requests.Can_Reserve (win.inputRequest, token) then
         Report_Stop (win, Wait_Not_Reserved, "token=" & Unsigned_64'Image (token));
         return;
      end if;
      request := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Input_Request ((DP.Wait_Input, DP.Live_Surface_Name (win.surfaceId), win.lastEvent, deadline)));
      CuBit.Async_Requests.Reserve (win.inputRequest, token, accepted);
      if not accepted then
         Report_Stop (win, Wait_Not_Reserved, "token=" & Unsigned_64'Image (token));
         return;
      end if;
      accepted := capSubmit (UI_Desktop_Service.Endpoint, request, token);
      if accepted then
         win.inputStallReported := False;
      else
         Report_Stop (win, Wait_Not_Submitted);
      end if;
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
      --  After Close the window ended by the program's own choice: no report.
      if win.sentBye then healthy := False; return; end if;
      healthy := receipt.status = COMPLETION_OK;
      if not healthy then
         Report_Stop (win, Reply_Failed, "status=" & Completion_Image (receipt.status));
         return;
      end if;
      decoded := DP.Decode_Input_Result (CuBit.Desktop_Messages.To_Wire (receipt.msg), DP.Wait_Input);
      healthy := decoded.Status = DP.Success;
      --  Bad_Object is reported by Apply_Input_Result, which stops input.
      if not healthy and then decoded.Status /= DP.Bad_Object then
         Report_Stop (win, Reply_Rejected, "status=ok verdict=" & DP.Status_Code'Image (decoded.Status) &
           " label=" & Unsigned_32'Image (receipt.msg.tag.label));
      end if;
      Apply_Input_Result (win, decoded, event, found);
   end Complete_Input_Wait;

   procedure Add_Input_Count (Value : in out Unsigned_64; Amount : Unsigned_64 := 1) is
   begin
      Value := (if Amount > Unsigned_64'Last - Value then Unsigned_64'Last else Value + Amount);
   end Add_Input_Count;

   function Input_Statistics (win : Window) return Input_Diagnostics is
      Result : Input_Diagnostics := win.inputStats;
   begin
      Result.Channel_Disabled := not Ring.Is_Open (win.ring);
      Result.Cache_Rejections := Unsigned_64'Max (Result.Cache_Rejections, Ring.Stray_Answers (win.ring));
      return Result;
   end Input_Statistics;

   --  Count, saturating.
   procedure Count (Value : in out Unsigned_64; Amount : Unsigned_64 := 1) renames Add_Input_Count;

   --  Every answer the ring holds: input into the window's event queue,
   --  frame answers to the frame pair; then a backlogged attached present
   --  if a frame slot is free. Never blocks; no system call unless Desktop
   --  sleeps and a request is submitted.
   procedure Ring_Pump (win : in out Window) is
      Tag : Ring.Token;
      Answer : Wire.Result;
      Got, Matched, Refused : Boolean;
      Result : Ring.Submit_Result;
      Ignore : Ring.Token;
      r : CuBit.UI.Rect;
   begin
      if not Ring.Is_Open (win.ring) then
         return;
      end if;
      loop
         Ring.Reap (win.ring, Tag, Answer, Got);
         exit when not Got;
         case Answer.Kind is
            when Wire.Receive_Input =>
               if Answer.Status = DP.Success and then win.ringNext >= win.ringCount then
                  Count (win.inputStats.Successful_Fetches);
                  Count (win.inputStats.Fetched_Events, Unsigned_64 (Answer.Count));
                  win.ringEvents := Answer.Events;
                  win.ringCount := Answer.Count;
                  win.ringNext := 0;
                  win.ringMore := Answer.More;
                  win.ringAcked := Answer.Events (Answer.Count).Serial;
               elsif Answer.Status = DP.Success then
                  --  Never asked for while events wait: dropped.
                  Count (win.inputStats.Cache_Rejections);
               else
                  declare
                     Ignore_Event : Input_Event;
                     Ignore_Found : Boolean;
                  begin
                     --  Bad_Object (the surface is gone) stops the window's
                     --  input; any other refusal is reported once.
                     Apply_Input_Result
                       (win, (if Answer.Status = DP.Bad_Object then (Status => DP.Bad_Object)
                              else (Status => DP.Invalid_Request)), Ignore_Event, Ignore_Found);
                  end;
               end if;
            when Wire.Present =>
               null;
            when Wire.Publish | Wire.Await_Retirement =>
               FP.Note_Answer (win.frames, win.ring, Tag, Answer, Matched, Refused);
               if Refused then
                  --  Perhaps a configuration this window has not seen yet.
                  win.configStale := True;
               end if;
         end case;
      end loop;
      if not CuBit.UI.Is_Empty (win.presentBacklog) and then Ring.Can_Submit (win.ring, Wire.Present) then
         r := win.presentBacklog;
         Ring.Submit (win.ring,
           (Kind => Wire.Present, Surface => DP.Live_Surface_Name (win.surfaceId),
            Area => (DP.Pixel_Coordinate (r.x), DP.Pixel_Coordinate (r.y),
                     DP.Pixel_Extent (r.w), DP.Pixel_Extent (r.h)),
            Input_After => win.lastEvent), Ignore, Result);
         if Result = Ring.Queued then
            win.presentBacklog := (others => 0);
         end if;
      end if;
   end Ring_Pump;

   --  Take one received event (More: others follow, here or at Desktop).
   procedure Take_Ring_Input
     (win : in out Window; event : out Input_Event; found : out Boolean) is
   begin
      event := (others => <>); found := False;
      if win.ringNext >= win.ringCount then return; end if;
      win.ringNext := win.ringNext + 1;
      Count (win.inputStats.Delivered_Events);
      declare
         Item : DP.Input_Envelope := win.ringEvents (win.ringNext);
      begin
         Item.More_Pending := win.ringNext < win.ringCount or else win.ringMore;
         Apply_Input_Result (win, (DP.Success, Item), event, found);
      end;
   end Take_Ring_Input;

   --  Keep one input request outstanding while nothing waits here.
   procedure Request_Ring_Input (win : in out Window) is
      Result : Ring.Submit_Result;
      Ignore : Ring.Token;
   begin
      if win.surfaceId = 0 or else win.inputStopped or else win.ringNext < win.ringCount or else
        Input_Wait_Pending (win) or else Ring.Outstanding (win.ring, Wire.Receive_Input) > 0
      then return; end if;
      --  Parking the next request needs no wake: Desktop answers it from
      --  input yet to come, and that input wakes Desktop. Unless Desktop
      --  already holds input for this window (its generation moved since
      --  the last request) or said more was pending: then it must hear now.
      Ring.Submit (win.ring, (Kind => Wire.Receive_Input, Surface => DP.Live_Surface_Name (win.surfaceId),
                              After => win.ringAcked), Ignore, Result, Wake => False);
      if Result = Ring.Queued then
         declare
            Generation : constant Interfaces.Unsigned_32 := Ring.Input_Generation (win.ring);
         begin
            if not win.inputRequested or else Generation /= win.inputGeneration or else win.ringMore then
               Ring.Owe_Skipped_Wake (win.ring);
            end if;
            win.inputGeneration := Generation;
            win.inputRequested := True;
         end;
      end if;
      case Result is
         when Ring.Queued => win.inputStallReported := False;
         when Ring.Full => Report_Stop (win, Wait_Not_Submitted, "ring=full");
         when Ring.Ended => Report_Stop (win, Wait_Not_Submitted, "ring=ended");
      end case;
   end Request_Ring_Input;

   procedure Poll_Cached_Input
     (win : in out Window; event : out Input_Event; found : out Boolean) is
   begin
      event := (others => <>); found := False;
      if win.surfaceId = 0 or else win.inputStopped then return; end if;
      Take_Ring_Input (win, event, found);
   end Poll_Cached_Input;

   --  Poll_Input without the doorbell: Run wakes Desktop once a frame
   --  (Present) or before it sleeps (Ring.Arm), not per request.
   procedure Poll_Ring_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      event := (others => <>); found := False;
      if win.surfaceId = 0 or else win.inputStopped or else Input_Wait_Pending (win) then return; end if;
      Ring_Pump (win);
      Take_Ring_Input (win, event, found);
      Request_Ring_Input (win);
   end Poll_Ring_Input;

   procedure Poll_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Poll_Ring_Input (win, event, found);
      Ring.Flush (win.ring);
   end Poll_Input;

   --  Kernel events (a Stop, grant notices) are taken after every wake
   --  (Sleep_For_Activity); a loop that does not sleep looks every
   --  EVENT_POLL_MS, not every turn (each look is a system call).
   EVENT_POLL_MS : constant := 50;
   --  KERN-013: the completions this loop consumed, against the kernel's
   --  readiness page (CuBit.Readiness); events have their one reader.
   Completions_Read : CuBit.Readiness.Tracker;
   Events_Polled_At : Unsigned_64 := 0;

   --  Sleep until this process has activity (a ring answer Desktop rang
   --  for, a completion, a message) or Deadline (a kernel word). Armed
   --  first: an answer that arrived meanwhile returns at once. One system
   --  call per wake; a kernel notice (the ring's wake) comes back with it.
   procedure Sleep_For_Activity (win : in out Window; Deadline : Unsigned_64; Woke : out Wake_Result) is
      Notice : aliased Message;
      Slept : constant Unsigned_64 := (if Trace_Wakes then CuBit.Monotonic.Microseconds else 0);
   begin
      if not Ring.Arm (win.ring) then
         Woke := Work_Available;
         if Trace_Wakes then
            Wake_Trace := (CuBit.UI.App.Answer_Waiting, Slept, Slept,
                           (if Deadline = Wait_Forever then 0 else Deadline));
         end if;
         return;
      end if;
      Wait_For_Activity_Or_Event (Deadline, Notice, Woke);
      if Trace_Wakes then
         Wake_Trace := ((case Woke is
                           when Event_Taken => CuBit.UI.App.Notice,
                           when Work_Available => CuBit.UI.App.Work,
                           when Deadline_Reached => CuBit.UI.App.Deadline,
                           when CuBit.Messages.Unavailable => CuBit.UI.App.Unavailable),
                        Slept, CuBit.Monotonic.Microseconds,
                        (if Deadline = Wait_Forever then 0 else Deadline));
      end if;
      Ring.Disarm (win.ring);
      Events_Polled_At := CuBit.Published_Clock.Milliseconds;
      case Woke is
         when Event_Taken =>
            if CuBit.Process_Events.Has_Room then
               CuBit.Process_Events.File (Notice);
            end if;
         when Work_Available =>
            --  Kernel notices and events go to their one reader.
            CuBit.Process_Events.Poll;
         when Deadline_Reached | Unavailable =>
            null;
      end case;
   end Sleep_For_Activity;

   procedure Wait_Input_Until
      (win : in out Window;
       deadline : Unsigned_64;
       event : out Input_Event;
       found : out Boolean)
   is
      Woke : Wake_Result;
   begin
      loop
         Poll_Input (win, event, found);
         exit when found or else win.surfaceId = 0 or else win.inputStopped or else
           Input_Wait_Pending (win) or else not Ring.Is_Open (win.ring);
         exit when deadline /= 0 and then CuBit.Published_Clock.Milliseconds >= deadline;
         Sleep_For_Activity (win, (if deadline = 0 then Wait_Forever else deadline), Woke);
         if Woke = Unavailable then
            Report_Stop (win, Activity_Unavailable);
            exit;
         end if;
      end loop;
   end Wait_Input_Until;

   procedure Wait_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean)
   is
   begin
      Wait_Input_Until (win, 0, event, found);
   end Wait_Input;

   function Cached_Input_Count (win : Window) return Natural is
     (win.ringCount - win.ringNext);

   function Input_May_Remain (win : Window) return Boolean is
     (win.inputMayRemain or else win.ringNext < win.ringCount);

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
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
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
         FP.Publish (win.frames, win.ring, (r.x, r.y, r.x + r.w, r.y + r.h), accepted, Watermark);
         Ring.Flush (win.ring);
         if Trace_Wakes then
            Publish_Trace := (CuBit.Monotonic.Microseconds,
                              Unsigned_64 (r.w) * Unsigned_64 (r.h), accepted);
         end if;
         IP.End_Paint (win.provenance, accepted, Watermark);
         if accepted and then not win.firstManagedFrame then
            win.firstManagedFrame := True;
            debugPrint ("ui-app: protected frame published" & LF);
         end if;
         return;
      end if;

      --  The attached buffer: one Present in the ring (answered once Desktop
      --  has drawn it). With every frame slot taken, the damage waits and
      --  goes with the next free slot (Ring_Pump): never an unbounded queue.
      win.presentBacklog := CuBit.UI.Union_Rect (win.presentBacklog, r);
      Ring_Pump (win);
      --  The frame is complete: one wake for all it queued, if Desktop sleeps.
      Ring.Flush (win.ring);
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


   procedure Run (win : in out Window)
   is
      running : Boolean := True;
      drainLimit : constant Natural := 32;
      dirtyBatchLimit : constant Natural := 4;
      pointer : Pointer_Interaction;
      pendingEvent : Input_Event;
      hasPendingEvent : Boolean := False;
      --  Run stopped the window for a reason it reported (Report_Stop).
      abnormal : Boolean := False;
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
      --  procmgr's Stop (PWR-003): the kernel's control event becomes a
      --  System_Shutdown close request, after the app's recovery hook.
      --  Grant events taken on the way are filed back (none is lost).
      procedure Take_Stop is
         Item : CuBit.Control_Events.Event;
         Found : Boolean;
         Stop_Seen : Boolean := False;
         Kept : array (1 .. CuBit.Process_Events.Kept_Events) of CuBit.Control_Events.Event;
         Kept_Count : Natural := 0;
         dirty : CuBit.UI.Rect := (others => 0);
         use type CuBit.Control_Events.Event_Kind, CuBit.Control_Events.Control_Kind;
         Now : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
      begin
         if Now < Events_Polled_At or else Now - Events_Polled_At >= EVENT_POLL_MS then
            CuBit.Process_Events.Poll;
            Events_Polled_At := Now;
         end if;
         loop
            CuBit.Process_Events.Next_Filed (Item, Found);
            exit when not Found;
            if Item.Kind = CuBit.Control_Events.Control then
               Stop_Seen := Stop_Seen or else Item.Control = CuBit.Control_Events.Stop;
            elsif Item.Kind in CuBit.Control_Events.Grant_Revoked | CuBit.Control_Events.Grant_Returned then
               --  The window's ring ending (Desktop closed it, or Desktop
               --  is gone) is the toolkit's; every other grant event is
               --  filed back for the program.
               declare
                  Ring_Ends : Boolean;
               begin
                  Ring.Note_Event (win.ring, Item, Ring_Ends);
                  if not Ring_Ends and then Kept_Count < Kept'Last then
                     Kept_Count := Kept_Count + 1;
                     Kept (Kept_Count) := Item;
                  end if;
               end;
            end if;
         end loop;
         for K in 1 .. Kept_Count loop
            exit when not CuBit.Process_Events.Has_Room;
            declare
               M : Message := NULL_MESSAGE;
            begin
               M.tag := (label => (if Kept (K).Kind = CuBit.Control_Events.Grant_Revoked
                                   then CuBit.Control_Events.Grant_Revoked_Label
                                   else CuBit.Control_Events.Grant_Returned_Label),
                         length => CuBit.Control_Events.Grant_Event_Words,
                         protocol => 0, ipc => <>, reserved => 0);
               M.words (0) := Kept (K).Slot;
               M.words (1) := Kept (K).Generation;
               M.words (2) := CuBit.Process_IDs.To_Word (Kept (K).Peer);
               CuBit.Process_Events.File (M);
            end;
         end loop;
         if Stop_Seen and then running then
            debugPrint ("ui-app: Stop received: closing (System_Shutdown)" & LF);
            Save_Recovery (win, CuBit.UI.Input.System_Shutdown);
            Handle_Event (win, CuBit.UI.Input.Close_Event (CuBit.UI.Input.System_Shutdown), dirty, running);
            --  Shutdown never waits on an app: it ends whatever it chose.
            running := False;
         end if;
      end Take_Stop;
   begin
      if not Is_Open (win) then
         return;
      end if;

      Paint (Full_Rect (win));

      while running and then not win.inputStopped loop
         Take_Stop;
         exit when not running;
         if not Ring.Is_Open (win.ring) then
            Report_Stop (win, Ring_Ended);
            abnormal := True;
            exit;
         end if;
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
                     Poll_Ring_Input (win, event, found);
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

            --  Events already received are taken first: sleeping with some
            --  still here would wait for an answer that is not coming.
            if running and then not win.inputStopped and then not hasPendingEvent and then
              Cached_Input_Count (win) = 0
            then
               --  Sleep until the ring answers (input, a frame's receipt or
               --  retirement), a completion of the program's own, or its
               --  deadline. Input already queued at Desktop answers the
               --  request at once; a busy window never sleeps and so never
               --  makes a call for its frames or input.
               declare
                  appDeadline : constant Unsigned_64 := Next_Deadline;
                  now : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
                  --  Back pressure: a frame waits for Desktop to let go of
                  --  a buffer (its answer is coming through the ring).
                  --  Timed work that is due only adds damage meanwhile, so
                  --  the loop sleeps for that answer instead of spinning.
                  blocked : constant Boolean := Frame_Pending (win) and then FP.Awaiting (win.frames);
                  --  A frame waiting on nothing the ring will answer (no
                  --  slot was free to ask) is retried shortly.
                  deadline : constant Unsigned_64 :=
                    (if blocked then 0
                     else Client_Frame_Wakeup.Deadline
                       (now, appDeadline,
                        Frame_Pending (win) and then Ring.Outstanding (win.ring, Wire.Present) = 0));
                  timerDirty : CuBit.UI.Rect := (others => 0);
                  woke : Wake_Result;
                  consumed : Boolean;
                  receipt : CompletionEntry;
               begin
                  if appDeadline /= 0 and then now >= appDeadline and then not blocked then
                     On_Deadline (win, timerDirty, running);
                  else
                     Request_Ring_Input (win);
                     Sleep_For_Activity (win, (if deadline = 0 then Wait_Forever else deadline), woke);
                     if woke = Unavailable then
                        Report_Stop (win, Activity_Unavailable);
                        abnormal := True;
                        running := False;
                     elsif not Ring.Is_Open (win.ring) then
                        Report_Stop (win, Ring_Ended);
                        abnormal := True;
                        running := False;
                     end if;
                     if Activity_Wait and then
                       CuBit.Readiness.Pending (Completions_Read, CuBit.Readiness.Completions)
                     then
                        declare
                           Seen : CuBit.Readiness.Count;
                        begin
                           while running and then
                             CuBit.Readiness.Pending (Completions_Read, CuBit.Readiness.Completions)
                           loop
                              Seen := CuBit.Readiness.Produced (CuBit.Readiness.Completions);
                              if Poll_Completion (receipt'Address) = 0 then
                                 CuBit.Readiness.Caught_Up
                                   (Completions_Read, CuBit.Readiness.Completions, Seen);
                                 exit;
                              end if;
                              CuBit.Readiness.Took (Completions_Read, CuBit.Readiness.Completions);
                              On_Completion (win, receipt, consumed, timerDirty, running);
                           end loop;
                        end;
                     end if;
                     if running and then appDeadline /= 0 and then
                       CuBit.Published_Clock.Milliseconds >= appDeadline and then not blocked
                     then
                        On_Deadline (win, timerDirty, running);
                     end if;
                  end if;
                  if running and then not CuBit.UI.Is_Empty (timerDirty) then Paint (timerDirty); end if;
               end;
            end if;
         end;
      end loop;
      --  A stop the user did not ask for was reported: let logstore take
      --  the record before the program exits.
      if abnormal or else win.inputStopped then CuBit.Log.Flush; end if;
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
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
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
      Ring.Close (win.ring);
      win.ringCount := 0; win.ringNext := 0;
      win.presentBacklog := (others => 0);
   end Close;
end CuBit.UI.App;
