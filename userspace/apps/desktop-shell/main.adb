------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  First desktop shell client
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Startup;
--  Withed for its elaboration, the startup check (CAP-003); nothing in it
--  is used here.
pragma Unreferenced (Startup);
with UI_Desktop_Service;
with CuBit.Memory_Grants;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Deadlines;
with CuBit.Channels;
with CuBit.Channel_Protocol;
with CuBit.Process_Events;
with CuBit.Published_Clock;
with CuBit.UI;
with Client_Surface_Ring;
with Compositor_Surface_Wire;

procedure main is
   use ASCII;
   package DP renames CuBit.Desktop_Protocol;
   package Ring renames Client_Surface_Ring;
   package Wire renames Compositor_Surface_Wire;
   use type DP.Status_Code;
   use type DP.Input_Event_Kind;
   use type Wire.Request_Kind;
   use type Ring.Submit_Result;

   OP_SURFACE_CREATE   : constant Unsigned_32 := 16#0810#;

   SURFACE_FLAG_SHELL  : constant Unsigned_64 := 1;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;

   --  Opening the window's surface ring: one call, at most this long.
   RING_OPEN_LIMIT_MS : constant := 2_000;

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;
   KEY_R : constant Unsigned_64 := 16#13#;

   surfaceId : Unsigned_64 := 0;
   windowId  : Unsigned_64 := 0;
   width     : Unsigned_64 := 0;
   height    : Unsigned_64 := 0;
   windowW   : Unsigned_64 := 340;
   windowH   : Unsigned_64 := 220;
   bufferW   : constant Natural := 320;
   bufferH   : constant Natural := 176;
   bufferPitch : constant Natural := bufferW * 4;
   bufferAddr : System.Address := System.Null_Address;
   bufferGrant : CuBit.Memory_Grants.Grant_Reference;
   lastEvent : Unsigned_64 := 0;
   compact   : Boolean := False;
   running   : Boolean := True;
   sentBye   : Boolean := False;
   ignore    : Unsigned_64;
   --  The window's surface ring (IPC-004): presents and input, no call
   --  unless a side sleeps. presentWanted: a frame waits for a free slot.
   surfaceRing   : Ring.Connection;
   presentWanted : Boolean := False;

   function canvas return CuBit.UI.Canvas is
   begin
      return
        (addr        => bufferAddr,
         width       => bufferW,
         height      => bufferH,
         pitch       => bufferPitch,
         clipEnabled => False,
         clip        => (others => 0), others => <>);
   end canvas;

   function callDesktop
      (label : Unsigned_32;
       w0    : Unsigned_64 := 0;
       w1    : Unsigned_64 := 0;
       w2    : Unsigned_64 := 0;
       w3    : Unsigned_64 := 0) return Message
   is
      msg : Message :=
        (tag      => (label  => label,
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
         authorityTag => 0,
         words    => (w0, w1, w2, w3));
      tag : MessageTag;
   begin
      --  All shell protocol traffic goes through the desktop endpoint cap.
      --  The app never maps the framebuffer and never registers for raw
      --  keyboard/mouse input; desktop.svc remains the display/input boundary.
      tag := capCall (UI_Desktop_Service.Endpoint, msg, CuBit.Messages.Wait_Forever);
      msg.tag := tag;
      return msg;
   end callDesktop;

   function alignUpPage (value : Unsigned_64) return Unsigned_64 is
   begin
      return (value + 4095) and not Unsigned_64'(4095);
   end alignUpPage;

   procedure writePixel (x, y : Natural; color : Unsigned_32) is
   begin
      CuBit.UI.Set_Pixel (canvas, x, y, color);
   end writePixel;

   procedure drawPixelTest (phase : Unsigned_64) is
      c : constant CuBit.UI.Canvas := canvas;
      r : Unsigned_32;
      g : Unsigned_32;
      b : Unsigned_32;
   begin
      for y in 0 .. bufferH - 1 loop
         for x in 0 .. bufferW - 1 loop
            r := Unsigned_32 ((x + Natural (phase mod 64)) mod 256);
            g := Unsigned_32 ((y * 2) mod 256);
            b := Unsigned_32 (((x / 8 + y / 8) * 24) mod 256);
            writePixel
              (x, y,
               Shift_Left (r, 16) or Shift_Left (g, 8) or b);
         end loop;
      end loop;

      CuBit.UI.Draw_Button_Frame
        (c,
         (x => 8, y => 8, w => 132, h => 28),
         CuBit.UI.Mirage,
         CuBit.UI.Button_Active);
      CuBit.UI.Draw_Button_Frame
        (c,
         (x => bufferW - 46, y => 8, w => 38, h => 28),
         CuBit.UI.Mirage,
         CuBit.UI.Button_Normal);
   end drawPixelTest;

   procedure attachPixelBuffer is
      raw : Unsigned_64;
      pages : constant Unsigned_64 :=
         (Unsigned_64 (bufferPitch * bufferH) + 4095) / 4096;
      grantOk : Boolean;
      reply : Message;
   begin
      raw := syscall (SYSCALL_SBRK, pages * 4096 + 4096);
      if raw = Unsigned_64'Last then
         debugPrint ("desktop-shell: pixel buffer alloc failed" & LF);
         return;
      end if;

      bufferAddr := To_Address (Integer_Address (alignUpPage (raw)));
      drawPixelTest (0);

      CuBit.Memory_Grants.Create_Via_Capability
        (slot      => UI_Desktop_Service.Endpoint,
         localAddr => bufferAddr,
         numPages  => Natural (pages),
         readWrite => False,
         reference => bufferGrant,
         success   => grantOk);
      if not grantOk then
         debugPrint ("desktop-shell: pixel grant failed" & LF);
         return;
      end if;

      reply := CuBit.Desktop_Messages.From_Wire
        (CuBit.Desktop_Protocol.Encode_Attachment
           ((CuBit.Desktop_Protocol.Live_Surface_Name (windowId),
             bufferGrant,
             (CuBit.Desktop_Protocol.Positive_Extent (bufferW),
              CuBit.Desktop_Protocol.Positive_Extent (bufferH), bufferPitch))));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
      if reply.words (0) /= 0 then
         debugPrint ("desktop-shell: pixel attach failed" & LF);
      else
         debugPrint ("desktop-shell: pixel buffer attached" & LF);
      end if;
   end attachPixelBuffer;

   --  Draw and present the window's buffer through the ring; with no frame
   --  slot free, it goes when one is (pumpRing).
   procedure present is
      result : Ring.Submit_Result;
      tag : Ring.Token;
   begin
      if windowId = 0 then
         return;
      end if;
      if not Ring.Can_Submit (surfaceRing, Wire.Present) then
         presentWanted := True;
         return;
      end if;
      if bufferAddr /= System.Null_Address then
         drawPixelTest (lastEvent);
      end if;
      Ring.Submit (surfaceRing,
        (Kind => Wire.Present, Surface => DP.Live_Surface_Name (windowId),
         Area => (0, 0, 0, 0), Input_After => lastEvent), tag, result);
      presentWanted := result /= Ring.Queued;
      --  The frame is queued: wake Desktop if it sleeps.
      Ring.Flush (surfaceRing);
   end present;

   procedure requestResize;

   --  One input event from the ring.
   procedure handleInput (item : DP.Input_Envelope) is
   begin
      lastEvent := item.Serial;
      if item.Kind = DP.Surface_Configured then
         windowW := item.Payload0;
         windowH := item.Payload1;
      elsif item.Kind = DP.Key_Pressed then
         if item.Payload0 = KEY_ESC or else item.Payload0 = KEY_Q then
            running := False;
         elsif item.Payload0 = KEY_R then
            requestResize;
         end if;
      elsif item.Kind = DP.Close_Requested then
         running := False;
      end if;
   end handleInput;

   --  Every answer the ring holds; then keep one input request outstanding
   --  and send a waiting frame. No call unless Desktop sleeps.
   procedure pumpRing is
      tag : Ring.Token;
      answer : Wire.Result;
      got : Boolean;
      result : Ring.Submit_Result;
   begin
      loop
         Ring.Reap (surfaceRing, tag, answer, got);
         exit when not got;
         if answer.Kind = Wire.Receive_Input then
            if answer.Status = DP.Success then
               for i in 1 .. answer.Count loop
                  handleInput (answer.Events (i));
               end loop;
            elsif answer.Status = DP.Bad_Object then
               debugPrint ("desktop-shell: input rejected" & LF);
               running := False;
            end if;
         end if;
      end loop;
      if presentWanted then
         present;
      end if;
      if running and then Ring.Outstanding (surfaceRing, Wire.Receive_Input) = 0 then
         Ring.Submit (surfaceRing, (Kind => Wire.Receive_Input, Surface => DP.Live_Surface_Name (windowId),
                                    After => lastEvent), tag, result);
      end if;
   end pumpRing;

   procedure requestResize is
      reply : Message;
      resized : DP.Resize_Result;
      nextW : Unsigned_64 := windowW;
      nextH : Unsigned_64 := windowH;
   begin
      --  Bring-up resize exercise: press R to toggle a single child window.
      --  This proves resize belongs to a surface, not the whole desktop.
      if windowId = 0 then
         return;
      end if;
      if compact then
         compact := False;
      else
         compact := True;
      end if;

      if compact then
         nextW := 520;
         nextH := 320;
      else
         nextW := 360;
         nextH := 220;
      end if;

      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Resize ((DP.Live_Surface_Name (windowId),
                            DP.Pixel_Extent (nextW), DP.Pixel_Extent (nextH))));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
      resized := DP.Decode_Resize_Result (CuBit.Desktop_Messages.To_Wire (reply));
      if resized.Status = DP.Success then
         windowW := Unsigned_64 (resized.Width);
         windowH := Unsigned_64 (resized.Height);
         present;
      end if;
   end requestResize;

   procedure closeSession is
      reply : Message;
   begin
      if sentBye then
         return;
      end if;

      --  This is the graceful prototype exit path. The shell client asks the
      --  desktop service to remove its surface, then exits so the CLI shell's
      --  foreground-child logic can reclaim keyboard/mouse focus and redraw.
      reply := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Goodbye));
      reply.tag := capCall (UI_Desktop_Service.Endpoint, reply, CuBit.Messages.Wait_Forever);
      if DP.Decode_Status (CuBit.Desktop_Messages.To_Wire (reply), DP.Goodbye) /= DP.Success then
         debugPrint ("desktop-shell: goodbye failed" & LF);
         return;
      end if;
      sentBye := True;
   end closeSession;

begin
   debugPrint ("desktop-shell: starting" & LF);

   declare
      hello : Message := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Hello (DP.Current_Revision));
   begin
      hello.tag := capCall (UI_Desktop_Service.Endpoint, hello, CuBit.Messages.Wait_Forever);
      if DP.Decode_Hello_Result (CuBit.Desktop_Messages.To_Wire (hello)).Status /= DP.Success then
         debugPrint ("desktop-shell: hello failed" & LF);
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      info : Message := CuBit.Desktop_Messages.From_Wire
        (DP.Encode_Empty_Request (DP.Get_Information));
      decoded : DP.Information_Result;
   begin
      info.tag := capCall (UI_Desktop_Service.Endpoint, info, CuBit.Messages.Wait_Forever);
      decoded := DP.Decode_Information_Result (CuBit.Desktop_Messages.To_Wire (info));
      if decoded.Status /= DP.Success then
         debugPrint ("desktop-shell: display information failed" & LF);
         closeSession;
         return;
      end if;
      width := Unsigned_64 (decoded.Width);
      height := Unsigned_64 (decoded.Height);
   end;

   declare
      created : constant Message :=
         callDesktop (OP_SURFACE_CREATE,
                      width,
                      height,
                      SURFACE_FLAG_SHELL,
                      0);
   begin
      surfaceId := created.words (0);
      width := created.words (1);
      height := created.words (2);
      if surfaceId = 0 then
         debugPrint ("desktop-shell: create surface failed" & LF);
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   declare
      created : constant Message :=
         callDesktop (OP_SURFACE_CREATE,
                      windowW,
                      windowH,
                      SURFACE_FLAG_WINDOW,
                      0);
   begin
      windowId := created.words (0);
      windowW := created.words (1);
      windowH := created.words (2);
      if windowId = 0 then
         debugPrint ("desktop-shell: create window failed" & LF);
         closeSession;
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   debugPrint ("desktop-shell: connected" & LF);
   attachPixelBuffer;
   declare
      opened : Boolean;
      result : CuBit.Channels.Open_Result;
      refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Ring.Open (surfaceRing, UI_Desktop_Service.Endpoint,
                 CuBit.Deadlines.At_Monotonic (CuBit.Published_Clock.Milliseconds + RING_OPEN_LIMIT_MS),
                 opened, result, refusal);
      if not opened then
         debugPrint ("desktop-shell: surface ring open failed: " &
                     CuBit.Channels.Open_Result'Image (result) & " " &
                     CuBit.Channel_Protocol.Open_Refusal'Image (refusal) & LF);
         closeSession;
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;
   present;

   --  Input arrives in the ring; sleep until Desktop answers (it wakes this
   --  process only while it sleeps), never polling on a timer.
   while running loop
      pumpRing;
      exit when not running or else not Ring.Is_Open (surfaceRing);
      if Ring.Arm (surfaceRing) then
         declare
            notice : aliased Message;
            woke : Wake_Result;
         begin
            Wait_For_Activity_Or_Event (Wait_Forever, notice, woke);
            Ring.Disarm (surfaceRing);
            case woke is
               when Event_Taken =>
                  if CuBit.Process_Events.Has_Room then
                     CuBit.Process_Events.File (notice);
                  end if;
               when Work_Available =>
                  CuBit.Process_Events.Poll;
               when Deadline_Reached =>
                  null;
               when Unavailable =>
                  running := False;
            end case;
         end;
      end if;
   end loop;

   declare
   begin
      closeSession;
   end;

   ignore := syscall (SYSCALL_EXIT, 0);
end main;
