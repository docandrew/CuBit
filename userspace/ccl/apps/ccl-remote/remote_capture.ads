------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Screenshots for the paired host (REMOTE-001): one frame through
--  Desktop's display-capture endpoint (CuBit.Display_Capture), when this
--  process's startup entry granted it ((display-capture approve-declared))
--  and the person at the device turned on "Allow screen capture".
--
--  @description
--  The frame lands in a buffer this process owns (charged to it) and lends
--  Desktop for the one copy: Desktop maps it, copies, returns the mapping
--  and replies; the loan is revoked right after. Two consumers: the wire's
--  Capture operation (Save_PNG: the frame as a PNG in the Captures place,
--  which the host then reads with Get) and CCL's (display.capture) (the
--  frame as an Image, Remote_Display_Bindings). Each capture is an
--  activity entry and a "ccl-remote: host capture ..." log line, whether
--  it was taken or refused. Desktop rate-limits and shows a "screen
--  captured" mark; masked fields are captured masked (they are drawn so).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with Remote_Wire;

package Remote_Capture is
   package W renames Remote_Wire;

   --  The display-capture endpoint was granted.
   function Available return Boolean;

   --  Tell Desktop what the remote session is (its Remote access section).
   type Session_State is (Ended, Listening, Paired);
   procedure Announce (State : Session_State; Peer : String);

   type Area is record
      X, Y, Width, Height : W.Area_Field := 0;
   end record;

   --  One frame into this process's buffer. When Result = Captured, the
   --  frame is Width x Height BGRA 8888 at Pixels (unpadded rows) until the
   --  next capture. Detail: a short reason for the activity line.
   procedure Take
     (Output : W.Output_Number; Where : Area; Result : out W.Capture_Result;
      Width, Height : out Natural; Pixels : out System.Address);

   --  The frame just taken, as the PNG Name in the Captures place: its size
   --  and SHA-256 (as Remote_Files.Stat reads them back).
   procedure Save_PNG
     (Name : String; Width, Height : Positive; Pixels : System.Address;
      Size : out Unsigned_64; Hash : out W.Digest; Result : out W.Status);

   --  The reason for an activity line ("screen capture is off ...").
   function Reason (Item : W.Capture_Result) return String;

   --  Record one capture: the activity view and the log
   --  ("ccl-remote: host capture <argument>: <outcome>").
   procedure Note (Argument : String; Result : W.Capture_Result);
end Remote_Capture;
