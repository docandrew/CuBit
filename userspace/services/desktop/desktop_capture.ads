------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's display-capture endpoint (CuBit.Display_Capture, REMOTE-001
--  "Remote screenshots"): a read-only copy of one composed frame into the
--  holder's own buffer, when the person at the device allows it.
--
--  @description
--  Desktop composes, so Desktop answers; display.svc only scans out. The
--  holder calls down to Desktop and Desktop never calls it: every request
--  is answered at once, in the event loop's turn that received it (map the
--  holder's buffer, copy, return the mapping, reply). No IPC waits here.
--  The policy (the person's switch, the rate limit, the Deskbar mark) is
--  the proved Desktop_Capture_Policy; this unit checks the badge, maps and
--  copies, and logs every capture and refusal ("desktop: screen captured
--  ...", "desktop: screen capture refused ..."). Nothing of the frame is
--  kept after the copy.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Display_Capture;
with Desktop_Capture_Policy;

package Desktop_Capture with SPARK_Mode => Off is
   package DC renames CuBit.Display_Capture;

   --  True for the display-capture request labels.
   function Handles (Label : Unsigned_32) return Boolean;

   --  An output's newest completed frame as Desktop holds it: BGRA 8888
   --  rows of Pitch bytes at Address, Width x Height native pixels.
   type Frame is record
      Present : Boolean := False;
      Address : System.Address := System.Null_Address;
      Width, Height : DC.Coordinate := 0;
      Pitch : Natural := 0;
   end record;
   Maximum_Outputs : constant := 2;
   subtype Output_Number is Natural range 0 .. Maximum_Outputs - 1;
   type Frame_Array is array (Output_Number) of Frame;

   --  Answer one request. Changed: what the status panel or the Deskbar
   --  shows changed (a session announced, a capture's mark).
   procedure Handle
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Frames : Frame_Array; Now : Unsigned_64;
      Reply : out CuBit.Messages.Message; Changed : out Boolean);

   ---------------------------------------------------------------------------
   --  What the status panel and the Deskbar show
   ---------------------------------------------------------------------------
   function Session return Desktop_Capture_Policy.Session;
   --  The paired host's address as the holder announced it ("" if none).
   function Peer return String;
   function Allowed return Boolean;
   --  The "screen captured" mark is shown, and until when.
   function Marked return Boolean;
   function Mark_End return Unsigned_64;

   --  The person flipped "Allow screen capture".
   procedure Set_Allowed (On : Boolean; Now : Unsigned_64);
   --  Time passed: Changed when the mark ended.
   procedure Tick (Now : Unsigned_64; Changed : out Boolean);
end Desktop_Capture;
