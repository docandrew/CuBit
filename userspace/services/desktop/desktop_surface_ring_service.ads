------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's side of the windows' surface rings (IPC-004), without the
--  channels: what one request copied out of a ring means, what Desktop owes
--  for it, and when the answer is written.
--
--  @description
--  Desktop_Surface_Rings feeds this package the requests it takes from the
--  rings (CuBit.Request_Channels copies each one out of the client's page
--  once; nothing here reads shared memory). Each copy is decoded
--  (Compositor_Surface_Wire); its damage is clamped to the bounds Desktop
--  fixed for the surface when the client attached or configured it (never
--  to anything in shared memory); then Desktop's own operations (the
--  formal subprograms) do the work.
--
--  A request that does not decode, or that breaks the ring's declared
--  limits (more presents, retirement waits or input requests outstanding
--  than Compositor_Surface_Ring allows), ends that ring with a typed
--  reason: Report says which ring and why, End_Ring closes it, and Desktop
--  goes on serving every other ring. A request naming a surface the client
--  does not own is answered Denied; one naming no surface, Bad_Object.
--
--  Answers owed later (a present until the frame showing it is drawn, a
--  retirement wait until the buffer retires, an input request until input
--  arrives) are kept per ring, in a table that grows with the server's.
--  The same code runs in Desktop and in the hosted adversary test
--  (tests/surface-ring-adversary).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Process_IDs;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;
with Compositor_Surface_Ring;
with Compositor_Surface_Wire;

generic
   --  The server's channel index (CuBit.Channel_Servers.Index).
   type Ring_Index is range <>;
   --  The server: whether ring I is open, who opened it, write an answer
   --  into it, end it (its client loses the channel).
   with function Is_Active (I : Ring_Index) return Boolean;
   with function Client_Of (I : Ring_Index) return CuBit.Process_IDs.Process_ID;
   --  Kind: the answer's request kind (a client may arm for some only).
   with procedure Complete
     (I : Ring_Index; Tag : Compositor_Surface_Ring.RC.Token;
      Answer : Compositor_Surface_Wire.Result_Words;
      Kind : Compositor_Surface_Wire.Request_Kind);
   with procedure End_Ring (I : Ring_Index);
   --  Input was queued for one of ring I's client's windows and no request
   --  could take it: count up the ring's unrequested-input generation
   --  (Compositor_Surface_Ring.Input_Generation_At).
   with procedure Note_Unrequested_Input (I : Ring_Index);

   --  Desktop. Surface_Access: Success when From owns Target, Bad_Object
   --  when there is no such surface, Denied otherwise.
   with function Surface_Access
     (From : CuBit.Process_IDs.Process_ID; Target : Unsigned_64)
      return CuBit.Desktop_Protocol.Status_Code;
   --  The bounds Desktop fixed for Target: its attached buffer's client
   --  area (Present damage) and its current publication layout (Publish
   --  damage, in source pixels). Zero when it has none.
   with procedure Bounds
     (Target : Unsigned_64; Attached_Width, Attached_Height : out Natural;
      Source_Width, Source_Height : out Natural);
   with procedure Present_Attached
     (From : CuBit.Process_IDs.Process_ID; Value : CuBit.Desktop_Protocol.Present_Request;
      Status : out CuBit.Desktop_Protocol.Status_Code);
   with procedure Stage
     (From : CuBit.Process_IDs.Process_ID;
      Value : CuBit.Desktop_Protocol.Publication.Stage_Request;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   with procedure Publish
     (From : CuBit.Process_IDs.Process_ID;
      Value : CuBit.Desktop_Protocol.Publication.Publish_Request;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   --  Success: Ticket has retired; Bad_State: still read (asked again
   --  later); anything else: the wait fails with that status.
   with procedure Retirement
     (From : CuBit.Process_IDs.Process_ID; Value : CuBit.Desktop_Protocol.Publication.Query;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   --  The first input event for Target after serial After (Got False:
   --  none queued), and whether any follow serial After.
   with procedure Next_Input
     (Target, After : Unsigned_64; Item : out CuBit.Desktop_Protocol.Input_Envelope;
      Got : out Boolean);
   with function Input_After (Target, After : Unsigned_64) return Boolean;
   --  A line for Desktop's log (an ended ring).
   with procedure Report (Text : String);
package Desktop_Surface_Ring_Service is

   package SR renames Compositor_Surface_Ring;
   package Wire renames Compositor_Surface_Wire;
   package DP renames CuBit.Desktop_Protocol;

   --  Why Desktop ended a ring.
   type End_Reason is
     (Malformed_Request,        --  the copy did not decode as a request
      Too_Many_Frames,          --  presents owed past Frames_In_Flight
      Too_Many_Retirement_Waits,--  waits owed past Retirements_In_Flight
      Input_Already_Requested); --  a second input request while one is owed
   function Reason_Text (Reason : End_Reason) return String is
     (case Reason is
        when Malformed_Request => "malformed request",
        when Too_Many_Frames => "more frames in flight than its limit",
        when Too_Many_Retirement_Waits => "more retirement waits than its limit",
        when Input_Already_Requested => "a second input request while one is owed");

   --  False until Grow succeeds once; then whether rings up to Count are
   --  tracked. Grow keeps every debt where it is (False: no memory, and
   --  nothing changes; the caller does not serve ring Count yet).
   procedure Grow (Count : Ring_Index; Grown : out Boolean);
   function Tracked (I : Ring_Index) return Boolean;

   --  One request copied out of ring I. Ended: the ring was ended for it
   --  (take nothing more from I).
   procedure Serve
     (I : Ring_Index; Item : SR.RC.Queues.Submission; Ended : out Boolean)
     with Pre => Tracked (I);

   --  Input was queued for Target: answer its ring's input request.
   procedure Input_Queued (Target : Unsigned_64);
   --  A frame was drawn: every owed present has been read.
   procedure Frame_Drawn;
   --  Answer the retirement waits whose buffer has retired since.
   procedure Pump_Retirements;
   --  Target is going: everything owed for it is answered Bad_Object.
   procedure Surface_Ended (Target : Unsigned_64);
   --  Ring I ended (closed by its client, its client died, or End_Ring):
   --  nothing owed to it is sent.
   procedure Ring_Ended (I : Ring_Index);

   --  Answers owed to ring I now (presents, retirement waits, input).
   function Owed (I : Ring_Index) return Natural;
   --  Rings ended for a bad request, by reason, since Desktop started.
   type Reason_Counts is array (End_Reason) of Unsigned_64;
   function Ended_Rings return Reason_Counts;
   --  Input events skipped because they were not valid (never expected).
   function Skipped_Input return Unsigned_64;

end Desktop_Surface_Ring_Service;
