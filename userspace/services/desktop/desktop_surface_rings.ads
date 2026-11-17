------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The windows' surface rings at Desktop (IPC-004): one request channel per
--  window (Compositor_Surface_Ring), served by Desktop's loop. main.adb
--  only calls in: open requests, a turn's serving, the answers owed when a
--  frame is drawn or a buffer retires, the wakes at the end of a turn, and
--  the arming before Desktop sleeps.
--
--  @description
--  The channels are a CuBit.Channel_Servers instance; what a request means
--  and what Desktop owes for it is Desktop_Surface_Ring_Service. Desktop
--  only copies requests out of a client's ring and writes answers into its
--  own; it never waits on a client. A ring whose client breaks the
--  protocol is ended with a typed reason in Desktop's log; Desktop's loop
--  and every other ring go on.
--
--  A turn: Serve (every ring with requests, a budget each); Frame_Drawn
--  once a frame showing the presented pixels is drawn; Pump_Retirements;
--  Wake (clients owed a wake, each of which declared itself asleep: one
--  system call each, none while a client is busy). Before sleeping, Quiet
--  arms every ring (False: a request arrived meanwhile; serve it first).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Control_Events;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;

generic
   --  Desktop's operations (Desktop_Surface_Ring_Service's formals).
   with function Surface_Access
     (From : CuBit.Messages.Process_ID; Target : Unsigned_64)
      return CuBit.Desktop_Protocol.Status_Code;
   with procedure Bounds
     (Target : Unsigned_64; Attached_Width, Attached_Height : out Natural;
      Source_Width, Source_Height : out Natural);
   with procedure Present_Attached
     (From : CuBit.Messages.Process_ID; Value : CuBit.Desktop_Protocol.Present_Request;
      Status : out CuBit.Desktop_Protocol.Status_Code);
   with procedure Stage
     (From : CuBit.Messages.Process_ID;
      Value : CuBit.Desktop_Protocol.Publication.Stage_Request;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   with procedure Publish
     (From : CuBit.Messages.Process_ID;
      Value : CuBit.Desktop_Protocol.Publication.Publish_Request;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   with procedure Retirement
     (From : CuBit.Messages.Process_ID; Value : CuBit.Desktop_Protocol.Publication.Query;
      Result : out CuBit.Desktop_Protocol.Publication.Receipt);
   with procedure Next_Input
     (Target, After : Unsigned_64; Item : out CuBit.Desktop_Protocol.Input_Envelope;
      Got : out Boolean);
   with function Input_After (Target, After : Unsigned_64) return Boolean;
   with procedure Report (Text : String);
package Desktop_Surface_Rings is

   --  False: no memory for the channel table; windows cannot open (their
   --  toolkit has no synchronous fallback) and Desktop says so.
   procedure Configure (Ready : out Boolean);
   function Ready return Boolean;

   --  A request on Desktop's endpoint. Is_Open: it was a ring open, and
   --  Answer is the reply to send to From.
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message);

   --  Take what the rings hold: each ring once per turn, a budget of
   --  requests each. Served: at least one request was taken.
   procedure Serve (Served : out Boolean);

   --  Input was queued for Target (its ring's input request is answered).
   procedure Input_Queued (Target : Unsigned_64);
   --  A frame was drawn: every presented buffer has been read.
   procedure Frame_Drawn;
   --  Retirement waits whose buffer retired since.
   procedure Pump_Retirements;
   --  Target is going: what is owed for it is answered Bad_Object.
   procedure Surface_Ended (Target : Unsigned_64);

   --  Wake every window owed a wake (each declared itself asleep).
   procedure Wake;
   --  Before sleeping: every ring armed, and none holds a request.
   function Quiet return Boolean;

   --  A kernel notice from the event queue (Publisher: No_Process). Kind:
   --    Not_Ours: not a ring's (pass it on);
   --    Handled: a channel wake, or the end of a ring (its debts dropped);
   --    Grant_Event: a grant event that ended no ring (Event, for
   --      Desktop's own grant bookkeeping).
   type Notice_Kind is (Not_Ours, Handled, Grant_Event);
   procedure Take_Notice
     (Notice : CuBit.Messages.Message; Publisher : CuBit.Messages.Process_ID;
      Kind : out Notice_Kind; Event : out CuBit.Control_Events.Event);

   --  For Desktop's statistics: rings ended for a bad request; input
   --  events skipped because they were not valid.
   function Ended_For_Bad_Requests return Unsigned_64;
   function Skipped_Input return Unsigned_64;

end Desktop_Surface_Rings;
