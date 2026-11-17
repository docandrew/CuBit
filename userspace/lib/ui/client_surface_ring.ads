------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A window's side of its surface ring (Compositor_Surface_Ring, IPC-004):
--  submit requests and reap answers through shared memory, for one event
--  loop (one thread; the toolkit's windows are serialized by their loop).
--
--  @description
--  Open is the one synchronous call (with the caller's deadline). Submit and
--  Reap never block and make no system call. A request that finds Desktop
--  asleep (it declared so, once per arming) owes Desktop a wake; Flush pays
--  it, one system call however many requests a frame queued. The loop
--  flushes when a frame is complete; Arm flushes before it declares this
--  side asleep (False: an answer arrived meanwhile, do not sleep), then the
--  loop waits for this process's activity; Desktop's wake comes back as a
--  channel wake notice. Each answer is decoded against the kind
--  of request its tag was submitted as; an answer that does not decode, or
--  names no outstanding request, is counted and dropped.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Channel_Protocol;
with CuBit.Control_Events;
with CuBit.Deadlines;
with CuBit.Handles;
with Compositor_Surface_Ring;
with Compositor_Surface_Wire;

package Client_Surface_Ring is

   package SR renames Compositor_Surface_Ring;
   package Wire renames Compositor_Surface_Wire;
   subtype Token is SR.RC.Token;

   type Connection is limited private;

   --  Opened False: Result says how (Refused: Refusal says why).
   procedure Open
     (C : in out Connection; Endpoint : CuBit.Handles.Endpoint;
      Deadline : CuBit.Deadlines.Deadline; Opened : out Boolean;
      Result : out CuBit.Channels.Open_Result;
      Refusal : out CuBit.Channel_Protocol.Open_Refusal);
   function Is_Open (C : Connection) return Boolean;
   procedure Close (C : in out Connection);

   --  Requests of Kind outstanding now, and how many the window may have.
   function Outstanding (C : Connection; Kind : Wire.Request_Kind) return Natural;
   function Limit (Kind : Wire.Request_Kind) return Natural is
     (case Kind is
         when Wire.Present | Wire.Publish => SR.Frames_In_Flight,
         when Wire.Await_Retirement => SR.Retirements_In_Flight,
         when Wire.Receive_Input => SR.Input_Requests);
   --  Presents and publications share the frame limit.
   function Can_Submit (C : Connection; Kind : Wire.Request_Kind) return Boolean;

   type Submit_Result is (Queued, Full, Ended);
   procedure Submit
     (C : in out Connection; Item : Wire.Request; Tag : out Token; Result : out Submit_Result)
   with Pre => Wire.Valid (Item);

   --  Wake Desktop if a request queued since the last Flush found it
   --  asleep (one system call); nothing otherwise.
   procedure Flush (C : in out Connection);

   --  The next answer, if one waits (decoded; never blocks).
   procedure Reap
     (C : in out Connection; Tag : out Token; Answer : out Wire.Result; Got : out Boolean);

   --  Before sleeping: declare this side asleep. False: an answer arrived
   --  meanwhile (do not sleep; Reap). Disarm after waking.
   function Arm (C : in out Connection) return Boolean;
   procedure Disarm (C : Connection);

   --  A kernel grant event: whether it ended this channel (Desktop gone).
   procedure Note_Event (C : in out Connection; Event : CuBit.Control_Events.Event; Ends : out Boolean);

   --  Answers dropped: malformed, or for no outstanding request.
   function Stray_Answers (C : Connection) return Unsigned_64;

private
   type Outstanding_Entry is record
      Used : Boolean := False;
      Tag  : Token := 0;
      Kind : Wire.Request_Kind := Wire.Present;
   end record;
   type Outstanding_Table is array (1 .. SR.Owed_Per_Channel) of Outstanding_Entry;
   type Connection is limited record
      Channel  : CuBit.Channels.Channel;
      Ring     : SR.RC.Client;
      Open     : Boolean := False;
      Next_Tag : Token := 0;
      Table    : Outstanding_Table;
      Strays   : Unsigned_64 := 0;
      --  A queued request found Desktop asleep: Flush wakes it.
      Wake_Owed : Boolean := False;
   end record;
end Client_Surface_Ring;
