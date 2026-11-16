------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's kernel event lane (docs/data-plane.md, "Control
--  messages"), read in one place so that no reader drops another's events:
--  child exits (CuBit.Child_Exits), grant lifecycle events and control
--  messages (CuBit.Control_Events).
--
--  @description
--  Poll drains the lane. Child exits are kept for Take_Exit (CuBit.Launching
--  waits with it). A grant revoke for a ring CuBit.Streams adopted is
--  answered here, the runtime's default: the mapping is returned and the
--  outlet closed. Every other grant event and control message is kept for
--  Next. At most Kept_Events of each wait here; beyond that they stay with
--  the kernel until there is room (none is dropped: docs/ipc-delivery.md).
--  A child's exit report keeps its PID from reuse until it is read, so a
--  launcher should take its children's exits (Take_Exit).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Child_Exits;
with CuBit.Process_IDs;
with CuBit.Control_Events;
with CuBit.Messages;

package CuBit.Process_Events is

   Kept_Events : constant := 32;

   procedure Poll;

   --  Whether File may be given one more event (both stores have room).
   function Has_Room return Boolean;

   --  One event the caller already took from the kernel (the activity wait
   --  returns a kernel notice directly), filed exactly as Poll files one.
   procedure File (Event : CuBit.Messages.Message)
   with Pre => Has_Room;

   --  The exit of Process (an identity: one life), if it has arrived: polls.
   procedure Take_Exit
     (Process : CuBit.Process_IDs.Process_ID; Found : out Boolean;
      Report : out CuBit.Child_Exits.Report);

   --  The oldest kept grant event or control message: polls.
   procedure Next (Item : out CuBit.Control_Events.Event; Found : out Boolean);

   --  For a program with no use for grant events or control messages
   --  beyond the runtime's own answers (a revoked adopted ring returned;
   --  Stop as CuBit.Messages.Exit_On_Stop or On_Stop declared): reads the
   --  lane, gives those answers, and lets go of the rest. Child exits stay
   --  for Take_Exit. A loop that waits with Wait_For_Activity_Until and
   --  takes only requests and completions calls it each turn, so that its
   --  notices neither wait unread (the wait would return at once, again
   --  and again) nor keep a Stop from it.
   procedure Serve_Defaults;

end CuBit.Process_Events;
