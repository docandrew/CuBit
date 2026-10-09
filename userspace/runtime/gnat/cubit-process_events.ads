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

package CuBit.Process_Events is

   Kept_Events : constant := 32;

   procedure Poll;

   --  The exit of Process (an identity: one life), if it has arrived: polls.
   procedure Take_Exit
     (Process : CuBit.Process_IDs.Process_ID; Found : out Boolean;
      Report : out CuBit.Child_Exits.Report);

   --  The oldest kept grant event or control message: polls.
   procedure Next (Item : out CuBit.Control_Events.Event; Found : out Boolean);

end CuBit.Process_Events;
