------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's input source rings (IPC-009, DEV-002): every input device's
--  reports come straight from its driver through its own ring; the kernel
--  only set the ring up and wakes Desktop when it sleeps. main.adb only
--  calls in: open and close requests, the turn's drain, the arming before
--  Desktop sleeps, and the kernel's grant events.
--
--  @description
--  An open is admitted only through the endpoint devmgr badged for input
--  sources (CuBit.Input.Source_Ring_Tag) and with the source-ring contract
--  (CuBit.Input_Rings), at most Sources_Per_Driver per driver; anything
--  else is refused with its reason in Desktop's log. Each admitted ring is
--  its source's identity (Input_Source_Set). A ring that ends (its driver
--  closed it, died, or the device went away) is removed and Source_Ended
--  says which identity is gone.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Control_Events;
with CuBit.Input;
with CuBit.Messages;

generic
   with procedure Deliver (Words : CuBit.Input.Report_Words; Source : Unsigned_64);
   with procedure Source_Ended (Source : Unsigned_64);
   with procedure Report (Text : String);
package Desktop_Input_Sources is

   --  The most rings at once: several drivers' worth of keyboards and
   --  pointers (a named ceiling, LIM-001).
   Maximum_Sources : constant := 64;
   --  Reports delivered per turn before the turn goes on (the rest wait in
   --  their rings; a full ring is back pressure on its driver alone).
   Drain_Budget : constant := 64;

   --  A request on Desktop's endpoint. Is_Open: it was a source-ring open
   --  (Answer is the reply to send to From). Other opens are not ours.
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message);

   --  A driver's one-way OP_CLOSE: Closed when it named one of our rings.
   procedure Handle_Close
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Closed : out Boolean);

   --  Deliver what the rings hold, oldest acquisition first.
   procedure Drain (Delivered : out Natural);

   --  Before sleeping: every ring armed (False: a report arrived; drain).
   function Quiet return Boolean;
   procedure Disarm;

   --  A kernel grant event: Ended when it ended one of our rings.
   procedure Take_Event (Event : CuBit.Control_Events.Event; Ended : out Boolean);

   function Count return Natural;

end Desktop_Input_Sources;
