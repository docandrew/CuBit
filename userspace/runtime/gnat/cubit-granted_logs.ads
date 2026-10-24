------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A log publisher for a service devmgr starts before logstore runs
--  (netstack, netmgr): its lines go to the serial console at once, and to
--  logstore once devmgr grants a publisher for it (the run-time log grant,
--  as xhci's), so they show in the Logs app and the boot-logs view on a
--  machine without a serial port (coordination/release-stack.md, "bring-up
--  logging convention"). Lines written before the grant wait in a bounded
--  buffer; any that do not fit are counted and reported once.
--
--  The program owns its completion queue: it calls Request when it may ask
--  (logstore is registered), and passes each completion to Take.
--  No secondary stack is used.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;

--  One per program (its own publisher). Its spec names no unit that needs
--  the secondary stack, so netstack (built without one) can use it.
package CuBit.Granted_Logs is

   --  The completion token of the grant request.
   Token : constant Unsigned_64 := 16#4752_4E54_4C4F_4701#;

   type Level is (Information, Warning);

   --  devmgr's operation that answers with a logstore endpoint badged for
   --  the calling program's publication (devmgr decides which programs).
   Grant_Label : constant Unsigned_32 := 16#0228#;

   --  One line: on serial now, to logstore when granted.
   procedure Line (Text : String; Severity : Level := Information);

   --  Ask devmgr (Device_Manager: the program's Startup "ready" binding)
   --  for the publisher, if not granted or asked yet and logstore has
   --  registered. Cheap when there is nothing to do.
   procedure Request (Device_Manager : CuBit.Handles.Endpoint);

   --  Completion C was this program's grant answer: take it (True).
   function Take (C : CuBit.Messages.CompletionEntry) return Boolean;

   --  A request is outstanding (the program polls completions meanwhile).
   function Waiting return Boolean;
   function Granted return Boolean;
   --  Granted, or devmgr refused: nothing more to ask.
   function Settled return Boolean;

end CuBit.Granted_Logs;
