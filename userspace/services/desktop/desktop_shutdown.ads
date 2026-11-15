------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shut Down... from the Apps menu (PWR-003): one confirmation (Cancel /
--  Shut Down, Enter = Shut Down, Escape = Cancel), then the typed request
--  to power.svc as Desktop, through Desktop's own requester badge
--  (CuBit.Power_Requests, catalog role power-desktop, startup plan only).
--  power.svc answers at once; Desktop then says "Shutting down..." until
--  procmgr stops it with the rest. Cancel sends nothing.
------------------------------------------------------------------------------
with CuBit.Power_Requests;

package Desktop_Shutdown is
   type Phase is
     (Closed,         --  no dialog
      Confirming,     --  the confirmation is up
      Shutting_Down,  --  power.svc accepted: the system is stopping
      Refused);       --  power.svc refused, or could not be asked (Note says why)

   function Current return Phase;
   function Note return String;

   --  The menu's Shut Down...: the confirmation.
   procedure Ask;
   --  Escape or Cancel: nothing is sent.
   procedure Cancel;
   --  Enter or Shut Down: the request (bounded: power.svc answers at once,
   --  within CuBit.Power_Requests.Answer_Budget_Milliseconds).
   procedure Confirm;
   --  Dismiss a refusal.
   procedure Dismiss;

   Title_Text   : constant String := "Shut down the computer?";
   Body_Text    : constant String := "Apps are asked to close; unsaved work is kept as a recovery copy.";
   Cancel_Text  : constant String := "Cancel";
   Confirm_Text : constant String := "Shut Down";
   Progress_Text : constant String := "Shutting down...";
end Desktop_Shutdown;
