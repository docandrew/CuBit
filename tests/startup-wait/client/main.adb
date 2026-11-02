------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  startup-wait-client (tests/startup-wait, headless test startup-wait):
--  requires the "startup-wait-test" service. procmgr starts it only once
--  the provider has registered, so its first call is answered.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_Wait_Test;
with Startup;

procedure Main is
   use ASCII;
   package SW renames CuBit.Startup_Wait_Test;
   --  The provider is registered before this runs: one call, promptly.
   Call_Deadline_Ms : constant := 2_000;
   Request : Message := NULL_MESSAGE;
begin
   debugPrint ("startup-wait-client: started" & LF);
   Request.tag.label := SW.Probe_Label;
   Request.tag := capCall (Startup.Provider, Request, Deadline_After (Call_Deadline_Ms));
   if Request.tag.label = SW.Probe_Answer then
      debugPrint ("startup-wait-client: provider answered PASS" & LF);
   else
      debugPrint ("startup-wait-client: provider did not answer FAIL" & LF);
   end if;
end Main;
