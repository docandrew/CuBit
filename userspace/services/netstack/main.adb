------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace network stack service (netstack.svc): see Netstack_Service.
------------------------------------------------------------------------------
with CuBit.Console_Route;
with CuBit.Messages;
with Netstack_Service;

procedure main is
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console;
   --  queued until devmgr introduces logstore (CuBit.Late_Introduction).
   --  The route needs no secondary stack (netstack is built without one).
   CuBit.Console_Route.Route_Named ("logstore");
   --  Stop (devmgr's, at shutdown): nothing to finish; the runtime ends
   --  the process when a receive returns it (CuBit.Messages).
   CuBit.Messages.Exit_On_Stop;
   Netstack_Service.Run;
end main;
