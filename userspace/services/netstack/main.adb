------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace network stack service (netstack.svc): see Netstack_Service.
------------------------------------------------------------------------------
with CuBit.Messages;
with Netstack_Service;

procedure main is
begin
   --  Stop (devmgr's, at shutdown): nothing to finish; the runtime ends
   --  the process when a receive returns it (CuBit.Messages).
   CuBit.Messages.Exit_On_Stop;
   Netstack_Service.Run;
end main;
