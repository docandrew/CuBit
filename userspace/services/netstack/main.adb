------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace network stack service (netstack.svc): see Netstack_Service.
------------------------------------------------------------------------------
with Netstack_Service;

procedure main is
begin
   Netstack_Service.Run;
end main;
