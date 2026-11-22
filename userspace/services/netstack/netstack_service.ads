------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The network stack service (netstack.svc): its state and request loop
--  (see the body). State is at library level so that several threads of
--  the service can run the loop.
------------------------------------------------------------------------------
package Netstack_Service is
   procedure Run;
end Netstack_Service;
