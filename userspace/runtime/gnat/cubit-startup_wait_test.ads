------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The startup-wait headless test's service (tests/startup-wait): a role
--  procmgr launches and tracks like any started service, so the test can
--  start a provider that registers late, ends first, or misses its ready
--  deadline, and a client that requires it (CAP-003 startup waits).
--  Never part of a production image: procmgr lets only
--  com.cubit.startup-wait-test register it, and only from the startup plan.
------------------------------------------------------------------------------
package CuBit.Startup_Wait_Test is
   pragma Pure;

   --  Its role (userspace/ccl/catalogs/native-runtime-services.ccl).
   Service_Role : constant := 36;

   --  The client's probe; the provider answers it with Probe_Answer.
   Probe_Label  : constant := 16#5701#;
   Probe_Answer : constant := 16#5702#;
end CuBit.Startup_Wait_Test;
