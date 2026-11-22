------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The headless gpio-launch fixtures' probe (SUP-001): which controller an
--  "i2c" binding reaches.
--
--  @description
--  A test-only device operation. generic-check.drv (tests/generic-launch)
--  answers it with the PCI location it was launched for and the badge the
--  request arrived with; hid-launch-check.drv calls it through its "i2c"
--  endpoint, so the test sees whether devmgr's binding reached the driver
--  of the offer's controller (two controllers, 00:15.0 and 00:15.1, as on
--  the IdeaPad) with the I2C target's badge.
--
--  Request: label Where_Label, no words. Reply: Location_Word = the
--  driver's Launch_Configuration.Routing_ID, Badge_Word = the request's
--  authority tag as the driver received it.
------------------------------------------------------------------------------
package Fixture_Probe with Pure is
   Where_Label   : constant := 16#02F0#;
   Request_Words : constant := 0;
   Reply_Words   : constant := 2;
   Location_Word : constant := 0;
   Badge_Word    : constant := 1;
   --  How long hid-launch-check waits for the answer (explicit, bounded).
   --  (Past generic-check's own bounded interrupt wait, 2 s, before it serves.)
   Probe_Deadline_Ms : constant := 5_000;
end Fixture_Probe;
