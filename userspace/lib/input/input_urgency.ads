------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  An input driver's urgency (SCHED-001, docs/scheduler.md §4): the
--  INTERACTIVE contract its manifest's "urgency" grant covers, reserved
--  Input_Wakes_Only, so only the report signals and event publications
--  that carry input to Desktop lend urgent turns, never the driver's other
--  traffic (xhci.drv's USB disks, hid-i2c.drv's I2C transactions).
--
--  Every input driver's manifest declares the same share:
--  (scheduling "urgency" 1000 10000) or
--  (request-scheduling urgency realtime (budget-us 1000) (period-us 10000)).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles;

package Input_Urgency is
   Budget_Us : constant := 1_000;
   Period_Us : constant := 10_000;

   --  Reserve it with Authority (the manifest's "urgency"). True when the
   --  kernel admitted it.
   function Reserve (Authority : CuBit.Handles.Scheduling) return Boolean;
end Input_Urgency;
