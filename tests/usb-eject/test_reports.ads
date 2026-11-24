--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--  Disk_Reports over four device slots, for the hosted tests.
with Disk_Reports;
package Test_Reports with SPARK_Mode is
   type Slot is range 1 .. 4;
   package Reports is new Disk_Reports (Slot);
end Test_Reports;
