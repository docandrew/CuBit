------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config keys of the clock service's settings, shared with their writers.
------------------------------------------------------------------------------
package Clock_Settings with SPARK_Mode, Pure is
   --  The IANA zone wall time is shown in ("America/Denver"); absent means
   --  UTC. The clock service reads it for every snapshot; Desktop Settings
   --  writes it from the bundled zone list (Time_Zones).
   Time_Zone_Key : constant String := "clock.time-zone";
end Clock_Settings;
