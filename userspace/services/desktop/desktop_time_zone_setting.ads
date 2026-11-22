------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  clock.time-zone for Settings > Date & Time: read from and written to
--  Config as an IANA name from the bundled database (Time_Zones).
------------------------------------------------------------------------------
with Time_Zones;

package Desktop_Time_Zone_Setting is
   --  The zone in Config; UTC (the clock service's meaning) when absent.
   --  Known is False when Config holds a name the database lacks.
   procedure Load (Zone : out Time_Zones.Time_Zone; Known : out Boolean);
   procedure Store (Zone : Time_Zones.Time_Zone; Saved : out Boolean);
end Desktop_Time_Zone_Setting;
