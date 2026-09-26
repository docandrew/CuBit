with Interfaces; use Interfaces;
package CuBit.Clocks is
   Snapshot_Operation : constant Unsigned_32 := 16#0B01#;
   Endpoint_Slot : constant Unsigned_64 := 25;
   --  Network_Unauthenticated: agreeing SNTP samples; an on-path attacker
   --  can shift it within the clock's step bound. Network_Authenticated:
   --  NTS-authenticated samples. Valid wall time is any of RTC_Only or the
   --  two network qualities; see Is_Valid_Wall_Time.
   type Time_Quality is
     (Unknown_Time, RTC_Only, Invalid_Zone, Unsupported_Date,
      Network_Unauthenticated, Network_Authenticated);
   for Time_Quality use
     (Unknown_Time => 0, RTC_Only => 1, Invalid_Zone => 2,
      Unsupported_Date => 3, Network_Unauthenticated => 4,
      Network_Authenticated => 5);
   function Is_Valid_Wall_Time (Quality : Time_Quality) return Boolean is
     (Quality in RTC_Only | Network_Unauthenticated | Network_Authenticated);
   --  The runtime discards enumeration names, so 'Image gives positions.
   function Name (Quality : Time_Quality) return String is
     (case Quality is
         when Unknown_Time => "unknown",
         when RTC_Only => "rtc-only",
         when Invalid_Zone => "invalid zone",
         when Unsupported_Date => "unsupported date",
         when Network_Unauthenticated => "network-unauthenticated",
         when Network_Authenticated => "network-authenticated");
   type Snapshot is record
      UTC_Seconds : Unsigned_64 := 0;
      Year : Natural range 1970 .. 2399 := 1970;
      Month : Positive range 1 .. 12 := 1;
      Day : Positive range 1 .. 31 := 1;
      Hour : Natural range 0 .. 23 := 0;
      Minute, Second : Natural range 0 .. 59 := 0;
      UTC_Offset_Seconds : Integer range -86_400 .. 86_400 := 0;
      Quality : Time_Quality := Unknown_Time;
   end record;
   procedure Read (Value : out Snapshot; Success : out Boolean);
end CuBit.Clocks;
