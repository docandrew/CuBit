with Interfaces; use Interfaces;
package CuBit.Clocks is
   Snapshot_Operation : constant Unsigned_32 := 16#0B01#;
   Endpoint_Slot : constant Unsigned_64 := 25;
   type Time_Quality is
     (Unknown_Time, RTC_Only, Invalid_Zone, Unsupported_Date);
   for Time_Quality use
     (Unknown_Time => 0, RTC_Only => 1, Invalid_Zone => 2,
      Unsupported_Date => 3);
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
