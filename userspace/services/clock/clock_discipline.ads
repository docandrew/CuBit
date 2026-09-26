pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Clocks; use CuBit.Clocks;
with CuBit.Clock_Control; use CuBit.Clock_Control;

--  Pure wall-clock state and adjustment policy. Times are milliseconds: UTC
--  since the Unix epoch, and the kernel's monotonic clock. Monotonic time is
--  never changed; only the UTC-to-monotonic mapping is.
--
--  Policy (see docs/clock-and-time-services.md):
--  * UTC never falls below the time floor, and the RTC is ignored when it is
--    below the floor (for example after CMOS battery loss).
--  * A sample must be recent, observed no later than now, precise enough,
--    and inside the supported date range.
--  * Unauthenticated samples need several agreeing sources and may not move
--    the clock further than Maximum_Unauthenticated_Step_MS from the boot
--    RTC reading, so an attacker cannot walk it away in small steps.
--  * Once time is authenticated, unauthenticated samples cannot replace it.
--  Samples are applied as steps; gradual slewing is not implemented.
package Clock_Discipline with SPARK_Mode is
   Maximum_Sample_Age_MS : constant := 10_000;
   Maximum_Uncertainty_MS : constant := 1_000;
   Maximum_Unauthenticated_Step_MS : constant := 15 * 60 * 1_000;
   Minimum_Unauthenticated_Sources : constant := 2;
   --  Last millisecond of 2099, the end of the bundled time-zone tables.
   Latest_UTC_MS : constant := 4_102_444_799_999;

   subtype UTC_Milliseconds is Unsigned_64 range 0 .. Latest_UTC_MS;

   type State is private;

   --  Start from this boot's RTC sample, if any. Valid only when the sample
   --  is at or above the floor.
   procedure Initialize
     (Item : out State; RTC_Available : Boolean;
      RTC_UTC_Seconds, RTC_Monotonic_MS : Unsigned_64;
      Floor_MS : UTC_Milliseconds);

   function Quality (Item : State) return Time_Quality;
   function Available (Item : State) return Boolean;

   --  Current UTC at a monotonic instant. Unavailable before any valid
   --  source, if Now precedes the anchor, or past Latest_UTC_MS.
   procedure Current
     (Item : State; Now_MS : Unsigned_64; UTC_MS : out UTC_Milliseconds;
      Success : out Boolean);

   procedure Apply
     (Item : in out State; Candidate : Sample; Now_MS : Unsigned_64;
      Floor_MS : UTC_Milliseconds; Result : out Outcome)
   with
     Post =>
       (if Result /= Stepped then Item = Item'Old) and then
       (if Result = Stepped then
          Available (Item) and then
          Quality (Item) = (if Candidate.Authenticated then Network_Authenticated
                            else Network_Unauthenticated) and then
          (not Candidate.Authenticated or else
           Candidate.Sources >= 1) and then
          (Candidate.Authenticated or else
           (Candidate.Sources >= Minimum_Unauthenticated_Sources and then
            Quality (Item'Old) /= Network_Authenticated)));
private
   type State is record
      Valid : Boolean := False;
      Base_UTC_MS : UTC_Milliseconds := 0;
      Base_Monotonic_MS : Unsigned_64 := 0;
      RTC_Valid : Boolean := False;
      RTC_UTC_MS : UTC_Milliseconds := 0;
      RTC_Monotonic_MS : Unsigned_64 := 0;
      Current_Quality : Time_Quality := Unknown_Time;
   end record;

   function Quality (Item : State) return Time_Quality is (Item.Current_Quality);
   function Available (Item : State) return Boolean is (Item.Valid);
end Clock_Discipline;
