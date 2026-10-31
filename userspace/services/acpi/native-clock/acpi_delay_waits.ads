pragma Ada_2022;
with Interfaces;
with AML_Delays;
-- Raw callbacks share a monotonic microsecond epoch, return normally in bounded
-- time, and never reenter. Sleep_Raw relinquishes the processor even for a due
-- target. No yield/sleep callback is invoked for Stall. Physical clock accuracy
-- and OS scheduling are trusted boundaries, not established by this policy.
generic
   with procedure Read_Raw (Value : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Sleep_Raw (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean);
   Maximum_Sleep_Slice : Positive;
package ACPI_Delay_Waits with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   Microseconds_Per_Millisecond : constant := 1_000;
   Maximum_Observations : constant := 65_536;
   subtype Observation_Count is Natural range 0 .. Maximum_Observations;
   subtype Observation_Budget is Observation_Count range 1 .. Maximum_Observations;
   type Status is (Not_Attempted, Completed, Clock_Unavailable,
      Clock_Regressed, Deadline_Overflow, Deadline_Expired,
      Delay_Exceeds_Deadline, Sleep_Refused, Observation_Limit);
   type Report is record
      Code : Status := Not_Attempted;
      Began, Target, Last : Tick := 0;
      Samples, Sleeps : Observation_Count := 0;
   end record;
   function Duration (Item : AML_Delays.Request) return Tick;
   -- Every attempt, including stopped/unavailable clocks, consumes a sample.
   -- Early wake is not completion. The absolute owner deadline and sample
   -- budget are checked throughout; neither is reset by a wake or retry.
   -- Zero Sleep still relinquishes the CPU; zero Stall needs no extra sample.
   procedure Run (Item : AML_Delays.Request; Deadline : Tick;
      Budget : Observation_Budget; Result : out Report)
     with Post => Result.Samples <= Budget
       and then (if Result.Code = Completed then
          Result.Samples > 0 and then Result.Last >= Result.Target
          and then Result.Last < Deadline);
end ACPI_Delay_Waits;
