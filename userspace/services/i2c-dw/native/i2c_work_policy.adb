pragma Ada_2022;
with Interfaces;
package body I2C_Work_Policy with SPARK_Mode is
   subtype Wide is Interfaces.Unsigned_64;
   use type Wide;
   use type Intel_LPSS_I2C_Profile.Outcome;
   Milliseconds_Per_Second : constant Wide := 1000;
   -- Eight data bits, ACK, plus amortized address/START/STOP allowance.
   Wire_Bits_Per_Byte : constant Wide := 12;
   Timing_Overhead_Cycles : constant Wide := 16;
   Stretch_Allowance : constant Wide := 4;
   Planner_Accesses_Per_Byte : constant Wide := 2;
   function Budget
     (Writes, Reads : I2C_Transfer_Wire.Count;
      Timing : Intel_LPSS_I2C_Profile.Timing) return Steps
   is
      Root : constant Wide := Wide (Intel_LPSS_I2C_Profile.Root_Clock);
      Divisor : constant Wide := Root * Wide (Poll_Milliseconds);
      Per_Byte, Needed, Period : Wide;
   begin
      if Timing.Status /= Intel_LPSS_I2C_Profile.Calculated then return Base_Steps; end if;
      Period := Wide (Timing.High_Count) + Wide (Timing.Low_Count) + Timing_Overhead_Cycles;
      Per_Byte := Planner_Accesses_Per_Byte +
        (Wide (Work_Burst) * Milliseconds_Per_Second * Wire_Bits_Per_Byte * Period *
         Stretch_Allowance + Divisor - 1) / Divisor;
      Needed := Wide (Base_Steps) + (Wide (Writes) + Wide (Reads)) * Per_Byte;
      return (if Needed > Wide (Steps'Last) then Steps'Last else Steps (Needed));
   end Budget;
end I2C_Work_Policy;
