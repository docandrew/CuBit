pragma Ada_2022;
with I2C_Transfer_Wire;
with Intel_LPSS_I2C_Profile;
package I2C_Work_Policy with SPARK_Mode is
   type Steps is range 1 .. 2 ** 32 - 1;
   Work_Burst : constant := 256;
   Poll_Milliseconds : constant := 1;
   Base_Steps : constant := 16_384;
   function Budget
     (Writes, Reads : I2C_Transfer_Wire.Count;
      Timing : Intel_LPSS_I2C_Profile.Timing) return Steps;
   -- Finite driver work policy, independent of caller deadlines. Allows four
   -- modeled wire periods per byte and two planner accesses per byte, plus
   -- fixed configuration/quiescence headroom. It scales with the selected
   -- counters rather than assuming all clients run at the fastest bus rate.
   -- Electrical timing/root clock are profile assumptions; arbitrary clock
   -- stretching is bounded by this policy and/or the caller's deadline.
end I2C_Work_Policy;
