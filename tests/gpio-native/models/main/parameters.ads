pragma Ada_2022;
-- Host fixture: exact generated value type declarations; Value is modeled.
package Parameters is
   type GPIO_Capacity_Value is range 1 .. 257;
   type GPIO_Budget_Value is range 1 .. 65535;
   type GPIO_Pages_Value is range 1 .. 1024;
   type GPIO_Burst_Value is range 1 .. 64;
   type GPIO_Work_Value is record
      events : GPIO_Burst_Value := GPIO_Burst_Value'First;
      requests : GPIO_Burst_Value := GPIO_Burst_Value'First;
      steps : GPIO_Burst_Value := GPIO_Burst_Value'First;
   end record;
   type GPIO_Milliseconds_Value is range 1 .. 9223372036854775807;
   type GPIO_Timing_Value is record
      startup : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      admission : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      cleanup : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      readiness : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      poll_interval : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      poll_window : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
      progress : GPIO_Milliseconds_Value := GPIO_Milliseconds_Value'First;
   end record;
   type GPIO_Parameters_Value is record
      capacity : GPIO_Capacity_Value := GPIO_Capacity_Value'First;
      operation_budget : GPIO_Budget_Value := GPIO_Budget_Value'First;
      startup_budget : GPIO_Budget_Value := GPIO_Budget_Value'First;
      pages_per_resource : GPIO_Pages_Value := GPIO_Pages_Value'First;
      work : GPIO_Work_Value := (others => <>);
      timing : GPIO_Timing_Value := (others => <>);
   end record;

   function Value return GPIO_Parameters_Value;
end Parameters;
