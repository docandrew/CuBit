pragma Ada_2022;
with GPIO_Controller_Resources;
with GPIO_Mapping_Policy;
with GPIO_Loop_Time;
-- Compiled driver policy, not firmware facts or launcher-supplied parameters.
-- devmgr's generic driver path does not supply typed parameter blocks.
package GPIO_Native_Policy with Pure, SPARK_Mode is
   use type GPIO_Mapping_Policy.Address;
   Maximum_Community_Bytes : constant GPIO_Mapping_Policy.Address := 65_536;
   Maximum_Operation_Budget : constant := 65_535;
   Maximum_Burst : constant := 64;
   type Budget is range 1 .. Maximum_Operation_Budget;
   type Burst is range 1 .. Maximum_Burst;
   subtype Milliseconds is GPIO_Loop_Time.Duration;
   subtype Pages is GPIO_Mapping_Policy.Address range 1 .. GPIO_Mapping_Policy.Maximum_Map_Pages;
   type Work_Limits is record
      events, requests, steps : Burst;
   end record;
   type Timing_Limits is record
      startup, admission, cleanup, readiness, sweep, progress, poll, poll_window : Milliseconds;
   end record;
   type Settings is record
      capacity : GPIO_Controller_Resources.Pin_Limit;
      operation_budget, startup_budget : Budget;
      pages_per_resource : Pages;
      work : Work_Limits;
      timing : Timing_Limits;
   end record;
   Value : constant Settings :=
     (capacity => GPIO_Controller_Resources.Pin_Limit'Last,
      operation_budget => 32, startup_budget => 4096,
      pages_per_resource => Maximum_Community_Bytes / GPIO_Mapping_Policy.Page_Bytes,
      work => (events => 2, requests => 2, steps => 4),
      timing => (startup => 5000, admission => 1000, cleanup => 1000,
         readiness => 1000, sweep => 100, progress => 1, poll => 5, poll_window => 100));
end GPIO_Native_Policy;
