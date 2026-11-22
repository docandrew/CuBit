pragma Ada_2022;
with GPIO_Service_Pump;
with GPIO_Loop_Time;
generic
   with package Pump is new GPIO_Service_Pump (<>);
   with procedure Clock (Now : out Pump.Admin.I.Tick; Available : out Boolean);
   Poll_Interval, Poll_Window, Progress_Interval : GPIO_Loop_Time.Duration;
package GPIO_Polling_Loop is
   package Admin renames Pump.Admin;
   package R renames Admin.Registry;
   package I renames R.I;
   package Time renames GPIO_Loop_Time;
   type Clock_Condition is (Sampled, Unavailable, Reversed, Exhausted);
   type Turn_Report is record
      Work : Pump.Turn_Report;
      Poll_Started, Supervision_Required : Boolean := False;
      Clock_Status : Clock_Condition := Sampled;
      Wait_For : Time.Wait_Plan;
   end record;
   procedure Turn (S : in out Admin.State; Result : out Turn_Report);
   -- Polling-only mode: never binds/enables/ACKs a parent IRQ. Only the sealed
   -- admitted armed inputs are polled; retained/delivered masked events wait
   -- for backpressure/exact client ACK. No MMIO in this scheduler: Pump remains
   -- sole controller advancer. Interval/window/progress periods are explicit.
   -- Main handles Terminal/Supervision_Required before waiting; uncertainty
   -- never becomes idle readiness or permission to release resources.
   -- Main executes Wait_For between Turns; idle waits on activity, armed inputs
   -- get finite poll wakes, pending work gets finite progress wakes. Bad clocks
   -- latch stop and use relative finite backoff while bounded cleanup proceeds.
end GPIO_Polling_Loop;
