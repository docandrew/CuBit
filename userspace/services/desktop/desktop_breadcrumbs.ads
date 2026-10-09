with Desktop_Logs;
with Desktop_Log_IO;
with Desktop_Startup_Clock;
package Desktop_Breadcrumbs with SPARK_Mode, Abstract_State => State,
  Initializes => State is
 type Stage is (Entered, Init_Before, Init_After, Health_Before, Health_After, Targets_Before, Targets_After, Pipeline_Before, Pipeline_After, Upload_Before, Upload_After, Readback_Before, Readback_After, Selected, Begin_Before, Begin_After, Deferred, Pending, Unsafe, Complete, Submit, Released);
 -- Each startup checkpoint is attempted once through Desktop's log channel,
 -- with the milliseconds since the first one (t=...ms).
 procedure Mark (Point : Stage)
   with Global => (Input => Desktop_Startup_Clock.Clock,
                   In_Out => (State, Desktop_Logs.State, Desktop_Log_IO.State));
end Desktop_Breadcrumbs;
