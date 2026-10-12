with Interfaces;
with Desktop_Bootstrap_Notes;
--  Log records for the Desktop bootstrap screen, read through Desktop's
--  log-observer binding (the boot-logs reader). Never blocks on logstore:
--  each call to it is bounded by Call_Limit_Ms, a refused or slow logstore
--  leaves the notes as they are, and a new subscription is tried at most
--  every Retry_Ms.
package Desktop_Bootstrap_Logs is
   Call_Limit_Ms : constant := 50;
   Retry_Ms      : constant := 2_000;
   --  Read what has arrived, for at most Budget_Ms, into Notes.
   procedure Gather (Budget_Ms : Interfaces.Unsigned_64);
   function Current return Desktop_Bootstrap_Notes.Notes;

   --  Before the bootstrap frame: gather until every device-memory refusal
   --  the kernel has counted (System facts) has arrived as a record and at
   --  least Minimum_Wait_Ms have passed (one logstore boot poll, so
   --  intel-gpu's startup warnings get through too), or Maximum_Wait_Ms.
   Minimum_Wait_Ms : constant := 250;
   Maximum_Wait_Ms : constant := 1_000;
   procedure Gather_Before_Bootstrap;
   --  Device-memory refusals the kernel has counted, read directly.
   function Kernel_Denials return Interfaces.Unsigned_64;
   --  User processes the kernel has killed for a fault, read directly.
   function Kernel_Kills return Interfaces.Unsigned_64;
   --  Desktop_Startup_Clock milliseconds when the notes were last gathered.
   function Gathered_At_Ms return Interfaces.Unsigned_64;
end Desktop_Bootstrap_Logs;
