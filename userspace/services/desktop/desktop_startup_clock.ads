with Interfaces;
--  Elapsed time for startup checkpoints (Desktop_Breadcrumbs), so a
--  hardware boot shows where startup time goes. Read from the kernel clock:
--  an external input to SPARK callers.
package Desktop_Startup_Clock with SPARK_Mode,
  Abstract_State => (Clock with External => Async_Writers)
is
   --  Milliseconds since the first call (the first checkpoint).
   function Elapsed_Ms return Interfaces.Unsigned_64
     with Volatile_Function, Global => Clock;
end Desktop_Startup_Clock;
