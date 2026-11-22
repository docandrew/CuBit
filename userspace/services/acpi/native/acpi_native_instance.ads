-- One process-lifetime, descriptor-sized owner. Start latches before any work
-- and never retries AML startup or service registration after a failed attempt.
package ACPI_Native_Instance is
   procedure Start;
   -- After Start returns: the process served, was asked to Stop (procmgr's
   -- control event: a live replacement or a shutdown), and the shutdown
   -- controller retired the root QUIESCENT. Main exits 0 only then: for a
   -- (replace graceful) entry, code 0 is the promise that every resource
   -- was released (Service_Switches). Any other return (startup did not
   -- reach serving) exits failing. A root that cannot retire never returns
   -- (it parks).
   function Stopped_And_Retired return Boolean;
end ACPI_Native_Instance;
