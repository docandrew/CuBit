-- One process-lifetime, descriptor-sized owner. Start latches before any work
-- and never retries AML startup or service registration after a failed attempt.
package ACPI_Native_Instance is
   procedure Start;
end ACPI_Native_Instance;
