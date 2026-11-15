pragma Ada_2022;
generic
   -- Trusted serialized owner: bounded attributable result/backing cleanup
   -- only. Preserve Core/catalog identity and all other outstanding owners.
   -- No AML evaluation, hardware I/O, synchronous peer call, reset or replay.
   with procedure Step (Service : aliased in out ACPI_Service.State; Completed : out Boolean);
package ACPI_Bootstrap.With_Owner_Cleanup with SPARK_Mode is
   -- Unlike runtime maintenance, failure/unfinished device initialization
   -- and retained results do not bar cleanup of those very result owners.
   procedure Advance (Session : in out State; Completed : out Boolean)
      with Pre=>Valid(Session),Post=>Valid(Session);
end ACPI_Bootstrap.With_Owner_Cleanup;
