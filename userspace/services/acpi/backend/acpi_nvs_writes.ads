pragma Ada_2022;
package ACPI_NVS_Writes with Pure, SPARK_Mode is
   -- Probe must have no hardware effect. Ready is accepted only for Probe;
   -- Completed only for a performed write. Default in-out Unavailable lets
   -- an omitted generic callback advertise and perform no writes.
   type Completion is (Unavailable, Ready, Completed, Unknown, Fault);
end ACPI_NVS_Writes;
