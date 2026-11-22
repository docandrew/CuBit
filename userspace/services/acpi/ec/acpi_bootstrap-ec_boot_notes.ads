pragma Ada_2022;
with ACPI_EC_Boot_Observations;
package ACPI_Bootstrap.EC_Boot_Notes with SPARK_Mode is
   -- The catalog must be complete, but hardware-backed device initialization
   -- need not have succeeded. Reads no prefix during collection, runs no AML,
   -- and does not change session, notification, result or hardware ownership.
   procedure Observe (Session : State;
      Result : out ACPI_EC_Boot_Observations.Report;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes)
     with Pre => Valid (Session);
end ACPI_Bootstrap.EC_Boot_Notes;
