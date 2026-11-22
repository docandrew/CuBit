pragma Ada_2022;
with ACPI_EC_Boot_Observations;
package ACPI_Requests.EC_Boot_Notes with SPARK_Mode is
   -- In-process immutable-table diagnostic only; no new wire label/authority.
   procedure Observe (Server : State;
      Result : out ACPI_EC_Boot_Observations.Report;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes)
     with Pre => Valid (Server);
end ACPI_Requests.EC_Boot_Notes;
