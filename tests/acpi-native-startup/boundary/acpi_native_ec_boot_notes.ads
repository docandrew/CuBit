pragma Ada_2022;
with ACPI_Requests;
with ACPI_EC_Boot_Observations;
package ACPI_Native_EC_Boot_Notes is
   -- Serialized startup observation only. No AML, provider installation,
   -- reservation, namespace association or GPE/IRQ enrollment is performed.
   procedure Observe (Server : ACPI_Requests.State;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes);
end ACPI_Native_EC_Boot_Notes;
