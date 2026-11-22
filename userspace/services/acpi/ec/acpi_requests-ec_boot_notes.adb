pragma Ada_2022;
with ACPI_Bootstrap.EC_Boot_Notes;
package body ACPI_Requests.EC_Boot_Notes with SPARK_Mode is
   procedure Observe (Server : State;
      Result : out ACPI_EC_Boot_Observations.Report;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes) is
   begin
      ACPI_Bootstrap.EC_Boot_Notes.Observe (Server.Boot, Result, Limit);
   end Observe;
end ACPI_Requests.EC_Boot_Notes;
