pragma Ada_2022;
with ACPI_Requests.EC_Boot_Notes;
with ACPI_EC_Boot_Diagnostics;
with ACPI_Native_Log;
package body ACPI_Native_EC_Boot_Notes is
   procedure Observe (Server : ACPI_Requests.State;
      Limit : ACPI_EC_Boot_Observations.Inspection_Budget :=
        ACPI_EC_Boot_Observations.Default_Inspection_Bytes) is
      package O renames ACPI_EC_Boot_Observations;
      use type O.Outcome;
      Result : O.Report;
   begin
      ACPI_Requests.EC_Boot_Notes.Observe (Server, Result, Limit);
      if ACPI_EC_Boot_Diagnostics.Described (Result) or else Result.Status = O.Missing_ECDT then
         ACPI_Native_Log.Info (ACPI_EC_Boot_Diagnostics.Format (Result));
      else
         ACPI_Native_Log.Warning (ACPI_EC_Boot_Diagnostics.Format (Result));
      end if;
   end Observe;
end ACPI_Native_EC_Boot_Notes;
