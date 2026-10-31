with ACPI_Native_Log;
package body ACPI_Native_Module_Notes is
   use type ACPI_Module_Diagnostics.Observation;
   use type ACPI_Module_Diagnostics.Count;
   Last : ACPI_Module_Diagnostics.Observation;
   procedure Observe (Item : ACPI_Module_Diagnostics.Observation) is
   begin
      if Item.Count = 0 or else Item = Last then return; end if;
      -- Commit before logging so an observer/logger failure never retries AML
      -- or floods the next loop turn. Changed last data still emits at saturation.
      Last := Item;
      ACPI_Native_Log.Warning (ACPI_Module_Diagnostics.Format (Item));
   end Observe;
end ACPI_Native_Module_Notes;
