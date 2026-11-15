pragma Ada_2022;
with ACPI_Service;
-- Hosted only: synthetic no-Field AML uses the real Core directly. This
-- models the local owner callbacks, not its hardware/cache/lease lifecycle.
-- Native ACPI_One_Service is checked separately against the actual SDK.
package ACPI_One_Service is
   package Root is
      package Evaluation is
         procedure Evaluate_Named
           (Service:aliased in out ACPI_Service.State;Item:ACPI_Service.Node_Descriptor;
            Budget:Natural;Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status)
           renames ACPI_Service.Evaluate_Named;
         procedure Invoke_With_Arguments
           (Service:aliased in out ACPI_Service.State;Node:ACPI_Service.Namespace.Node_ID;
            Args:ACPI_Service.Values.Arguments;Argument_Count:ACPI_Service.Values.Argument_Count;
            Budget:Natural;Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status)
           renames ACPI_Service.Invoke_With_Arguments;
      end Evaluation;
   end Root;
end ACPI_One_Service;
