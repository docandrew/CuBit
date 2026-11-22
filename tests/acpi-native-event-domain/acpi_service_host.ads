with AML_Delays;
with ACPI_Service_Core;
-- Hosted configuration witness only; native fixture retains its16K namespace,
-- real timing/delay/provider callbacks and separate semantic evidence.
package ACPI_Service is new ACPI_Service_Core
  (Perform_Delay=>AML_Delays.Unavailable_Provider,Namespace_Node_Capacity=>512,Retained_Result_Capacity=>16);
