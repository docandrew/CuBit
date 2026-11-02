pragma Ada_2022;
with ACPI_Service_Core;
with AML_Delays;
package Locked_Test_Core is new ACPI_Service_Core
  (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64, Retained_Result_Capacity => 16);
