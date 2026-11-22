pragma Ada_2022;
pragma SPARK_Mode (On);
with AML_Namespace;
with AML_Delays;
-- Match the current service namespace capacity without an OS/provider instance.
package ACPI_Namespace_Proof is new AML_Namespace
  (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 512);
