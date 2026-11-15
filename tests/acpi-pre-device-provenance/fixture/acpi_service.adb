-- Hosted test model only; never a native runtime/provider implementation.
with Prep_Model;
package body ACPI_Service is function Retained_Results(Service : State) return Natural is pragma Unreferenced(Service); begin return Prep_Model.Roots; end Retained_Results; end ACPI_Service;