pragma Ada_2022;
with ACPI_GPE_Methods;
with ACPI_Requests.GPE_Events;
-- Process-owned bridge only: this instantiation uses the existing Fields
-- runtime and Requests' private Core. It enrolls no hardware source by itself.
package ACPI_One_Service.GPE_Events is
   package Methods is new ACPI_GPE_Methods
     (Core => ACPI_Service, Evaluate_Named => Runtime.Evaluate_Named);
   package Requests is new ACPI_Requests.GPE_Events (Methods);
end ACPI_One_Service.GPE_Events;
