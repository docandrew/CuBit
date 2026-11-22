pragma Ada_2022;
with ACPI_Service;
with ACPI_Requests.HID_Offers;
with ACPI_One_Service;
with ACPI_Native_HID_Detection;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Resolved;
with ACPI_HID_Offer_Builder;
with ACPI_HID_Offer_Provenance;
with ACPI_PCI_Observation_Reader;
-- Fixed native binding: fresh probes and PCI queries use the SAME runtime
-- evaluator and private Requests Core as the service's event owner. Startup's
-- closed setup evaluator is deliberately not used after Event_Domain.Start.
-- This package does not open the runtime window or send offers. The serialized
-- native instance must own a process-lived Controller.Run, open/close the
-- original-deadline event window, and complete all startup AML before transport.
package ACPI_Native_HID_Collection is
   package Candidates renames ACPI_Native_HID_Detection.Candidates;
   package Discovery is new ACPI_HID_I2C_Discovery
     (ACPI_Service, ACPI_One_Service.Runtime.Invoke_With_Arguments,
      ACPI_One_Service.Runtime.Evaluate_Named);
   package Resolver is new ACPI_HID_I2C_Resolved (ACPI_Service, Candidates);
   package Builder is new ACPI_HID_Offer_Builder (Candidates);
   package Provenance is new ACPI_HID_Offer_Provenance
     (ACPI_Service, Candidates, Resolver, Builder);
   package Reader is new ACPI_PCI_Observation_Reader
     (ACPI_Service, ACPI_One_Service.Runtime.Evaluate_Named);
   package Controller is new ACPI_Requests.HID_Offers
     (Candidates, Discovery, Resolver, Builder, Provenance, Reader);
   pragma Compile_Time_Error
     (Controller.Maximum_Candidates /= ACPI_Native_HID_Detection.Maximum_Candidates,
      "native HID discovery and collection capacities must agree");
end ACPI_Native_HID_Collection;
