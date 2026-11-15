pragma Ada_2022;
with ACPI_Bootstrap;
with ACPI_Native_HID_Detection;
with ACPI_Native_Power_Detection;
with CuBit.Consumer_Initialization_Exchange;
with ACPI_Startup_Completion;
package ACPI_Native_Startup_Completion is
   -- Historical reports from the same serialized startup pass. This function
   -- performs no AML or I/O and does not establish transport quiescence.
   function Classify (Devices : ACPI_Bootstrap.Device_Report;
      HID : ACPI_Native_HID_Detection.Report;
      Power : ACPI_Native_Power_Detection.Report; Setup_Closed : Boolean;
      Events : ACPI_Startup_Completion.Phase_Result := ACPI_Startup_Completion.Finished;
      Module_Errors : Boolean := False)
      return CuBit.Consumer_Initialization_Exchange.Phase_Status;
end ACPI_Native_Startup_Completion;
