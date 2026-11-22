pragma Ada_2022;
with Interfaces; with ACPI_Service; with ACPI_Sleep_Preparation; with ACPI_Sleep_Objects;
with ACPI_Sleep_Controller; with ACPI_Requests.Power_Preparation; with ACPI_Local_Runtime_Status;
package ACPI_One_Service.Power_Preparation is
   procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64; Available : out Boolean);
   function May_Continue (Service : ACPI_Service.State) return Boolean;
   procedure Begin_Event (Service : ACPI_Service.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   package Methods is new ACPI_Sleep_Preparation (ACPI_Service);
   package Objects is new ACPI_Sleep_Objects (ACPI_Service);
   package Controller is new ACPI_Sleep_Controller (ACPI_Service, Methods, Objects, Read_Clock, May_Continue);
   package Requests is new ACPI_Requests.Power_Preparation (Methods, Objects, Controller, Begin_Event, End_Event);
end ACPI_One_Service.Power_Preparation;
