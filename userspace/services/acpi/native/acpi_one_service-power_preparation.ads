pragma Ada_2022;
with Interfaces; with ACPI_Sleep_Preparation; with ACPI_Sleep_Objects;
with ACPI_Sleep_Controller; with ACPI_Requests.Power_Preparation;
package ACPI_One_Service.Power_Preparation is
   procedure Read_Clock (Now_Microseconds : out Interfaces.Unsigned_64; Available : out Boolean);
   function May_Continue (Service : ACPI_Service.State) return Boolean;
   package Methods is new ACPI_Sleep_Preparation (ACPI_Service, Runtime.Invoke_Preparation);
   package Objects is new ACPI_Sleep_Objects (ACPI_Service, Runtime.Evaluate_Preparation);
   package Controller is new ACPI_Sleep_Controller (ACPI_Service, Methods, Objects, Read_Clock, May_Continue);
   package Requests is new ACPI_Requests.Power_Preparation
     (Methods, Objects, Controller, Runtime.Begin_Event, Runtime.End_Event);
end ACPI_One_Service.Power_Preparation;
