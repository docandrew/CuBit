pragma Ada_2022;
package body ACPI_Requests.Battery_Observations with SPARK_Mode is
   procedure Find (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Controller.Discovery_Report) is
   begin Controller.Find (Server.Boot, After, Budget, Device, Report); end Find;
   procedure Observe (Server : in out State; Device : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Admitted : out Boolean; Report : out Controller.Observation_Report) is
   begin Controller.Observe (Server.Boot, Device, Budget, Admitted, Report); end Observe;
end ACPI_Requests.Battery_Observations;
