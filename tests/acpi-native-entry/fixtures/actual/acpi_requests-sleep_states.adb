pragma Ada_2022;
package body ACPI_Requests.Sleep_States with SPARK_Mode is
   procedure Observe (Server : in out State; Registers : Driver.Register_Model;
      Budget : Natural; Result : out Controller.Report) is
   begin Controller.Observe (Server.Boot, Registers, Budget, Result); end Observe;
end ACPI_Requests.Sleep_States;
