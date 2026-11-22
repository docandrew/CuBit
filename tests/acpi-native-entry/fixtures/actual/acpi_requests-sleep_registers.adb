pragma Ada_2022;
with ACPI_Bootstrap.Sleep_Registers;
package body ACPI_Requests.Sleep_Registers with SPARK_Mode is
   procedure Observe (Server : State; Result : out ACPI_FADT.Sleep_Registers.Source_Report) is
   begin ACPI_Bootstrap.Sleep_Registers.Observe (Server.Boot, Result); end Observe;
end ACPI_Requests.Sleep_Registers;
