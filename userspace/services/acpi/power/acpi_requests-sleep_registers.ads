pragma Ada_2022;
with ACPI_FADT.Sleep_Registers;
package ACPI_Requests.Sleep_Registers with SPARK_Mode is
   procedure Observe (Server : State; Result : out ACPI_FADT.Sleep_Registers.Source_Report)
     with Pre => Valid (Server);
end ACPI_Requests.Sleep_Registers;
