pragma Ada_2022;
with Interfaces;
with ACPI_Battery_Observation_Protocol;
with ACPI_Requests;
with CuBit.Messages;
-- Fixed copied-inventory queries. Received message/stamp are kernel supplied;
-- local admission is not a request flag. No battery AML acquisition here.
package ACPI_Native_Cached_Battery is
   Metrics_Battery_Page : constant Interfaces.Unsigned_64 := ACPI_Battery_Observation_Protocol.Battery_Page;
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Read_Allowed : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean)
     with Pre => ACPI_Requests.Valid (Server);
end ACPI_Native_Cached_Battery;
