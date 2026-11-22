pragma Ada_2022;
with Interfaces;
with ACPI_Power_Observation_Protocol;
with ACPI_Requests;
with CuBit.Messages;
-- Fixed copied-cache queries on the existing observer capability. The received
-- message and stamp come from the kernel; local Read_Allowed is not a wire flag.
package ACPI_Native_Cached_Power is
   Metrics_Lid_Page : constant Interfaces.Unsigned_64 := ACPI_Power_Observation_Protocol.Lid_Page;
   Metrics_Source_Page : constant Interfaces.Unsigned_64 := ACPI_Power_Observation_Protocol.Source_Page;
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Read_Allowed : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean)
     with Pre => ACPI_Requests.Valid (Server);
end ACPI_Native_Cached_Power;
