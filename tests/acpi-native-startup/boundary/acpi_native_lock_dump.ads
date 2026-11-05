pragma Ada_2022;
with CuBit.Handles.Startup;
package ACPI_Native_Lock_Dump is
   -- One diagnostic call after startup has ended; no grants, retry or
   -- extension of the original AML/hardware-access startup deadline.
   procedure Request (Endpoint : CuBit.Handles.Startup.Optional_Endpoint);
end ACPI_Native_Lock_Dump;
