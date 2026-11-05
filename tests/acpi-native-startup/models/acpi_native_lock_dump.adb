pragma Ada_2022;
-- No diagnostic IPC in owner-control scenarios; actual dump transport is
-- checked separately by native-entry and lock trace/model tests.
package body ACPI_Native_Lock_Dump is
 procedure Request (Endpoint : CuBit.Handles.Startup.Optional_Endpoint) is
  pragma Unreferenced(Endpoint);
 begin null; end Request;
end ACPI_Native_Lock_Dump;
