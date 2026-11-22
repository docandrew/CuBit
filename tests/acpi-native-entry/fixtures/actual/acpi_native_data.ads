pragma Ada_2022;
with ACPI_Native_Firmware_Data;
with CuBit.Handles.IPC;
package ACPI_Native_Data is new ACPI_Native_Firmware_Data (CuBit.Handles.IPC.Close);
