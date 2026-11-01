pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Catalog;
with ACPI_Backend_NVS_Records;
with ACPI_FADT.Transactions;
generic
   with package Catalog is new ACPI_Backend_Catalog
     (Memory_Authority => CuBit.Handles.Device_Memory, No_Memory => <>,
      Port_Authority => CuBit.Handles.IO_Ports, No_Ports => <>, others => <>);
   Hardware : in out Catalog.Context;
   with function Is_NVS (Window : Catalog.W.Window_ID) return Boolean;
   Maximum_Records : Positive := 16;
package ACPI_Backend_NVS_Native is
   -- One exact load through the retained view selected by Records.Read.
   -- This is never a public address-based request entry point.
   procedure Transfer
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean);
   package Records is new ACPI_Backend_NVS_Records (Catalog, Hardware, Is_NVS, Transfer, Maximum_Records);
end ACPI_Backend_NVS_Native;
