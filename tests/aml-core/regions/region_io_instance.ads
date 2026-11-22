pragma SPARK_Mode (On);
with ACPI_Region_IO;
with Region_Mock;
package Region_IO_Instance is new ACPI_Region_IO (Region_Mock.State, Region_Mock.Transact);
