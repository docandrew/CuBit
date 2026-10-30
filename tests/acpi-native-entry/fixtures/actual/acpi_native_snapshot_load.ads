pragma Ada_2022;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Read;
package ACPI_Native_Snapshot_Load is
   package L renames ACPI_Snapshot_Startup;
   type Report is record
      Startup : L.Report;
      Last_Copy : ACPI_Native_Snapshot_Read.Kernel_Observation;
      Catalog_Status, Descriptor_Status : ACPI_Native_Snapshot_Read.K.Outcome :=
        ACPI_Native_Snapshot_Read.K.Malformed;
   end record;
   procedure Load
     (Deadline : L.R.Tick; Owner : out L.State_Access; Result : out Report);
end ACPI_Native_Snapshot_Load;
