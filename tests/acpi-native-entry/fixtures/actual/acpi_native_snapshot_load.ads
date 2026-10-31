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
private
   -- Shared native source adapters. They observe only generated Startup.Tables
   -- and write diagnostic values; no service/context/buffer aliases are saved.
   procedure Read_Catalog
     (Diagnostics : in out Report; Item : out L.Inventory;
      Status : out L.R.Copy_Status);
   procedure Read_Description
     (Diagnostics : in out Report; ID : Positive; Item : out L.Description;
      Status : out L.R.Copy_Status);
   procedure Read_Chunk
     (Diagnostics : in out Report; ID : L.R.S.Table_ID; Offset : Natural;
      Into : out L.R.F.Bytes; Copied : out L.R.Byte_Count;
      Status : out L.R.Copy_Status);
   procedure Read_Clock (Now : out L.R.Tick; Available : out Boolean);
end ACPI_Native_Snapshot_Load;
