pragma Ada_2022;
with CuBit.Firmware_Snapshot;
with ACPI_Backend_FADT_Source;
with ACPI_FADT;
with ACPI_Snapshot_Reader;
package ACPI_Backend_FADT_Native is
   package K renames CuBit.Firmware_Snapshot;
   procedure Clock (Now : out ACPI_Snapshot_Reader.Tick; Available : out Boolean);
   package Source is new ACPI_Backend_FADT_Source (Clock, K.Chunk_Max);
   type Kernel_Copy is record
      Available : Boolean := False;
      ID : K.Table_ID := 0;
      Offset, Capacity, Copied : K.Byte_Count := 0;
      Status : K.Outcome := K.Malformed;
   end record;
   type Report is record
      Source_Result : Source.Report;
      Last_Kernel_Status : K.Outcome := K.Malformed;
      Last_Copy : Kernel_Copy;
   end record;
   -- One process-lifetime attempt; no authority or borrowed pointer survives
   -- the call. The owner passes its generated Startup.Tables authority.
   -- The exact-size owned allocation remains retained for diagnostics/lifetime.
   -- Native OOM terminates through the runtime; no exception recovery promise.
   procedure Read
     (Authority : K.Authority; Deadline : Source.R.Tick; Budget : Natural;
      Item : out ACPI_FADT.Description; Result : out Report);
end ACPI_Backend_FADT_Native;
