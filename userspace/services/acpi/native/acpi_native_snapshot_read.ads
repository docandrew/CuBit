pragma Ada_2022;
with ACPI_Snapshot_Reader;
with CuBit.Firmware_Snapshot;
package ACPI_Native_Snapshot_Read is
   package R renames ACPI_Snapshot_Reader;
   package K renames CuBit.Firmware_Snapshot;
   type Kernel_Observation is record
      Available : Boolean := False;
      Table : K.Table_ID := 0;
      Offset, Capacity, Copied : K.Byte_Count := 0;
      Status : K.Outcome := K.Malformed;
   end record;
   procedure Copy_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status;
      Observation : out Kernel_Observation);
   type Report is record
      Copy : R.Report;
      -- Own offset/table/capacity prevent later clock failures from relabeling
      -- the last actual kernel copy. Available=False means no call occurred.
      Last_Kernel_Call : Kernel_Observation;
   end record;
   procedure Read
     (Item : R.S.Public_Descriptor; Deadline : R.Tick;
      Data : out R.F.Bytes; Result : out Report);
end ACPI_Native_Snapshot_Read;
