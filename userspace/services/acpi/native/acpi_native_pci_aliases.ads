pragma Ada_2022;
with ACPI_Native_Snapshot_Read;
with ACPI_PCI_Protocol;
with AML_Field_Layout;
package ACPI_Native_PCI_Aliases with SPARK_Mode => Off is
   package R renames ACPI_Native_Snapshot_Read.R;
   type Phase is (Fresh, Preparing, Ready, Absent, Failed);
   type Failure is (None, Already_Consumed, Invalid_Deadline, Clock_Unavailable,
      Clock_Reversed, Deadline_Expired, Source_Refused, Duplicate_Table,
      Invalid_Extent, Allocation_Refused, Copy_Refused, Invalid_Table);
   type Report is record
      Stage : Phase := Fresh;
      Why : Failure := None;
      Bytes, Allocations : Natural := 0;
      Copy : ACPI_Native_Snapshot_Read.Report;
   end record;
   function Name (Why : Failure) return String;
   -- One call before field/AML startup. Owns a complete, checked immutable
   -- snapshot copy for process lifetime. Failure leaves no published prefix.
   -- Deadline is absolute MICROSECONDS, in the snapshot clock epoch.
   procedure Initialize (Deadline : R.Tick; Result : out Report);
   -- No copying, allocator, hardware access, mapping or IPC. Metadata only;
   -- actual window authority is acquired and checked by the independent broker.
   procedure Resolve
     (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Protocol.P.Bus_Key; Accepted : in out Boolean);
end ACPI_Native_PCI_Aliases;
