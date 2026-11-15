pragma Ada_2022;
with ACPI_Platform_Pool_Native;
with ACPI_Native_PCI_Windows;
package body ACPI_Local_Root is
   function Control_Allowed return Boolean is
     (Fields.Control_Idle and then Hardware.May_Exit);
   -- Memory.Initialize independently requires Hardware.Admitting. Disposal
   -- may also run after stop, once physical control is idle; do not strand a
   -- rejected carriage merely because Fields has already closed admission.
   function Field_Access_Allowed (For_Write : Boolean) return Boolean is
     (Fields.Check_Field_Access (For_Write));
   function Backing_Quiet return Boolean is
      Local : constant Backing.Report := Backing.Observe;
      Configuration : constant ACPI_Native_PCI_Windows.Statistics := ACPI_Native_PCI_Windows.Observe;
      Pool_Busy : constant Boolean := Local.Pool.Stage in
        ACPI_Platform_Pool_Native.Owner.Working |
        ACPI_Platform_Pool_Native.Owner.Cleanup_Required |
        ACPI_Platform_Pool_Native.Owner.Uncertain;
   begin
      return not Pool_Busy and then Fields.Control_Idle
        and then not Backing.Regions.Cache.Pending (Backing.Memory)
        and then not Configuration.Working and then not Configuration.Data_Stopped
        and then Configuration.Cleanup_Pending = 0;
   end Backing_Quiet;
   procedure Read_Retirement (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Word) is
   begin
      Other_Owners_View (Quiescent,Wake_Deadline_Milliseconds);
      Quiescent := Quiescent and then Backing_Quiet;
      -- No synthetic deadline for unresolved local state; preserve the
      -- external observer's actual kernel-clock wake/retirement deadline.
   end Read_Retirement;
end ACPI_Local_Root;
