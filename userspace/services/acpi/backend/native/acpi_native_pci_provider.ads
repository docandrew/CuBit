pragma Ada_2022;
with ACPI_Local_PCI_Provider;
with ACPI_Local_PCI_Results;
with ACPI_PCI_Bus_Geometry;
with AML_Field_Layout;
generic
   Scope : ACPI_Local_PCI_Results.Word;
   with function Setup_Allowed return Boolean;
   with function Access_Allowed (For_Write : Boolean) return Boolean;
   with procedure Deadlines (Setup_Milliseconds,Data_Microseconds : out ACPI_Local_PCI_Results.Word;
      Available : out Boolean);
   with procedure Resolve_Alias (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Bus_Geometry.P.Bus_Key; Accepted : in out Boolean) is null;
package ACPI_Native_PCI_Provider is
   package G renames ACPI_PCI_Bus_Geometry;
   package P renames G.P;
   package R renames ACPI_Local_PCI_Results;
   -- Root supplies its authenticated scope and same retained Field predicate;
   -- Setup_Allowed must forbid every held/pending/uncertain physical lease.
   -- Native window owner and exclusive arena are initialized separately once.
   procedure Prepare (Key : P.Bus_Key; Deadline_Milliseconds : R.Word; Result : out R.Window_Description);
   procedure Inspect (Epoch,ID : R.Word; Result : out R.Window_Description);
   procedure Read_Data (Epoch,ID : R.Word; Region : P.Region; Offset : R.Word;
      Width : P.T.Access_Width; Deadline_Microseconds : R.Word;
      Guard : not null access function return Boolean; Result : out R.Result);
   function Backing_Pending return Boolean;
   package Provider is new ACPI_Local_PCI_Provider
      (Prepare,Inspect,Read_Data,Backing_Pending,Setup_Allowed,Access_Allowed,Deadlines,Resolve_Alias);
end ACPI_Native_PCI_Provider;
