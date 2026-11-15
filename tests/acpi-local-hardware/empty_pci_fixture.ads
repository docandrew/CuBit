pragma Ada_2022;
with ACPI_Local_PCI_Provider;
with ACPI_Local_PCI_Results;
with ACPI_PCI_Bus_Geometry;
generic
   with function Access_Allowed (For_Write : Boolean) return Boolean;
package Empty_PCI_Fixture is
   package R renames ACPI_Local_PCI_Results;
   package P renames ACPI_PCI_Bus_Geometry.P;
   function Not_Pending return Boolean is (False);
   function No_Setup return Boolean is (False);
   procedure Prepare (Key : P.Bus_Key; Deadline : R.Word; Result : out R.Window_Description);
   procedure Inspect (Epoch,ID : R.Word; Result : out R.Window_Description);
   procedure Read_Data (Epoch,ID : R.Word; Region : P.Region; Offset : R.Word; Width : P.T.Access_Width;
      Deadline : R.Word; Guard : not null access function return Boolean; Result : out R.Result);
   procedure Deadlines (Setup,Data : out R.Word; Available : out Boolean);
   package Provider is new ACPI_Local_PCI_Provider
      (Prepare,Inspect,Read_Data,Not_Pending,No_Setup,Access_Allowed,Deadlines);
end Empty_PCI_Fixture;
