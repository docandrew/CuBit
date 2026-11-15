pragma Ada_2022;
with ACPI_Native_PCI_Windows;
with ACPI_PCI_Protocol;
package body ACPI_Native_PCI_Provider is
   package Native renames ACPI_Native_PCI_Windows;
   procedure Prepare (Key : P.Bus_Key; Deadline_Milliseconds : R.Word; Result : out R.Window_Description) is
   begin
      Native.Prepare_Local (Scope,Key,ACPI_PCI_Protocol.Wire.Time_Instant (Deadline_Milliseconds),
         Setup_Allowed,Result);
   end Prepare;
   procedure Inspect (Epoch,ID : R.Word; Result : out R.Window_Description) is
   begin Native.Describe_Local (Scope,Epoch,ID,Result); end Inspect;
   procedure Read_Data (Epoch,ID : R.Word; Region : P.Region; Offset : R.Word;
      Width : P.T.Access_Width; Deadline_Microseconds : R.Word;
      Guard : not null access function return Boolean; Result : out R.Result) is
   begin Native.Read_Local (Scope,Epoch,ID,Region,Offset,Width,Deadline_Microseconds,Guard,Result); end Read_Data;
   function Backing_Pending return Boolean is (Native.Pending);
end ACPI_Native_PCI_Provider;
