with Owner_Test_Control;
package body ACPI_Native_Snapshot_Read is
   procedure Copy_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status;
      Observation : out Kernel_Observation) is
      pragma Unreferenced (ID, Offset);
   begin
      Into := [others => 0]; Copied := 0; Status := R.Invalid_Request;
      Observation := (others => <>);
      Owner_Test_Control.Native_Reads := Owner_Test_Control.Native_Reads + 1;
      raise Program_Error with "unexpected real-adapter call in owner model";
   end Copy_Part;
   procedure Read
     (Item : R.S.Public_Descriptor; Deadline : R.Tick;
      Data : out R.F.Bytes; Result : out Report) is
      pragma Unreferenced (Item, Deadline);
   begin
      Data := [others => 0]; Result := (others => <>);
      Owner_Test_Control.Native_Reads := Owner_Test_Control.Native_Reads + 1;
      raise Program_Error with "unexpected real-adapter call in owner model";
   end Read;
end ACPI_Native_Snapshot_Read;
