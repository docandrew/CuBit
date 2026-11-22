with Producer_Model; with Sender_Model;
package body ACPI_Native_HID_Collection is
   package M renames Producer_Model;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, ACPI_Service.Node_Descriptor;
   package body Controller is
      procedure Collect (Item : in out Run; Server : in out ACPI_Requests.State;
         Devices : Device_Array; Count : Candidate_Count;
         Admission_Device : ACPI_Service.Node_Descriptor;
         Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
         Budget : Natural; Resource_Bytes : Positive; Result : out Report) is
         pragma Unreferenced (Item, Server);
      begin
         M.Collections := M.Collections + 1;
         M.Check (M.Window_Open and then Sender_Model.Calls = 0 and then M.Collections = 1, "one fresh collection before all IPC");
         M.Check (Count = 2 and then Devices (1) = 11 and then Devices (2) = 12, "actual discovery descriptors forwarded");
         M.Check (Admission_Device = 90 and then Admission.Epoch = Sender_Model.Own_ID, "exact submitted GPIO owner forwarded");
         M.Check (History.Epoch = Sender_Model.Own_ID and then History.Catalog_Revision = 16#FFFF_FFFF_0000_0001#
           and then History.Observation_Sequence = 1 and then History.Initializer_Count = 17
           and then History.Initializer_Digest = 16#F111_2222_3333_4444#, "full-width actual owner stamps forwarded");
         M.Check (Budget = 100_000 and then Resource_Bytes = 65_536, "one configured collection budget");
         if M.Mode = 35 then raise Program_Error with "controlled collection interruption"; end if;
         Result := (Status => Prepared, Count => Count, Charged => 123, others => <>);
         Result.Status_Known := True; Result.Status_Flags := 15;
         if M.Mode in 31 .. 34 then
            Result.Status := Device_Unavailable; Result.Count := 0;
            Result.Status_Flags := (case M.Mode is when 31 => 0, when 32 => 1, when 33 => 2, when others => 8);
         elsif M.Mode = 11 then Result.Status := Stale_Device; Result.Count := 0;
         elsif M.Mode = 12 then Result.Status := Recovery_Required; Result.Recovery_Needed := True; M.Pending := True;
         elsif M.Mode = 39 then Result.Count := 1; Result.Node_Errors := 1;
         elsif M.Mode = 40 then Result.Count := 0; Result.Node_Errors := 2;
         end if;
         for I in 1 .. Count loop
            Result.Values (I).Epoch := History.Epoch; Result.Values (I).Observation_Sequence := 1;
            Result.Values (I).Catalog_Revision := History.Catalog_Revision;
            Result.Values (I).Maximum_Hz := 400000; Result.Values (I).Address := 21; Result.Values (I).Pin := 160;
            Result.Values (I).GPIO_HID := "INTC1057"; Result.Values (I).Table := "DSDT";
            Result.Values (I).Table_ID := "CUBIT   "; Result.Values (I).Device_Path.Count := 1;
            Result.Values (I).Device_Path.Names (1) := (if I = 1 then "TPD0" else "TPD1");
            Result.Values (I).I2C_Path.Count := 1; Result.Values (I).I2C_Path.Names (1) := "I2C1";
            Result.Values (I).GPIO_Path.Count := 1; Result.Values (I).GPIO_Path.Names (1) := "GPI0";
         end loop;
      end Collect;
   end Controller;
end ACPI_Native_HID_Collection;
