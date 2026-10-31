pragma Ada_2022;
package body ACPI_Resource_Reader with SPARK_Mode is
   package V renames Core.Values;
   package R renames ACPI_Resource_Data;
   use type V.Access_Status;
   use type V.Value_Kind;
   procedure Copy_Template
     (Service : Core.State; Handle : V.Value_Handle;
      Data : out Firmware_Tables.Bytes; Result : out Report;
      Maximum_Items : R.Item_Count := R.Default_Item_Limit) is
      Description : V.Value_Description;
      Status : V.Access_Status;
      Count : Natural := 0;
   begin
      Data := [others => 0];
      Result := (others => <>);
      Core.Describe_Result (Service, Handle, Description, Status);
      Result.Access_Status := Status;
      if Status /= V.Available then return; end if;
      if Description.Kind /= V.Buffer_Description then
         Result.Status := Not_A_Buffer; return;
      end if;
      Result.Required := R.Byte_Count (Description.Length);
      if Description.Length > Data'Length then
         Result.Status := Insufficient_Capacity; return;
      end if;
      if Description.Length > 0 then
         Core.Read_Result_Bytes (Service, Handle, 0,
           Data (Data'First .. Data'First + (Description.Length - 1)), Count, Status);
         Result.Access_Status := Status;
         if Status /= V.Available or else Count /= Description.Length then
            Data := [others => 0]; Result.Status := Read_Failed; return;
         end if;
      end if;
      Result.Framing := (if Count = 0 then R.Scan ([], Maximum_Items)
        else R.Scan (Data (Data'First .. Data'First + (Count - 1)), Maximum_Items));
      Result.Template_Checked := True;
      if Result.Framing.Status /= R.Complete then
         Data := [others => 0]; Result.Status := Invalid_Template; return;
      end if;
      Result.Copied := R.Byte_Count (Count);
      Result.Status := Copied_Template;
   end Copy_Template;
end ACPI_Resource_Reader;
