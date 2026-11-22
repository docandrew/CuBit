pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Resource_Data;
with Firmware_Tables;
-- Borrows an existing retained result. Never invokes AML or releases the handle.
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Resource_Reader with SPARK_Mode is
   use type ACPI_Resource_Data.Byte_Count;
   use type ACPI_Resource_Data.Template_Status;
   use type Firmware_Tables.Byte;
   type Copy_Status is (Copied_Template, Invalid_Result, Not_A_Buffer,
      Insufficient_Capacity, Read_Failed, Invalid_Template);
   type Report is record
      Status : Copy_Status := Invalid_Result;
      Access_Status : Core.Values.Access_Status := Core.Values.Invalid_Value;
      Required, Copied : ACPI_Resource_Data.Byte_Count := 0;
      Template_Checked : Boolean := False;
      Framing : ACPI_Resource_Data.Scan_Result;
   end record;
   -- Caller owns Handle and must keep it retained for this call. Data is fully
   -- defaulted on failure and its unused suffix on success. A capacity failure
   -- reports Required without re-evaluation, allocation, truncation or release;
   -- retry Copy_Template with the SAME retained result and larger caller storage.
   -- Framing success validates checksum/quota only, not resource admission.
   procedure Copy_Template
     (Service : Core.State; Handle : Core.Values.Value_Handle;
      Data : out Firmware_Tables.Bytes; Result : out Report;
      Maximum_Items : ACPI_Resource_Data.Item_Count := ACPI_Resource_Data.Default_Item_Limit)
     with Pre => Core.Valid (Service),
     Post => Result.Copied <= ACPI_Resource_Data.Byte_Count (Data'Length)
       and then (if Result.Status = Copied_Template then
         Result.Copied = Result.Required and then Result.Template_Checked
         and then Result.Framing.Status = ACPI_Resource_Data.Complete
         and then (for all I in Data'Range =>
           (if ACPI_Resource_Data.Byte_Count (I - Data'First) >= Result.Copied then Data (I) = 0))
       else Result.Copied = 0 and then (for all I in Data'Range => Data (I) = 0));
end ACPI_Resource_Reader;
