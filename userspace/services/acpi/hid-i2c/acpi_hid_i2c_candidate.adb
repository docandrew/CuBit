package body ACPI_HID_I2C_Candidate with SPARK_Mode is
   use type R.Byte_Count;
   use type Resources.Analysis_Outcome;
   procedure Copy (Data : Firmware_Tables.Bytes;
      Descriptor_Register : HID_I2C_Descriptors.Register_Number;
      Result : out Report; Maximum_Items : R.Item_Count := R.Default_Item_Limit) is
      Analysis : Resources.Analysis_Report;
      Value : Candidate;
      Pin : G.Pin_Result;
      procedure Copy_Span (First, Length : R.Byte_Count; Kind : Span_Kind;
         Target : out Owned_Span) is
      begin
         Target := (others => <>);
         if Result.Status /= Copied then return; end if;
         if First > R.Byte_Count (Data'Length)
           or else Length > R.Byte_Count (Data'Length) - First
         then
            Result.Status := Invalid_Span; Result.At_Span := Kind;
            Result.Required := Length; return;
         end if;
         if Length > R.Byte_Count (Maximum_Span_Bytes) then
            Result.Status := Capacity_Refused; Result.At_Span := Kind;
            Result.Required := Length; return;
         end if;
         Target.Length := Natural (Length);
         for I in 1 .. Target.Length loop
            Target.Data (I) := Data (Data'First + Natural (First) + (I - 1));
         end loop;
      end Copy_Span;
   begin
      Result := (others => <>);
      Resources.Analyze (Data, Analysis, Maximum_Items);
      Result.Analysis := Analysis.Outcome;
      if Analysis.Outcome /= Resources.Connections_Found then return; end if;
      Pin := G.Read_Pin (Data, Analysis.Interrupt_Offset, 1);
      if not Pin.Available then Result.Status := Pin_Unavailable; return; end if;
      Value.Descriptor_Register := Descriptor_Register;
      Value.I2C := Analysis.I2C.Information;
      Value.I2C_Revision := Analysis.I2C.Revision;
      Value.I2C_Type_Revision := Analysis.I2C.Type_Revision;
      Value.I2C_Source_Index := Analysis.I2C.Source_Index;
      Value.I2C_Specific_Flags := Analysis.I2C.Specific_Flags;
      Value.I2C_Role := Analysis.I2C.Role;
      Value.Initiated_By := Analysis.I2C.Initiated_By;
      Value.I2C_Shared := Analysis.I2C.Shared;
      Value.Interrupt := Analysis.Interrupt.Information;
      Value.Interrupt_Role := Analysis.Interrupt.Role;
      Value.Interrupt_Shared := Analysis.Interrupt.Shared;
      Value.Configuration := Analysis.Interrupt.Configuration;
      Value.Drive := Analysis.Interrupt.Drive_Strength;
      Value.Debounce := Analysis.Interrupt.Debounce_Timeout;
      Value.Pin := Pin.Value;
      Result.Status := Copied;
      Copy_Span (Analysis.I2C.Source.First, Analysis.I2C.Source.Length, I2C_Source, Value.I2C_Source);
      Copy_Span (Analysis.I2C.Vendor.First, Analysis.I2C.Vendor.Length, I2C_Vendor, Value.I2C_Vendor);
      Copy_Span (Analysis.Interrupt.Source.First, Analysis.Interrupt.Source.Length, GPIO_Source, Value.GPIO_Source);
      Copy_Span (Analysis.Interrupt.Vendor.First, Analysis.Interrupt.Vendor.Length, GPIO_Vendor, Value.GPIO_Vendor);
      if Result.Status = Copied then Result.Value := Value; end if;
   end Copy;
end ACPI_HID_I2C_Candidate;
