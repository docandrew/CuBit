package body ACPI_HID_I2C_Resolved with SPARK_Mode is
   use type Candidates.Outcome;
   use type Sources.Outcome;
   use type Core.Namespace.Object_Kind;
   function Is_Device (Service : Core.State; Node : Core.Node_Descriptor) return Boolean is
     (Core.Current_Node (Service, Node)
      and then Core.Values.Node_Kind (Node) = Core.Namespace.Device_Object);
   function Current (Service : Core.State; Item : Observation) return Boolean is
     (Is_Device (Service, Item.Device_Node) and then Is_Device (Service, Item.I2C_Node)
      and then Is_Device (Service, Item.GPIO_Node));
   procedure Resolve (Service : Core.State; Device : Core.Node_Descriptor;
      Data : Firmware_Tables.Bytes;
      Descriptor_Register : HID_I2C_Descriptors.Register_Number;
      Result : out Report;
      Maximum_Items : Candidates.R.Item_Count := Candidates.R.Default_Item_Limit) is
   begin
      Result := (others => <>);
      if not Is_Device (Service, Device) then return; end if;
      Candidates.Copy (Data, Descriptor_Register, Result.Copy, Maximum_Items);
      if Result.Copy.Status /= Candidates.Copied then Result.Status := Candidate_Failed; return; end if;
      Sources.Resolve (Service, Device,
        Result.Copy.Value.I2C_Source.Data (1 .. Result.Copy.Value.I2C_Source.Length), Result.I2C);
      if Result.I2C.Status /= Sources.Resolved then Result.Status := I2C_Source_Failed; return; end if;
      Sources.Resolve (Service, Device,
        Result.Copy.Value.GPIO_Source.Data (1 .. Result.Copy.Value.GPIO_Source.Length), Result.GPIO);
      if Result.GPIO.Status /= Sources.Resolved then Result.Status := GPIO_Source_Failed; return; end if;
      Result.Value := (Device, Result.I2C.Target, Result.GPIO.Target, Result.Copy.Value);
      Result.Status := Resolved;
   end Resolve;
end ACPI_HID_I2C_Resolved;
