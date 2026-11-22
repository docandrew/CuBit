pragma Ada_2022;
package body ACPI_Battery_Status with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   Discharging_Mask : constant Raw_Integer := 1;
   Charging_Mask : constant Raw_Integer := 2;
   Direction_Mask : constant Raw_Integer := Discharging_Mask or Charging_Mask;
   Critical_Mask : constant Raw_Integer := 4;
   Charge_Limiting_Mask : constant Raw_Integer := 8;
   Reserved_Mask : constant Interfaces.Unsigned_32 := 16#FFFF_FFF0#;
   type Package_Field is (Battery_State, Present_Rate, Remaining_Capacity,
                          Present_Voltage);
   type Field_Values is array (Package_Field) of Raw_Integer;
   Names : constant array (Package_Field) of Field :=
     [State_Field, Rate_Field, Capacity_Field, Voltage_Field];
   function Decode (Values : Raw_Package) return Decode_Result is
      Raw : Field_Values := [others => 0];
      Result : Decode_Result;
      function Read_Measurement (Value : Raw_Integer) return Measurement is
        (if Value = Unknown_Quantity then (Known => False, Value => 0)
         else (Known => True, Value => Quantity_Value (Value)))
        with Pre => Value <= Maximum_Quantity or else Value = Unknown_Quantity;
   begin
      if Values'Length /= Expected_Elements then return Result; end if;
      for F in Package_Field loop
         Raw (F) := Values (Values'First + Package_Field'Pos (F));
         if Raw (F) > Unknown_Quantity then
            Result.Status := Integer_Too_Wide;
            Result.Failure_Field := Names (F);
            return Result;
         end if;
      end loop;
      if (Raw (Battery_State) and Direction_Mask) = Direction_Mask then
         Result.Status := Conflicting_Directions;
         Result.Failure_Field := State_Field;
         return Result;
      end if;
      for F in Present_Rate .. Present_Voltage loop
         if Raw (F) > Maximum_Quantity and then Raw (F) /= Unknown_Quantity then
            Result.Status := Invalid_Measurement;
            Result.Failure_Field := Names (F);
            return Result;
         end if;
      end loop;
      Result.Value.Flow :=
        (if (Raw (Battery_State) and Discharging_Mask) /= 0 then Discharging
         elsif (Raw (Battery_State) and Charging_Mask) /= 0 then Charging
         else Neither);
      Result.Value.Critical := (Raw (Battery_State) and Critical_Mask) /= 0;
      Result.Value.Charge_Limiting :=
        (Raw (Battery_State) and Charge_Limiting_Mask) /= 0;
      Result.Value.Reserved_State_Bits :=
        Interfaces.Unsigned_32 (Raw (Battery_State)) and Reserved_Mask;
      Result.Value.Rate := Read_Measurement (Raw (Present_Rate));
      Result.Value.Remaining_Capacity := Read_Measurement (Raw (Remaining_Capacity));
      Result.Value.Voltage := Read_Measurement (Raw (Present_Voltage));
      Result.Status := Decoded;
      return Result;
   end Decode;
end ACPI_Battery_Status;
