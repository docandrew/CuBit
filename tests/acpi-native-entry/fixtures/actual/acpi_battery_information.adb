package body ACPI_Battery_Information with SPARK_Mode is
   function Decode (Numbers : Raw_Numbers; Identifiers : Texts;
                    Has_Swap : Boolean; Swap : Unsigned_64 := 0) return Decode_Result is
      Value : Information := Empty_Information;
      function Refuse (Status : Outcome; Item : Field) return Decode_Result is
        ((Status, Item, Empty_Information));
      function Optional (Raw : Unsigned_64) return Optional_Quantity is
        (if Raw = Unknown_Word then (Known => False, Value => 0)
         else (Known => True, Value => Unsigned_32 (Raw)))
        with Pre => Raw <= Unsigned_64 (Unsigned_32'Last);
   begin
      if Numbers (Revision) > 1 then return Refuse (Unsupported_Revision, Revision); end if;
      if Has_Swap /= (Numbers (Revision) = 1) then
         return Refuse (Wrong_Revision_Shape, Swapping_Capability);
      end if;
      for Item in Numeric_Field loop
         if Numbers (Item) > Unsigned_64 (Unsigned_32'Last) then
            return Refuse (Integer_Too_Wide, Item);
         end if;
      end loop;
      if Has_Swap and then Swap > Unsigned_64 (Unsigned_32'Last) then
         return Refuse (Integer_Too_Wide, Swapping_Capability);
      end if;
      if Numbers (Power_Unit) > 1 then return Refuse (Invalid_Units, Power_Unit); end if;
      if Numbers (Technology) > 1 then return Refuse (Invalid_Technology, Technology); end if;
      for Item in Design_Capacity .. Low_Capacity loop
         if Item /= Technology and then Numbers (Item) > Maximum_Capacity
           and then not (Item in Design_Capacity | Last_Full_Charge | Design_Voltage
                         and then Numbers (Item) = Unknown_Word)
         then return Refuse (Invalid_Capacity, Item); end if;
      end loop;
      if Numbers (Accuracy) > Maximum_Accuracy then return Refuse (Invalid_Accuracy, Accuracy); end if;
      for Item in Text_Field loop
         Value.Identifiers (Item).Length := Identifiers (Item).Length;
         for I in 1 .. Identifiers (Item).Length loop
            if Identifiers (Item).Data (I) not in 1 .. 127 then
               return Refuse (Invalid_Text, Item);
            end if;
            Value.Identifiers (Item).Data (I) := Identifiers (Item).Data (I);
         end loop;
      end loop;
      Value.Version := (if Numbers (Revision) = 0 then Revision_Zero else Revision_One);
      Value.Units := (if Numbers (Power_Unit) = 0 then Energy_And_Power else Charge_And_Current);
      Value.Battery_Technology := (if Numbers (Technology) = 0 then Primary else Secondary);
      Value.Design := Optional (Numbers (Design_Capacity));
      Value.Last_Full := Optional (Numbers (Last_Full_Charge));
      Value.Voltage := Optional (Numbers (Design_Voltage));
      Value.Warning := Unsigned_32 (Numbers (Warning_Capacity));
      Value.Low := Unsigned_32 (Numbers (Low_Capacity));
      Value.Cycles := Optional (Numbers (Cycle_Count));
      Value.Measurement_Accuracy := Accuracy_Value (Numbers (Accuracy));
      Value.Sampling_Max := Optional (Numbers (Maximum_Sampling));
      Value.Sampling_Min := Optional (Numbers (Minimum_Sampling));
      Value.Averaging_Max := Unsigned_32 (Numbers (Maximum_Averaging));
      Value.Averaging_Min := Unsigned_32 (Numbers (Minimum_Averaging));
      Value.Granularity_One := Unsigned_32 (Numbers (Granularity_Low));
      Value.Granularity_Two := Unsigned_32 (Numbers (Granularity_High));
      Value.Swap_Reported := Has_Swap;
      if Has_Swap then Value.Swap_Raw := Unsigned_32 (Swap); end if;
      return (Decoded, No_Field, Value);
   end Decode;
end ACPI_Battery_Information;
