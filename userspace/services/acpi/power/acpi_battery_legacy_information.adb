package body ACPI_Battery_Legacy_Information with SPARK_Mode is
   procedure Lemma_Text_Validity (Numbers : Raw_Numbers; Identifiers : Texts;
      Item : Text_Field) is
   begin
      null;
   end Lemma_Text_Validity;
   function Decode (Numbers : Raw_Numbers; Identifiers : Texts) return Decode_Result is
      Value : Information := Empty_Information;
      function Refuse (Status : Outcome; Item : Field) return Decode_Result is
        ((Status, Item, Empty_Information))
        with Post => Refuse'Result.Status = Status
          and then Refuse'Result.Failure_Field = Item
          and then Refuse'Result.Value = Empty_Information;
      function Optional (Raw : Unsigned_64) return Optional_Quantity is
        (if Raw = Unknown_Word then (Known => False, Value => 0)
         else (Known => True, Value => Unsigned_32 (Raw)))
        with Pre => Raw <= Unsigned_64 (Unsigned_32'Last),
          Post => Quantity_Matches (Optional'Result, Raw);
   begin
      for Item in Numeric_Field loop
         if Numbers (Item) > Unsigned_64 (Unsigned_32'Last) then
            pragma Assert (not Inputs_Valid (Numbers, Identifiers));
            return Refuse (Integer_Too_Wide, Item);
         end if;
         pragma Loop_Invariant
           (for all Prior in Numeric_Field'First .. Item =>
              Numbers (Prior) <= Unsigned_64 (Unsigned_32'Last));
      end loop;
      if Numbers (Power_Unit) > 1 then
         pragma Assert (not Inputs_Valid (Numbers, Identifiers));
         return Refuse (Invalid_Units, Power_Unit);
      end if;
      if Numbers (Technology) > 1 then
         pragma Assert (not Inputs_Valid (Numbers, Identifiers));
         return Refuse (Invalid_Technology, Technology);
      end if;
      for Item in Design_Capacity .. Low_Capacity loop
         if Item /= Technology and then Numbers (Item) > Maximum_Capacity
           and then not (Item in Design_Capacity | Last_Full_Charge | Design_Voltage
                         and then Numbers (Item) = Unknown_Word)
         then
            pragma Assert (not Inputs_Valid (Numbers, Identifiers));
            return Refuse (Invalid_Capacity, Item);
         end if;
         pragma Loop_Invariant
           (for all Prior in Capacity_Field'First .. Item =>
              Capacity_Valid (Prior, Numbers (Prior)));
      end loop;
      for Item in Text_Field loop
         Value.Identifiers (Item).Length := Identifiers (Item).Length;
         for I in 1 .. Identifiers (Item).Length loop
            if Identifiers (Item).Data (I) not in 1 .. 127 then
               pragma Assert (I in 1 .. Identifiers (Item).Length);
               pragma Assert (not Text_Valid (Identifiers (Item)));
               Lemma_Text_Validity (Numbers, Identifiers, Item);
               pragma Assert (not Inputs_Valid (Numbers, Identifiers));
               return Refuse (Invalid_Text, Item);
            end if;
            Value.Identifiers (Item).Data (I) := Identifiers (Item).Data (I);
            pragma Loop_Invariant
              (Value.Identifiers (Item).Length = Identifiers (Item).Length);
            pragma Loop_Invariant
              (for all J in Text_Buffer'Range =>
                 (if J <= I then Value.Identifiers (Item).Data (J) = Identifiers (Item).Data (J)
                    and then Identifiers (Item).Data (J) in ASCII_Byte
                  else Value.Identifiers (Item).Data (J) = 0));
         end loop;
         pragma Loop_Invariant
           (for all Prior in Text_Field =>
              (if Prior <= Item then Text_Matches (Value.Identifiers (Prior), Identifiers (Prior))
               else Value.Identifiers (Prior) = Empty_Texts (Prior)));
      end loop;
      Value.Units := (if Numbers (Power_Unit) = 0 then Energy_And_Power else Charge_And_Current);
      Value.Battery_Technology := (if Numbers (Technology) = 0 then Primary else Secondary);
      Value.Design := Optional (Numbers (Design_Capacity));
      Value.Last_Full := Optional (Numbers (Last_Full_Charge));
      Value.Voltage := Optional (Numbers (Design_Voltage));
      Value.Warning := Unsigned_32 (Numbers (Warning_Capacity));
      Value.Low := Unsigned_32 (Numbers (Low_Capacity));
      Value.Granularity_One := Unsigned_32 (Numbers (Granularity_Low));
      Value.Granularity_Two := Unsigned_32 (Numbers (Granularity_High));
      pragma Assert (Numbers_Valid (Numbers));
      pragma Assert (for all Item in Text_Field => Text_Valid (Identifiers (Item)));
      pragma Assert (Inputs_Valid (Numbers, Identifiers));
      return (Decoded, No_Field, Value);
   end Decode;
end ACPI_Battery_Legacy_Information;
