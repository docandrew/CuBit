with ACPI_Battery_Status;
package body ACPI_Battery_Cache.Observers with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type Samples.Availability;
   Critical_Flag : constant Word := 4;
   Charge_Limiting_Flag : constant Word := 8;
   Byte_Radix : constant Word := 256;
   function Code (Status : Samples.Availability) return Result_Code is
     (case Status is
      when Samples.Available => OK, when Samples.Not_Ready => Not_Ready,
      when Samples.Missing => Missing, when Samples.Failure => Acquisition_Failed,
      when Samples.Too_Old => Too_Old, when Samples.Clock_Unknown => Clock_Unknown,
      when Samples.Clock_Regressed => Clock_Regressed,
      when Samples.Counter_Exhausted => Counter_Exhausted);
   function Reply (Revision : Samples.Revision_Number; Status : Result_Code;
      First, Second : Word := 0) return Words is
     ([Word (Revision), Word (Result_Code'Enum_Rep (Status)), First, Second]);
   function Flag (Value : Boolean) return Word is (if Value then 1 else 0);
   function Header (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Words is
      View : constant Samples.View := Read (Cache, Now, Maximum_Age);
   begin
      return Reply (View.Revision, Code (View.Status), Schema_Version, Last_Page);
   end Header;
   function Read_Page (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Words is
      View : constant Samples.View := Read (Cache, Now, Maximum_Age);
      Info : Format.Information renames View.Value.Information;
      Dynamic : ACPI_Battery_Status.Sample renames View.Value.Status;
      Selected : Page;
      function Pair (First, Second : Word) return Words is
        (Reply (View.Revision, OK, First, Second));
      function Optional (Value : Format.Optional_Quantity) return Words is
        (Pair (Flag (Value.Known), (if Value.Known then Word (Value.Value) else 0)));
      function Measurement (Value : ACPI_Battery_Status.Measurement) return Words is
        (Pair (Flag (Value.Known), (if Value.Known then Word (Value.Value) else 0)));
   begin
      if Expected_Revision /= View.Revision then return Reply (View.Revision, Stale); end if;
      if View.Status /= Samples.Available then return Reply (View.Revision, Code (View.Status)); end if;
      if Page_Number > Last_Page then return Reply (View.Revision, Bad_Page); end if;
      Selected := Page'Enum_Val (Natural (Page_Number));
      if Selected < Model_Number and then Offset /= 0 then return Reply (View.Revision, Bad_Offset); end if;
      case Selected is
         when Units =>
            return Pair
              ((case Info.Units is when Format.Unspecified_Unit => 0,
                  when Format.Energy_And_Power => 1, when Format.Charge_And_Current => 2),
               (case Info.Battery_Technology is when Format.Unspecified_Technology => 0,
                  when Format.Primary => 1, when Format.Secondary => 2));
         when BIX_Version =>
            return Pair ((case Info.Version is when Format.Revision_Zero => 0, when Format.Revision_One => 1), 0);
         when Dynamic_State =>
            return Pair
              ((case Dynamic.Flow is when ACPI_Battery_Status.Neither => 0,
                  when ACPI_Battery_Status.Discharging => 1, when ACPI_Battery_Status.Charging => 2)
                + Critical_Flag * Flag (Dynamic.Critical) + Charge_Limiting_Flag * Flag (Dynamic.Charge_Limiting),
               Word (Dynamic.Reserved_State_Bits));
         when Rate => return Measurement (Dynamic.Rate);
         when Remaining => return Measurement (Dynamic.Remaining_Capacity);
         when Voltage => return Measurement (Dynamic.Voltage);
         when Design => return Optional (Info.Design);
         when Last_Full => return Optional (Info.Last_Full);
         when Design_Voltage => return Optional (Info.Voltage);
         when Cycles => return Optional (Info.Cycles);
         when Sampling_Max => return Optional (Info.Sampling_Max);
         when Sampling_Min => return Optional (Info.Sampling_Min);
         when Capacity_Thresholds => return Pair (Word (Info.Warning), Word (Info.Low));
         when Accuracy => return Pair (Word (Info.Measurement_Accuracy), 0);
         when Averaging => return Pair (Word (Info.Averaging_Max), Word (Info.Averaging_Min));
         when Granularity => return Pair (Word (Info.Granularity_One), Word (Info.Granularity_Two));
         when Swap => return Pair (Flag (Info.Swap_Reported), (if Info.Swap_Reported then Word (Info.Swap_Raw) else 0));
         when Model_Number .. OEM_Information =>
            declare
               Field : constant Format.Text_Field :=
                 (case Selected is when Model_Number => Format.Model_Number,
                  when Serial_Number => Format.Serial_Number,
                  when Battery_Type => Format.Battery_Type,
                  when others => Format.OEM_Information);
               Text : Format.Text renames Info.Identifiers (Field);
               Packed : Word := 0;
               Factor : Word := 1;
            begin
               if Offset > Interfaces.Unsigned_64 (Text.Length) then return Reply (View.Revision, Bad_Offset); end if;
               -- The difference form avoids Offset+7 overflow at full width.
               for I in 1 .. Natural'Min (Text_Bytes_Per_Word, Text.Length - Natural (Offset)) loop
                  Packed := Packed + Word (Text.Data (Natural (Offset) + I)) * Factor;
                  if I < Text_Bytes_Per_Word then Factor := Factor * Byte_Radix; end if;
               end loop;
               return Pair (Word (Text.Length), Packed);
            end;
      end case;
   end Read_Page;
end ACPI_Battery_Cache.Observers;
