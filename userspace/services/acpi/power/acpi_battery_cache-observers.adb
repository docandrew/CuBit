with ACPI_Battery_Status;
package body ACPI_Battery_Cache.Observers with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type Samples.Availability;
   use type Static_Info.Source_Kind;
   package Protocol renames ACPI_Power_Query_Protocol;
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
   function Header_View (View : Samples.View) return Words is
     (Reply (View.Revision, Code (View.Status), Schema_Version, Last_Page));
   function Header (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Words is
      View : constant Samples.View := Read (Cache, Now, Maximum_Age);
   begin
      return Header_View (View);
   end Header;
   function Read_Page (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Words is
   begin
      return Read_View (Read (Cache, Now, Maximum_Age), Expected_Revision, Page_Number, Offset);
   end Read_Page;
   function Read_View (View : Samples.View; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Words is
      Selected_Info : Static_Info.Information renames View.Value.Information;
      Info : constant Static_Info.Common_Information := Static_Info.Common (Selected_Info);
      Dynamic : ACPI_Battery_Status.Sample renames View.Value.Status;
      Selected : Page;
      function Pair (First, Second : Word) return Words is
        (Reply (View.Revision, OK, First, Second));
      function Optional (Value : Static_Info.Quantity) return Words is
        (Pair (Flag (Value.Known), (if Value.Known then Word (Value.Value) else 0)));
      function Extended_Optional (Value : Format.Optional_Quantity) return Words is
        (Pair (Flag (Value.Known), (if Value.Known then Word (Value.Value) else 0)));
      function Measurement (Value : ACPI_Battery_Status.Measurement) return Words is
        (Pair (Flag (Value.Known), (if Value.Known then Word (Value.Value) else 0)));
   begin
      if Expected_Revision /= View.Revision then return Reply (View.Revision, Stale); end if;
      if View.Status /= Samples.Available then return Reply (View.Revision, Code (View.Status)); end if;
      if Selected_Info.Source = Static_Info.None_Reported then
         return Reply (View.Revision, Acquisition_Failed);
      end if;
      if Page_Number > Last_Page then return Reply (View.Revision, Bad_Page); end if;
      Selected := Page'Enum_Val (Natural (Page_Number));
      if Selected < Model_Number and then Offset /= 0 then return Reply (View.Revision, Bad_Offset); end if;
      if Selected = Information_Source and then Offset /= 0 then
         return Reply (View.Revision, Bad_Offset);
      end if;
      if Selected in BIX_Version | Cycles | Sampling_Max | Sampling_Min | Accuracy | Averaging | Swap
        and then Selected_Info.Source /= Static_Info.Extended_Reported
      then
         return Reply (View.Revision, Not_Applicable);
      end if;
      case Selected is
         when Units =>
            return Pair
              ((case Info.Units is when Static_Info.Unspecified_Unit => 0,
                  when Static_Info.Energy_And_Power => 1, when Static_Info.Charge_And_Current => 2),
               (case Info.Technology is when Static_Info.Unspecified_Technology => 0,
                  when Static_Info.Primary => 1, when Static_Info.Secondary => 2));
         when BIX_Version =>
            return Pair ((case Selected_Info.Extended.Version is when Format.Revision_Zero => 0, when Format.Revision_One => 1), 0);
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
         when Cycles => return Extended_Optional (Selected_Info.Extended.Cycles);
         when Sampling_Max => return Extended_Optional (Selected_Info.Extended.Sampling_Max);
         when Sampling_Min => return Extended_Optional (Selected_Info.Extended.Sampling_Min);
         when Capacity_Thresholds => return Pair (Word (Info.Warning), Word (Info.Low));
         when Accuracy => return Pair (Word (Selected_Info.Extended.Measurement_Accuracy), 0);
         when Averaging => return Pair (Word (Selected_Info.Extended.Averaging_Max), Word (Selected_Info.Extended.Averaging_Min));
         when Granularity => return Pair (Word (Info.Granularity_One), Word (Info.Granularity_Two));
         when Swap => return Pair (Flag (Selected_Info.Extended.Swap_Reported), (if Selected_Info.Extended.Swap_Reported then Word (Selected_Info.Extended.Swap_Raw) else 0));
         when Information_Source =>
            return Pair
              (Word (Protocol.Information_Source'Enum_Rep
                ((case Selected_Info.Source is when Static_Info.Extended_Reported => Protocol.BIX,
                  when Static_Info.Legacy_Reported => Protocol.BIF,
                  when Static_Info.None_Reported => Protocol.Unavailable))), 0);
         when Model_Number .. OEM_Information =>
            declare
               Field : constant Static_Info.Text_Field :=
                 (case Selected is when Model_Number => Static_Info.Model_Number,
                  when Serial_Number => Static_Info.Serial_Number,
                  when Battery_Type => Static_Info.Battery_Type,
                  when others => Static_Info.OEM_Information);
               Text : Static_Info.Text renames Info.Identifiers (Field);
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
   end Read_View;
end ACPI_Battery_Cache.Observers;
