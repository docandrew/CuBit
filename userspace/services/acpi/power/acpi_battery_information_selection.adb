with AML_Execute;
package body ACPI_Battery_Information_Selection with SPARK_Mode is
   package E renames Extended_Reader;
   package L renames Legacy_Reader;
   use type E.Query_Outcome, L.Query_Outcome;
   function Uncertain (Status : AML_Execute.Execution_Status) return Boolean is
     (Status in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested);
   function Common (Value : Information) return Common_Information is
      R : Common_Information;
      function Extended_Field (Item : Text_Field) return Extended_Format.Text_Field is
        (case Item is when Model_Number => Extended_Format.Model_Number,
          when Serial_Number => Extended_Format.Serial_Number,
          when Battery_Type => Extended_Format.Battery_Type,
          when OEM_Information => Extended_Format.OEM_Information);
      function Legacy_Field (Item : Text_Field) return Legacy_Format.Text_Field is
        (case Item is when Model_Number => Legacy_Format.Model_Number,
          when Serial_Number => Legacy_Format.Serial_Number,
          when Battery_Type => Legacy_Format.Battery_Type,
          when OEM_Information => Legacy_Format.OEM_Information);
      function From_Extended (Item : Extended_Format.Optional_Quantity) return Quantity is
        ((Item.Known, (if Item.Known then Item.Value else 0)));
      function From_Legacy (Item : Legacy_Format.Optional_Quantity) return Quantity is
        ((Item.Known, (if Item.Known then Item.Value else 0)));
   begin
      R.Source := Value.Source;
      case Value.Source is
         when None_Reported => null;
         when Extended_Reported =>
            R.Units := (case Value.Extended.Units is
              when Extended_Format.Unspecified_Unit => Unspecified_Unit,
              when Extended_Format.Energy_And_Power => Energy_And_Power,
              when Extended_Format.Charge_And_Current => Charge_And_Current);
            R.Technology := (case Value.Extended.Battery_Technology is
              when Extended_Format.Unspecified_Technology => Unspecified_Technology,
              when Extended_Format.Primary => Primary, when Extended_Format.Secondary => Secondary);
            R.Design := From_Extended (Value.Extended.Design);
            R.Last_Full := From_Extended (Value.Extended.Last_Full);
            R.Voltage := From_Extended (Value.Extended.Voltage);
            R.Warning := Value.Extended.Warning; R.Low := Value.Extended.Low;
            R.Granularity_One := Value.Extended.Granularity_One;
            R.Granularity_Two := Value.Extended.Granularity_Two;
            for Item in Text_Field loop
               R.Identifiers (Item).Length := Value.Extended.Identifiers (Extended_Field (Item)).Length;
               for I in 1 .. R.Identifiers (Item).Length loop
                  R.Identifiers (Item).Data (I) := Value.Extended.Identifiers (Extended_Field (Item)).Data (I);
               end loop;
            end loop;
         when Legacy_Reported =>
            R.Units := (case Value.Legacy.Units is
              when Legacy_Format.Unspecified_Unit => Unspecified_Unit,
              when Legacy_Format.Energy_And_Power => Energy_And_Power,
              when Legacy_Format.Charge_And_Current => Charge_And_Current);
            R.Technology := (case Value.Legacy.Battery_Technology is
              when Legacy_Format.Unspecified_Technology => Unspecified_Technology,
              when Legacy_Format.Primary => Primary, when Legacy_Format.Secondary => Secondary);
            R.Design := From_Legacy (Value.Legacy.Design);
            R.Last_Full := From_Legacy (Value.Legacy.Last_Full);
            R.Voltage := From_Legacy (Value.Legacy.Voltage);
            R.Warning := Value.Legacy.Warning; R.Low := Value.Legacy.Low;
            R.Granularity_One := Value.Legacy.Granularity_One;
            R.Granularity_Two := Value.Legacy.Granularity_Two;
            for Item in Text_Field loop
               R.Identifiers (Item).Length := Value.Legacy.Identifiers (Legacy_Field (Item)).Length;
               for I in 1 .. R.Identifiers (Item).Length loop
                  R.Identifiers (Item).Data (I) := Value.Legacy.Identifiers (Legacy_Field (Item)).Data (I);
               end loop;
            end loop;
      end case;
      return R;
   end Common;
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Result : out Report) is
      Remaining : Natural;
   begin
      Result := (others => <>);
      E.Query (Service, Device, Budget, Result.Extended_Data);
      if Result.Extended_Data.Charged > Budget then
         Result.Status := Recovery_Required; return;
      end if;
      Result.Charged := Result.Extended_Data.Charged;
      if Result.Extended_Data.Outcome in E.Cleanup_Failed | E.Provider_Contract_Error
        or else Uncertain (Result.Extended_Data.Execution)
      then
         Result.Status := Recovery_Required; return;
      end if;
      case Result.Extended_Data.Outcome is
         when E.Sampled =>
            Result.Value := (Source => Extended_Reported, Extended => Result.Extended_Data.Value);
            Result.Status := Sampled; return;
         when E.Budget_Exhausted => Result.Status := Budget_Exhausted; return;
         when E.Missing_BIX => null;
         when others => Result.Status := Extended_Failed; return;
      end case;
      Remaining := Budget - Result.Charged;
      Result.Legacy_Attempted := True;
      L.Query (Service, Device, Remaining, Result.Legacy_Data);
      if Result.Legacy_Data.Charged > Remaining then
         Result.Status := Recovery_Required; return;
      end if;
      Result.Charged := Result.Charged + Result.Legacy_Data.Charged;
      if Result.Legacy_Data.Outcome in L.Cleanup_Failed | L.Provider_Contract_Error
        or else Uncertain (Result.Legacy_Data.Execution)
      then
         Result.Status := Recovery_Required; return;
      end if;
      case Result.Legacy_Data.Outcome is
         when L.Sampled =>
            Result.Value := (Source => Legacy_Reported, Legacy => Result.Legacy_Data.Value);
            Result.Status := Sampled;
         when L.Missing_BIF => Result.Status := Missing_Information;
         when L.Budget_Exhausted => Result.Status := Budget_Exhausted;
         when others => Result.Status := Legacy_Failed;
      end case;
   end Query;
end ACPI_Battery_Information_Selection;
