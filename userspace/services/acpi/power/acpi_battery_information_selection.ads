with ACPI_Service_Core;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;
with ACPI_Battery_Legacy_Information_Reader;
with Interfaces;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Extended_Format is new ACPI_Battery_Information (<>);
   with package Legacy_Format is new ACPI_Battery_Legacy_Information (<>);
   with package Extended_Reader is new ACPI_Battery_Information_Reader
     (Core => Core, Format => Extended_Format, others => <>);
   with package Legacy_Reader is new ACPI_Battery_Legacy_Information_Reader
     (Core => Core, Format => Legacy_Format, others => <>);
package ACPI_Battery_Information_Selection with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Source_Kind is (None_Reported, Extended_Reported, Legacy_Reported);
   type Information (Source : Source_Kind := None_Reported) is record
      case Source is
         when None_Reported => null;
         when Extended_Reported => Extended : Extended_Format.Information;
         when Legacy_Reported => Legacy : Legacy_Format.Information;
      end case;
   end record;
   type Unit_Kind is (Unspecified_Unit, Energy_And_Power, Charge_And_Current);
   type Technology_Kind is (Unspecified_Technology, Primary, Secondary);
   type Quantity is record
      Known : Boolean := False;
      Value : Interfaces.Unsigned_32 := 0;
   end record;
   Text_Capacity : constant Positive := Positive'Max
     (Extended_Format.Text_Capacity, Legacy_Format.Text_Capacity);
   subtype Text_Length is Natural range 0 .. Text_Capacity;
   type Text_Bytes is array (Positive range 1 .. Text_Capacity) of Interfaces.Unsigned_8;
   type Text is record Length : Text_Length := 0; Data : Text_Bytes := [others => 0]; end record;
   type Text_Field is (Model_Number, Serial_Number, Battery_Type, OEM_Information);
   type Texts is array (Text_Field) of Text;
   type Common_Information is record
      Source : Source_Kind := None_Reported;
      Units : Unit_Kind := Unspecified_Unit;
      Technology : Technology_Kind := Unspecified_Technology;
      Design, Last_Full, Voltage : Quantity;
      Warning, Low, Granularity_One, Granularity_Two : Interfaces.Unsigned_32 := 0;
      Identifiers : Texts := [others => (others => <>)];
   end record;
   -- Explicit copied projection, with the source retained. Extended-only
   -- fields are absent from this common type, rather than filled with zeros.
   function Common (Value : Information) return Common_Information;
   type Outcome is (Sampled, Missing_Information, Extended_Failed, Legacy_Failed,
                    Budget_Exhausted, Recovery_Required);
   type Report is record
      Status : Outcome := Extended_Failed;
      Value : Information;
      Charged : Natural := 0;
      Legacy_Attempted : Boolean := False;
      Extended_Data : Extended_Reader.Query_Report;
      Legacy_Data : Legacy_Reader.Query_Report;
   end record;
   -- Service-owner operation under an existing admitted context. Only a
   -- canonical Missing_BIX permits a single BIF query. Malformed/evaluation/
   -- budget/cleanup/uncertain results never trigger a fallback or AML replay.
   -- Both readers share this Core and one Budget. Values remain distinct;
   -- there is no invented BIX revision or extended metadata for legacy data.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Result : out Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Result.Charged <= Budget
         and then (if Result.Status = Sampled then Result.Value.Source /= None_Reported
           else Result.Value.Source = None_Reported)
         and then (Result.Status = Recovery_Required or else
           Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Battery_Information_Selection;
