with Interfaces;
generic
   Maximum_Text_Bytes : Positive := 128;
package ACPI_Battery_Information with Pure, SPARK_Mode is
   use Interfaces;
   Text_Capacity : constant Positive := Maximum_Text_Bytes;
   type Field is (No_Field, Revision, Power_Unit, Design_Capacity,
      Last_Full_Charge, Technology, Design_Voltage, Warning_Capacity,
      Low_Capacity, Cycle_Count, Accuracy, Maximum_Sampling,
      Minimum_Sampling, Maximum_Averaging, Minimum_Averaging,
      Granularity_Low, Granularity_High, Model_Number, Serial_Number,
      Battery_Type, OEM_Information, Swapping_Capability);
   subtype Numeric_Field is Field range Revision .. Granularity_High;
   subtype Text_Field is Field range Model_Number .. OEM_Information;
   type Raw_Numbers is array (Numeric_Field) of Unsigned_64;
   subtype Text_Length is Natural range 0 .. Maximum_Text_Bytes;
   type Text_Buffer is array (Positive range 1 .. Maximum_Text_Bytes) of Unsigned_8;
   type Text is record
      Length : Text_Length := 0;
      Data : Text_Buffer := [others => 0];
   end record;
   type Texts is array (Text_Field) of Text;
   Empty_Texts : constant Texts := [others => (others => <>)];
   Revision_Zero_Elements : constant Positive := 20;
   Revision_One_Elements : constant Positive := 21;
   Unknown_Word : constant Unsigned_64 := 16#FFFF_FFFF#;
   Maximum_Capacity : constant Unsigned_64 := 16#7FFF_FFFF#;
   Maximum_Accuracy : constant Unsigned_64 := 100_000;
   type Unit_Kind is (Unspecified_Unit, Energy_And_Power, Charge_And_Current);
   type Technology_Kind is (Unspecified_Technology, Primary, Secondary);
   type BIX_Revision is (Revision_Zero, Revision_One);
   type Optional_Quantity is record
      Known : Boolean := False;
      Value : Unsigned_32 := 0;
   end record;
   subtype Accuracy_Value is Unsigned_32 range 0 .. 100_000;
   type Information is record
      Version : BIX_Revision := Revision_Zero;
      Units : Unit_Kind := Unspecified_Unit;
      Battery_Technology : Technology_Kind := Unspecified_Technology;
      Design, Last_Full, Voltage, Cycles, Sampling_Max, Sampling_Min : Optional_Quantity;
      Warning, Low : Unsigned_32 := 0;
      Measurement_Accuracy : Accuracy_Value := 0;
      Averaging_Max, Averaging_Min, Granularity_One, Granularity_Two : Unsigned_32 := 0;
      Identifiers : Texts := Empty_Texts;
      Swap_Reported : Boolean := False;
      Swap_Raw : Unsigned_32 := 0;
   end record;
   Empty_Information : constant Information := (others => <>);
   type Outcome is (Decoded, Unsupported_Revision, Wrong_Revision_Shape,
                    Integer_Too_Wide, Invalid_Units, Invalid_Technology,
                    Invalid_Capacity, Invalid_Accuracy, Invalid_Text);
   type Decode_Result is record
      Status : Outcome := Unsupported_Revision;
      Failure_Field : Field := No_Field;
      Value : Information := Empty_Information;
   end record;
   -- Inputs have already been extracted as integers/strings. No narrowing,
   -- truncation, unit conversion or ordering assumptions are applied silently.
   -- Unknown differs from zero. Swap is kept as a raw DWORD, without policy.
   -- Text capacity is an explicit instance budget, not an ACPI string limit.
   function Decode (Numbers : Raw_Numbers; Identifiers : Texts;
                    Has_Swap : Boolean; Swap : Unsigned_64 := 0) return Decode_Result;
end ACPI_Battery_Information;
