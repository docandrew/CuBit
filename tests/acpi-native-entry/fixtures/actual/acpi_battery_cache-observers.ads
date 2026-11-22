with Interfaces;
generic
package ACPI_Battery_Cache.Observers with SPARK_Mode is
   -- Service-owner adapter for read-only CCL integer observations. The native
   -- endpoint must authorize its received stamp before calling this adapter.
   -- No wire label/authority is allocated here. Queries never evaluate AML.
   subtype Word is Interfaces.Unsigned_64 range 0 .. Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
   type Words is array (Natural range 0 .. 3) of Word;
   type Result_Code is (OK, Not_Ready, Missing, Acquisition_Failed, Too_Old,
      Clock_Unknown, Clock_Regressed, Counter_Exhausted, Stale, Bad_Page, Bad_Offset);
   for Result_Code use (OK => 0, Not_Ready => 1, Missing => 2,
      Acquisition_Failed => 3, Too_Old => 4, Clock_Unknown => 5,
      Clock_Regressed => 6, Counter_Exhausted => 7, Stale => 8,
      Bad_Page => 9, Bad_Offset => 10);
   Schema_Version : constant Word := 1;
   type Page is (Units, BIX_Version, Dynamic_State, Rate, Remaining, Voltage,
      Design, Last_Full, Design_Voltage, Cycles, Sampling_Max, Sampling_Min,
      Capacity_Thresholds, Accuracy, Averaging, Granularity, Swap,
      Model_Number, Serial_Number, Battery_Type, OEM_Information);
   for Page use (Units => 0, BIX_Version => 1, Dynamic_State => 2, Rate => 3,
      Remaining => 4, Voltage => 5, Design => 6, Last_Full => 7,
      Design_Voltage => 8, Cycles => 9, Sampling_Max => 10, Sampling_Min => 11,
      Capacity_Thresholds => 12, Accuracy => 13, Averaging => 14,
      Granularity => 15, Swap => 16, Model_Number => 17, Serial_Number => 18,
      Battery_Type => 19, OEM_Information => 20);
   Last_Page : constant Word := 20;
   Text_Bytes_Per_Word : constant Positive := 7;
   -- Every reply: [revision, result code, value1, value2]. Header values are
   -- static schema version/last page, even while the cache is unavailable.
   function Header (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Words;
   -- A client reads Header, then supplies its exact revision on every page.
   -- Numeric pages require offset zero. Text pages return total byte length
   -- and up to seven bytes, little endian, from Offset; offset=length returns
   -- zero bytes. Seven bytes remain nonnegative in signed CCL integers.
   -- Optional quantities return [known (0/1), value], unknown value always 0.
   -- Rejected/unavailable pages carry zero payload. No implicit retry/callback.
   -- Page/offset/revision are checked at full width before any narrowing.
   function Read_Page (Cache : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Words;
end ACPI_Battery_Cache.Observers;
