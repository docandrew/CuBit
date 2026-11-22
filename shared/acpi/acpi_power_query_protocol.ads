pragma Ada_2022;
with Interfaces;
package ACPI_Power_Query_Protocol with Pure, SPARK_Mode is
   Maximum_Age_Microseconds : constant Interfaces.Unsigned_64 := 5_000_000;
   Lid_Header : constant Interfaces.Unsigned_32 := 29;
   Lid_Freshness : constant Interfaces.Unsigned_32 := 30;
   Source_Header : constant Interfaces.Unsigned_32 := 31;
   Source_Freshness : constant Interfaces.Unsigned_32 := 32;
   Battery_Header : constant Interfaces.Unsigned_32 := 33;
   Battery_Freshness : constant Interfaces.Unsigned_32 := 34;
   Battery_Entry : constant Interfaces.Unsigned_32 := 35;
   Battery_Page : constant Interfaces.Unsigned_32 := 38;
   subtype Word is Interfaces.Unsigned_64 range 0 ..
     Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
   Schema_Version : constant Word := 1;
   Battery_Schema_Version : constant Word := 2;
   type Information_Source is (Unavailable, BIX, BIF);
   for Information_Source use (Unavailable => 0, BIX => 1, BIF => 2);
   Timestamp_Known_Bit : constant Word := 16#1_0000_0000#;
   type Result_Code is (OK, Not_Ready, Missing, Acquisition_Failed, Too_Old,
      Clock_Unknown, Clock_Regressed, Counter_Exhausted, Stale, Bad_Page,
      Bad_Offset, Bad_Index, Denied, Invalid_Request, Not_Applicable);
   for Result_Code use (OK=>0,Not_Ready=>1,Missing=>2,Acquisition_Failed=>3,
      Too_Old=>4,Clock_Unknown=>5,Clock_Regressed=>6,Counter_Exhausted=>7,
      Stale=>8,Bad_Page=>9,Bad_Offset=>10,Bad_Index=>11,Denied=>12,Invalid_Request=>13,
      Not_Applicable=>14);
   -- Header arguments all zero; freshness[expected revision,0,0,0]; battery
   -- entry[expected revision,index,0,0]; page[expected,index,page,offset].
   -- Replies[family revision,result,value1,value2]. Lid/source header values
   -- are state code and schema:0unknown,1closed/offline,2open/source-in-use.
   -- Source-in-use is not a whole-machine battery/cable-presence assertion.
   -- Freshness values are time-low32,time-high32+known-bit. Metadata remains
   -- descriptive when stale; unavailable measurements are always zeroed.
end ACPI_Power_Query_Protocol;
