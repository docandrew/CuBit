pragma Ada_2022;
with Interfaces;
-- Existing observer Read_Metrics label0, identity page23.
-- Request [23,source-index,byte-offset,expected-family-cache-generation].
-- Index0 is the AC source measured by page21;1..16 are page22 battery slots.
-- Obtain that family's generation from its summary first; it is NOT the watch
-- generation. Index/offset are full-width checked. Identity reads never refresh.
-- Reply labelF000 [current-family-generation,result,total-length,packed7bytes].
-- Existing F001 error framing is used only for malformed/unauthorized requests.
-- Offset=length gives an empty chunk; beyond length is Bad_Offset. No prefixes
-- are returned for overlong paths. Stable canonical namespace paths identify a
-- source location across inventory reorder/refresh, not a physical battery UUID.
-- These are descriptive strings, never invocation/register/mapping authority.
package ACPI_Power_Identity_Protocol with Pure, SPARK_Mode is
   Read_Metrics_Label:constant Interfaces.Unsigned_32:=0;
   Identity_Page:constant Interfaces.Unsigned_64:=23;
   Maximum_Battery_Index:constant Interfaces.Unsigned_64:=16;
   Maximum_Path_Characters:constant Interfaces.Unsigned_64:=320;
   type Result_Code is (OK,Not_Ready,Stale,Index_Out_Of_Range,Too_Long,Bad_Offset);
   for Result_Code use(OK=>0,Not_Ready=>1,Stale=>2,Index_Out_Of_Range=>3,Too_Long=>4,Bad_Offset=>5);
end ACPI_Power_Identity_Protocol;
