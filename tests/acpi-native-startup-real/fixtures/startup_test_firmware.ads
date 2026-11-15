pragma Ada_2022;
with Firmware_Tables;
-- One synthetic immutable catalog shared by the loader and native adapters.
-- Structural parsing fixture only; these bytes grant no hardware authority.
package Startup_Test_Firmware is
   subtype Bytes is Firmware_Tables.Bytes;
   DSDT_ID : constant Positive := 1;
   FADT_ID : constant Positive := 2;
   Table_Count : constant Positive := 2;
   function Image(ID:Positive) return Bytes;
end Startup_Test_Firmware;
