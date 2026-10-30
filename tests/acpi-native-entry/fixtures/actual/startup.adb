--  Generated with the ELF manifest; do not edit.
pragma Ada_2022;
with CuBit.Startup_Check.Logged;
package body Startup with SPARK_Mode => Off is
begin
   --  Stops here, saying which entry and why, if the loaded startup
   --  table lacks a required capability declared above (CAP-003).
   Standard.CuBit.Startup_Check.Logged.Require
     (Expected, "com.cubit.acpi",
      Standard.CuBit.Handles.Services.Logstore.Optional_Named ("logstore"));
end Startup;
