--  Generated with the ELF manifest; do not edit.
--  Startup capabilities of com.cubit.acpi, by name (CAP-002).
--  Handle limit: standard (4096).
pragma Ada_2022;
with CuBit.Handles.Startup;
with CuBit.Firmware_Snapshot;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with CuBit.Handles.Services;
package Startup with SPARK_Mode, Elaborate_Body is

   --  Endpoint "logstore", required.
   function Logstore return Standard.CuBit.Handles.Services.Logstore.Endpoint is
     (Standard.CuBit.Handles.Services.Logstore.Named ("logstore"));

   --  Firmware_Snapshot "tables", required.
   function Tables return Standard.CuBit.Firmware_Snapshot.Authority is
     (Standard.CuBit.Firmware_Snapshot.Named ("tables"));

   --  Notification "registration", required.
   function Registration return Standard.CuBit.Handles.Notification is
     (Standard.CuBit.Handles.Startup.Notification_Named ("registration", Standard.CuBit.Startup_Tables.Notification));

   --  The table declared above; the package's elaboration checks the
   --  loaded one against it and stops the program if it falls short.
   Expected : constant Standard.CuBit.Startup_State.Expectations :=
     [(Standard.CuBit.Startup_Tables.To_Name ("logstore"), Standard.CuBit.Startup_Tables.Endpoint, Standard.CuBit.Startup_Tables.Required),
      (Standard.CuBit.Startup_Tables.To_Name ("tables"), Standard.CuBit.Startup_Tables.Firmware_Snapshot, Standard.CuBit.Startup_Tables.Required),
      (Standard.CuBit.Startup_Tables.To_Name ("registration"), Standard.CuBit.Startup_Tables.Notification, Standard.CuBit.Startup_Tables.Required)];
end Startup;
