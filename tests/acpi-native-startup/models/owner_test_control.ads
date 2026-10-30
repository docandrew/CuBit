pragma Ada_2022;
with AML_Decode;
with ACPI_Snapshot_Startup;
with Interfaces;
package Owner_Test_Control is
   type Scenario is
     (Success, Module_Fatal, Root_Fatal, Bus_Fatal, Device_Fatal,
      Ordinary_Error, Unconfigured_Region, Clock_Unavailable, Clock_Overflow,
      Snapshot_Refused, Copy_Refused, Register_Refused, Load_Exception);
   Selected : Scenario := Success;
   Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   Broken_Critical : Boolean := False;
   Loads, Registrations, Serves, Native_Reads, Copies, Clock_Reads : Natural := 0;
   Sequence, Load_Completed_At, Registered_At, Served_At : Natural := 0;
   Passed_Deadline : ACPI_Snapshot_Startup.R.Tick := 0;
   Registered_Role : Interfaces.Unsigned_64 := 0;
   Last_Load : ACPI_Snapshot_Startup.Report;
   type Level is (Information, Warning, Critical);
   Log_Capacity : constant := 16;
   Text_Capacity : constant := 512;
   type Record_Item is record
      Severity : Level := Information;
      Length : Natural := 0;
      Text : String (1 .. Text_Capacity) := [others => ' '];
   end record;
   type Records is array (Positive range 1 .. Log_Capacity) of Record_Item;
   Log : Records;
   Log_Count, Critical_Attempts : Natural := 0;
   procedure Write (Severity : Level; Text : String);
end Owner_Test_Control;
