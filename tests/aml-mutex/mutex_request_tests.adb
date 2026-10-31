with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
with Root_Test_Support; use Root_Test_Support;
procedure Mutex_Request_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   Payload : constant Bytes := [16#5B#, 1] & Enc ("M000") & [0, 16#5B#, 16#27#] & Enc ("M000");
   Raw : Bytes (1 .. 36 + Payload'Length) := [others => 0];
   Sum : Unsigned_8 := 0;
   S : State (1, 256, 256, Load_Capacity => 1, Initial_Revision => 0);
   Reply : Response;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
begin
   Raw (1 .. 4) := Enc ("DSDT"); Raw (5) := Unsigned_8 (Raw'Length); Raw (9) := 2;
   Raw (11 .. 16) := Enc ("CUBIT "); Raw (17 .. 24) := Enc ("MUTEXERR");
   Raw (37 .. Raw'Last) := Payload;
   for B of Raw loop Sum := Sum + B; end loop;
   Raw (10) := 0 - Sum;
   Check (Startup_Error_Code'Enum_Rep (Error_Notify_Limit) = 38, "old notify tag");
   Check (Startup_Error_Code'Enum_Rep (Error_Module_Completed) = 39, "old module success tag");
   Check (Startup_Error_Code'Enum_Rep (Error_Mutex_Not_Acquired) = 40, "new error tag");
   Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>), Reply);
   Check (Reply.Status = OK, "start");
   Import_Block (S, Snapshot_Provider, Revision (S), 1, ACPI_Service.DSDT, Raw, Reply);
   Check (Reply.Status = OK, "import");
   Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (S), 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = Incomplete and then Current (S) = Failed, "unowned release is a real module failure");
   Handle (S, Observer, (Label => Read_Metrics, Data => [Metrics_Module_Error_Page, 0, 0, 0], others => <>), Reply);
   Check (Reply.Status = OK and then Reply.Data = [Revision (S), 40, 0, 0], "actual module error wire mapping");
   Ada.Text_IO.Put_Line ("Mutex request checks" & Checks'Image);
end Mutex_Request_Tests;
