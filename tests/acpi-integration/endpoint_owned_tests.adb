with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Service;
with Firmware_Tables;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Endpoint_Owned_Tests is
   Data : Firmware_Tables.Bytes (1 .. 36) := [others => 0];
   Sum : Firmware_Tables.Byte := 0;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Data (1 .. 4) := [68, 83, 68, 84]; Data (5) := 36; Data (9) := 2;
   for B of Data loop Sum := Sum + B; end loop;
   Data (10) := 0 - Sum;
   for Config_ID in 0 .. 2 loop
      for Stamp in Unsigned_64 range 10 .. 12 loop
         declare
            S : State (2, 256, 128, 0);
            Config : constant ACPI_Endpoint.Configuration :=
              (Observer_Tag => (if Config_ID = 1 then 0 else 10),
               Provider_Tag => (if Config_ID = 2 then 10 else 11));
            Reply : Packet;
            Allowed : constant Boolean := Config_ID = 0 and Stamp = 11;
         begin
            ACPI_Endpoint.Dispatch (S, Config, Stamp,
              (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>), Reply);
            Check (Reply.Label = (if Allowed then ACPI_Endpoint.Reply_OK else ACPI_Endpoint.Reply_Error));
            Check (Current (S) = (if Allowed then Receiving else Idle));
            Check (Revision (S) = (if Allowed then 1 else 0));
            if not Allowed then Check (Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0]); end if;
            ACPI_Endpoint.Dispatch_Block (S, Config, Stamp, Revision (S), 1, ACPI_Service.DSDT, Data, Reply);
            Check (Observe (S).Tables = (if Allowed then 1 else 0));
            Check (Reply.Label = (if Allowed then ACPI_Endpoint.Reply_OK else ACPI_Endpoint.Reply_Error));
            if Allowed then
               Check (Revision (S) = 2);
               ACPI_Endpoint.Dispatch (S, Config, Stamp,
                 (Label => Finish_Snapshot, Data => [2, 0, 0, 0], others => <>), Reply);
               Check (Reply.Label = ACPI_Endpoint.Reply_OK and Current (S) = Complete and Revision (S) = 3);
               ACPI_Endpoint.Dispatch (S, Config, 10,
                 (Label => Read_Table_Info, Data => [3, 1, 0, 0], others => <>), Reply);
               Check (Reply.Label = ACPI_Endpoint.Reply_OK and Reply.Data (1) = 1 and Reply.Data (2) = 36);
               ACPI_Endpoint.Dispatch (S, Config, 12,
                 (Label => Start_Snapshot, Data => [11, 1, 0, 0], others => <>), Reply);
               Check (Reply.Data = [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0] and Revision (S) = 3);
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Owned endpoint PASS" & Checks'Image);
end Endpoint_Owned_Tests;
