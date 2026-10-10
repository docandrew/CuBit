with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Requests_Owned_Tests is
   use type ACPI_Service.Metrics;
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
   for Bulk in Boolean loop
      declare
         S : State (2, 256, 128, 0);
         Reply : Response;
         Request : Packet;
         Source : Firmware_Tables.Bytes := Data;
         procedure Send (Label : Unsigned_32; A, B, C : Unsigned_64 := 0;
                         Expected : Outcome := OK) is
            Prior : constant Revision_Number := Revision (S);
         begin
            Handle (S, Snapshot_Provider,
              (Label => Label, Data => [Prior, A, B, C], others => <>), Reply);
            Check (Reply.Status = Expected and Reply.Data (0) = Revision (S));
            Check (Revision (S) = Prior + (if Expected = OK then 1 else 0));
         end Send;
      begin
         Check (Valid (S) and Current (S) = Idle and Revision (S) = 0);
         for Label in Unsigned_32 range 0 .. 8 loop
            Handle (S, No_Authority, (Label => Label, others => <>), Reply);
            Check (Reply.Status = Denied and Reply.Data = [0, 0, 0, 0]);
            Check (Current (S) = Idle and Revision (S) = 0 and Observe (S) = ACPI_Service.Metrics'(others => <>));
         end loop;
         for Label in Start_Snapshot .. Finish_Snapshot loop
            Handle (S, Observer, (Label => Label, Data => [0, 1, 0, 0], others => <>), Reply);
            Check (Reply.Status = Denied and Current (S) = Idle and Revision (S) = 0);
         end loop;
         Send (Start_Snapshot, 0, Expected => Malformed);
         Send (Start_Snapshot, 3, Expected => Malformed);
         Send (Start_Snapshot, 1);
         Send (Start_Snapshot, 1, Expected => Wrong_Order);
         Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0, 1, 0, 0], others => <>), Reply);
         Check (Reply.Status = Stale and Revision (S) = 1);
         for Origin in No_Authority .. Observer loop
            Import_Block (S, Origin, Revision (S), 1, ACPI_Service.DSDT, Data, Reply);
            Check (Reply.Status = Denied and Observe (S).Tables = 0 and Revision (S) = 1);
         end loop;
         if Bulk then
            Import_Block (S, Snapshot_Provider, 0, 1, ACPI_Service.DSDT, Data, Reply);
            Check (Reply.Status = Stale and Revision (S) = 1);
            Import_Block (S, Snapshot_Provider, Revision (S), 1, ACPI_Service.DSDT, Source, Reply);
            Check (Reply.Status = OK and Revision (S) = 2);
         else
            Send (Begin_Table, 1, 0, 36);
            Send (Commit_Table, Expected => Wrong_Order);
            Send (Finish_Snapshot, Expected => Wrong_Order);
            for Part in 0 .. 2 loop
               Request := (Label => Write_Chunk, Data => [Revision (S), Unsigned_64 (Part * 16), 0, 0], others => <>);
               for I in 0 .. Natural'Min (15, 35 - Part * 16) loop
                  Request.Data (2 + I / 8) := Request.Data (2 + I / 8) or
                    Shift_Left (Unsigned_64 (Source (1 + Part * 16 + I)), (I mod 8) * 8);
               end loop;
               Handle (S, Snapshot_Provider, Request, Reply);
               Check (Reply.Status = OK and Received (S) = Natural'Min (36, (Part + 1) * 16));
            end loop;
            Send (Commit_Table);
         end if;
         Source := [others => 255];
         Check (Observe (S).Tables = 1 and Observe (S).Bytes = 36);
         Check (not Table_Open (S) and Received (S) = 0);
         Send (Finish_Snapshot);
         Check (Current (S) = Complete);
         Send (Start_Snapshot, 1, Expected => Wrong_Order);
         for Offset in 0 .. 36 loop
            Handle (S, Observer, (Label => Read_Table_Chunk,
              Data => [Revision (S), 1, Unsigned_64 (Offset), 0], others => <>), Reply);
            Check (Reply.Status = OK and Reply.Data (1) = Unsigned_64 (Natural'Min (8, 36 - Offset)));
            for I in 0 .. Natural'Min (8, 36 - Offset) - 1 loop
               Check (((Shift_Right (Reply.Data (2 + I / 4), (I mod 4) * 8)) and 255) =
                 Unsigned_64 (Data (1 + Offset + I)));
            end loop;
         end loop;
         for Page in Unsigned_64 range 0 .. 6 loop
            Handle (S, Observer, (Label => Read_Metrics, Data => [Page, 0, 0, 0], others => <>), Reply);
            Check (Reply.Status = OK and Current (S) = Complete);
         end loop;
      end;
   end loop;
   for Initial in Max_Revision - 1 .. Max_Revision loop
      declare
         S : State (1, 128, 128, Initial);
         Reply : Response;
      begin
         Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [Initial, 1, 0, 0], others => <>), Reply);
         Check (Reply.Status = (if Initial = Max_Revision then Resource_Limit else OK));
         Check (Revision (S) = Max_Revision);
         Import_Block (S, Snapshot_Provider, Revision (S), 1, ACPI_Service.DSDT, Data, Reply);
         Check (Reply.Status = Resource_Limit and Observe (S).Tables = 0 and Revision (S) = Max_Revision);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Owned requests PASS" & Checks'Image);
end Requests_Owned_Tests;
