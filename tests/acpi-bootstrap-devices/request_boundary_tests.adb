with Ada.Text_IO;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with ACPI_Bootstrap;
with Interfaces; use Interfaces;
with Firmware_Tables;
with Migration_Tables;
procedure Request_Boundary_Tests is
   use type ACPI_Bootstrap.Finish_Stage;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   First : constant Firmware_Tables.Bytes := Migration_Tables.Make ([8,86,65,76,48,1]);
   Later : constant Firmware_Tables.Bytes := Migration_Tables.Make ([], True);
begin
   for Bad_Header in Boolean loop
      declare
         S : State (2, 256, 128, 0);
         Reply : Response;
         Bad : Firmware_Tables.Bytes := First;
      begin
         Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [0,2,0,0], others => <>), Reply);
         Check (Reply.Status = OK);
         if Bad_Header then
            Bad (10) := Bad (10) + 1;
            Import_Block (S, Snapshot_Provider, Revision (S), 3, ACPI_Service.DSDT, Bad, Reply);
            Check (Reply.Status = Table_Rejected and Reply.Data (1) = ACPI_Service.Install_Status'Pos (ACPI_Service.Invalid_Table));
            Check (Revision (S) = 2 and Observe (S).Tables = 0);
         else
            Import_Block (S, Snapshot_Provider, Revision (S), 3, ACPI_Service.DSDT, First, Reply);
            Check (Reply.Status = OK);
            Import_Block (S, Snapshot_Provider, Revision (S), 3, ACPI_Service.SSDT, Later, Reply);
            Check (Reply.Status = Table_Rejected and Reply.Data (1) = ACPI_Service.Install_Status'Pos (ACPI_Service.Duplicate_ID));
            Check (Revision (S) = 3 and Observe (S).Tables = 1);
         end if;
         Check (Current (S) = Failed and Observe (S).Objects = 0);
         Check (Completion (S).Stage = ACPI_Bootstrap.Not_Attempted);
         declare Prior : constant Revision_Number := Revision (S); Before : constant State_Model := Model (S) with Ghost; begin
            Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Prior,0,0,0], others => <>), Reply);
            Check (Reply.Status = Wrong_Order and Revision (S) = Prior);
            pragma Assert (Model (S) = Before);
         end;
      end;
   end loop;
   declare
      S : State (1, 128, 128, Max_Revision - 2);
      Reply : Response;
   begin
      Handle (S, Snapshot_Provider, (Label => Start_Snapshot, Data => [Revision (S),1,0,0], others => <>), Reply);
      Check (Reply.Status = OK);
      Import_Block (S, Snapshot_Provider, Revision (S), 7, ACPI_Service.DSDT, First, Reply);
      Check (Reply.Status = OK and Revision (S) = Max_Revision);
      declare Before : constant State_Model := Model (S) with Ghost; begin
         Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [Revision (S),0,0,0], others => <>), Reply);
         Check (Reply.Status = Resource_Limit and Revision (S) = Max_Revision);
         Check (Current (S) = Receiving and Observe (S).Objects = 0
           and Completion (S).Stage = ACPI_Bootstrap.Not_Attempted);
         pragma Assert (Model (S) = Before);
      end;
   end;
   Ada.Text_IO.Put_Line ("Request boundary checks" & Checks'Image);
end Request_Boundary_Tests;
