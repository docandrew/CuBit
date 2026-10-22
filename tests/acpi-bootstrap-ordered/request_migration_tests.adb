with Ada.Text_IO;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
with Interfaces; use Interfaces;
with Migration_Tables;
procedure Request_Migration_Tests is
   use type ACPI_Bootstrap.Finish_Stage;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
begin
   for Bulk in Boolean loop
      for Bad_AML in Boolean loop
         declare
            S : State (1, 128, 128, 7);
            Reply : Response;
            Data : constant Firmware_Tables.Bytes := Migration_Tables.Make
              ((if Bad_AML then Firmware_Tables.Bytes'[16#FE#]
                else Firmware_Tables.Bytes'[8, 86, 65, 76, 48, 1]));
            procedure Send (Label : Unsigned_32; A, B, C : Unsigned_64 := 0;
                            Expected : Outcome := OK) is
               Prior : constant Revision_Number := Revision (S);
            begin
               Handle (S, Snapshot_Provider,
                 (Label => Label, Data => [Prior, A, B, C], others => <>), Reply);
               Check (Reply.Status = Expected);
               Check (Revision (S) = Prior + (if Expected in OK | Incomplete then 1 else 0));
            end Send;
         begin
            Send (Start_Snapshot, 1);
            if Bulk then
               Import_Block (S, Snapshot_Provider, Revision (S), 42, ACPI_Service.DSDT, Data, Reply);
               Check (Reply.Status = OK and Revision (S) = 9);
            else
               Send (Begin_Table, 42, 0, Data'Length);
               Send (Finish_Snapshot, Expected => Wrong_Order);
               declare Offset : Natural := 0; begin
                  while Offset < Data'Length loop
                     declare
                        Amount : constant Natural := Natural'Min (16, Data'Length - Offset);
                        Low, High : Unsigned_64 := 0;
                     begin
                        for I in 0 .. Amount - 1 loop
                           if I < 8 then Low := Low or Shift_Left (Unsigned_64 (Data (1 + Offset + I)), I * 8);
                           else High := High or Shift_Left (Unsigned_64 (Data (1 + Offset + I)), (I - 8) * 8); end if;
                        end loop;
                        Send (Write_Chunk, Unsigned_64 (Offset), Low, High);
                        Offset := Offset + Amount;
                     end;
                  end loop;
               end;
               Send (Commit_Table);
            end if;
            Check (Observe (S).Objects = 0 and Observe (S).Tables = 1);
            declare
               Before : constant State_Model := Model (S) with Ghost;
               Prior : constant Revision_Number := Revision (S);
            begin
               for Origin in No_Authority .. Observer loop
                  Handle (S, Origin, (Label => Finish_Snapshot, Data => [Prior, 0, 0, 0], others => <>), Reply);
                  Check (Reply.Status = Denied and Revision (S) = Prior);
                  pragma Assert (Model (S) = Before);
               end loop;
               Handle (S, Snapshot_Provider, (Label => Finish_Snapshot, Data => [7, 0, 0, 0], others => <>), Reply);
               Check (Reply.Status = Stale and Revision (S) = Prior);
               pragma Assert (Model (S) = Before);
               Send (Finish_Snapshot, 1, Expected => Malformed);
               pragma Assert (Model (S) = Before);
            end;
            Send (Finish_Snapshot, Expected => (if Bad_AML then Incomplete else OK));
            Check (Current (S) = (if Bad_AML then Failed else Complete));
            Check (Completion (S).Stage = (if Bad_AML then ACPI_Bootstrap.Execution_Failed else ACPI_Bootstrap.Finished));
            Check (Completion (S).Execution.Modules.Charged <= ACPI_Bootstrap.Default_Startup_Budget);
            if Bad_AML then Check (Completion (S).Execution.Failed_Table_ID = 42);
            else Check (Observe (S).Objects = ACPI_Service.Values.Firmware_Seed_Count + 1); end if;
            declare Before : constant State_Model := Model (S) with Ghost; begin
               Send (Finish_Snapshot, Expected => Wrong_Order);
               pragma Assert (Model (S) = Before);
            end;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Request migration checks" & Checks'Image);
end Request_Migration_Tests;
