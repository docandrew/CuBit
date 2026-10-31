with Ada.Text_IO;
with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
with Migration_Tables;
procedure Bootstrap_Migration_Tests is
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Values.Module_Outcome;
   use type Firmware_Tables.Byte;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   Good : constant Firmware_Tables.Bytes := Migration_Tables.Make ([8, 86, 65, 76, 48, 1]);
   Bad : constant Firmware_Tables.Bytes := Migration_Tables.Make ([16#FE#]);
begin
   for Malformed in Boolean loop
      for Budget in Startup_Budget range 0 .. 1 loop
         declare
            S : State (2, 256, 128, Load_Capacity => 2);
            Accepted : Boolean;
            Result : ACPI_Service.Install_Status;
            Source : Firmware_Tables.Bytes := (if Malformed then Bad else Good);
            Fuel : constant Startup_Budget := (if Budget = 0 then 0 else 64);
            Before : State_Model (2, 256, Load_Capacity => 2) with Ghost;
         begin
            Start (S, 1, Accepted); Check (Accepted);
            Import_Table (S, 77, ACPI_Service.DSDT, Source, Result);
            Check (Result = ACPI_Service.Installed and Current (S) = Receiving);
            Check (Observe (S).Objects = 0 and Pending_Members (S) = 0);
            for I in Source'Range loop Source (I) := 0; end loop;
            Check (Table_Byte (S, 1, 0) = Character'Pos ('D'));
            Finish (S, Fuel);
            Check (Completion (S).Execution.Modules.Charged <= Fuel);
            if Malformed or Budget = 0 then
               Check (Current (S) = Failed and not Members_Initialized (S));
               Check (Completion (S).Stage = Execution_Failed);
               Check (Completion (S).Execution.Failed_Table_ID = 77);
            else
               Check (Current (S) = Complete and Members_Initialized (S));
               Check (Completion (S).Stage = Finished and Observe (S).Objects = ACPI_Service.Values.Firmware_Seed_Count + 1);
               Check (Completion (S).Execution.Modules.Outcome = ACPI_Service.Values.Modules_Completed);
            end if;
            Before := Model (S);
            declare Saved : constant Finish_Report := Completion (S); begin
               Finish (S, 0); Check (Completion (S) = Saved);
               pragma Assert (Model (S) = Before);
               Import_Table (S, 78, ACPI_Service.DSDT, Good, Result);
               Check (Result = ACPI_Service.Wrong_Order and Completion (S) = Saved);
               pragma Assert (Model (S) = Before);
            end;
         end;
      end loop;
   end loop;
   declare
      S : State (2, 256, 128, Load_Capacity => 2);
      Accepted : Boolean;
      Result : ACPI_Service.Install_Status;
   begin
      Start (S, 2, Accepted); Check (Accepted);
      Import_Table (S, 1, ACPI_Service.DSDT, Good, Result); Check (Result = ACPI_Service.Installed);
      Finish (S); Check (Current (S) = Failed and Completion (S).Stage = Not_Attempted);
      Check (Observe (S).Objects = 0 and Installed (S) = 1);
   end;
   Ada.Text_IO.Put_Line ("Bootstrap migration checks" & Checks'Image);
end Bootstrap_Migration_Tests;
