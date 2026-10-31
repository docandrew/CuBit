pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Firmware_Tables.Catalog;
with ACPI_Service;
package body ACPI_Snapshot_Startup is
   use type Count;
   use type R.Tick;
   use type R.Copy_Status;
   use type R.Outcome;
   use type ACPI_Requests.Outcome;
   use type ACPI_Requests.Phase;
   procedure Load_Using
     (Deadline : R.Tick; Owner : out State_Access; Result : out Report)
   is
      type Descriptions is array (Positive range <>) of Description;
      type Descriptions_Access is access Descriptions;
      type Bytes_Access is access R.F.Bytes;
      procedure Free is new Ada.Unchecked_Deallocation
        (ACPI_Requests.State, State_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Descriptions, Descriptions_Access);
      procedure Free is new Ada.Unchecked_Deallocation (R.F.Bytes, Bytes_Access);
      Items : Descriptions_Access;
      Data : Bytes_Access;
      Candidate : State_Access;
      Completion_Entered : Boolean := False;
      Last_Time : R.Tick := 0;
      Have_Time : Boolean := False;
      Reversed : Boolean := False;
      Total, Largest : Count := 0;
      Reply : ACPI_Requests.Response;
      procedure Guarded_Clock (Now : out R.Tick; Available : out Boolean) is
      begin
         Clock (Now, Available);
         if Available then
            if Have_Time and then Now < Last_Time then
               Reversed := True; Available := False;
            else Last_Time := Now; Have_Time := True;
            end if;
         end if;
      end Guarded_Clock;
      procedure Read is new R.Read_Table (Chunk_Max, Read_Part, Guarded_Clock);
      function Timely return Boolean is
         Now : R.Tick;
         Available : Boolean;
      begin
         Guarded_Clock (Now, Available);
         if Reversed then Result.Status := Clock_Reversed;
         elsif not Available then Result.Status := Clock_Unavailable;
         elsif Now >= Deadline then Result.Status := Deadline_Expired;
         else return True;
         end if;
         return False;
      end Timely;
      procedure Attempt is
      begin
         if not Timely then return; end if;
         Info (Result.Catalog, Result.Source_Status);
         if Result.Source_Status /= R.Copy_OK then Result.Status := Source_Refused; return; end if;
         if not Timely then return; end if;
         if not Result.Catalog.Ready then Result.Status := Inventory_Not_Ready; return; end if;
         if Result.Catalog.Tables = 0 or else
           Result.Catalog.Largest_Table < Count (R.F.Table_Header_Size) or else
           Result.Catalog.Total_Bytes < Result.Catalog.Largest_Table
         then Result.Status := Invalid_Inventory; return; end if;
         -- Check full-width values before ANY array bound or discriminant cast.
         if Result.Catalog.Tables > Count (Positive'Last) or else
           Result.Catalog.Total_Bytes > Count (Positive'Last) or else
           Result.Catalog.Largest_Table > Count (Positive'Last)
         then Result.Status := Representation_Limit; return; end if;
         if Result.Catalog.Tables > Result.Catalog.Total_Bytes / Count (R.F.Table_Header_Size)
         then Result.Status := Invalid_Inventory; return; end if;
         Items := new Descriptions (1 .. Positive (Result.Catalog.Tables));
         for ID in Items'Range loop
            Result.Table := ID;
            if not Timely then return; end if;
            Describe (ID, Result.Descriptor, Result.Source_Status);
            if Result.Source_Status /= R.Copy_OK then Result.Status := Source_Refused; return; end if;
            if not Timely then return; end if;
            if Result.Descriptor.Extent < Count (R.F.Table_Header_Size) or else
              Result.Descriptor.Extent > Result.Catalog.Largest_Table or else
              Result.Descriptor.Name = "FACS" or else
              ((ID = 1) /= (Result.Descriptor.Name = "DSDT"))
            then Result.Status := Invalid_Description; return; end if;
            if Result.Descriptor.Extent > Result.Catalog.Total_Bytes - Total then
               Result.Status := Inventory_Mismatch; return;
            end if;
            Total := Total + Result.Descriptor.Extent;
            Largest := Count'Max (Largest, Result.Descriptor.Extent);
            Items (ID) := Result.Descriptor;
         end loop;
         if Total /= Result.Catalog.Total_Bytes or else Largest /= Result.Catalog.Largest_Table
         then Result.Status := Inventory_Mismatch; return; end if;
         if not Timely then return; end if;
         -- Construct the limited owner IN PLACE. Neither initialized arenas nor
         -- tables are copied through a fixed-size default instance.
         Candidate := new ACPI_Requests.State
           (Positive (Result.Catalog.Tables), Positive (Total), Positive (Largest),
            Positive (Result.Catalog.Tables), 0);
         ACPI_Requests.Handle (Candidate.all, ACPI_Requests.Snapshot_Provider,
           (Label => ACPI_Requests.Start_Snapshot,
            Data => [0, Result.Catalog.Tables, 0, 0], others => <>), Reply);
         Result.Admission := Reply.Status;
         if Reply.Status /= ACPI_Requests.OK then Result.Status := Admission_Refused; return; end if;
         for ID in Items'Range loop
            Result.Table := ID; Result.Descriptor := Items (ID);
            if not Timely then return; end if;
            Data := new R.F.Bytes (1 .. Positive (Items (ID).Extent));
            Read ((R.S.Table_ID (ID), Items (ID).Name, Items (ID).Revision,
              Firmware_Tables.Catalog.Table_Length (Items (ID).Extent)),
              Deadline, Data.all, Result.Copy);
            if Result.Copy.Status /= R.Copied_Table then Result.Status := Copy_Refused; return; end if;
            if not Timely then return; end if;
            ACPI_Requests.Import_Block
              (Candidate.all, ACPI_Requests.Snapshot_Provider,
               ACPI_Requests.Revision (Candidate.all), ID,
               (if Items (ID).Name = "DSDT" then ACPI_Service.DSDT
                elsif Items (ID).Name = "SSDT" then ACPI_Service.SSDT
                else ACPI_Service.Description), Data.all, Reply);
            Free (Data);
            Result.Admission := Reply.Status;
            if Reply.Status /= ACPI_Requests.OK then Result.Status := Admission_Refused; return; end if;
         end loop;
         if not Timely then return; end if;
         Completion_Entered := True;
         Complete_Startup (Candidate.all, Reply);
         Result.Admission := Reply.Status;
         Result.Completion := ACPI_Requests.Completion (Candidate.all);
         -- Copy before any failed candidate is released. A device initializer
         -- Fatal may also coexist with a complete retained table catalog.
         Result.Fatal := ACPI_Requests.Fatal_Diagnostic (Candidate.all);
         Result.Module_Errors := ACPI_Requests.Module_Errors (Candidate.all);
         if Reply.Status /= ACPI_Requests.OK or else
           ACPI_Requests.Current (Candidate.all) /= ACPI_Requests.Complete
         then Result.Status := Startup_Refused; return; end if;
         if not Timely then return; end if;
         Owner := Candidate; Candidate := null;
         Result.Status := Loaded;
      end Attempt;
      procedure Cleanup is
      begin
         if Retain_Failed_Owner and then Completion_Entered and then Candidate /= null then
            Owner := Candidate; Candidate := null; Result.Owner_Retained := True;
         end if;
         if Data /= null then Data.all := [others => 0]; Free (Data); end if;
         Free (Items);
         Free (Candidate);
      end Cleanup;
   begin
      Owner := null; Result := (others => <>);
      -- Allocation and callbacks must return normally. Native resource
      -- exhaustion or an unexpected failure terminates the process. Cleanup
      -- and retained ownership below cover every legal typed refusal.
      Attempt;
      Cleanup;
   end Load_Using;
   procedure Default_Completion
     (Server : in out ACPI_Requests.State; Reply : out ACPI_Requests.Response) is
   begin
      ACPI_Requests.Handle (Server, ACPI_Requests.Snapshot_Provider,
        (Label => ACPI_Requests.Finish_Snapshot,
         Data => [ACPI_Requests.Revision (Server), 0, 0, 0], others => <>), Reply);
   end Default_Completion;
   procedure Load
     (Deadline : R.Tick; Owner : out State_Access; Result : out Report) is
      procedure Import_And_Complete is new Load_Using
        (Info, Describe, Chunk_Max, Read_Part, Clock, Default_Completion, False);
   begin
      Import_And_Complete (Deadline, Owner, Result);
   end Load;
end ACPI_Snapshot_Startup;
