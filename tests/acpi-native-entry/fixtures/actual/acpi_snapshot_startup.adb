pragma Ada_2022;
with ACPI_Fallible_Heap;
with Firmware_Tables.Catalog;
with ACPI_Service;
with AML_Storage_Capacities;
with AML_Objects;
with System;
package body ACPI_Snapshot_Startup is
   package D renames ACPI_Allocation_Diagnostics;
   use type D.Count;
   use type Count;
   use type R.Tick;
   use type R.Copy_Status;
   use type R.Outcome;
   use type ACPI_Requests.Outcome;
   use type ACPI_Requests.Phase;
   procedure Release_Owner (Owner : in out State_Access; Released : out Boolean) is
   begin
      if Owner = null then Released := True; return; end if;
      ACPI_Requests.Release_Storage (Owner.all, Released);
      if Released then
         ACPI_Fallible_Heap.Release (Owner.all'Address);
         Owner := null;
      end if;
   end Release_Owner;
   procedure Load_Using
     (Deadline : R.Tick; Owner : out State_Access; Result : out Report)
   is
      type Descriptions is array (Positive range <>) of Description;
      type Description_Store (Length : Positive) is limited record
         Values : Descriptions (1 .. Length);
      end record;
      type Byte_Store (Length : Positive) is limited record
         Values : R.F.Bytes (1 .. Length);
      end record;
      type Descriptions_Access is access all Description_Store;
      type Bytes_Access is access all Byte_Store;
      procedure Free (Item : in out Descriptions_Access) is
      begin
         if Item = null then return; end if;
         ACPI_Fallible_Heap.Release (Item.all'Address); Item := null;
      end Free;
      procedure Free (Item : in out Bytes_Access) is
      begin
         if Item = null then return; end if;
         ACPI_Fallible_Heap.Release (Item.all'Address); Item := null;
      end Free;
      procedure Free (Item : in out State_Access) is
      begin
         if Item = null then return; end if;
         ACPI_Fallible_Heap.Release (Item.all'Address); Item := null;
      end Free;
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
      function Allocate_Items return Descriptions_Access is
         subtype Shape is Description_Store (Positive (Result.Catalog.Tables));
         function Create return Descriptions_Access is
            use type System.Address;
            Bits : constant Count := Shape'Object_Size;
            Bytes : constant Count := Bits / System.Storage_Unit +
              (if Bits mod System.Storage_Unit = 0 then 0 else 1);
            Address : constant System.Address := ACPI_Fallible_Heap.Reserve
              (Bytes, Count'Max (16, Shape'Alignment));
         begin
            if Address = System.Null_Address then return null; end if;
            declare
               -- This view initializes fresh heap storage, not an existing object.
               pragma Warnings (Off, "default initialization of * may modify overlaid storage");
               Value : aliased Shape with Address => Address;
               pragma Warnings (On, "default initialization of * may modify overlaid storage");
            begin
               return Value'Unchecked_Access;
            end;
         end Create;
         Item : constant Descriptions_Access := Create;
      begin
         if Item = null then
            Result.Status := Allocation_Refused;
            Result.Allocation := (D.Snapshot_Catalog, D.Process_Storage,
              D.Count (Result.Catalog.Tables), D.Entries, 0);
         end if;
         return Item;
      end Allocate_Items;
      function Allocate_Copy (Extent : Positive) return Bytes_Access is
         subtype Shape is Byte_Store (Extent);
         function Create return Bytes_Access is
            use type System.Address;
            Bits : constant Count := Shape'Object_Size;
            Bytes : constant Count := Bits / System.Storage_Unit +
              (if Bits mod System.Storage_Unit = 0 then 0 else 1);
            Address : constant System.Address := ACPI_Fallible_Heap.Reserve
              (Bytes, Count'Max (16, Shape'Alignment));
         begin
            if Address = System.Null_Address then return null; end if;
            declare
               -- This view initializes fresh heap storage, not an existing object.
               pragma Warnings (Off, "default initialization of * may modify overlaid storage");
               Value : aliased Shape with Address => Address;
               pragma Warnings (On, "default initialization of * may modify overlaid storage");
            begin
               return Value'Unchecked_Access;
            end;
         end Create;
         Item : constant Bytes_Access := Create;
      begin
         if Item = null then
            Result.Status := Allocation_Refused;
            Result.Allocation := (D.Table_Copy, D.Process_Storage, D.Count (Extent), D.Payload_Bytes, 0);
         end if;
         return Item;
      end Allocate_Copy;
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
         if Result.Catalog.Tables > Count (Positive'Last) then
            Result.Status := Representation_Limit;
            Result.Allocation := (D.Snapshot_Catalog, D.Entry_Representation,
              D.Count (Result.Catalog.Tables), D.Entries, D.Count (Positive'Last)); return;
         elsif Result.Catalog.Total_Bytes > Count (Positive'Last) then
            Result.Status := Representation_Limit;
            Result.Allocation := (D.Copied_Input, D.Byte_Representation,
              D.Count (Result.Catalog.Total_Bytes), D.Payload_Bytes, D.Count (Positive'Last)); return;
         elsif Result.Catalog.Largest_Table > Count (Positive'Last) then
            Result.Status := Representation_Limit;
            Result.Allocation := (D.Table_Copy, D.Byte_Representation,
              D.Count (Result.Catalog.Largest_Table), D.Payload_Bytes, D.Count (Positive'Last)); return;
         end if;
         if not ACPI_Service.Storage_Input_Supported (Positive (Result.Catalog.Total_Bytes)) then
            Result.Status := Representation_Limit;
            Result.Allocation := (D.AML_Owner, D.Byte_Representation,
              D.Count (Result.Catalog.Total_Bytes), D.Payload_Bytes, D.Count (AML_Objects.Storage_Capacity'Last)); return;
         end if;
         if Result.Catalog.Tables > Result.Catalog.Total_Bytes / Count (R.F.Table_Header_Size)
         then Result.Status := Invalid_Inventory; return; end if;
         Items := Allocate_Items;
         if Items = null then return; end if;
         for ID in Items.Values'Range loop
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
            Items.Values (ID) := Result.Descriptor;
         end loop;
         if Total /= Result.Catalog.Total_Bytes or else Largest /= Result.Catalog.Largest_Table
         then Result.Status := Inventory_Mismatch; return; end if;
         if not Timely then return; end if;
         -- Construct the limited owner IN PLACE. Neither initialized arenas nor
         -- tables are copied through a fixed-size default instance.
         declare
            Limits : constant AML_Storage_Capacities.Limits := ACPI_Service.Initial_Storage_Limits (Positive (Total));
            subtype Request_Shape is ACPI_Requests.State
              (Positive (Result.Catalog.Tables), Positive (Total), Positive (Largest),
               Positive (Result.Catalog.Tables), 0, Limits.Objects, Limits.Bytes, Limits.Elements);
            function Create return State_Access is
               use type System.Address;
               Bits : constant Count := Request_Shape'Object_Size;
               Bytes : constant Count := Bits / System.Storage_Unit +
                 (if Bits mod System.Storage_Unit = 0 then 0 else 1);
               Address : constant System.Address := ACPI_Fallible_Heap.Reserve
                 (Bytes, Count'Max (16, Request_Shape'Alignment));
            begin
               if Address = System.Null_Address then return null; end if;
               declare
                  -- This view initializes fresh heap storage, not an existing object.
                  pragma Warnings (Off, "default initialization of * may modify overlaid storage");
                  Value : aliased Request_Shape with Address => Address;
                  pragma Warnings (On, "default initialization of * may modify overlaid storage");
               begin
                  return Value'Unchecked_Access;
               end;
            end Create;
            function Bytes_Of (Bits : D.Count) return D.Count is
              (Bits / D.Count (System.Storage_Unit) +
                (if Bits mod D.Count (System.Storage_Unit) = 0 then 0 else 1));
            Requested : constant D.Count := Bytes_Of (D.Count (Request_Shape'Object_Size));
         begin
            Candidate := Create;
            if Candidate = null then
               Result.Status := Allocation_Refused;
               Result.Allocation := (D.AML_Owner, D.Process_Storage, Requested, D.Owner_Block_Bytes, 0);
               return;
            end if;
         end;
         ACPI_Requests.Handle (Candidate.all, ACPI_Requests.Snapshot_Provider,
           (Label => ACPI_Requests.Start_Snapshot,
            Data => [0, Result.Catalog.Tables, 0, 0], others => <>), Reply);
         Result.Admission := Reply.Status;
         if Reply.Status /= ACPI_Requests.OK then Result.Status := Admission_Refused; return; end if;
         for ID in Items.Values'Range loop
            Result.Table := ID; Result.Descriptor := Items.Values (ID);
            if not Timely then return; end if;
            Data := Allocate_Copy (Positive (Items.Values (ID).Extent));
            if Data = null then return; end if;
            Read ((R.S.Table_ID (ID), Items.Values (ID).Name, Items.Values (ID).Revision,
              Firmware_Tables.Catalog.Table_Length (Items.Values (ID).Extent)),
              Deadline, Data.Values, Result.Copy);
            if Result.Copy.Status /= R.Copied_Table then Result.Status := Copy_Refused; return; end if;
            if not Timely then return; end if;
            ACPI_Requests.Import_Block
              (Candidate.all, ACPI_Requests.Snapshot_Provider,
               ACPI_Requests.Revision (Candidate.all), ID,
               (if Items.Values (ID).Name = "DSDT" then ACPI_Service.DSDT
                elsif Items.Values (ID).Name = "SSDT" then ACPI_Service.SSDT
                else ACPI_Service.Description), Data.Values, Reply);
            Free (Data);
            Result.Admission := Reply.Status;
            if Reply.Status /= ACPI_Requests.OK then Result.Status := Admission_Refused; return; end if;
         end loop;
         if not Timely then return; end if;
         Completion_Entered := True;
         Complete_Startup (Candidate.all, Reply);
         Result.Admission := Reply.Status;
         Result.Completion := ACPI_Requests.Completion (Candidate.all);
         declare
            Used : constant ACPI_Service.Metrics := ACPI_Requests.Observe (Candidate.all);
            Limits : constant AML_Storage_Capacities.Limits := ACPI_Requests.Storage_Limits (Candidate.all);
         begin
            Result.Values := (Available => True,
              Objects_Used => D.Count (Used.Value_Objects), Objects_Capacity => D.Count (Limits.Objects),
              Bytes_Used => D.Count (Used.Value_Bytes), Bytes_Capacity => D.Count (Limits.Bytes),
              Elements_Used => D.Count (Used.Package_Elements), Elements_Capacity => D.Count (Limits.Elements));
         end;
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
         if Data /= null then Data.Values := [others => 0]; Free (Data); end if;
         Free (Items);
         if Candidate /= null then
            declare Released : Boolean; begin
               ACPI_Requests.Release_Storage (Candidate.all, Released);
               if Released then Free (Candidate);
               else
                  -- Never drop an owner that still carries a live execution
                  -- frame or an uncertain provider observation.
                  Owner := Candidate; Candidate := null; Result.Owner_Retained := True;
               end if;
            end;
         end if;
      end Cleanup;
   begin
      Owner := null; Result := (others => <>);
      -- These three outer storage reservations return typed refusals.
      -- Nested owner default allocations remain native fail-stop until their
      -- staged construction is integrated; no all-graph graceful refusal claim.
      -- Callback failures remain fatal: an unexpected effect cannot be undone
      -- by this loader. Legal allocation refusals follow normal cleanup.
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
