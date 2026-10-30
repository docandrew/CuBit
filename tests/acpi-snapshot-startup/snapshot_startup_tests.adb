pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;
with ACPI_Requests;
with ACPI_Snapshot_Startup;
with ACPI_Snapshot_Reader;
procedure Snapshot_Startup_Tests is
   package L renames ACPI_Snapshot_Startup;
   package R renames ACPI_Snapshot_Reader;
   use type L.Count;
   use type L.Outcome;
   use type L.State_Access;
   use type R.Tick;
   use type R.Copy_Status;
   use type R.F.Byte;
   use type R.S.Table_ID;
   use type ACPI_Requests.Phase;
   use type Interfaces.Unsigned_32;
   Large_Extent : constant := 65_573;
   DSDT : R.F.Bytes (1 .. R.F.Table_Header_Size + 1);
   OEM : R.F.Bytes (1 .. Large_Extent);
   Catalog : L.Inventory;
   First, Second : L.Description;
   Info_Status, Descriptor_Status : R.Copy_Status := R.Copy_OK;
   Refused_Descriptor : Natural := 0;
   Short_At : Natural := 0;
   Time : R.Tick := 10;
   Clock_Calls, Clock_Failure_At, Clock_Reverse_At, Clock_Expire_At : Natural := 0;
   Describe_Calls, Copies : Natural := 0;
   Raise_From_Describe : Boolean := False;
   Checks : Natural := 0;
   Owner : L.State_Access;
   Result : L.Report;
   procedure Free is new Ada.Unchecked_Deallocation (ACPI_Requests.State, L.State_Access);
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Label; end if;
   end Check;
   procedure Make_Table (Data : out R.F.Bytes; Name : String) is
      Length : Interfaces.Unsigned_32 := Data'Length;
      Sum : R.F.Byte := 0;
   begin
      Data := [others => 0];
      for I in 1 .. 4 loop
         Data (I) := Character'Pos (Name (I));
         Data (I + 4) := R.F.Byte (Length mod 256);
         Length := Length / 256;
      end loop;
      Data (9) := 2;
      -- One AML Noop in the DSDT; the large OEM table is opaque description.
      if Name = "DSDT" then Data (Data'Last) := 16#A3#; end if;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
   end Make_Table;
   procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
   begin Item := Catalog; Status := Info_Status; end Info;
   procedure Describe (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
   begin
      Describe_Calls := Describe_Calls + 1;
      if Raise_From_Describe then raise Program_Error with "modeled adapter exception"; end if;
      Item := (if ID = 1 then First else Second);
      Status := (if ID = Refused_Descriptor then Descriptor_Status else R.Copy_OK);
   end Describe;
   procedure Clock (Now : out R.Tick; Available : out Boolean) is
   begin
      Clock_Calls := Clock_Calls + 1;
      Now := (if Clock_Calls = Clock_Reverse_At then 0
              elsif Clock_Calls = Clock_Expire_At then 100 else Time);
      Available := Clock_Calls /= Clock_Failure_At;
   end Clock;
   procedure Read_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status) is
      procedure Fill (Source : R.F.Bytes) is
      begin
         Into := Source (Offset + 1 .. Offset + Into'Length);
      end Fill;
   begin
      Copies := Copies + 1;
      if ID = 1 then Fill (DSDT); else Fill (OEM); end if;
      Status := R.Copy_OK;
      Copied := R.Byte_Count (Into'Length);
      if Copies = Short_At then Copied := Copied - 1; end if;
   end Read_Part;
   procedure Load is new L.Load (Info, Describe, 4_096, Read_Part, Clock);
   procedure Reset is
   begin
      Check (Owner = null, "previous owner released");
      Make_Table (DSDT, "DSDT"); Make_Table (OEM, "OEMX");
      Catalog := (True, 2, L.Count (DSDT'Length + OEM'Length), OEM'Length);
      First := ("DSDT", 2, DSDT'Length); Second := ("OEMX", 2, OEM'Length);
      Info_Status := R.Copy_OK; Descriptor_Status := R.Copy_OK;
      Refused_Descriptor := 0; Short_At := 0;
      Time := 10; Clock_Calls := 0;
      Clock_Failure_At := 0; Clock_Reverse_At := 0; Clock_Expire_At := 0;
      Describe_Calls := 0; Copies := 0; Raise_From_Describe := False;
   end Reset;
   procedure Refusal (Expected : L.Outcome) is
   begin
      Load (100, Owner, Result);
      Check (Owner = null, "refusal publishes no partial arena");
      Check (Result.Status = Expected, "typed refusal " & Result.Status'Image & " expected " & Expected'Image);
   end Refusal;
   Successful_Clock_Calls : Natural;
begin
   Reset;
   Load (100, Owner, Result);
   Check (Result.Status = L.Loaded, "direct local startup " & Result.Status'Image);
   Check (Owner /= null, "complete owner published");
   Check (Owner.Table_Byte_Limit = Large_Extent, "descriptor-sized largest >64KiB");
   Check (Owner.Byte_Capacity = DSDT'Length + OEM'Length, "exact aggregate capacity");
   Check (Owner.Table_Capacity = 2, "exact inventory capacity");
   Check (ACPI_Requests.Current (Owner.all) = ACPI_Requests.Complete, "actual Core startup completes");
   Check (ACPI_Requests.Observe (Owner.all).Tables = 2, "both tables retained");
   Check (Copies = 18 and Describe_Calls = 2, "bounded copies no metadata retries");
   Successful_Clock_Calls := Clock_Calls;
   Free (Owner);
   for Status in R.Copy_Status loop
      if Status /= R.Copy_OK then
         Reset; Info_Status := Status; Refusal (L.Source_Refused);
         Check (Copies = 0 and Describe_Calls = 0, "catalog refusal performs no more IO");
         for ID in 1 .. 2 loop
            Reset; Refused_Descriptor := ID; Descriptor_Status := Status;
            Refusal (L.Source_Refused);
            Check (Copies = 0 and Describe_Calls = ID, "describe refuses before table IO");
         end loop;
      end if;
   end loop;
   Reset; Catalog.Ready := False; Refusal (L.Inventory_Not_Ready);
   Reset; Catalog.Tables := 0; Refusal (L.Invalid_Inventory);
   Reset; Catalog.Largest_Table := 0; Refusal (L.Invalid_Inventory);
   Reset; Catalog.Total_Bytes := Catalog.Largest_Table - 1; Refusal (L.Invalid_Inventory);
   Reset; Catalog.Tables := Catalog.Total_Bytes; Refusal (L.Invalid_Inventory);
   for Field in 1 .. 3 loop
      Reset;
      case Field is
         when 1 => Catalog.Tables := L.Count'Last;
         when 2 => Catalog.Total_Bytes := L.Count'Last;
         when 3 => Catalog.Total_Bytes := L.Count'Last; Catalog.Largest_Table := L.Count'Last;
      end case;
      Refusal (L.Representation_Limit);
      Check (Describe_Calls = 0 and Copies = 0, "full width rejected before indexing");
   end loop;
   Reset; First.Name := "FACP"; Refusal (L.Invalid_Description);
   Reset; Second.Name := "DSDT"; Refusal (L.Invalid_Description);
   Reset; Second.Name := "FACS"; Refusal (L.Invalid_Description);
   Reset; Second.Extent := 35; Refusal (L.Invalid_Description);
   Reset; Second.Extent := L.Count'Last; Refusal (L.Invalid_Description);
   Reset; Catalog.Total_Bytes := Catalog.Total_Bytes + 1; Refusal (L.Inventory_Mismatch);
   Reset; Catalog.Total_Bytes := Catalog.Total_Bytes - 1; Refusal (L.Inventory_Mismatch);
   Reset; Catalog.Largest_Table := Catalog.Largest_Table + 1; Refusal (L.Inventory_Mismatch);
   for Position in 1 .. 18 loop
      Reset; Short_At := Position; Refusal (L.Copy_Refused);
      Check (Copies = Position, "short copy is never retried");
   end loop;
   Reset; OEM (OEM'Last) := 1; Refusal (L.Copy_Refused);
   Check (Result.Table = 2, "late checksum failure discards staged prefix");
   Reset; First.Revision := 3; Refusal (L.Copy_Refused);
   -- A valid checksum with malformed AML must never become a serving owner.
   Reset; DSDT (DSDT'Last) := 16#FE#; DSDT (10) := DSDT (10) - (16#FE# - 16#A3#);
   Refusal (L.Startup_Refused);
   for Position in 1 .. Successful_Clock_Calls loop
      Reset; Clock_Failure_At := Position;
      Load (100, Owner, Result);
      Check (Owner = null and Result.Status in L.Clock_Unavailable | L.Copy_Refused,
             "clock unavailable never publishes owner");
      Reset; Clock_Expire_At := Position;
      Load (100, Owner, Result);
      Check (Owner = null and Result.Status in L.Deadline_Expired | L.Copy_Refused,
             "deadline never publishes owner");
      if Position > 1 then
         Reset; Clock_Reverse_At := Position;
         Load (100, Owner, Result);
         Check (Owner = null and Result.Status in L.Clock_Reversed | L.Copy_Refused,
                "clock reversal never publishes owner");
      end if;
   end loop;
   Reset; Raise_From_Describe := True;
   begin
      Load (100, Owner, Result);
      raise Constraint_Error with "missing adapter exception";
   exception
      when Program_Error => Check (Owner = null, "adapter exception has no owner");
   end;
   Put_Line ("Direct snapshot startup checks" & Checks'Image);
end Snapshot_Startup_Tests;
