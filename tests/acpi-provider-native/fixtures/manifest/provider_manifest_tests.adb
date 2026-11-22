pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Provider_Transport; use ACPI_Provider_Transport;
with ACPI_Snapshot_Manifest;
with ACPI_Snapshot_Provider;
with ACPI_Requests;
with ACPI_Service;
with CuBit.Handles;
with CuBit.Grant_References;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Bridge_Trace; use Bridge_Trace;
procedure Provider_Manifest_Tests is
   package Manifest renames ACPI_Snapshot_Manifest;
   use type Manifest.Outcome;
   use type Manifest.Requirements;
   use type ACPI_Snapshot_Provider.Cleanup_State;
   type Scenario is (Normal, Old_First,
     Incomplete, Malformed, Extra_Data, Short_Data, Empty_Count,
     Table_Limit, Byte_Limit, Invalid_Target, Required_Interest);
   Mode : Scenario;
   Items : Manifest.Catalog (5 .. 7) :=
     [(1, "DSDT", 2, 36), (2, "SSDT", 2, 40), (3, "TEST", 2, 44)];
   Data : Firmware_Tables.Bytes (11 .. 131);
   Target : CuBit.Handles.Endpoint := CuBit.Handles.Test_Endpoint;
   Count : Natural := Items'Length;
   Result, Repeated : Snapshot_Startup_Result;
   Accepted, Known : Boolean;
   Reply : ACPI_Requests.Packet;
   Loan : ACPI_Snapshot_Provider.Cleanup_State;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Check_Rebind is
      Before : constant Natural := Used;
   begin
      Initialize_Snapshot (CuBit.Handles.Test_Endpoint, Items, 3,
        Data (11 .. 130), (others => <>), Repeated);
      Check (Repeated.Status = Already_Attempted and then Used = Before);
      Initialize (CuBit.Handles.Test_Endpoint,
        [(Item => (1, ACPI_Service.DSDT, 120), Offset => 0)],
        Data (11 .. 130), (others => <>), Accepted);
      Check (not Accepted and then Used = Before);
   end Check_Rebind;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "one scenario required";
   end if;
   Mode := Scenario'Value (Ada.Command_Line.Argument (1));
   for I in Data'Range loop Data (I) := Unsigned_8 (I - Data'First); end loop;
   case Mode is
      when Incomplete => Count := 4;
      when Empty_Count => Count := 0;
      when Malformed => Items (6).ID := 1;
      when Byte_Limit => Items (7).Extent := Max_Catalog_Bytes;
      when Invalid_Target => Target := CuBit.Handles.No_Endpoint;
      when others => null;
   end case;
   if Mode = Old_First then
      Initialize (Target, [(Item => (41, ACPI_Service.DSDT, 120), Offset => 0)],
        Data (11 .. 130), (others => <>), Accepted);
      Check (Accepted and then Used = 1);
   end if;
   if Mode = Table_Limit then
      declare
         Many : Manifest.Catalog (1 .. Max_Catalog_Tables + 1);
      begin
         for I in Many'Range loop
            Many (I) := (Firmware_Tables.Snapshots.Table_ID (I),
              (if I = 1 then "DSDT" else "SSDT"), 2, 36);
         end loop;
         Initialize_Snapshot (Target, Many, Many'Length,
           Data (11 .. 130), (others => <>), Result);
         Check (Result.Status = Manifest_Rejected
           and then Result.Sizing.Status = Manifest.Table_Quota
           and then Result.Sizing.Needed.Tables = Many'Length);
      end;
   else
      Initialize_Snapshot (Target, Items, Count,
        Data (11 .. (if Mode = Extra_Data then 131
                    elsif Mode = Short_Data then 129 else 130)),
        (others => <>), Result,
        (if Mode = Required_Interest then Required else Disabled));
   end if;
   case Mode is
      when Normal | Required_Interest =>
         Check (Result.Status = Snapshot_Initialized
           and then Result.Sizing.Status = Manifest.Approved
           and then Result.Sizing.Needed = (3, 120, 44));
         Check (Used = 1 and then Log (1) = Allocation and then Calls = 0);
      when Incomplete | Empty_Count =>
         Check (Result.Status = Manifest_Rejected
           and then Result.Sizing.Status = Manifest.Incomplete);
      when Malformed =>
         Check (Result.Status = Manifest_Rejected
           and then Result.Sizing.Status = Manifest.Malformed);
      when Extra_Data | Short_Data =>
         Check (Result.Status = Data_Length_Mismatch
           and then Result.Sizing.Status = Manifest.Approved);
      when Byte_Limit =>
         Check (Result.Status = Manifest_Rejected
           and then Result.Sizing.Status = Manifest.Byte_Quota
           and then Result.Sizing.Needed.Bytes = Manifest.Byte_Count (Max_Catalog_Bytes) + 76);
      when Invalid_Target =>
         Check (Result.Status = Transport_Unavailable
           and then Result.Sizing.Status = Manifest.Approved);
      when Old_First => Check (Result.Status = Already_Attempted and then Used = 1);
      when Table_Limit => null;
   end case;
   Check_Rebind;
   if Result.Status /= Snapshot_Initialized then
      Check (Used = (if Mode = Old_First then 1 else 0)
        and then Calls = 0 and then Creates = 0);
   else
      -- Input is copied; mutating caller-owned values cannot change upload.
      Data := [others => 16#EE#];
      Items (6).Name := "FACS";
      Exchange ((Label => ACPI_Requests.Start_Snapshot,
        Data => [0, 3, 0, 0], others => <>), Reply, Known);
      if Mode = Required_Interest then
         Check (not Known and then Calls = 0 and then Creates = 0);
      else
         Check (Known and then Calls = 1);
         declare
            Offset : Natural := 0;
         begin
            for I in 1 .. 3 loop
               declare
                  Item : constant ACPI_Snapshot_Provider.Table_Descriptor :=
                    (I, (case I is when 1 => ACPI_Service.DSDT,
                                   when 2 => ACPI_Service.SSDT,
                                   when others => ACPI_Service.Description),
                     (case I is when 1 => 36, when 2 => 40, when others => 44));
               begin
                  Import_Table (I, Unsigned_64 (I), Item, Reply, Known, Loan);
                  Check (Known and then Loan = ACPI_Snapshot_Provider.Clear);
                  Check (not Last_Read_Write and then Last_Pages = 1);
                  Check (Last_Message.words (2) = Unsigned_64 (I));
                  Check (Last_Message.words (3) = Unsigned_64 (ACPI_Service.Table_Kind'Pos (Item.Kind))
                    * CuBit.Grant_References.Wire_Field_Base + Unsigned_64 (Item.Length));
                  Check ((for all J in 1 .. Item.Length =>
                    Buffer (J) = Unsigned_8 (Offset + J - 1)));
                  Check ((for all J in Item.Length + 1 .. 4096 => Buffer (J) = 0));
                  Offset := Offset + Item.Length;
               end;
            end loop;
         end;
         Check (Creates = 3 and then Calls = 4 and then Revokes = 3 and then Queries = 3);
      end if;
   end if;
   Ada.Text_IO.Put_Line ("Provider manifest " & Mode'Image & " checks" & Checks'Image);
end Provider_Manifest_Tests;
