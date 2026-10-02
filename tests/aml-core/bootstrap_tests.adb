with Ada.Text_IO;
with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
procedure Bootstrap_Tests is
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Table_Kind;
   use type Firmware_Tables.Byte;
   use type Firmware_Tables.Bytes;
   use type ACPI_Service.Namespace.State;
   Session, Before : State := Start (0);
   Outcome : ACPI_Service.Install_Status;
   Checks : Natural := 0;
   function Table (Kind : ACPI_Service.Table_Kind; Payload : Firmware_Tables.Bytes) return Firmware_Tables.Bytes is
      Data : Firmware_Tables.Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sig : constant String :=
        (case Kind is when ACPI_Service.DSDT => "DSDT",
         when ACPI_Service.SSDT => "SSDT", when ACPI_Service.Description => "TEST");
      Length : Natural := Data'Length;
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in 1 .. 4 loop
         Data (I) := Character'Pos (Sig (I));
         Data (I + 4) := Firmware_Tables.Byte (Length mod 256);
         Length := Length / 256;
      end loop;
      Data (9) := 2;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   DSDT : constant Firmware_Tables.Bytes := Table (ACPI_Service.DSDT, [1 .. 0 => 0]);
   SSDT : constant Firmware_Tables.Bytes := Table (ACPI_Service.SSDT, [1 .. 0 => 0]);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Session := Start (0);
   Check (Current (Session) = Failed);
   Session := Start (ACPI_Service.Max_Tables + 1);
   Check (Current (Session) = Failed);
   Session := Start (Natural'Last);
   Check (Current (Session) = Failed);
   for Required in 1 .. ACPI_Service.Max_Tables loop
      Session := Start (Required);
      Check (Current (Session) = Receiving and Expected (Session) = Required and Installed (Session) = 0);
      for ID in 1 .. Required loop
         Import_Table (Session, ID, (if ID = 1 then ACPI_Service.DSDT else ACPI_Service.SSDT),
                       (if ID = 1 then DSDT else SSDT), Outcome);
         Check (Outcome = ACPI_Service.Installed and Installed (Session) = ID and Current (Session) = Receiving);
      end loop;
      Before := Session;
      Import_Table (Session, Required + 1, ACPI_Service.SSDT, SSDT, Outcome);
      Check (Outcome = ACPI_Service.Table_Limit and Current (Session) = Failed);
      Finish (Session);
      Check (Current (Session) = Failed);
      Session := Before;
      Finish (Session);
      Check (Current (Session) = Complete and Installed (Session) = Required);
      Before := Session;
      Finish (Session);
      Check (Session = Before);
      Import_Table (Session, Required + 1, ACPI_Service.SSDT, SSDT, Outcome);
      Check (Outcome = ACPI_Service.Wrong_Order and Session = Before);
      Session := Start (Required);
      for ID in 1 .. Required - 1 loop
         Import_Table (Session, ID, (if ID = 1 then ACPI_Service.DSDT else ACPI_Service.SSDT),
                       (if ID = 1 then DSDT else SSDT), Outcome);
         Check (Outcome = ACPI_Service.Installed);
      end loop;
      Finish (Session);
      Check (Current (Session) = Failed and Installed (Session) = Required - 1);
   end loop;
   Session := Start (2);
   Import_Table (Session, 1, ACPI_Service.DSDT,
                 Table (ACPI_Service.DSDT, [8,86,65,76,48,1]), Outcome);
   Check (Outcome = ACPI_Service.Installed and Observe (Session).Objects = 1);
   Before := Session;
   Import_Table (Session, 1, ACPI_Service.SSDT, SSDT, Outcome);
   Check (Outcome = ACPI_Service.Duplicate_ID and Current (Session) = Failed
          and Snapshot (Session) = Snapshot (Before));
   Before := Session;
   Import_Table (Session, 2, ACPI_Service.SSDT, SSDT, Outcome);
   Check (Outcome = ACPI_Service.Wrong_Order and Session = Before);
   Finish (Session);
   Check (Session = Before);
   Session := Start (1);
   Import_Table (Session, 1, ACPI_Service.SSDT, SSDT, Outcome);
   Check (Outcome = ACPI_Service.Wrong_Order and Current (Session) = Failed);
   Session := Start (1);
   Import_Table (Session, 1, ACPI_Service.DSDT, Table (ACPI_Service.DSDT, [1 => 16#FE#]), Outcome);
   Check (Outcome = ACPI_Service.Invalid_AML and Current (Session) = Failed);
   declare
      Length : constant Positive := 1_048_577;
      Large : State := Start (35, 35, Length + 34 * 36, Length);
      Data : Firmware_Tables.Bytes :=
        Table (ACPI_Service.Description, [1 .. Length - 36 => 16#A5#]);
   begin
      Check (Current (Large) = Receiving and Expected (Large) = 35);
      Import_Table (Large, 1, ACPI_Service.DSDT, DSDT, Outcome);
      Check (Outcome = ACPI_Service.Installed);
      Import_Table (Large, 2, ACPI_Service.Description, Data, Outcome);
      Check (Outcome = ACPI_Service.Installed);
      Data := [others => 0];
      for I in 3 .. 35 loop
         Import_Table (Large, I, ACPI_Service.SSDT, SSDT, Outcome);
         Check (Outcome = ACPI_Service.Installed);
      end loop;
      Finish (Large);
      Check (Current (Large) = Complete and Installed (Large) = 35);
      Check (Observe (Large).Bytes = Length + 34 * 36);
      Check (Table_Info (Large, 2).Extent = Length);
      Check (Table_Byte (Large, 2, Length - 1) = 16#A5#);
      declare
         Saved : constant State := Large;
      begin
         Import_Table (Large, 36, ACPI_Service.SSDT, SSDT, Outcome);
         Check (Outcome = ACPI_Service.Wrong_Order and Large = Saved);
         Finish (Large);
         Check (Large = Saved);
      end;
   end;
   for Count in 0 .. 4 loop
      declare
         S : constant State := Start (Count, 3, 108, 36);
      begin
         Check (S.Table_Capacity = 3 and S.Byte_Capacity = 108 and S.Table_Byte_Limit = 36);
         Check (Current (S) = (if Count in 1 .. 3 then Receiving else Failed));
         Check (Expected (S) = (if Count in 1 .. 3 then Count else 0));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ACPI-BOOTSTRAP-CHECK: PASS" & Checks'Image);
end Bootstrap_Tests;
