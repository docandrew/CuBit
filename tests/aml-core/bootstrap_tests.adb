with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
procedure Bootstrap_Tests is
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Table_Kind;
   use type Firmware_Tables.Byte;
   use type Firmware_Tables.Bytes;
   use type ACPI_Service.Values.Audit_Value;
   type State_Access is access State;
   procedure Free is new Ada.Unchecked_Deallocation (State, State_Access);
   Session_Ptr : State_Access := null;
   Before : State_Model (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes) with Ghost;
   Before_Tree : ACPI_Service.Values.Audit_Value with Ghost;
   procedure Reset_Fixture (Count : Natural) is
      Accepted : Boolean;
   begin
      Free (Session_Ptr);
      Session_Ptr := new State (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes);
      Start (Session_Ptr.all, Count, Accepted);
   end Reset_Fixture;
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

begin
   Reset_Fixture (0);
   Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Failed);
   Reset_Fixture (ACPI_Service.Max_Tables + 1);
   Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Failed);
   Reset_Fixture (Natural'Last);
   Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Failed);
   for Required in 1 .. ACPI_Service.Max_Tables loop
      Reset_Fixture (Required);
      Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Receiving and Expected (Session_Ptr.all) = Required and Installed (Session_Ptr.all) = 0);
      for ID in 1 .. Required loop
         Import_Table (Session_Ptr.all, ID, (if ID = 1 then ACPI_Service.DSDT else ACPI_Service.SSDT),
                       (if ID = 1 then DSDT else SSDT), Outcome);
         Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed and Installed (Session_Ptr.all) = ID and Current (Session_Ptr.all) = Receiving);
      end loop;
      Import_Table (Session_Ptr.all, Required + 1, ACPI_Service.SSDT, SSDT, Outcome);
      Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Table_Limit and Current (Session_Ptr.all) = Failed);
      Finish (Session_Ptr.all);
      Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Failed);
      Reset_Fixture (Required);
      for ID in 1 .. Required loop
         Import_Table (Session_Ptr.all, ID, (if ID = 1 then ACPI_Service.DSDT else ACPI_Service.SSDT),
           (if ID = 1 then DSDT else SSDT), Outcome);
         pragma Assert (Outcome = ACPI_Service.Installed);
      end loop;
      Finish (Session_Ptr.all);
      Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Complete and Installed (Session_Ptr.all) = Required);
      Before := Model (Session_Ptr.all);
      Finish (Session_Ptr.all);
      Checks := Checks + 1; pragma Assert (Model (Session_Ptr.all) = Before);
      Import_Table (Session_Ptr.all, Required + 1, ACPI_Service.SSDT, SSDT, Outcome);
      Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Wrong_Order and Model (Session_Ptr.all) = Before);
      Reset_Fixture (Required);
      for ID in 1 .. Required - 1 loop
         Import_Table (Session_Ptr.all, ID, (if ID = 1 then ACPI_Service.DSDT else ACPI_Service.SSDT),
                       (if ID = 1 then DSDT else SSDT), Outcome);
         Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed);
      end loop;
      Finish (Session_Ptr.all);
      Checks := Checks + 1; pragma Assert (Current (Session_Ptr.all) = Failed and Installed (Session_Ptr.all) = Required - 1);
   end loop;
   Reset_Fixture (2);
   Import_Table (Session_Ptr.all, 1, ACPI_Service.DSDT,
                 Table (ACPI_Service.DSDT, [8,86,65,76,48,1]), Outcome);
   Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed and Observe (Session_Ptr.all).Objects = 1);
   Before_Tree := Audit (Session_Ptr.all);
   Import_Table (Session_Ptr.all, 1, ACPI_Service.SSDT, SSDT, Outcome);
   Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Duplicate_ID and Current (Session_Ptr.all) = Failed
          and Audit (Session_Ptr.all) = Before_Tree);
   Before := Model (Session_Ptr.all);
   Import_Table (Session_Ptr.all, 2, ACPI_Service.SSDT, SSDT, Outcome);
   Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Wrong_Order and Model (Session_Ptr.all) = Before);
   Finish (Session_Ptr.all);
   Checks := Checks + 1; pragma Assert (Model (Session_Ptr.all) = Before);
   Reset_Fixture (1);
   Import_Table (Session_Ptr.all, 1, ACPI_Service.SSDT, SSDT, Outcome);
   Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Wrong_Order and Current (Session_Ptr.all) = Failed);
   Reset_Fixture (1);
   Import_Table (Session_Ptr.all, 1, ACPI_Service.DSDT, Table (ACPI_Service.DSDT, [1 => 16#FE#]), Outcome);
   Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Invalid_AML and Current (Session_Ptr.all) = Failed);
   declare
      Length : constant Positive := 1_048_577;
      Large : State (35, Length + 34 * 36, Length);
      Accepted : Boolean;
      Data : Firmware_Tables.Bytes :=
        Table (ACPI_Service.Description, [1 .. Length - 36 => 16#A5#]);
   begin
      Start (Large, 35, Accepted);
      Checks := Checks + 1; pragma Assert (Current (Large) = Receiving and Expected (Large) = 35);
      Import_Table (Large, 1, ACPI_Service.DSDT, DSDT, Outcome);
      Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed);
      Import_Table (Large, 2, ACPI_Service.Description, Data, Outcome);
      Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed);
      Data := [others => 0];
      for I in 3 .. 35 loop
         Import_Table (Large, I, ACPI_Service.SSDT, SSDT, Outcome);
         Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Installed);
      end loop;
      Finish (Large);
      Checks := Checks + 1; pragma Assert (Current (Large) = Complete and Installed (Large) = 35);
      Checks := Checks + 1; pragma Assert (Observe (Large).Bytes = Length + 34 * 36);
      Checks := Checks + 1; pragma Assert (Table_Info (Large, 2).Extent = Length);
      Checks := Checks + 1; pragma Assert (Table_Byte (Large, 2, Length - 1) = 16#A5#);
      declare
         Saved : constant State_Model (Large.Table_Capacity, Large.Byte_Capacity) := Model (Large) with Ghost;
      begin
         Import_Table (Large, 36, ACPI_Service.SSDT, SSDT, Outcome);
         Checks := Checks + 1; pragma Assert (Outcome = ACPI_Service.Wrong_Order and Model (Large) = Saved);
         Finish (Large);
         Checks := Checks + 1; pragma Assert (Model (Large) = Saved);
      end;
   end;
   for Count in 0 .. 4 loop
      declare
         S : State (3, 108, 36);
         Accepted : Boolean;
      begin
         Start (S, Count, Accepted);
         Checks := Checks + 1; pragma Assert (S.Table_Capacity = 3 and S.Byte_Capacity = 108 and S.Table_Byte_Limit = 36);
         Checks := Checks + 1; pragma Assert (Current (S) = (if Count in 1 .. 3 then Receiving else Failed));
         Checks := Checks + 1; pragma Assert (Expected (S) = (if Count in 1 .. 3 then Count else 0));
      end;
   end loop;
   Free (Session_Ptr);
   Ada.Text_IO.Put_Line ("ACPI-BOOTSTRAP-CHECK: PASS" & Checks'Image);
end Bootstrap_Tests;
