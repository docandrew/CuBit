with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
with Ada.Text_IO;
procedure Bootstrap_Owned_Tests is
   use type ACPI_Service.Install_Status;
   use type Firmware_Tables.Byte;
   use type ACPI_Service.Values.Audit_Value;
   use type ACPI_Service.Namespace.Initialization_Report;
   Data : Firmware_Tables.Bytes (1 .. 36) := [others => 0];
   Sum : Firmware_Tables.Byte := 0;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Table (Payload : Firmware_Tables.Bytes; Secondary : Boolean := False)
      return Firmware_Tables.Bytes is
      Result : Firmware_Tables.Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Checksum : Firmware_Tables.Byte := 0;
   begin
      Result (1 .. 4) := (if Secondary then [83,83,68,84] else [68,83,68,84]);
      Result (5) := Firmware_Tables.Byte (Result'Length); Result (9) := 2;
      Result (37 .. Result'Last) := Payload;
      for B of Result loop Checksum := Checksum + B; end loop;
      Result (10) := 0 - Checksum; return Result;
   end Table;
begin
   Data (1 .. 4) := [68, 83, 68, 84]; Data (5) := 36; Data (9) := 2;
   for B of Data loop Sum := Sum + B; end loop;
   Data (10) := 0 - Sum;
   for Count in 0 .. 4 loop
      declare
         S : State (3, 256, 128);
         OK : Boolean;
         Status : ACPI_Service.Install_Status;
      begin
         Check (Valid (S) and not Started (S) and Current (S) = Failed);
         Import_Table (S, 1, ACPI_Service.DSDT, Data, Status);
         Check (Status = ACPI_Service.Wrong_Order and not Started (S));
         Start (S, Count, OK);
         Check (Started (S) and (OK = (Count in 1 .. 3)));
         Check (Current (S) = (if Count in 1 .. 3 then Receiving else Failed));
         Check (Expected (S) = (if Count in 1 .. 3 then Count else 0));
         Start (S, 1, OK);
         Check (not OK and Expected (S) = (if Count in 1 .. 3 then Count else 0));
         if Count in 1 .. 3 then
            Import_Table (S, 1, ACPI_Service.DSDT, Data, Status);
            Check (Status = ACPI_Service.Installed and Installed (S) = 1);
            Start (S, 3, OK);
            Check (not OK and Installed (S) = 1 and Expected (S) = Count);
            Finish (S);
            Check (Current (S) = (if Count = 1 then Complete else Failed));
            declare
               Before : constant ACPI_Service.Values.Audit_Value := Audit (S);
               Before_Phase : constant Phase := Current (S);
            begin
               Import_Table (S, 2, ACPI_Service.DSDT, Data, Status);
               Check (Status = ACPI_Service.Wrong_Order and Audit (S) = Before);
               Finish (S);
               Check (Current (S) = Before_Phase and Installed (S) = 1);
               for I in Data'Range loop Check (Table_Byte (S, 1, I - 1) = Data (I)); end loop;
            end;
         else
            Finish (S);
            Check (Current (S) = Failed and Installed (S) = 0);
         end if;
      end;
   end loop;
   declare
      S : State (2, 256, 128);
      OK : Boolean;
      Status : ACPI_Service.Install_Status;
      Bad : Firmware_Tables.Bytes := Data;
   begin
      Start (S, 1, OK); Check (OK);
      Bad (10) := Bad (10) + 1;
      Import_Table (S, 1, ACPI_Service.DSDT, Bad, Status);
      Check (Status = ACPI_Service.Invalid_Table and Current (S) = Failed);
      Start (S, 1, OK); Check (not OK and Installed (S) = 0);
      Import_Table (S, 1, ACPI_Service.DSDT, Data, Status);
      Check (Status = ACPI_Service.Wrong_Order and Current (S) = Failed);
   end;
   declare
      Package_Data : constant Firmware_Tables.Bytes := Table
        ([16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,6,1,16#53#,16#52#,16#43#,16#30#]);
      Source_Data : constant Firmware_Tables.Bytes := Table
        ([16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,7], True);
   begin
      for Mode in 0 .. 2 loop
         declare
            S : State (2, 256, 128);
            OK : Boolean;
            Status : ACPI_Service.Install_Status;
            Before : ACPI_Service.Values.Audit_Value;
         begin
            Check (not Members_Initialized (S) and then Member_Initialization (S) = (0,0,0));
            Start (S, (if Mode = 2 then 1 else 2), OK); Check (OK);
            Import_Table (S, 1, ACPI_Service.DSDT, Package_Data, Status);
            Check (Status = ACPI_Service.Installed and then Pending_Members (S) = 1);
            Check (not Members_Initialized (S));
            if Mode = 0 then
               Import_Table (S, 2, ACPI_Service.SSDT, Source_Data, Status);
               Check (Status = ACPI_Service.Installed and then Pending_Members (S) = 1);
            end if;
            Before := Audit (S); Finish (S);
            if Mode = 1 then
               Check (Current (S) = Failed and then not Members_Initialized (S));
               Check (Pending_Members (S) = 1 and then Audit (S) = Before);
               Check (Member_Initialization (S) = (0,0,0));
            else
               Check (Current (S) = Complete and then Members_Initialized (S));
               Check (Pending_Members (S) = 0);
               Check (Member_Initialization (S) =
                 (if Mode = 0 then (1,0,0) else (0,1,0)));
            end if;
            for I in Package_Data'Range loop Check (Table_Byte (S, 1, I - 1) = Package_Data (I)); end loop;
            Before := Audit (S); Finish (S); Check (Audit (S) = Before);
            Check (Members_Initialized (S) = (Mode /= 1));
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Owned bootstrap PASS" & Checks'Image);
end Bootstrap_Owned_Tests;
