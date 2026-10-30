pragma Ada_2022;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with AML_Decode;
with Firmware_Tables;
with ACPI_Requests;
with ACPI_Service;
with ACPI_Fatal_Diagnostics;
with ACPI_Snapshot_Startup;
with Root_Test_Support; use Root_Test_Support;
procedure Local_Diagnostic_Tests is
   package L renames ACPI_Snapshot_Startup;
   package R renames L.R;
   package Q renames ACPI_Requests;
   use type L.Outcome;
   use type L.State_Access;
   use type R.S.Table_ID;
   use type Q.Outcome;
   use type Q.Phase;
   use type Firmware_Tables.Bytes;
   use type AML_Decode.Integer_Width;
   use type ACPI_Fatal_Diagnostics.Observation;
   subtype Bytes is Firmware_Tables.Bytes;
   type Bytes_Access is access Bytes;
   procedure Free is new Ada.Unchecked_Deallocation (Bytes, Bytes_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Q.State, L.State_Access);
   Data : Bytes_Access;
   Checks : Natural := 0;
   type Scenario is (No_Fatal, Module_Fatal, Root_Fatal, Bus_Fatal, Device_Fatal,
                     Ordinary_Error, Unconfigured_Region);
   Fatal : constant Bytes := [16#5B#,16#32#,16#A7#,16#78#,16#56#,16#34#,16#F2#,16#0A#,16#55#];
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   function Table (Code : Bytes; Width : AML_Decode.Integer_Width) return Bytes is
      Raw : Bytes (1 .. Firmware_Tables.Table_Header_Size + Code'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Raw (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Raw (4 + I) := Unsigned_8 ((Raw'Length / 256 ** (I - 1)) mod 256); end loop;
      Raw (9) := (if Width = AML_Decode.Bits_32 then 1 else 2);
      Raw (37 .. Raw'Last) := Code;
      for B of Raw loop Sum := Sum + B; end loop;
      Raw (10) := 0 - Sum;
      return Raw;
   end Table;
   procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
   begin Item := (True, 1, Data'Length, Data'Length); Status := R.Copy_OK; end Info;
   procedure Describe (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
   begin
      Check (ID = 1, "one descriptor");
      Item := ("DSDT", Data (9), Data'Length); Status := R.Copy_OK;
   end Describe;
   procedure Clock (Now : out R.Tick; Available : out Boolean) is
   begin Now := 10; Available := True; end Clock;
   procedure Copy_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status) is
   begin
      Check (ID = 1, "logical ID, no address");
      Into := Data (Offset + 1 .. Offset + Into'Length);
      Copied := Into'Length; Status := R.Copy_OK;
   end Copy_Part;
   procedure Load is new L.Load (Info, Describe, 4_096, Copy_Part, Clock);
begin
   for Width in AML_Decode.Integer_Width loop
      for Case_ID in Scenario loop
         declare
            Payload : constant Bytes := (case Case_ID is
              when No_Fatal => [16#A3#],
              when Module_Fatal => Fatal,
              when Root_Fatal => Method ("_INI", Fatal),
              when Bus_Fatal => Block ([16#10#], Enc ("_SB_") & Method ("_INI", Fatal)),
              when Device_Fatal => Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", Fatal)),
              when Ordinary_Error => Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", [16#FE#])),
              when Unconfigured_Region => [16#5B#,16#80#] & Enc ("REG0") & [3,0,1] &
                Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", Fatal)));
            Reference : Q.State (1, 512, 512, 0);
            Reply : Q.Response;
            Owner : L.State_Access;
            Result : L.Report;
            Expected : ACPI_Fatal_Diagnostics.Observation;
            Wanted : constant Boolean := Case_ID in Module_Fatal .. Device_Fatal;
         begin
            Data := new Bytes'(Table (Payload, Width));
            Q.Handle (Reference, Q.Snapshot_Provider,
              (Label => Q.Start_Snapshot, Data => [0,1,0,0], others => <>), Reply);
            Check (Reply.Status = Q.OK, "reference start");
            Q.Import_Block (Reference, Q.Snapshot_Provider, Q.Revision (Reference),
                            1, ACPI_Service.DSDT, Data.all, Reply);
            Check (Reply.Status = Q.OK, "reference import");
            Q.Handle (Reference, Q.Snapshot_Provider,
              (Label => Q.Finish_Snapshot, Data => [Q.Revision (Reference),0,0,0], others => <>), Reply);
            Expected := Q.Fatal_Diagnostic (Reference);
            Check (Expected.Present = Wanted, "reference fatal reachability");
            Load (100, Owner, Result);
            Check (Result.Fatal = Expected, "startup retains exact Fatal and context");
            Check ((Result.Status = L.Loaded) = (Q.Current (Reference) = Q.Complete),
                   "fatal retention does not change startup outcome");
            if Owner /= null then
               Check (Q.Fatal_Diagnostic (Owner.all) = Result.Fatal, "live owner matches copied report");
            else
               Check (Result.Status = L.Startup_Refused, "failed startup publishes no owner");
            end if;
            Free (Owner);
            Data.all := [others => 0]; Free (Data);
            -- The report must survive both owner teardown and source overwrite.
            Check (Result.Fatal = Expected, "Fatal is independent copied data");
            if Wanted then
               Check (Result.Fatal.Item.Literal.Kind = 16#A7# and
                 Result.Fatal.Item.Literal.Code = 16#F2345678# and
                 Result.Fatal.Item.Evaluated_Argument = 16#55# and
                 Result.Fatal.Item.Width = Width, "exact fatal values retained");
               Check (ACPI_Fatal_Diagnostics.Format (Result.Fatal) =
                 ACPI_Fatal_Diagnostics.Format (Expected), "exact bounded diagnostic retained");
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Local startup diagnostic checks" & Checks'Image);
end Local_Diagnostic_Tests;
