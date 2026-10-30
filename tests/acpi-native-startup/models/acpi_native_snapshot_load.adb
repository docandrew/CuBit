pragma Ada_2022;
with Owner_Test_Control;
with Root_Test_Support; use Root_Test_Support;
with AML_Decode;
with Interfaces; use Interfaces;
package body ACPI_Native_Snapshot_Load is
   package T renames Owner_Test_Control;
   package R renames L.R;
   package K renames ACPI_Native_Snapshot_Read.K;
   use type T.Scenario;
   use type AML_Decode.Integer_Width;
   use type R.F.Bytes;
   subtype Bytes is R.F.Bytes;
   Fatal : constant Bytes := [16#5B#,16#32#,16#A7#,16#78#,16#56#,16#34#,16#F2#,16#0A#,16#55#];
   function Table (Code : Bytes) return Bytes is
      Raw : Bytes (1 .. R.F.Table_Header_Size + Code'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Raw (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Raw (4 + I) := Unsigned_8 ((Raw'Length / 256 ** (I - 1)) mod 256); end loop;
      Raw (9) := (if T.Width = AML_Decode.Bits_32 then 1 else 2);
      Raw (37 .. Raw'Last) := Code;
      for B of Raw loop Sum := Sum + B; end loop;
      Raw (10) := 0 - Sum; return Raw;
   end Table;
   procedure Load
     (Deadline : R.Tick; Owner : out L.State_Access; Result : out Report) is
      Payload : constant Bytes := (case T.Selected is
        when T.Module_Fatal => Fatal,
        when T.Root_Fatal => Method ("_INI", Fatal),
        when T.Bus_Fatal => Block ([16#10#], Enc ("_SB_") & Method ("_INI", Fatal)),
        when T.Device_Fatal => Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", Fatal)),
        when T.Ordinary_Error => Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", [16#FE#])),
        when T.Unconfigured_Region => [16#5B#,16#80#] & Enc ("REG0") & [3,0,1] &
          Block ([16#5B#,16#82#], Enc ("DEV0") & Method ("_INI", Fatal)),
        when others => [16#A3#]);
      Raw : constant Bytes := Table (Payload);
      procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
      begin
         Item := (True, 1, Raw'Length, Raw'Length);
         Status := (if T.Selected = T.Snapshot_Refused then R.Not_Authorized else R.Copy_OK);
         Result.Catalog_Status := (if T.Selected = T.Snapshot_Refused then K.Not_Authorized else K.Ok);
      end Info;
      procedure Describe (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
      begin
         if ID /= 1 then raise Program_Error with "wrong model descriptor ID"; end if;
         Item := ("DSDT", Raw (9), Raw'Length); Status := R.Copy_OK;
         Result.Descriptor_Status := K.Ok;
      end Describe;
      procedure Copy_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status) is
         use type R.S.Table_ID;
      begin
         if ID /= 1 then raise Program_Error with "wrong model copy ID"; end if;
         T.Copies := T.Copies + 1;
         Into := [others => 0]; Copied := 0;
         Status := (if T.Selected = T.Copy_Refused then R.Copy_Failed else R.Copy_OK);
         if T.Selected /= T.Copy_Refused then
            Into := Raw (Offset + 1 .. Offset + Into'Length);
            Copied := Into'Length;
         end if;
         Result.Last_Copy := (Available => True, Table => K.Table_ID (ID),
           Offset => K.Byte_Count (Offset), Capacity => K.Byte_Count (Into'Length),
           Copied => K.Byte_Count (Copied),
           Status => (if T.Selected = T.Copy_Refused then K.Copy_Failed else K.Ok));
      end Copy_Part;
      procedure Clock (Now : out R.Tick; Available : out Boolean) is
      begin Now := 10; Available := True; end Clock;
      procedure Actual_Load is new L.Load (Info, Describe, 4_096, Copy_Part, Clock);
   begin
      Owner := null; Result := (others => <>);
      T.Loads := T.Loads + 1; T.Passed_Deadline := Deadline;
      if T.Selected = T.Load_Exception then raise Program_Error with "modeled loader exception"; end if;
      Actual_Load (Deadline, Owner, Result.Startup);
      T.Last_Load := Result.Startup;
      T.Sequence := T.Sequence + 1; T.Load_Completed_At := T.Sequence;
   end Load;
end ACPI_Native_Snapshot_Load;
