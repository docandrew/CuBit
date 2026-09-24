with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config;
with CuBit.Config_Reader;
with CuBit.Config_Inspection;
with CuBit.Memory_Grants;
with CuBit.Config_Protocol;
with System;
with CCL.Catalog;
with CCL.Language;
with CCL.Host_Values;
with CCL.Sessions;
with CCL_Config_Bindings;

procedure Main is
   use CuBit.Config_Inspection;
   use type CuBit.Config.ConfigStatus;
   Value : Text;
   Result : Status;
   Write_Status : CuBit.Config.ConfigStatus;
   Fixture : constant String := "inspector-native-ok";
   Oversized : constant String (1 .. 1025) := [others => 'x'];
   Ignore : Unsigned_64;
   Msg : Message := NULL_MESSAGE;
   Buffer : String (1 .. 8192) := [others => ' '] with Alignment => 4096;
   Grant : CuBit.Memory_Grants.Grant_Reference;
   Created : Boolean;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Installed : Boolean;
   Outcome : CCL.Language.Interpretation_Result;
   type Context is null record;
   Host : Context;
   procedure Invoke
     (State : in out Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value;
      Success : out Boolean) is
      pragma Unreferenced (State);
   begin
      CCL_Config_Bindings.Invoke (Binding, Argument, Value, Success);
   end Invoke;
   procedure Evaluate is new CCL.Language.Interpret_With_Values (Context, Invoke);
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL config-inspection " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Check;
   procedure Check_Data_Protocol is
      use CuBit.Config_Protocol;
      Full_Value : constant String (1 .. Maximum_Value) := [others => 'v'];
      Address : System.Address;
      Length : Natural;
      Prefix : constant String := "test.config.";
      Key : constant String := Prefix & String'(1 .. 128 - Prefix'Length => 'k');
   begin
      CuBit.Config_Reader.Query (Read_Value, "config.store", Value, Result);
      Check (Result = Missing, "no hidden backing path");
      CuBit.Config_Reader.Query (Read_Value, "clock.time-zone", Value, Result);
      Check (Result = OK and then Value.Data (1 .. Value.Length) = "UTC", "CCL seed wins over old disk overlay");
      for Label in Unsigned_32 range 16#0604# .. 16#0605# loop
         Msg := NULL_MESSAGE;
         Msg.tag := (Label, 0, 0, 0);
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label = 16#F001#, "retired persistence opcode");
      end loop;
      CuBit.Config.set (Key, Full_Value'Address, Full_Value'Length, Write_Status);
      Check (Write_Status = CuBit.Config.OK, "full 128-byte key plus 4096-byte value");
      CuBit.Config.get (Key, Address, Length, Write_Status);
      Check (Write_Status = CuBit.Config.OK and Length = Full_Value'Length, "full value read size");
      declare Data : String (1 .. Length) with Import, Address => Address; begin
         Check (Data = Full_Value, "full value bytes");
      end;
      CuBit.Config.delete (Key, Write_Status);
      Check (Write_Status = CuBit.Config.OK, "native delete");
      CuBit.Config.get (Key, Address, Length, Write_Status);
      Check (Write_Status = CuBit.Config.NotFound, "native missing result");
      CuBit.Config.set (Key, System.Null_Address, Natural'Last, Write_Status);
      Check (Write_Status = CuBit.Config.Error, "oversized client value rejected before copy");
      CuBit.Config.set (Key, System.Null_Address, 0, Write_Status);
      Check (Write_Status = CuBit.Config.OK, "empty value");
      CuBit.Config.get (Key, Address, Length, Write_Status);
      Check (Write_Status = CuBit.Config.OK and Length = 0, "empty value distinct from missing");
      CuBit.Config.delete (Key, Write_Status);
      CuBit.Config.list ("test.config", Address, Length, Write_Status);
      Check (Write_Status = CuBit.Config.OK and Length = 0, "empty native listing");
      for I in 0 .. 39 loop
         declare
            List_Key : constant String := "test.config.bulk." &
              Character'Val (Character'Pos ('A') + I) &
              String'(1 .. 110 => 'k');
         begin
            CuBit.Config.set (List_Key, System.Null_Address, 0, Write_Status);
            Check (Write_Status = CuBit.Config.OK, "full native listing fixture");
         end;
      end loop;
      CuBit.Config.list ("test.config.bulk", Address, Length, Write_Status);
      Check (Write_Status = CuBit.Config.Error and Length = 0,
             "native listing rejects overflow without partial publication");
      for I in 0 .. 39 loop
         declare
            List_Key : constant String := "test.config.bulk." &
              Character'Val (Character'Pos ('A') + I) &
              String'(1 .. 110 => 'k');
         begin
            CuBit.Config.delete (List_Key, Write_Status);
            Check (Write_Status = CuBit.Config.OK, "native listing cleanup");
         end;
      end loop;
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_CONFIG, Buffer'Address, 1, True, Grant, Created);
      Check (Created, "data malformed fixture");
      Buffer (1 .. 17) := "test.config.value";
      for Op in CuBit.Config_Protocol.Operation loop
         Msg := NULL_MESSAGE;
         Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Op), 4, 0, 0);
         Msg.words := [Grant.slot, Grant.generation, Unsigned_64'Last, 0];
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label = 16#F001#, "oversized wire key");
         Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Op), 4, 0, 0);
         Msg.words := [Unsigned_64'Last, Grant.generation, 17, 0];
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label = 16#F001#, "invalid data slot");
         Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Op), 2, 0, 0);
         Msg.words := [Grant.slot, 17, 0, 0];
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label = 16#F001#, "legacy raw-slot frame rejected");
      end loop;
      Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Set_Value), 4, 0, 0);
      Msg.words := [Grant.slot, Grant.generation, 17, 4096];
      Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
      Check (Msg.tag.label = 16#F001#, "short mapping rejected");
      CuBit.Memory_Grants.Revoke (Grant, Created);
      Check (Created, "data fixture revoked");
      for Op in CuBit.Config_Protocol.Operation loop
         Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Op), 4, 0, 0);
         Msg.words := [Grant.slot, Grant.generation, 17, 0];
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label = 16#F001#, "stale data grant rejected");
      end loop;
      Check (CuBit.Memory_Grants.Retirement_Confirmed (Grant),
             "data service returned every acquisition");
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_CONFIG, Buffer'Address, 1, False, Grant, Created);
      Check (Created, "read-only data fixture");
      for Op in CuBit.Config_Protocol.Operation loop
         Buffer (1 .. 17) := "test.config.value";
         Msg.tag := (CuBit.Config_Protocol.Operation'Enum_Rep (Op), 4, 0, 0);
         Msg.words := [Grant.slot, Grant.generation, 17, 0];
         Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
         Check (Msg.tag.label =
                  (if Op in Set_Value | Delete_Value then 16#F000# else 16#F001#),
                "read-only grant permits input but forbids output");
      end loop;
      CuBit.Memory_Grants.Revoke (Grant, Created);
      Check (Created and then CuBit.Memory_Grants.Retirement_Confirmed (Grant),
             "read-only fixture retired without outstanding acquisitions");
   end Check_Data_Protocol;
begin
   Check_Data_Protocol;
   CuBit.Config_Reader.Query (Probe, "", Value, Result);
   Check (Result = OK, "explicit wildcard read grant");
   -- A data reader cannot install or revoke policy. Huge untrusted counts
   -- must be denied before narrowing to Natural or inspecting grant memory.
   Msg.tag := (16#0080#, 4, 0, 0);
   Msg.words := [others => Unsigned_64'Last];
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Denied), "reader cannot install ACL");
   Msg.tag := (16#0081#, 1, 0, 0);
   Msg.words := [others => Unsigned_64'Last];
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Denied), "reader cannot revoke ACL");
   CuBit.Config.set ("test.config.value", Fixture'Address, Fixture'Length, Write_Status);
   Check (Write_Status = CuBit.Config.OK, "fixture write");
   CuBit.Config.set ("test.configuration", Fixture'Address, Fixture'Length, Write_Status);
   Check (Write_Status = CuBit.Config.AccessDenied, "component-boundary write denial");
   CuBit.Config.set ("desktop.appearance.v1", Fixture'Address, Fixture'Length, Write_Status);
   Check (Write_Status = CuBit.Config.AccessDenied, "global read is not write");
   CuBit.Config_Reader.Query (Read_Value, "test.config.value", Value, Result);
   Check (Result = OK and then Value.Data (1 .. Value.Length) = Fixture, "owned result");
   CuBit.Config_Reader.Query (List_Keys, "test.config", Value, Result);
   Check (Result = OK and then Value.Data (1 .. Value.Length) = "test.config.value" & ASCII.LF, "listing");
   CuBit.Config_Reader.Query (Read_Value, "test.config.absent", Value, Result);
   Check (Result = Missing, "missing distinct from denial");
   CuBit.Config_Reader.Query (Read_Value, "test.config.value", Value, Result, Context => 1);
   Check (Result = Invalid_Request, "no context fallback");
   CuBit.Config.set ("test.config.large", Oversized'Address, Oversized'Length, Write_Status);
   Check (Write_Status = CuBit.Config.OK, "oversized fixture");
   CuBit.Config_Reader.Query (Read_Value, "test.config.large", Value, Result);
   Check (Result = Too_Large and Value.Length = 0, "no truncated value");
   -- Invalid lengths and stale grant generations must be rejected before reads.
   Msg.tag := (Operation'Enum_Rep (Read_Value), 4, 0, 0);
   Msg.words := [0 => Unsigned_64'Last, 1 => 1, 2 => 1, 3 => 0];
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "invalid grant");
   CuBit.Memory_Grants.Create_Via_Capability
     (CAP_SLOT_CONFIG, Buffer'Address, 1, True, Grant, Created);
   Check (Created, "stale grant fixture");
   CuBit.Memory_Grants.Revoke (Grant, Created);
   Check (Created, "retire fixture");
   Msg.tag := (Operation'Enum_Rep (Read_Value), 4, 0, 0);
   Msg.words := [0 => Grant.slot, 1 => Grant.generation, 2 => 1, 3 => 0];
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "stale grant");
   CuBit.Memory_Grants.Create_Via_Capability
     (CAP_SLOT_CONFIG, Buffer'Address, 1, False, Grant, Created);
   Check (Created, "readonly grant fixture");
   Msg.tag := (Operation'Enum_Rep (Read_Value), 4, 0, 0);
   Msg.words := [0 => Grant.slot, 1 => Grant.generation, 2 => 1, 3 => 0];
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "readonly grant rejected");
   Msg.tag := (Operation'Enum_Rep (Read_Value), 4, 0, 0);
   Msg.words (2) := Unsigned_64'Last;
   Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "oversized request length");
   CuBit.Memory_Grants.Revoke (Grant, Created);
   Check (Created, "readonly fixture retired");
   for I in 0 .. 19 loop
      declare
         Key : constant String := "test.config.list." & Character'Val (Character'Pos ('a') + I) &
           ".padding.padding.padding.padding.padding.padding.padding";
      begin
         CuBit.Config.set (Key, Fixture'Address, Fixture'Length, Write_Status);
         Check (Write_Status = CuBit.Config.OK, "list fixture");
      end;
   end loop;
   CuBit.Config_Reader.Query (List_Keys, "test.config.list", Value, Result);
   Check (Result = Too_Large and Value.Length = 0, "no truncated enumeration");
   -- End-to-end source interpreter -> shared adapter -> kernel -> Config.
   CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
   CCL_Config_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "catalog installation");
   Evaluate ("(config.get ""test.config.value"")", 1024, Catalog, Grants, Host, Outcome);
   Check (CCL.Sessions.Result_Image (Outcome) = "String: " & Fixture, "CCL native value");
   debugPrint ("TEST: PASS config-inspection" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
