with CCL.Evaluation;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config_Inspection;
with CuBit.Settings.Inspection;
with CuBit.Handles.Services;
with CuBit.Memory_Grants;
with CuBit.Config_Protocol;
with Config_Worker_Startup;
with Startup;
with CCL.Catalog;
with CCL.Language;
with CCL.Host_Values;
with CCL.Sessions;
with CCL_Config_Bindings;

procedure Main is
   use CuBit.Config_Inspection;
   use type CuBit.Settings.Outcome;
   use type CuBit.Settings.Change_Outcome;
   package Settings renames CuBit.Settings;
   Config : constant Settings.Inspection.Inspector := Startup.Settings.Inspector;
   --  The raw protocol cases below call Config's endpoint directly.
   Service : constant CuBit.Handles.Endpoint := CuBit.Handles.Services.Config.Handle (Startup.Config);
   Value : Text;
   Result : Status;
   Write_Status : Settings.Change_Outcome;
   Stored : Settings.Text;
   Read_Status : Settings.Outcome;
   procedure Query (Op : Operation; Key : String; Value : out Text; Result : out Status;
                    In_Context : Context_ID := Machine_Context) is
   begin
      Settings.Inspection.Inspect (Config, Op, Key, In_Context, Value, Result);
   end Query;
   procedure Write (Key, Item : String; Result : out Settings.Change_Outcome) is
   begin
      Settings.Inspection.Write (Config, Key, Item, Result);
   end Write;
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
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result) is
      pragma Unreferenced (State);
   begin
      CCL_Config_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values (Context, Invoke);
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
      Listed : Natural := 0;
      procedure Count (Key : String) is
         pragma Unreferenced (Key);
      begin
         Listed := Listed + 1;
      end Count;
      procedure List is new Settings.Inspection.For_Each (Count);
      Prefix : constant String := "test.config.";
      Key : constant String := Prefix & String'(1 .. 128 - Prefix'Length => 'k');
   begin
      Msg := NULL_MESSAGE;
      Msg.tag := (label => Config_Worker_Startup.Operation'Enum_Rep
        (Config_Worker_Startup.Attach_Worker), length => 1,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := syscall (SYSCALL_GETPID);
      Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
      Check (Msg.tag.label = 16#F007#, "client cannot nominate storage backend");
      debugPrint ("TEST: PASS config-backend-nomination-denied" & ASCII.LF);
      Query (Read_Value, "config.store", Value, Result);
      Check (Result = Missing, "no hidden backing path");
      Query (Read_Value, "clock.time-zone", Value, Result);
      Check (Result = OK and then Value.Data (1 .. Value.Length) = "UTC", "CCL seed wins over old disk overlay");
      for Label in Unsigned_32 range 16#0604# .. 16#0605# loop
         Msg := NULL_MESSAGE;
         Msg.tag := (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
         Check (Msg.tag.label = 16#F001#, "retired persistence opcode");
      end loop;
      Write (Key, Full_Value, Write_Status);
      Check (Write_Status = Settings.Done, "full 128-byte key plus 4096-byte value");
      Settings.Inspection.Read (Config, Key, Stored, Read_Status);
      Check (Read_Status = Settings.Present and Stored.Length = Full_Value'Length, "full value read size");
      Check (Settings.Image (Stored) = Full_Value, "full value bytes");
      Settings.Inspection.Delete (Config, Key, Write_Status);
      Check (Write_Status = Settings.Done, "native delete");
      Settings.Inspection.Read (Config, Key, Stored, Read_Status);
      Check (Read_Status = Settings.Absent, "native missing result");
      Write (Key, Full_Value & 'v', Write_Status);
      Check (Write_Status = Settings.Too_Long, "oversized client value rejected before copy");
      Write (Key, "", Write_Status);
      Check (Write_Status = Settings.Done, "empty value");
      Settings.Inspection.Read (Config, Key, Stored, Read_Status);
      Check (Read_Status = Settings.Present and Stored.Length = 0, "empty value distinct from missing");
      Settings.Inspection.Delete (Config, Key, Write_Status);
      List (Config, "test.config", Read_Status);
      Check (Read_Status = Settings.Present and Listed = 0, "empty native listing");
      for I in 0 .. 39 loop
         declare
            List_Key : constant String := "test.config.bulk." &
              Character'Val (Character'Pos ('A') + I) &
              String'(1 .. 110 => 'k');
         begin
            Write (List_Key, "", Write_Status);
            Check (Write_Status = Settings.Done, "full native listing fixture");
         end;
      end loop;
      List (Config, "test.config.bulk", Read_Status);
      Check (Read_Status = Settings.Unavailable and Listed = 0,
             "native listing rejects overflow without partial publication");
      for I in 0 .. 39 loop
         declare
            List_Key : constant String := "test.config.bulk." &
              Character'Val (Character'Pos ('A') + I) &
              String'(1 .. 110 => 'k');
         begin
            Settings.Inspection.Delete (Config, List_Key, Write_Status);
            Check (Write_Status = Settings.Done, "native listing cleanup");
         end;
      end loop;
      CuBit.Memory_Grants.Create_Via_Capability
        (Service, Buffer'Address, 1, True, Grant, Created);
      Check (Created, "data malformed fixture");
      Buffer (1 .. 17) := "test.config.value";
      for Op in CuBit.Config_Protocol.Operation loop
         Msg := NULL_MESSAGE;
         Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Op), length => 4,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words := [Grant.slot, Grant.generation, Unsigned_64'Last, 0];
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
         Check (Msg.tag.label = 16#F001#, "oversized wire key");
         Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Op), length => 4,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words := [Unsigned_64'Last, Grant.generation, 17, 0];
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
         Check (Msg.tag.label = 16#F001#, "invalid data slot");
         Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Op), length => 2,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words := [Grant.slot, 17, 0, 0];
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
         Check (Msg.tag.label = 16#F001#, "legacy raw-slot frame rejected");
      end loop;
      Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Set_Value), length => 4,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [Grant.slot, Grant.generation, 17, 4096];
      Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
      Check (Msg.tag.label = 16#F001#, "short mapping rejected");
      CuBit.Memory_Grants.Revoke (Grant, Created);
      Check (Created, "data fixture revoked");
      for Op in CuBit.Config_Protocol.Operation loop
         Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Op), length => 4,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words := [Grant.slot, Grant.generation, 17, 0];
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
         Check (Msg.tag.label = 16#F001#, "stale data grant rejected");
      end loop;
      Check (CuBit.Memory_Grants.Retirement_Confirmed (Grant),
             "data service returned every acquisition");
      CuBit.Memory_Grants.Create_Via_Capability
        (Service, Buffer'Address, 1, False, Grant, Created);
      Check (Created, "read-only data fixture");
      for Op in CuBit.Config_Protocol.Operation loop
         Buffer (1 .. 17) := "test.config.value";
         Msg.tag := (label => CuBit.Config_Protocol.Operation'Enum_Rep (Op), length => 4,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words := [Grant.slot, Grant.generation, 17, 0];
         Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
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
   Query (Probe, "", Value, Result);
   Check (Result = OK, "explicit wildcard read grant");
   -- A data reader cannot install or revoke policy. Huge untrusted counts
   -- must be denied before narrowing to Natural or inspecting grant memory.
   Msg.tag := (label => 16#0080#, length => 4, protocol => 0, ipc => <>, reserved => 0);
   Msg.words := [others => Unsigned_64'Last];
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Denied), "reader cannot install ACL");
   Msg.tag := (label => 16#0081#, length => 1, protocol => 0, ipc => <>, reserved => 0);
   Msg.words := [others => Unsigned_64'Last];
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Denied), "reader cannot revoke ACL");
   Write ("test.config.value", Fixture, Write_Status);
   Check (Write_Status = Settings.Done, "fixture write");
   Write ("test.configuration", Fixture, Write_Status);
   Check (Write_Status = Settings.Refused, "component-boundary write denial");
   Write ("desktop.appearance.v1", Fixture, Write_Status);
   Check (Write_Status = Settings.Refused, "global read is not write");
   Query (Read_Value, "test.config.value", Value, Result);
   Check (Result = OK and then Value.Data (1 .. Value.Length) = Fixture, "owned result");
   Query (List_Keys, "test.config", Value, Result);
   Check (Result = OK and then Value.Data (1 .. Value.Length) = "test.config.value" & ASCII.LF, "listing");
   Query (Read_Value, "test.config.absent", Value, Result);
   Check (Result = Missing, "missing distinct from denial");
   Query (Read_Value, "test.config.value", Value, Result, In_Context => 1);
   Check (Result = Invalid_Request, "no context fallback");
   Write ("test.config.large", Oversized, Write_Status);
   Check (Write_Status = Settings.Done, "oversized fixture");
   Query (Read_Value, "test.config.large", Value, Result);
   Check (Result = Too_Large and Value.Length = 0, "no truncated value");
   -- Invalid lengths and stale grant generations must be rejected before reads.
   Msg.tag := (label => Operation'Enum_Rep (Read_Value), length => 4,
               protocol => 0, ipc => <>, reserved => 0);
   Msg.words := [0 => Unsigned_64'Last, 1 => 1, 2 => 1, 3 => 0];
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "invalid grant");
   CuBit.Memory_Grants.Create_Via_Capability
     (Service, Buffer'Address, 1, True, Grant, Created);
   Check (Created, "stale grant fixture");
   CuBit.Memory_Grants.Revoke (Grant, Created);
   Check (Created, "retire fixture");
   Msg.tag := (label => Operation'Enum_Rep (Read_Value), length => 4,
               protocol => 0, ipc => <>, reserved => 0);
   Msg.words := [0 => Grant.slot, 1 => Grant.generation, 2 => 1, 3 => 0];
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "stale grant");
   CuBit.Memory_Grants.Create_Via_Capability
     (Service, Buffer'Address, 1, False, Grant, Created);
   Check (Created, "readonly grant fixture");
   Msg.tag := (label => Operation'Enum_Rep (Read_Value), length => 4,
               protocol => 0, ipc => <>, reserved => 0);
   Msg.words := [0 => Grant.slot, 1 => Grant.generation, 2 => 1, 3 => 0];
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "readonly grant rejected");
   Msg.tag := (label => Operation'Enum_Rep (Read_Value), length => 4,
               protocol => 0, ipc => <>, reserved => 0);
   Msg.words (2) := Unsigned_64'Last;
   Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
   Check (Msg.tag.label = Status'Enum_Rep (Invalid_Request), "oversized request length");
   CuBit.Memory_Grants.Revoke (Grant, Created);
   Check (Created, "readonly fixture retired");
   for I in 0 .. 19 loop
      declare
         Key : constant String := "test.config.list." & Character'Val (Character'Pos ('a') + I) &
           ".padding.padding.padding.padding.padding.padding.padding";
      begin
         Write (Key, Fixture, Write_Status);
         Check (Write_Status = Settings.Done, "list fixture");
      end;
   end loop;
   Query (List_Keys, "test.config.list", Value, Result);
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
