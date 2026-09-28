with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CCL.Types;
with CCL.Objects;
with CCL.VM;
with Config_Object_Client;
with Config_Object_Client.VM;
with Config_Object_Messages;
with Nested_Fixture;
with VM_Fixture;
with Discovered_Fixture;
with Source_Fixture;
with Async_Fixture;
with Read_Fixture;
with Resource_Fixture;
with Receiver_Fixture;

-- Actual CuBit app: only a Config endpoint and scoped Config authority.
-- No filesystem, worker endpoint, database, CBOR or SQL access.
procedure Main is
   package Client renames Config_Object_Client;
   package Wire renames Config_Object_Messages;
   package Adapter renames Config_Object_Client.VM;
   use type Client.Submission;
   use type Client.Completion_Result;
   use type Wire.Status;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   use type Adapter.Read_State;
   use type CCL.VM.Value;
   Writer, Reader, Denied : Client.Client;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   First, Second : CCL.Objects.Image;
   First_Value, Second_Value : CCL.VM.Value;
   Built : CCL.Objects.Build_Result;
   Sent : Client.Submission;
   Item : Client.Response;
   Token, Ignore : Unsigned_64 := 0;
   Good : Boolean;
   Name : constant String := "org.cubit.publication";
   procedure Check (OK : Boolean; Step : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL config-objects " & Step & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Check;
   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1; return Token;
   end Next_Token;
   procedure Finish
     (Object : in out Client.Client; Expected : Wire.Status; Step : String;
      VM_Read : Boolean := False; Expected_Value : CCL.VM.Value := CCL.VM.Integer_Constant (0);
      Expected_Revision : Unsigned_64 := 0) is
      Completion : aliased CompletionEntry := NULL_COMPLETION;
      Done : Client.Completion_Result;
      Taken : Boolean;
      Activity : Activity_Result;
      VM_Item : Adapter.Read_Result;
   begin
      Check (Sent = Client.Submitted, Step & " submit");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            Client.Complete (Object, Completion, Done);
            Check (Done = Client.Completed, Step & " completion identity");
            exit;
         end if;
         Activity := Wait_For_Activity_Until (Unsigned_64'Last);
         Check (Activity /= Unavailable, Step & " wait");
      end loop;
      if VM_Read then
         Adapter.Take_Get_Result (Object, Types, VM_Item);
         Check (VM_Item.State = Adapter.Value_Ready and VM_Item.Code = Expected and
                VM_Item.Revision = Expected_Revision and
                VM_Item.Value = Expected_Value, Step & " VM reply");
         return;
      end if;
      Client.Take_Result (Object, Item, Taken);
      if not Taken or else not Item.Valid or else Item.Code /= Expected then
         debugPrint ("config-objects: result=" & Wire.Status'Enum_Rep (Item.Code)'Image & ASCII.LF);
      end if;
      Check (Taken and Item.Valid and Item.Code = Expected, Step & " reply");
   end Finish;
   procedure Close (Object : in out Client.Client) is
   begin
      Client.Close (Object, Next_Token, Sent); Finish (Object, Wire.Success, "close");
      Client.Retire (Object, Good); Check (Good, "grant retirement");
   end Close;
begin
   debugPrint ("CONFIG-OBJECTS: starting" & ASCII.LF);
   VM_Fixture.Run ("(+ 20 21)", Types, First_Value, Good); Check (Good, "compile and execute 41");
   VM_Fixture.Run ("(+ 20 22)", Types, Second_Value, Good); Check (Good, "compile and execute 42");
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   Check (Good, "binding");
   First := CCL.Objects.Empty (Contract); Second := First;
   CCL.Objects.Append (First, CCL.Objects.Integer_Cell (41), Built);
   Check (Built = CCL.Objects.Added, "first value");
   CCL.Objects.Append (Second, CCL.Objects.Integer_Cell (42), Built);
   Check (Built = CCL.Objects.Added, "second value");
   Client.Initialize (Writer, CAP_SLOT_CONFIG, Good); Check (Good, "writer grant");
   Client.Initialize (Reader, CAP_SLOT_CONFIG, Good); Check (Good, "reader grant");
   Client.Initialize (Denied, CAP_SLOT_CONFIG, Good); Check (Good, "denied grant");
   Client.Create (Denied, "org.other.private", Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Denied, Wire.Denied, "out of scope create");
   Client.Open (Denied, "org.other.private", Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Denied, Wire.Denied, "out of scope open");
   Client.Retire (Denied, Good); Check (Good, "denied retirement");
   debugPrint ("TEST: PASS config-objects-scope-denied" & ASCII.LF);
   Client.Create (Writer, Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Wire.Success, "create");
   Client.Get (Writer, Next_Token, Sent); Finish (Writer, Wire.Missing, "unset");
   Adapter.Set_Value (Writer, Types, First_Value, 0, Next_Token, Sent); Finish (Writer, Wire.Success, "first set");
   Check (Item.Revision = 1, "first revision");
   Client.Get (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "first get", True, First_Value, 1);
   Adapter.Set_Value (Writer, Types, Second_Value, 0, Next_Token, Sent); Finish (Writer, Wire.Conflict, "stale write");
   Client.Get (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "unchanged get");
   Check (Item.Revision = 1 and Item.Value = First, "conflict preserved value");
   Client.Open (Reader, Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Reader, Wire.Success, "read-only open");
   Adapter.Set_Value (Reader, Types, Second_Value, 1, Next_Token, Sent); Finish (Reader, Wire.Denied, "read-only set");
   Client.Get (Reader, Next_Token, Sent); Finish (Reader, Wire.Success, "read-only get");
   Check (Item.Revision = 1 and Item.Value = First, "read-only native object");
   Close (Reader);
   Adapter.Set_Value (Writer, Types, Second_Value, 1, Next_Token, Sent); Finish (Writer, Wire.Success, "second set");
   Check (Item.Revision = 2, "second revision");
   -- Reopen through idempotent Create without adding revisions or a new value.
   Client.Close (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "writer close");
   Client.Create (Writer, Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Wire.Success, "identical create");
   Client.Get (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "second get", True, Second_Value, 2);
   debugPrint ("TEST: PASS config-objects-compiled-vm" & ASCII.LF);
   Client.Close (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "scalar close before nested");
   Resource_Fixture.Run (Token, Good); Check (Good, "opaque resource factory/read/close");
   debugPrint ("TEST: PASS config-objects-resource-vm" & ASCII.LF);
   -- New collection on the same owned client: nested native objects, not a
   -- stringified record or a client-side disk codec.
   Nested_Fixture.Define (False, Contract, Good);
   Check (Good, "nested binding");
   Nested_Fixture.Values (Contract, First, Second, Good);
   Check (Good, "nested values");
   Client.Create (Writer, Nested_Fixture.Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Wire.Success, "nested create");
   Read_Fixture.Run (Writer, Contract, First, 0, Token, Good);
   Check (Good, "typed native Missing outcome");
   Read_Fixture.Store (Writer, Contract, First, 1, Token, Good,
     "(match (field (config-test.supplied) mode) " &
     "((Mode.Inactive) (Preferences ""Cubie"" Mode.Inactive -9223372036854775808)) " &
     "((Mode.Active active) (Preferences (concat ""Cub"" ""ie"") (Mode.Active active) -9223372036854775808)))");
   Check (Good, "source constructed nested record and variant Set");
   Client.Get (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "nested first get");
   Check (Item.Revision = 1 and Item.Value = First, "nested active exact snapshot");
   declare
      Malformed : CCL.Objects.Image := First;
   begin
      Malformed.Cells (3).First := 3; -- Mode has only two alternatives.
      Client.Set (Writer, Malformed, 1, Next_Token, Sent);
      Check (Sent = Client.Invalid_Request, "bad variant rejected locally");
   end;
   Client.Set (Writer, Second, 0, Next_Token, Sent);
   Finish (Writer, Wire.Conflict, "nested stale set");
   Client.Get (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "nested unchanged get");
   Check (Item.Revision = 1 and Item.Value = First, "nested conflict preserved value");
   Receiver_Fixture.Run (Contract, First, Second, Token, Good);
   Check (Good, "native resource receiver and aggregate get/set");
   debugPrint ("TEST: PASS config-objects-receiver-vm" & ASCII.LF);
   Client.Get (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "nested second get");
   Check (Item.Revision = 2 and Item.Value = Second, "nested maximum-text exact snapshot");
   Read_Fixture.Run (Writer, Contract, Second, 2, Token, Good);
   Check (Good, "typed native nested read outcome");
   debugPrint ("TEST: PASS config-objects-read-outcome" & ASCII.LF);
   Client.Close (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "nested close");
   Nested_Fixture.Define (True, Contract, Good);
   Check (Good, "shifted nested binding");
   Client.Create (Writer, Nested_Fixture.Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Wire.Success, "equivalent nested create");
   Client.Get (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "equivalent nested get");
   Check (Item.Revision = 2 and Item.Value = Second, "equivalent create preserved snapshot");
   Client.Close (Writer, Next_Token, Sent); Finish (Writer, Wire.Success, "equivalent nested close");
   debugPrint ("TEST: PASS config-objects-equivalent-schema" & ASCII.LF);
   Discovered_Fixture.Build (False, Contract, Types, First_Value, Second_Value, Good);
   Check (Good, "discovered compilation");
   Client.Create (Writer, Discovered_Fixture.Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Wire.Success, "discovered create");
   Async_Fixture.Run (Writer, Contract, Types, First_Value, 1, Token, Async_Fixture.Write_Initial, Good);
   Check (Good, "compiled typed CCL suspension and Config completion");
   Source_Fixture.Run (Writer, Contract, Types, "(config-test.get)", First_Value, 1, Token, False, Good);
   Check (Good, "typed interpreted CCL get");
   debugPrint ("TEST: PASS config-objects-source-host" & ASCII.LF);
   debugPrint ("TEST: PASS config-objects-async-vm" & ASCII.LF);
   Client.Get (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "discovered first get", True, First_Value, 1);
   Adapter.Set_Value (Writer, Types, Second_Value, 1, Next_Token, Sent);
   Finish (Writer, Wire.Success, "discovered second set");
   Client.Get (Writer, Next_Token, Sent);
   Finish (Writer, Wire.Success, "discovered second get", True, Second_Value, 2);
   Close (Writer);
   debugPrint ("TEST: PASS config-objects-discovered-type" & ASCII.LF);
   debugPrint ("TEST: PASS config-objects-nested" & ASCII.LF);
   debugPrint ("TEST: PASS config-objects-native" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
