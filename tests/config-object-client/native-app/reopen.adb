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

-- Second independent boot: manifest grants ONLY Read in the fixture scope.
procedure Reopen is
   package Client renames Config_Object_Client;
   package Wire renames Config_Object_Messages;
   package Adapter renames Config_Object_Client.VM;
   use type Client.Submission;
   use type Client.Completion_Result;
   use type Wire.Status;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   use type CCL.Types.Type_Reference;
   use type CCL.VM.Value;
   use type Adapter.Read_State;
   Reader : Client.Client;
   Types : CCL.Types.Registry;
   Contract, Wrong_Key : CCL.Objects.Binding;
   Expected : CCL.Objects.Image;
   Expected_Value : CCL.VM.Value;
   Built : CCL.Objects.Build_Result;
   Sent : Client.Submission;
   Item : Client.Response;
   Token, Ignore : Unsigned_64 := 0;
   Good : Boolean;
   Name : constant String := "org.cubit.publication";
   procedure Check (OK : Boolean; Step : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL config-objects-reopen " & Step & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Check;
   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1; return Token;
   end Next_Token;
   procedure Finish (Code : Wire.Status; Step : String; VM_Read : Boolean := False) is
      Completion : aliased CompletionEntry := NULL_COMPLETION;
      Done : Client.Completion_Result;
      Taken : Boolean;
      Activity : Activity_Result;
      VM_Item : Adapter.Read_Result;
   begin
      Check (Sent = Client.Submitted, Step & " submit");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            Client.Complete (Reader, Completion, Done);
            Check (Done = Client.Completed, Step & " completion");
            exit;
         end if;
         Activity := Wait_For_Activity_Until (Unsigned_64'Last);
         Check (Activity /= Unavailable, Step & " wait");
      end loop;
      if VM_Read then
         Adapter.Take_Get_Result (Reader, Types, VM_Item);
         Check (VM_Item.State = Adapter.Value_Ready and VM_Item.Code = Code and
                VM_Item.Revision = 2 and VM_Item.Value = Expected_Value, Step & " VM reply");
         return;
      end if;
      Client.Take_Result (Reader, Item, Taken);
      Check (Taken and Item.Valid and Item.Code = Code, Step & " reply");
   end Finish;
begin
   VM_Fixture.Run ("(+ 21 21)", Types, Expected_Value, Good); Check (Good, "compile expected value");
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   Check (Good, "binding");
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [others => 9], Wrong_Key, Good);
   Check (Good, "wrong key binding");
   Expected := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Expected, CCL.Objects.Integer_Cell (42), Built);
   Check (Built = CCL.Objects.Added, "native value");
   Client.Initialize (Reader, CAP_SLOT_CONFIG, Good); Check (Good, "grant");
   Client.Create (Reader, Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Denied, "read authority cannot create");
   Client.Open (Reader, Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Wire.Denied, "read authority cannot open writable");
   Client.Open (Reader, Name & ".absent", Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Missing, "absent definition");
   Client.Open (Reader, Name, Wrong_Key, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Schema_Mismatch, "wrong schema");
   Client.Open (Reader, Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Success, "restore and open");
   Client.Get (Reader, Next_Token, Sent); Finish (Wire.Success, "restored get", VM_Read => True);
   Adapter.Set_Value (Reader, Types, Expected_Value, 2, Next_Token, Sent); Finish (Wire.Denied, "read-only set");
   debugPrint ("TEST: PASS config-objects-compiled-vm-reopen" & ASCII.LF);
   Client.Close (Reader, Next_Token, Sent); Finish (Wire.Success, "close");
   -- Second Open is a cache hit; no second restore or new revision.
   Client.Open (Reader, Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Success, "cached open");
   Client.Get (Reader, Next_Token, Sent); Finish (Wire.Success, "cached get");
   Check (Item.Revision = 2 and Item.Value = Expected, "unchanged cached object");
   Client.Close (Reader, Next_Token, Sent); Finish (Wire.Success, "close cached");
   -- Local declaration IDs differ from the writer. Persisted schema identity
   -- and shape must restore native data without restoring process-local IDs.
   Nested_Fixture.Define (True, Contract, Good); Check (Good, "shifted binding");
   declare
      First : CCL.Objects.Image;
      Writer_Contract : CCL.Objects.Binding;
   begin
      Nested_Fixture.Define (False, Writer_Contract, Good);
      Check (Good and CCL.Objects.Root_Type (Contract) /= CCL.Objects.Root_Type (Writer_Contract),
             "different local type numbers");
      Nested_Fixture.Values (Contract, First, Expected, Good);
   end;
   Check (Good, "shifted native value");
   Client.Open (Reader, Nested_Fixture.Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Wire.Success, "nested restore");
   Client.Get (Reader, Next_Token, Sent); Finish (Wire.Success, "nested restored get");
   Check (Item.Revision = 2 and Item.Value = Expected, "nested restored exact value");
   Read_Fixture.Run (Reader, Contract, Expected, 2, Token, Good);
   Check (Good, "typed native restored read outcome");
   debugPrint ("TEST: PASS config-objects-read-outcome-reopen" & ASCII.LF);
   Client.Set (Reader, Expected, 2, Next_Token, Sent); Finish (Wire.Denied, "nested read-only set");
   Client.Close (Reader, Next_Token, Sent); Finish (Wire.Success, "nested close");
   declare
      First_Value : CCL.VM.Value;
   begin
      Discovered_Fixture.Build (True, Contract, Types, First_Value, Expected_Value, Good);
      Check (Good and Expected_Value.Data_Type /= CCL.Objects.Root_Type (Contract), "discovered local type translation");
      Client.Open (Reader, Discovered_Fixture.Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
      Finish (Wire.Success, "discovered open");
      Client.Get (Reader, Next_Token, Sent); Finish (Wire.Success, "discovered restored get", VM_Read => True);
      Source_Fixture.Run (Reader, Contract, Types, "(config-test.get)", Expected_Value, 2, Token, False, Good);
      Check (Good, "typed CCL source restored get");
      debugPrint ("TEST: PASS config-objects-source-host-reopen" & ASCII.LF);
      Async_Fixture.Run (Reader, Contract, Types, Expected_Value, 2, Token, Async_Fixture.Read_Existing, Good);
      Check (Good, "typed VM restored Config completion");
      debugPrint ("TEST: PASS config-objects-async-vm-reopen" & ASCII.LF);
      Async_Fixture.Run (Reader, Contract, Types, Expected_Value, 2, Token, Async_Fixture.Denied_Write, Good);
      Check (Good, "typed CCL denied write followed by unchanged reads");
      debugPrint ("TEST: PASS config-objects-write-outcome-denied" & ASCII.LF);
      Adapter.Set_Value (Reader, Types, First_Value, 2, Next_Token, Sent);
      Finish (Wire.Denied, "discovered read-only set");
      Client.Close (Reader, Next_Token, Sent); Finish (Wire.Success, "discovered close");
      debugPrint ("TEST: PASS config-objects-discovered-type-reopen" & ASCII.LF);
   end;
   Client.Retire (Reader, Good); Check (Good, "nested retire");
   debugPrint ("TEST: PASS config-objects-nested-reopen" & ASCII.LF);
   debugPrint ("TEST: PASS config-objects-reopen" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Reopen;
