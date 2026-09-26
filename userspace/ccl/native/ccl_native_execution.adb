with CCL_Config_IO;
with CCL.Types;
with CCL.Objects;
with Config_Object_Client.Resources.Runs;
with Config_Object_Interfaces;
with Config_Read_Outcomes;

package body CCL_Native_Execution is
   package H renames Config_Object_Client.Resources.Runs;
   package D renames Config_Object_Interfaces;
   use type Interfaces.Unsigned_64;
   use type CCL.Catalog.Grant_Result;
   use type CCL.VM.Execution_Status;
   -- SHA-256 of the checked-in workbench-config-*.schema definitions. These
   -- pin metadata, not publisher trust or authority. This is a trusted bootstrap
   -- specialization through CAP_SLOT_CONFIG, not network service discovery.
   Value_Key : constant CCL.Objects.Schema_Key :=
     [16#8ed97f14e39b4c54#, 16#3c314b37313b16bd#, 16#0d37fd15c3d4574f#, 16#88b942859045912b#];
   Read_Key : constant CCL.Objects.Schema_Key :=
     [16#bd242ae3d3b4268c#, 16#2ce512e6b76d3995#, 16#01b9af0722bef170#, 16#db5b93bd7bdd392f#];
   Interface_Key : constant CCL.Catalog.Descriptor_Digest :=
     [16#ccfe8bcee8ae4eb9#, 16#b2878bd96530f7ef#, 16#80016f6c212082c6#, 16#dba9f7116d2bded8#];
   Bindings : constant H.Binding_Table :=
     [D.Open_Collection => 16#0005_0001#, D.Read_Value => 16#0005_0002#,
      D.Write_Value => 16#0005_0003#, D.Close_Collection => 16#0005_0004#];
   Host : H.Runner (1);
   Tokens : Interfaces.Unsigned_64 := 0;
   Dirty : Boolean := False;
   Reported : Boolean := False;

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Candidate : CCL.Catalog.Interface_Catalog := Catalog;
      Proposed : CCL.Catalog.Granted_Bindings := Grants;
      Types : CCL.Types.Registry;
      Contract : CCL.Objects.Binding;
      Reads : Config_Read_Outcomes.Description;
      Kind : CCL.Types.Type_Reference;
      Operation : CCL.Catalog.Resolved_Operation;
      Granted : CCL.Catalog.Grant_Result;
   begin
      Success := True;
      if not CCL_Config_IO.Available then return; end if;
      CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Value_Key, Contract, Success);
      if not Success then return; end if;
      Config_Read_Outcomes.Define (Contract, CCL.Types.Named ("CounterSnapshot"),
        CCL.Types.Named ("CounterRead"), Read_Key, Reads, Success);
      if not Success then return; end if;
      D.Publish (Candidate, "config-values", Interface_Key, Contract, Reads, Kind, Success);
      if not Success then return; end if;
      for Action in D.Operation loop
         CCL.Catalog.Resolve (Candidate, "config-values." & D.Name (Action), Operation, Success);
         if not Success then return; end if;
         CCL.Catalog.Install (Proposed, Operation, Bindings (Action), Granted);
         if Granted /= CCL.Catalog.Grant_Added then Success := False; return; end if;
      end loop;
      H.Configure (Host, CCL.Catalog.Visible_Types (Candidate), Kind, Contract, Reads,
        Bindings, CuBit.Messages.CAP_SLOT_CONFIG, "com.cubit.ccl-workbench.demo.counter", Success);
      if Success then Catalog := Candidate; Grants := Proposed; end if;
      -- The service still enforces the manifest's narrow name scope on every
      -- open/read/write. Catalog installation cannot bypass that enforcement.
   end Install;
   function Next_Token return Interfaces.Unsigned_64 is
   begin
      if Tokens >= Interfaces.Unsigned_64'Last - 1 then return 0; end if;
      Tokens := Tokens + 1; return Tokens;
   end Next_Token;
   function Can_Replace return Boolean is (H.Can_Replace (Host));
   function Waiting_For_IO return Boolean is (H.Waiting_For_IO (Host));
   function Cleanup_Needs_Retry return Boolean is
     (H.Cleanup_Retry_Needed (Host));
   procedure Maintain is begin H.Maintain (Host, Tokens); end Maintain;
   procedure Load (Item : CCL.VM.Validated_Program; Fuel : Natural; Success : out Boolean) is
   begin
      Success := CCL.VM.Is_Valid (Item);
      if Success then H.Load (Host, Item, Fuel, Success); end if;
      if Success then Reported := False; end if;
   end Load;
   procedure Advance (Instructions : Natural; Result : out CCL.VM.Execution_Result) is
   begin
      H.Advance (Host, Instructions, Tokens, Result);
      if not Reported and then Result.Status = CCL.VM.Completed then
         -- Diagnostic status only: never log source text or Config payloads.
         CuBit.Messages.debugPrint ("ccl-workbench: bytecode completed" & ASCII.LF);
         Reported := True;
      end if;
   end Advance;
   procedure Complete_Scalar (Value : CCL.VM.Value; Accepted : Boolean) is
   begin H.Complete_Scalar (Host, Value, Accepted); end Complete_Scalar;
   procedure Stop is begin H.Stop (Host, Tokens); end Stop;
   function Snapshot return CCL.VM.Machine_Snapshot is (H.Snapshot (Host));
   procedure Inspect (Result : out CCL.VM.Inspection_Snapshot) is
   begin H.Inspect (Host, Result); end Inspect;
   procedure Take_Changed (Changed : out Boolean) is
   begin Changed := Dirty; Dirty := False; end Take_Changed;
   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean) is
   begin
      H.Complete (Host, Receipt, Tokens, Consumed);
      Dirty := Dirty or Consumed;
   end Deliver;
end CCL_Native_Execution;
