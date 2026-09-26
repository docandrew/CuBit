with CCL.VM.Native_Objects;

-- Linux preview has no CuBit Config endpoint. Never invent a live collection.
package body CCL_Execution is
   package N renames CCL.VM.Native_Objects;
   Program : CCL.VM.Validated_Program;
   Machine : N.Machine;
   Loaded : Boolean := False;
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean) is
      pragma Unreferenced (Catalog, Grants);
   begin Success := True; end Install;
   function Can_Replace return Boolean is (True);
   function Waiting_For_IO return Boolean is (False);
   procedure Load (Item : CCL.VM.Validated_Program; Fuel : Natural; Success : out Boolean) is
   begin
      Success := CCL.VM.Is_Valid (Item);
      if Success then Program := Item; N.Initialize (Program, Fuel, Machine); Loaded := True; end if;
   end Load;
   procedure Advance (Instructions : Natural; Result : out CCL.VM.Execution_Result) is
   begin
      Result := (others => <>);
      if Loaded then N.Continue_Execution_For (Program, Machine, Instructions, Result); end if;
   end Advance;
   procedure Complete_Scalar (Value : CCL.VM.Value; Accepted : Boolean) is
   begin if Loaded then N.Complete_Scalar (Program, Machine, Value, Accepted); end if; end Complete_Scalar;
   procedure Stop is begin N.Stop (Machine); end Stop;
   function Snapshot return CCL.VM.Machine_Snapshot is (N.Snapshot (Machine));
   procedure Inspect (Result : out CCL.VM.Inspection_Snapshot) is
   begin
      Result := (others => <>);
      if Loaded then N.Inspect (Program, Machine, Result); end if;
   end Inspect;
   procedure Take_Changed (Changed : out Boolean) is begin Changed := False; end Take_Changed;
end CCL_Execution;
