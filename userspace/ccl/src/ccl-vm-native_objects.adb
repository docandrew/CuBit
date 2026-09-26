with CCL.Objects.Values;
with CCL.VM.Resource_Values;

package body CCL.VM.Native_Objects with SPARK_Mode is
   use type CCL.Imports.Import_Phase;
   procedure Load_View
     (Store : Object_Array; Types : CCL.Types.Registry; Owner : Stored_Index;
      Node : CCL.Objects.Views.Cursor; Result : out Value; Accepted : out Boolean)
   is
      Ref : constant CCL.Types.Type_Reference := CCL.Objects.Views.Local_Type (Store (Owner), Node, Types);
      Cell : CCL.Objects.Cell;
      Choice : CCL.Types.Component_Count;
      D : CCL.Types.Description;
   begin
      Result := (others => <>); Accepted := False;
      if Ref = CCL.Types.Invalid_Type then return; end if;
      case Kind_For_Type (Types, Ref) is
         when Integer_Value =>
            Result := Integer_Constant (CCL.Objects.Integer_Of (CCL.Objects.Views.Scalar (Store (Owner), Node)));
         when Boolean_Value =>
            Result := Boolean_Constant (CCL.Objects.Views.Scalar (Store (Owner), Node).First = 1);
         when Variant_Value =>
            D := CCL.Types.Describe (Types, Ref);
            Choice := CCL.Objects.Views.Alternative (Store (Owner), Node);
            if Choice not in 1 .. D.Count then return; end if;
            Result := (Kind => Variant_Value, Data_Type => Ref, Alternative => Choice, others => <>);
            Cell := CCL.Objects.Views.Scalar (Store (Owner), CCL.Objects.Views.Payload (Store (Owner), Node));
            case D.Parts (Choice).Payload is
               when CCL.Types.Integer_Type => Result.Integer := CCL.Objects.Integer_Of (Cell);
               when CCL.Types.Boolean_Type => Result.Boolean := Cell.First = 1;
               when CCL.Types.Unit_Type => null;
               when others => return;
            end case;
         when Object_Value =>
            Result := (Kind => Object_Value, Data_Type => Ref, Object => Owner, Object_Node => Node, others => <>);
         when Resource_Value => return;
      end case;
      Accepted := True;
   end Load_View;

   procedure Evaluate_View
     (Store : in out Object_Array; Types : CCL.Types.Registry;
      Op : Instruction; Source : Value; Result : out Value;
      Alternative : out CCL.Types.Component_Count; Accepted : out Boolean)
     with Global => null
   is
      Node : CCL.Objects.Views.Cursor;
      D : CCL.Types.Description;
   begin
      Result := (others => <>); Alternative := 0; Accepted := False;
      if Source.Kind /= Object_Value or else Source.Object = 0 or else
        not Source.Copyable or else Source.Type_Tag /= 0 or else
        CCL.Objects.Views.Local_Type (Store (Source.Object), Source.Object_Node, Types) /= Source.Data_Type
      then return; end if;
      case Op.Op is
         when Project_Field =>
            if Op.Immediate not in 1 .. Integer_64 (CCL.Types.Maximum_Components) then return; end if;
            Node := CCL.Objects.Views.Field (Store (Source.Object), Source.Object_Node,
              CCL.Types.Component_Index (Op.Immediate));
         when Switch_Variant =>
            Alternative := CCL.Objects.Views.Alternative (Store (Source.Object), Source.Object_Node);
            D := CCL.Types.Describe (Types, Source.Data_Type);
            if Alternative not in 1 .. D.Count then return; end if;
            if D.Parts (Alternative).Payload = CCL.Types.Unit_Type then Accepted := True; return; end if;
            Node := CCL.Objects.Views.Payload (Store (Source.Object), Source.Object_Node);
         when others => return;
      end case;
      Load_View (Store, Types, Source.Object, Node, Result, Accepted);
   end Evaluate_View;
   procedure Run is new Continue_With_Native (Object_Array, Evaluate_View);

   procedure Clear (State : in out Machine) is
   begin
      for Object of State.Objects loop CCL.Objects.Views.Clear (Object); end loop;
      State.Used := 0;
   end Clear;

   procedure Initialize (Item : Validated_Program; Fuel : Natural; State : in out Machine) is
   begin
      Clear (State);
      CCL.VM.Initialize (Item, Fuel, State.Core);
      State.Initialized := True;
   end Initialize;

   function Snapshot (State : Machine) return Machine_Snapshot is
     (CCL.VM.Snapshot (State.Core));

   procedure Inspect
     (Item : Validated_Program; State : Machine;
      Result : out Inspection_Snapshot) is
   begin
      Result := (others => <>);
      Result.Machine := Snapshot (State);
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Inspect (Item, State.Core, Result);
      end if;
   end Inspect;

   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Machine;
      Instructions : Natural; Result : out Execution_Result) is
   begin
      Result := (others => <>);
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) then
         Result.Status := Invalid_Bytecode; return;
      end if;
      Run (Item, State.Core, State.Objects, Instructions, Result);
      -- Admission before the host effect, not only when its reply arrives.
      if Result.Status = Waiting_For_Host and then
        State.Core.Waiting_Result_Kind = Object_Value and then State.Used = MAX_OBJECT_VALUES
      then
         if State.Core.Waiting_Owned then
            -- No effect was submitted: return the offered local unchanged
            -- before making exhaustion terminal. Do not strand the lifecycle
            -- in Offered while clearing its Waiting flag.
            CCL.VM.Acknowledge_Host_Submission (Item, State.Core, False);
         end if;
         State.Core.Waiting := False;
         State.Core.Terminal := True;
         State.Core.Terminal_Status := Object_Storage_Exhausted;
         Result := (Status => Object_Storage_Exhausted,
           Fuel_Remaining => Result.Fuel_Remaining, Steps => Result.Steps, others => <>);
      end if;
   end Continue_Execution_For;

   function Pending_Call (Item : Validated_Program; State : Machine)
     return Execution_Result is
      Position : constant Machine_Snapshot := Snapshot (State.Core);
   begin
      if not State.Initialized or else not Is_Well_Formed (Item, State.Core) or else
        not State.Core.Waiting or else State.Core.Terminal
      then return (others => <>); end if;
      return (Status => Waiting_For_Host,
        Fuel_Remaining => Position.Fuel_Remaining, Steps => Position.Steps,
        Requested_Import => State.Core.Waiting_Import,
        Request_Argument => State.Core.Waiting_Argument,
        Request_Receiver => State.Core.Waiting_Receiver,
        Request_Owned => State.Core.Waiting_Owned,
        Requested_Authority => Item.Content.Imports (State.Core.Waiting_Import).Authority,
        Requested_Binding => Item.Content.Imports (State.Core.Waiting_Import).Binding,
        others => <>);
   end Pending_Call;

   function Ready_For_Completion (Item : Validated_Program; State : Machine) return Boolean is
     (State.Initialized and then Is_Well_Formed (Item, State.Core) and then
      State.Core.Waiting and then not State.Core.Terminal and then
      (not State.Core.Waiting_Owned or else
       CCL.Imports.Phase (State.Core.Import_Lifecycle) = CCL.Imports.Import_Accepted));

   function Accepts_Object_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding)
      return Boolean is
     (Pending_Call (Item, State).Status = Waiting_For_Host and then
      CCL.Objects.Matches_Type (Contract, Item.Content.Data_Types,
        (case State.Core.Waiting_Result_Kind is
          when Integer_Value => CCL.Types.Integer_Type,
          when Boolean_Value => CCL.Types.Boolean_Type,
          when Variant_Value | Object_Value => Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type,
          when Resource_Value => CCL.Types.Invalid_Type)) and then
      (State.Core.Waiting_Result_Kind /= Object_Value or else State.Used < MAX_OBJECT_VALUES));

   procedure Complete_Object
     (Item : Validated_Program; State : in out Machine;
      Contract : CCL.Objects.Binding; Response : CCL.Objects.Image; Accepted : Boolean)
   is
      Good : Boolean;
      Result : CCL.VM.Value;
   begin
      if not Ready_For_Completion (Item, State) then return; end if;
      Good := Accepted and then Accepts_Object_Result (Item, State, Contract);
      if Good and then State.Core.Waiting_Result_Kind = Object_Value then
         CCL.Objects.Views.Capture (State.Objects (State.Used + 1), Contract, Response, Good);
         if Good then
            State.Used := State.Used + 1;
            Result := (Kind => Object_Value,
              Data_Type => Item.Content.Imports (State.Core.Waiting_Import).Result_Data_Type, Object => State.Used,
              Object_Node => CCL.Objects.Views.Root (State.Objects (State.Used)), others => <>);
         end if;
      elsif Good then
         CCL.Objects.Values.To_VM (Contract, Item.Content.Data_Types, Response, Result, Good);
      end if;
      Complete_Checked_Host_Call (Item, State.Core, Result, Good, True);
   end Complete_Object;

   procedure Complete_Scalar
     (Item : Validated_Program; State : in out Machine; Response : Value; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Complete_Host_Call (Item, State.Core, Response, Accepted);
      end if;
   end Complete_Scalar;

   procedure Complete_Resource
     (Item : Validated_Program; State : in out Machine;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Resource_Values.Complete (Item, State.Core, Owner, Resource, Accepted);
      end if;
   end Complete_Resource;

   procedure Acknowledge_Host_Submission
     (Item : Validated_Program; State : in out Machine; Accepted : Boolean) is
   begin
      if State.Initialized and then Is_Well_Formed (Item, State.Core) then
         CCL.VM.Acknowledge_Host_Submission (Item, State.Core, Accepted);
      end if;
   end Acknowledge_Host_Submission;

   procedure Export_Value
     (Item : Validated_Program; State : Machine; Source : CCL.VM.Value;
      Contract : CCL.Objects.Binding; Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if not Source.Copyable or else Source.Type_Tag /= 0 then return; end if;
      if Source.Kind = Object_Value then
         if Source.Object = 0 or else Source.Object > State.Used or else
           not CCL.Objects.Matches_Type (Contract, Item.Content.Data_Types, Source.Data_Type)
         then return; end if;
         CCL.Objects.Views.Copy_Value
           (State.Objects (Source.Object), Source.Object_Node,
            Contract, Value, Accepted);
      else
         CCL.Objects.Values.From_VM (Contract, Item.Content.Data_Types, Source, Value, Accepted);
      end if;
   end Export_Value;

   procedure Export_Argument
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Waiting and then not State.Core.Terminal and then
        (not State.Core.Waiting_Owned or else Has_Receiver (Item.Content.Imports (State.Core.Waiting_Import)))
      then Export_Value (Item, State, State.Core.Waiting_Argument, Contract, Value, Accepted); end if;
   end Export_Argument;

   procedure Export_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean) is
   begin
      Value := CCL.Objects.Empty (Contract); Accepted := False;
      if State.Initialized and then State.Core.Terminal and then
        State.Core.Terminal_Status = Completed and then State.Core.Has_Value
      then Export_Value (Item, State, State.Core.Result_Value, Contract, Value, Accepted); end if;
   end Export_Result;

   procedure Stop (State : in out Machine) is
   begin
      CCL.VM.Stop (State.Core);
      Clear (State);
      State.Initialized := False;
   end Stop;
end CCL.VM.Native_Objects;
