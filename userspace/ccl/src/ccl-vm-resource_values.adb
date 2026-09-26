package body CCL.VM.Resource_Values with SPARK_Mode is
   procedure Complete
     (Item : Validated_Program; State : in out Machine_State;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean)
   is
      Response : Value;
   begin
      Accepted := False;
      if not State.Waiting or else State.Terminal or else State.Waiting_Owned or else
        State.Waiting_Result_Kind /= Resource_Value
      then return; end if;
      declare
         Operation : constant Import_Declaration := Item.Content.Imports (State.Waiting_Import);
      begin
         if not CCL.Resources.Matches_Type (Owner, Resource, Item.Content.Data_Types, Operation.Result_Data_Type)
         then return; end if;
         Response := (Kind => Resource_Value, Resource => Resource,
           Data_Type => Operation.Result_Data_Type, Type_Tag => Operation.Result_Type_Tag,
           Copyable => False, others => <>);
      end;
      Complete_Checked_Host_Call (Item, State, Response, True, False, Resource_Response => True);
      Accepted := not State.Terminal and not State.Waiting;
   end Complete;
end CCL.VM.Resource_Values;
