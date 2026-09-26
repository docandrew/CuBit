with Config_Object_Outcomes;
with CCL.Host_Values;
package body Config_Object_Client.VM.Calls is
   use type Interfaces.Unsigned_32;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Value;
   use type Config_Object_Messages.Operation;
   use type Config_Object_Messages.Status;

   procedure Submit
     (Object : in out Client; Action : Operation;
      Item : CCL.VM.Validated_Program; Machine : CCL.VM.Native_Objects.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Binding : Interfaces.Unsigned_32; Expected_Revision, Token : Number;
      Result : out Submission)
   is
      package N renames CCL.VM.Native_Objects;
      Request : constant CCL.VM.Execution_Result := N.Pending_Call (Item, Machine);
      Value : CCL.Objects.Image;
      Good : Boolean;
   begin
      Result := Admission (Object, Token);
      if Result /= Submitted then return; end if;
      Result := Invalid_Request;
      if Request.Request_Owned or else Binding = 0 or else Request.Status /= CCL.VM.Waiting_For_Host or else
        Request.Requested_Binding /= Binding then return; end if;
      case Action is
         when Read_Value =>
            if Request.Request_Argument /= CCL.VM.Integer_Constant (0) or else
              not Config_Read_Outcomes.Matches (Read_Description, Object.Contract) or else
              not N.Accepts_Object_Result (Item, Machine, Config_Read_Outcomes.Schema (Read_Description))
            then return; end if;
            Get (Object, Token, Result);
         when Write_Value =>
            if not N.Accepts_Object_Result (Item, Machine, Config_Object_Outcomes.Schema) then return; end if;
            N.Export_Argument (Item, Machine, Object.Contract, Value, Good);
            if Good then Set (Object, Value, Expected_Revision, Token, Result); end if;
      end case;
   end Submit;

   procedure Resume
     (Object : in out Client; Item : CCL.VM.Validated_Program;
      Machine : in out CCL.VM.Native_Objects.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Binding : Interfaces.Unsigned_32; Result : out Resume_State)
   is
      package N renames CCL.VM.Native_Objects;
      Request : constant CCL.VM.Execution_Result := N.Pending_Call (Item, Machine);
      Contract : CCL.Objects.Binding;
      Reply : CCL.Host_Values.Call_Result;
      Value : CCL.Objects.Image;
      Good : Boolean;
      use type CCL.Host_Values.Value_Kind;
   begin
      Result := No_Completion;
      if Object.Current /= Result_Ready then return; end if;
      Result := Other_Call;
      if Request.Request_Owned or else Binding = 0 or else Request.Status /= CCL.VM.Waiting_For_Host or else
        Request.Requested_Binding /= Binding or else
        Object.Action not in Config_Object_Messages.Get_Object | Config_Object_Messages.Set_Object
      then return; end if;
      Result := Type_Mismatch;
      if Object.Action = Config_Object_Messages.Get_Object then
         Contract := Config_Read_Outcomes.Schema (Read_Description);
         if not N.Accepts_Object_Result (Item, Machine, Contract) or else
           not Config_Read_Outcomes.Matches (Read_Description, Object.Contract) then return; end if;
         Config_Read_Outcomes.Build (Read_Description, Object.Output.Valid, Object.Output.Code,
           Object.Output.Revision, Object.Output.Value, Value, Good);
         if not Good then return; end if;
      else
         Contract := Config_Object_Outcomes.Schema;
         if not N.Accepts_Object_Result (Item, Machine, Contract) then return; end if;
         Config_Object_Outcomes.To_Host (Object.Output.Valid, Object.Output.Code, Object.Output.Revision, Reply);
         if not Reply.Success or else Reply.Value.Kind /= CCL.Host_Values.Object_Value then return; end if;
         Value := Reply.Value.Object;
      end if;
      Consume_Result (Object);
      N.Complete_Object (Item, Machine, Contract, Value, True);
      Result := Resumed;
   end Resume;

   procedure Submit
     (Object : in out Client; Action : Operation;
      Local_Types : CCL.Types.Registry; Request : CCL.VM.Execution_Result;
      Expected_Revision, Token : Number; Result : out Submission)
   is
   begin
      if Request.Request_Owned or else Request.Status /= CCL.VM.Waiting_For_Host or else
        Request.Requested_Binding = 0
      then
         Result := Invalid_Request;
         return;
      end if;
      case Action is
         when Read_Value =>
            if Request.Request_Argument /= CCL.VM.Integer_Constant (0) then
               Result := Invalid_Request;
            else
               Get (Object, Token, Result);
            end if;
         when Write_Value =>
            Set_Value (Object, Local_Types, Request.Request_Argument,
              Expected_Revision, Token, Result);
      end case;
   end Submit;

   procedure Take_Outcome
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Result : out Outcome)
   is
      Read : Read_Result;
      Converted : Boolean;
   begin
      Result := (others => <>);
      if Object.Current /= Result_Ready then return; end if;
      case Object.Action is
         when Config_Object_Messages.Get_Object =>
            Take_Get_Result (Object, Local_Types, Read);
            case Read.State is
               when Type_Mismatch => Result.State := Type_Mismatch;
               when Invalid_Completion => Result.State := Uncertain;
               when No_Value | Value_Ready =>
                  Result := (State => Service_Outcome, Action => Read_Value,
                    Code => Read.Code, Revision => Read.Revision,
                    Has_Value => Read.State = Value_Ready, Value => Read.Value);
               when No_Result => null;
               when Other_Operation => Result.State := Other_Operation;
            end case;
         when Config_Object_Messages.Set_Object =>
            Result.Action := Write_Value;
            Config_Object_Outcomes.To_VM (Local_Types, Object.Output.Valid,
              Object.Output.Code, Object.Output.Revision, Result.Value, Converted);
            if not Converted then Result.State := Type_Mismatch; return; end if;
            Result.State := (if Object.Output.Valid and Object.Output.Code /= Config_Object_Messages.Uncertain
                             then Service_Outcome else Uncertain);
            Result.Code := Object.Output.Code;
            Result.Revision := Object.Output.Revision;
            Result.Has_Value := True;
            Consume_Result (Object);
         when others => Result.State := Other_Operation;
      end case;
   end Take_Outcome;

   function Can_Resume (Result : Outcome; Policy : Freshness := Require_Current)
     return Boolean is
     (Result.Has_Value and then
      (if Result.Action = Write_Value then Result.State in Service_Outcome | Uncertain
       else Result.State = Service_Outcome and then
         (Result.Code = Config_Object_Messages.Success or else
          (Result.Code = Config_Object_Messages.Stale and Policy = Accept_Stale))));
end Config_Object_Client.VM.Calls;
