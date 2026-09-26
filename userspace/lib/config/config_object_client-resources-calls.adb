with CCL.Host_Values;
with Config_Object_Outcomes;

package body Config_Object_Client.Resources.Calls is
   package R renames CCL.Resources;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   package W renames Config_Object_Messages;
   use type Interfaces.Unsigned_32;
   use type R.Reference;
   use type R.Ticket;
   use type V.Execution_Status;
   use type V.Value;
   use type W.Operation;
   use type CCL.Host_Values.Value_Kind;

   function Owns_Call
     (Call : Invocation; Object : Collection; Owner : R.Registry) return Boolean is
     (Call.Active and then Call.Ticket = Object.Call and then
      R.Valid_Ticket (Owner, Call.Ticket) and then
      Object.Reference = Call.Receiver and then
      Object.Backend.Action =
        (if Call.Action = Read_Value then W.Get_Object else W.Set_Object));

   procedure Submit
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out R.Registry;
      Action : Operation; Binding : Interfaces.Unsigned_32;
      Item : V.Validated_Program; Machine : in out N.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Expected_Revision, Token : Number; Result : out Submission)
   is
      Request : constant V.Execution_Result := N.Pending_Call (Item, Machine);
      Value : CCL.Objects.Image;
      Good : Boolean;
   begin
      Result := Busy;
      if Call.Active then return; end if;
      Result := Invalid_Request;
      if Binding = 0 or else Request.Status /= V.Waiting_For_Host or else
        Request.Requested_Binding /= Binding or else not Request.Request_Owned or else
        Request.Request_Receiver = R.No_Reference or else
        Request.Request_Receiver /= Reference_Of (Object, Owner) or else
        N.Ready_For_Completion (Item, Machine)
      then return; end if;
      case Action is
         when Read_Value =>
            if Request.Request_Argument /= V.Integer_Constant (0) or else
              not Config_Read_Outcomes.Matches (Read_Description, Contract_Of (Object)) or else
              not N.Accepts_Object_Result (Item, Machine, Config_Read_Outcomes.Schema (Read_Description))
            then return; end if;
            Get (Object, Owner, Request.Request_Receiver, Token, Result);
         when Write_Value =>
            if not N.Accepts_Object_Result (Item, Machine, Config_Object_Outcomes.Schema) then return; end if;
            N.Export_Argument (Item, Machine, Contract_Of (Object), Value, Good);
            if not Good then return; end if;
            Set (Object, Owner, Request.Request_Receiver, Value, Expected_Revision, Token, Result);
      end case;
      if Result /= Submitted then return; end if;
      Call.Active := True;
      Call.Ticket := Object.Call;
      Call.Receiver := Request.Request_Receiver;
      Call.Binding := Binding;
      Call.Import_Index := Request.Requested_Import;
      Call.Action := Action;
      Call.Read_Description := Read_Description;
      N.Acknowledge_Host_Submission (Item, Machine, True);
   end Submit;

   procedure Resume
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out R.Registry; Item : V.Validated_Program;
      Machine : in out N.Machine; Result : out Resume_State)
   is
      Request : constant V.Execution_Result := N.Pending_Call (Item, Machine);
      Contract : CCL.Objects.Binding;
      Value : CCL.Objects.Image;
      Reply : CCL.Host_Values.Call_Result;
      Answer : Response;
      Good, Taken : Boolean;
   begin
      Result := No_Completion;
      if not Call.Active then return; end if;
      Result := Other_Call;
      if not Owns_Call (Call, Object, Owner) then return; end if;
      Result := No_Completion;
      if Object.Backend.Current /= Result_Ready then return; end if;
      Result := Other_Call;
      if Request.Status /= V.Waiting_For_Host or else not Request.Request_Owned or else
        Request.Requested_Binding /= Call.Binding or else Request.Requested_Import /= Call.Import_Index or else
        Request.Request_Receiver /= Call.Receiver or else not R.Current (Owner, Call.Receiver) or else
        not N.Ready_For_Completion (Item, Machine)
      then return; end if;
      Result := Type_Mismatch;
      if Call.Action = Read_Value then
         Contract := Config_Read_Outcomes.Schema (Call.Read_Description);
         if not N.Accepts_Object_Result (Item, Machine, Contract) then return; end if;
         Config_Read_Outcomes.Build (Call.Read_Description, Object.Backend.Output.Valid,
           Object.Backend.Output.Code, Object.Backend.Output.Revision,
           Object.Backend.Output.Value, Value, Good);
         if not Good then return; end if;
      else
         Contract := Config_Object_Outcomes.Schema;
         if not N.Accepts_Object_Result (Item, Machine, Contract) then return; end if;
         Config_Object_Outcomes.To_Host (Object.Backend.Output.Valid, Object.Backend.Output.Code,
           Object.Backend.Output.Revision, Reply);
         if not Reply.Success or else Reply.Value.Kind /= CCL.Host_Values.Object_Value then return; end if;
         Value := Reply.Value.Object;
      end if;
      Take_Result (Object, Owner, Answer, Taken);
      if not Taken then return; end if;
      Call.Active := False;
      N.Complete_Object (Item, Machine, Contract, Value, True);
      Result := Resumed;
   end Resume;

   procedure Drain
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out R.Registry; Item : out Response; Taken : out Boolean) is
   begin
      Item := (others => <>); Taken := False;
      if not Owns_Call (Call, Object, Owner) then return; end if;
      Take_Result (Object, Owner, Item, Taken);
      if Taken then Call.Active := False; end if;
   end Drain;
end Config_Object_Client.Resources.Calls;
