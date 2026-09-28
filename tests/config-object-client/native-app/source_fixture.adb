with CuBit.Messages; use CuBit.Messages;
with CCL.Catalog; use CCL.Catalog;
with CCL.Objects.Catalog;
with CCL.Host_Values;
with CCL.Language;
with Config_Object_Client.Host;
with Config_Object_Messages;
with Config_Object_Outcomes;

package body Source_Fixture is
   procedure Configure
     (Catalog : out CCL.Catalog.Interface_Catalog; Grants : out CCL.Catalog.Granted_Bindings;
      Contract : CCL.Objects.Binding; Types : CCL.Types.Registry;
      Write : Boolean; Good : out Boolean)
   is
      use type CCL.Types.Import_Result;
      use type CCL.Objects.Catalog.Publication_Result;
      Descriptor : Interface_Descriptor;
      Op : Operation_Descriptor;
      Error : Catalog_Error;
      Resolved : Resolved_Operation;
      Installed : Grant_Result;
      Publication : CCL.Objects.Catalog.Publication_Result;
      Ref : CCL.Types.Type_Reference;
      Imported : CCL.Types.Import_Result;
      Found : Boolean;
   begin
      Good := False;
      Initialize (Catalog); Initialize (Grants);
      for T in CCL.Types.Declared_Type'First .. CCL.Types.Last (Types) loop
         Publish_Type (Catalog, Types, T, Ref, Imported);
         if Imported /= CCL.Types.Imported then return; end if;
      end loop;
      Publish_Schema (Catalog, Contract, Publication);
      if Publication /= CCL.Objects.Catalog.Published then return; end if;
      Config_Object_Outcomes.Publish (Catalog, Good); if not Good then return; end if;
      Good := False;
      Define_Interface ("config-test", 1, 0, [91, 92, 93, 94], Descriptor, Error);
      if Error /= Catalog_Valid then return; end if;
      Define_Host_Operation ("get", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => CCL.Objects.Identity (Contract), others => <>),
        Op, Error);
      if Error /= Catalog_Valid then return; end if;
      Add_Operation (Descriptor, Op, Error); if Error /= Catalog_Valid then return; end if;
      Define_Host_Operation ("set", 1,
        (Argument => CCL.Host_Values.Object_Value, Argument_Schema => CCL.Objects.Identity (Contract),
         Result => CCL.Host_Values.Object_Value, Result_Schema => Config_Object_Outcomes.Key, others => <>), Op, Error);
      if Error /= Catalog_Valid then return; end if;
      Add_Operation (Descriptor, Op, Error); if Error /= Catalog_Valid then return; end if;
      Publish (Catalog, Descriptor, Error); if Error /= Catalog_Valid then return; end if;
      Resolve (Catalog, "config-test.get", Resolved, Found); if not Found then return; end if;
      Install (Grants, Resolved, Operation'Enum_Rep (Get_Value), Installed);
      if Installed /= Grant_Added then return; end if;
      if Write then
         Resolve (Catalog, "config-test.set", Resolved, Found); if not Found then return; end if;
         Install (Grants, Resolved, Operation'Enum_Rep (Set_Value), Installed);
         if Installed /= Grant_Added then return; end if;
      end if;
      Good := True;
   end Configure;
   procedure Run
     (Client : in out Config_Object_Client.Client;
      Contract : CCL.Objects.Binding; Types : CCL.Types.Registry;
      Source : String; Expected : CCL.VM.Value;
      Revision : Interfaces.Unsigned_64; Token : in out Interfaces.Unsigned_64;
      Write : Boolean; Good : out Boolean)
   is
      package C renames Config_Object_Client;
      package H renames Config_Object_Client.Host;
      package W renames Config_Object_Messages;
      use type Interfaces.Unsigned_64;
      use type Interfaces.Unsigned_32;
      use type C.Submission;
      use type C.Completion_Result;
      use type H.Read_State;
      use type W.Status;
      use type CCL.Language.Interpretation_Status;
      use type Interfaces.Integer_64;
      Catalog : Interface_Catalog;
      Grants : Granted_Bindings;
      type Context_Type is record Calls : Natural := 0; end record;
      Context : Context_Type;
      Result : CCL.Language.Interpretation_Result;
      function Next_Token return Interfaces.Unsigned_64 is
      begin
         Token := Token + 1; return Token;
      end Next_Token;
      -- Blocking only in this dedicated test process. The production Host
      -- adapter never waits; a GUI host must suspend and dispatch completions.
      function Complete (Sent : C.Submission) return Boolean is
         Entry_Data : aliased CompletionEntry := NULL_COMPLETION;
         Done : C.Completion_Result;
         Activity : Activity_Result;
      begin
         if Sent /= C.Submitted then return False; end if;
         loop
            if Poll_Completion (Entry_Data'Address) = 1 then
               C.Complete (Client, Entry_Data, Done);
               return Done = C.Completed;
            end if;
            Activity := Wait_For_Activity_Until (Interfaces.Unsigned_64'Last);
            if Activity = Unavailable then return False; end if;
         end loop;
      end Complete;
      procedure Invoke
        (State : in out Context_Type; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
      is
         Sent : C.Submission;
         Read : H.Read_Result;
         Saved : C.Response;
         Taken : Boolean;
      begin
         State.Calls := State.Calls + 1;
         Reply := (Value => CCL.Host_Values.Boolean_Constant (False), Success => False);
         if Binding = Operation'Enum_Rep (Get_Value) then
            C.Get (Client, Next_Token, Sent);
            if not Complete (Sent) then return; end if;
            H.Take_Get_Result (Client, Read);
            Reply.Success := Read.State = H.Value_Ready and Read.Code = W.Success and Read.Revision = Revision;
            if Reply.Success then Reply.Value := Read.Value; end if;
         elsif Binding = Operation'Enum_Rep (Set_Value) and Write then
            -- This fixture writes the first revision; no speculative success.
            H.Set_Value (Client, Argument, 0, Next_Token, Sent);
            if not Complete (Sent) then return; end if;
            C.Take_Result (Client, Saved, Taken);
            Config_Object_Outcomes.To_Host (Taken and Saved.Valid, Saved.Code, Saved.Revision, Reply);
         end if;
      end Invoke;
      procedure Evaluate is new CCL.Language.Interpret_With_Values (Context_Type, Invoke);
   begin
      Configure (Catalog, Grants, Contract, Types, Write, Good);
      if not Good then return; end if;
      Evaluate (Source, 4096, Catalog, Grants, Context, Result);
      Good := Result.Status = CCL.Language.Succeeded and Result.Has_Value and
        Context.Calls = (if Write then 2 else 1) and
        CCL.Types.Same (Result.Variant_Type_Name, CCL.Types.Describe (Types, Expected.Data_Type).Identifier) and
        CCL.Types.Same (Result.Variant_Member_Name,
          CCL.Types.Describe (Types, Expected.Data_Type).Parts (Expected.Alternative).Identifier) and
        Result.Result_Value.Integer = Expected.Integer;
   end Run;
end Source_Fixture;
