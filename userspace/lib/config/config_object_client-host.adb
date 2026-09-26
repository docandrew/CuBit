with CCL.Objects.Values;

package body Config_Object_Client.Host is
   use type Config_Object_Messages.Operation;
   use type Config_Object_Messages.Status;
   procedure Set_Value
     (Object : in out Client; Value : CCL.Host_Values.Value;
      Expected_Revision, Token : Number; Result : out Submission)
   is
      Owned : CCL.Objects.Image;
      Accepted : Boolean;
   begin
      Result := Admission (Object, Token);
      if Result /= Submitted then return; end if;
      CCL.Objects.Values.From_Host (Object.Contract, Value, Owned, Accepted);
      if not Accepted then Result := Invalid_Request; return; end if;
      Set (Object, Owned, Expected_Revision, Token, Result);
   end Set_Value;

   procedure Take_Get_Result (Object : in out Client; Item : out Read_Result) is
   begin
      Item := (others => <>);
      if Object.Current /= Result_Ready then return; end if;
      if Object.Action /= Config_Object_Messages.Get_Object then
         Item.State := Other_Operation; return;
      end if;
      if not Object.Output.Valid then
         Item.State := Invalid_Completion;
      else
         Item.Code := Object.Output.Code;
         Item.Revision := Object.Output.Revision;
         if Item.Code in Config_Object_Messages.Success | Config_Object_Messages.Stale then
            Item.Value := CCL.Host_Values.Object_Constant (Object.Output.Value);
            Item.State := Value_Ready;
         else Item.State := No_Value;
         end if;
      end if;
      Consume_Result (Object);
   end Take_Get_Result;

   procedure Take_Read_Outcome
     (Object : in out Client; Description : Config_Read_Outcomes.Description;
      Item : out CCL.Host_Values.Call_Result; State : out Outcome_State)
   is
      Value : CCL.Objects.Image;
      Accepted : Boolean;
   begin
      Item := (others => <>); State := No_Outcome;
      if Object.Current /= Result_Ready then return; end if;
      if Object.Action /= Config_Object_Messages.Get_Object then
         State := Other_Operation; return;
      end if;
      if not Config_Read_Outcomes.Matches (Description, Object.Contract) then
         State := Type_Mismatch; return;
      end if;
      Config_Read_Outcomes.Build
        (Description, Object.Output.Valid, Object.Output.Code, Object.Output.Revision,
         Object.Output.Value, Value, Accepted);
      if not Accepted then State := Type_Mismatch; return; end if;
      Item := (Value => CCL.Host_Values.Object_Constant (Value), Success => True);
      State := Outcome_Ready;
      Consume_Result (Object);
   end Take_Read_Outcome;
end Config_Object_Client.Host;
