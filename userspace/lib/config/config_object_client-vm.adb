with CCL.Objects.Values;

package body Config_Object_Client.VM is
   use type Config_Object_Messages.Operation;
   use type Config_Object_Messages.Status;

   procedure Set_Value
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Value : CCL.VM.Value; Expected_Revision, Token : Number;
      Result : out Submission)
   is
      Owned : CCL.Objects.Image;
      Accepted : Boolean;
   begin
      Result := Admission (Object, Token);
      if Result /= Submitted then return; end if;
      CCL.Objects.Values.From_VM
        (Object.Contract, Local_Types, Value, Owned, Accepted);
      if not Accepted then Result := Invalid_Request; return; end if;
      Set (Object, Owned, Expected_Revision, Token, Result);
   end Set_Value;

   procedure Take_Get_Result
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Item : out Read_Result)
   is
      Accepted : Boolean;
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
            CCL.Objects.Values.To_VM
              (Object.Contract, Local_Types, Object.Output.Value, Item.Value, Accepted);
            if not Accepted then
               Item.State := Type_Mismatch; return;
            end if;
            Item.State := Value_Ready;
         else
            Item.State := No_Value;
         end if;
      end if;
      Consume_Result (Object);
   end Take_Get_Result;
end Config_Object_Client.VM;
