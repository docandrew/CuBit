with CCL.Types;
with CCL.VM;

--  Shared, nonblocking adapter for hosts of the CCL bytecode VM. The host owns
--  the Client, approved binding and program registry, not the script. This is
--  not a new source-language import or an alternative authority mechanism.
package Config_Object_Client.VM is
   procedure Set_Value
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Value : CCL.VM.Value; Expected_Revision, Token : Number;
      Result : out Submission);

   type Read_State is
     (No_Result, Other_Operation, Invalid_Completion, No_Value,
      Type_Mismatch, Value_Ready);
   type Read_Result is record
      State : Read_State := No_Result;
      Code : Config_Object_Messages.Status := Config_Object_Messages.Unavailable;
      Revision : Number := 0;
      Value : CCL.VM.Value := CCL.VM.Integer_Constant (0);
   end record;
   procedure Take_Get_Result
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Item : out Read_Result);
   --  Call after the normal completion dispatcher has called Complete. Only
   --  Get results are consumed. Type_Mismatch leaves the owned result pending
   --  for retry with the correct registry or extraction via parent Take_Result.
   --  Value is meaningful only for Value_Ready; Code preserves Success/Stale.
   --  Missing/Denied/etc. yield No_Value. Invalid_Completion consumes the failed
   --  transport result and preserves the parent's poisoned-client semantics.
   --  No polling, wait, schema installation, private handle exposure or CBOR.
end Config_Object_Client.VM;
