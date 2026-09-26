with CCL.Host_Values;
with Config_Read_Outcomes;

-- Shared nonblocking adapter for interpreter hosts. All persistable shapes
-- remain owned native objects; no SQL/CBOR or private collection handle escapes.
package Config_Object_Client.Host is
   procedure Set_Value
     (Object : in out Client; Value : CCL.Host_Values.Value;
      Expected_Revision, Token : Number; Result : out Submission);
   type Read_State is (No_Result, Other_Operation, Invalid_Completion, No_Value, Value_Ready);
   type Read_Result is record
      State : Read_State := No_Result;
      Code : Config_Object_Messages.Status := Config_Object_Messages.Unavailable;
      Revision : Number := 0;
      Value : CCL.Host_Values.Value;
   end record;
   procedure Take_Get_Result (Object : in out Client; Item : out Read_Result);
   -- Consumes only a completed Get. Service statuses/revisions are preserved;
   -- failed transport retains the parent client's poisoned-state behavior.
   -- Value_Ready always carries Object_Value, even for a primitive schema.

   type Outcome_State is (No_Outcome, Other_Operation, Type_Mismatch, Outcome_Ready);
   procedure Take_Read_Outcome
     (Object : in out Client; Description : Config_Read_Outcomes.Description;
      Item : out CCL.Host_Values.Call_Result; State : out Outcome_State);
   -- Returns an ordinary typed result, including read failures, without waits
   -- or retries. Wrong value binding leaves the original completion pending.
   -- InvalidCompletion can be handled as data; the client still enters Failed.
end Config_Object_Client.Host;
