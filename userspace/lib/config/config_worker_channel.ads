with Interfaces;
with CCL.Objects;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Worker_Protocol;
with CCL.Objects.Schemas;
with Config_Schema_Protocol;

--  One nonblocking, single-dispatcher channel to an authorized worker.
--  Keep this limited object alive at a stable address until Retire confirms
--  grant retirement. No request may overlap another or an unconsumed result.
package Config_Worker_Channel is
   type Phase is (Fresh, Ready, Waiting, Result_Ready, Failed, Retired);
   type Submission is (Submitted, Busy, Invalid_Request, Unavailable, Not_Submitted);
   type Completion_Result is (Ignored, Completed);
   type Channel is limited private;
   Loan_Bytes_Count : constant := Config_Schema_Protocol.Frame_Bytes;
   function Status (Object : Channel) return Phase;
   function Pending_Token (Object : Channel) return Interfaces.Unsigned_64;
   procedure Initialize
     (Object : in out Channel; Endpoint : CuBit.Messages.CapabilitySlot;
      Success : out Boolean);

   --  Caller already authorized namespace/context and supplied a trusted type
   --  binding. Tokens are process-wide nonreusing identities, including across
   --  replacement channels. Queue rejection still burns the token.
   procedure Submit
     (Object : in out Channel; Request : Config_Worker_Protocol.Frame;
      Contract : CCL.Objects.Binding; Result : out Submission);
   procedure Provision
     (Object : in out Channel; Contract : CCL.Objects.Binding;
      Token : Interfaces.Unsigned_64; Result : out Submission);
   procedure Submit_Type
     (Object : in out Channel; Request : Config_Schema_Protocol.Frame; Result : out Submission);
   procedure Take_Type_Result
     (Object : in out Channel; Response : out Config_Schema_Protocol.Frame; Valid, Taken : out Boolean);
   --  Same single outstanding operation and token domain as Submit. Callers
   --  wait for a valid provisioning acknowledgment before issuing a data job.

   --  ONLY kernel completion-queue entries, never ordinary incoming messages.
   --  Reply authority binds the actual peer/request; token is correlation only.
   --  Snapshots returned bytes BEFORE validating against the retained request.
   procedure Complete
     (Object : in out Channel; Completion : CuBit.Messages.CompletionEntry;
      Result : out Completion_Result);
   --  Valid=False means uncertain transport/invalid output: recover, don't
   --  retry a commit blindly. A valid Uncertain/Load_Failed frame also retires
   --  this channel from further submissions once the result is consumed.
   procedure Take_Result
     (Object : in out Channel; Response : out Config_Worker_Protocol.Frame;
      Valid, Taken : out Boolean);
   procedure Take_Provision_Result (Object : in out Channel; Valid, Taken : out Boolean);
   --  Terminal, including in-flight work. No cancellation/rollback guarantee.
   --  A successful revoke alone is NOT proof that the buffer can be reclaimed.
   procedure Retire (Object : in out Channel; Confirmed : out Boolean);
private
   type Operation_Kind is (Data_Exchange, Schema_Provision, Type_Exchange);
   type Loan_Bytes is array (1 .. Loan_Bytes_Count) of Interfaces.Unsigned_8
     with Component_Size => 8, Alignment => 4096;
   type Channel is limited record
      Loan : Loan_Bytes := [others => 0];
      Original, Response : Config_Worker_Protocol.Frame;
      Type_Original, Type_Response : Config_Schema_Protocol.Frame;
      Contract : CCL.Objects.Binding;
      Current : Phase := Fresh;
      Endpoint : CuBit.Messages.CapabilitySlot := 0;
      Grant : CuBit.Memory_Grants.Grant_Reference := (slot => 0, generation => 1);
      Has_Grant, Revocation_Requested : Boolean := False;
      Valid_Response, Poisoned : Boolean := False;
      Last_Token, Active_Token : Interfaces.Unsigned_64 := 0;
      Active_Operation : Operation_Kind := Data_Exchange;
   end record;
end Config_Worker_Channel;
