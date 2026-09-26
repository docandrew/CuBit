with Interfaces;
with CCL.Objects;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Object_Messages;
with CuBit.Async_Requests;

--  Nonblocking, single-owner native Config client. One collection per client;
--  separate clients may share an endpoint but require a common token allocator
--  and completion dispatcher. No global buffer or hidden polling loop.
package Config_Object_Client is
   subtype Number is Interfaces.Unsigned_64;
   type Phase is (Fresh, Ready, Waiting, Result_Ready, Failed, Retired);
   type Submission is (Submitted, Busy, Unavailable, Invalid_Request, Not_Submitted);
   type Completion_Result is (Ignored, Completed);
   type Client is limited private;
   type Response is record
      Valid : Boolean := False;
      Code : Config_Object_Messages.Status := Config_Object_Messages.Unavailable;
      Revision : Number := 0;
      Value : CCL.Objects.Image;
   end record;
   function Status (Object : Client) return Phase;
   procedure Initialize
     (Object : in out Client; Endpoint : CuBit.Messages.CapabilitySlot; Success : out Boolean);
   procedure Open
     (Object : in out Client; Name : String; Contract : CCL.Objects.Binding;
      Access_Rights : Config_Object_Messages.Access_Mode; Context, Token : Number;
      Result : out Submission);
   procedure Create
     (Object : in out Client; Name : String; Contract : CCL.Objects.Binding;
      Access_Rights : Config_Object_Messages.Access_Mode; Context, Token : Number;
      Result : out Submission);
   -- Durable create-or-open of the identical definition. Returns a usable
   -- handle only after type creation and initial value recovery finish.
   -- An Uncertain receipt can mean the definition exists without a handle.
   -- Retire and open afresh with the same approved binding to inspect it.
   procedure Get (Object : in out Client; Token : Number; Result : out Submission);
   procedure Set
     (Object : in out Client; Value : CCL.Objects.Image;
      Expected_Revision, Token : Number; Result : out Submission);
   procedure Close (Object : in out Client; Token : Number; Result : out Submission);
   --  ONLY authenticated kernel completion-queue entries. Copies and validates
   --  returned objects against the retained approved binding, never a schema
   --  supplied by the server. Invalid transport is uncertain, not a rejection.
   procedure Complete
     (Object : in out Client; Completion : CuBit.Messages.CompletionEntry; Result : out Completion_Result);
   procedure Take_Result (Object : in out Client; Item : out Response; Taken : out Boolean);
   -- A valid Uncertain Set/Create receipt preserves its service status but consumes
   -- into Failed, not Ready. Retire this client and recover via a fresh open;
   -- an uncertain write is never silently retried on the old handle.
   --  Keep this object at a stable address until retirement is CONFIRMED.
   --  Retiring a grant does not close the collection handle or cancel a write;
   --  normally Close first. Dead-client handle cleanup belongs to the service.
   procedure Retire (Object : in out Client; Confirmed : out Boolean);
private
   procedure Start
     (Object : in out Client; Action : Config_Object_Messages.Operation;
      Token, Revision : Number; Result : out Submission);
   -- Private submission primitive. Resource cleanup alone may close a known
   -- handle after an uncertain data operation, without reviving that operation.
   function Admission (Object : Client; Token : Number) return Submission;
   procedure Consume_Result (Object : in out Client);
   type Client is limited record
      Loan : Config_Object_Messages.Creation_Frame;
      Contract : CCL.Objects.Binding;
      Output : Response;
      Current : Phase := Fresh;
      Endpoint : CuBit.Messages.CapabilitySlot := 0;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Has_Grant, Revocation_Requested, Poisoned : Boolean := False;
      Handle, Expected_Revision : Number := 0;
      Request : CuBit.Async_Requests.Tracker;
      Action : Config_Object_Messages.Operation := Config_Object_Messages.Open_Collection;
   end record;
end Config_Object_Client;
