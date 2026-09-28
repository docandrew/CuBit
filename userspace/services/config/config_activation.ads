with Interfaces;
with CCL.Configurations;
with CCL.Declarations;
with Config_Authority;

--  Single-dispatcher, owned activation state. This is not an IPC authenticator
--  or a storage adapter. The shell supplies authenticated subjects, the selected
--  durable base revision, and a nonreusing consumer instance identity. Calls
--  must be serialized with grant installation/revocation and publication.
--
--  First slice: one setting in a machine-context system-config v1 fragment.
--  No imports, ambient inputs, removals, schema changes or policy activation.
package Config_Activation with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Number is Interfaces.Unsigned_64;
   subtype Subject_ID is Config_Authority.Subject_ID;
   type Phase is
     (Empty, Candidate, Reviewed, Committing, Commit_Rejected, Commit_Uncertain,
      Selected, Applied, Apply_Failed);
   type Result is
     (Accepted, Denied, Invalid_Source, Wrong_Target, Stale_Review,
      Wrong_Phase, Busy, Identity_Exhausted, Ignored);
   type Storage_Outcome is (Stored, Not_Stored, Indeterminate);
   type Application_Outcome is (Succeeded, Failed);
   type State is limited private;
   type Commit_Request is private;

   function Current_Phase (Object : State) return Phase;
   function Candidate_ID (Object : State) return Number;
   function Selected_Revision (Object : State) return Number;
   function Corresponds
     (Object : State; Target, Source : String; Base : Number) return Boolean with Ghost;

   --  A new proposal invalidates the previous review even if compilation fails.
   --  In-flight or uncertain work cannot be overwritten; reconciliation must
   --  happen before another activation. IDs never wrap within this lifetime.
   procedure Propose
     (Object : in out State; Target, Source : String; Base : Number;
      ID : out Number; Status : out Result)
     with Post =>
       (if Status = Accepted then
          ID /= 0 and then ID = Candidate_ID (Object) and then
          Current_Phase (Object) = Candidate and then
          Corresponds (Object, Target, Source, Base)
        else ID = 0);

   --  Review view is copied, not borrowed from mutable UI/source storage.
   --  Inspection rechecks read authority, including after revocation.
   type Source_Text is record
      Length : Natural range 0 .. CCL.Declarations.MAX_SOURCE := 0;
      Data : String (1 .. CCL.Declarations.MAX_SOURCE) := [others => ' '];
   end record;
   type Review_View is record
      ID : Number := 0;
      Base : Number := 0;
      Source : Source_Text;
      Setting : CCL.Configurations.Setting_Entry;
   end record;
   procedure Inspect
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; View : out Review_View; Allowed : out Boolean)
     with Post => (if not Allowed then View = Review_View'(others => <>));
   procedure Approve
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID : Number; Status : out Result);

   function Authorized_Review
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID, Base : Number) return Boolean with Ghost;
   function Bound_To (Request : Commit_Request; Object : State) return Boolean with Ghost;

   --  The returned private request pins exactly the reviewed source/value,
   --  scope, base and authority revision. The trusted storage adapter MUST
   --  compare Base again inside its transaction, require managed registration,
   --  and atomically store source correspondence with the selected value.
   --  This package deliberately does not add an ordinary Set bypass.
   procedure Begin_Commit
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID, Base, Consumer_Instance : Number;
      Request : out Commit_Request; Status : out Result)
     with Post =>
       (if Status = Accepted then
          Current_Phase (Object) = Committing and then
          Authorized_Review (Object, Authority, Subject, ID, Base) and then
          Bound_To (Request, Object)
        else not Valid (Request));

   --  Trusted adapter accessors, NOT client-authorized inspection endpoints.
   function Valid (Request : Commit_Request) return Boolean;
   function Snapshot (Request : Commit_Request) return Review_View;
   function Reviewer (Request : Commit_Request) return Subject_ID;
   function Grant_Revision (Request : Commit_Request) return Number;

   --  Reply IDs are correlation only, not capabilities. The shell must verify
   --  the worker/consumer sender and session before passing completions here.
   procedure Complete_Storage
     (Object : in out State; ID : Number; Outcome : Storage_Outcome;
      Revision : Number; Status : out Result)
     with Post => (if Status = Accepted then Current_Phase (Object) /= Applied);
   function Acknowledgement_Matches
     (Object : State; ID, Revision, Consumer_Instance : Number) return Boolean with Ghost;
   procedure Complete_Application
     (Object : in out State; ID, Revision, Consumer_Instance : Number;
      Outcome : Application_Outcome; Status : out Result)
     with Post =>
       ((Status = Accepted) =
          Acknowledgement_Matches (Object, ID, Revision, Consumer_Instance)'Old) and
       (Status = Accepted or Current_Phase (Object) = Current_Phase (Object)'Old) and
       (Status /= Accepted or
          Current_Phase (Object) = (if Outcome = Succeeded then Applied else Apply_Failed));
private
   type Commit_Request is record
      Present : Boolean := False;
      View : Review_View;
      Subject : Subject_ID := Config_Authority.No_Subject;
      Authority_Revision : Number := 0;
   end record;
   type State is limited record
      Mode : Phase := Empty;
      Last_ID : Number := 0;
      View : Review_View;
      Subject : Subject_ID := Config_Authority.No_Subject;
      Authority_Revision : Number := 0;
      Consumer : Number := 0;
      Revision : Number := 0;
   end record;
   function Current_Phase (Object : State) return Phase is (Object.Mode);
   function Candidate_ID (Object : State) return Number is (Object.View.ID);
   function Selected_Revision (Object : State) return Number is (Object.Revision);
   function Valid (Request : Commit_Request) return Boolean is (Request.Present);
   function Snapshot (Request : Commit_Request) return Review_View is (Request.View);
   function Reviewer (Request : Commit_Request) return Subject_ID is (Request.Subject);
   function Grant_Revision (Request : Commit_Request) return Number is (Request.Authority_Revision);
end Config_Activation;
