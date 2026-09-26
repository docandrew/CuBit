with CCL.Objects;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Worker_Protocol;

--  Single-dispatcher join of collection authorization and typed publication.
--  No SQL, client serialization, syscalls, polling, or storage waits here.
--  The IPC shell authenticates subjects/completions; the worker does disk I/O.
package Config_Typed_Store with SPARK_Mode is
   use type CCL.Objects.Image;
   use type Config_Objects.Number;
   subtype Number is Config_Objects.Number;
   type State is limited private;
   type Published_Value is record
      Value : CCL.Objects.Image;
      Revision : Number;
   end record with Ghost;
   type Published_Values is array (Config_Collections.Registered_ID) of Published_Value with Ghost;
   function Published (Object : State) return Published_Values with Ghost;
   procedure Register
     (Object : in out State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Config_Collections.Collection_ID; Result : out Config_Collections.Result);
   procedure Check_Registration
     (Object : State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Config_Collections.Collection_ID; Result : out Config_Collections.Result);
   procedure Open
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Name : String; Context : Number;
      Requested : Config_Authority.Rights; Expected : CCL.Objects.Schema_Key;
      Handle : out Config_Collections.Handle; Result : out Config_Collections.Result);
   procedure Close
     (Object : in out State; Subject : Config_Authority.Subject_ID;
      Handle : Config_Collections.Handle; Result : out Config_Collections.Result)
     with Post => Published (Object) = Published (Object)'Old;
   procedure Revoke_Subject (Object : in out State; Subject : Config_Authority.Subject_ID);
   --  Admission before borrowing an object grant or reserving reply capacity.
   --  Get/Set still enforce authorization themselves; this is not an unchecked
   --  token that can be cached across policy changes.
   function Check_Access
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Config_Collections.Handle;
      Operation : Config_Authority.Operation) return Boolean;
   procedure Get
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Config_Collections.Handle;
      Value : out CCL.Objects.Image; Revision : out Number;
      Authorized : out Boolean; Result : out Config_Objects.Read_Result)
     with Post => (if not Authorized then Value = CCL.Objects.Image'(others => <>) and Revision = 0);
   --  Accepted means staged, NEVER durable success. Reply to the client only
   --  after Complete returns Published; other outcomes remain typed failures.
   --  Tokens come from the owning dispatcher's process-wide nonreusing source.
   procedure Set
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Config_Authority.Subject_ID; Handle : Config_Collections.Handle;
      Value : CCL.Objects.Image; Expected_Revision, Token : Number;
      Authorized : out Boolean; Result : out Config_Objects.Outcome)
     with Post => Published (Object) = Published (Object)'Old;

   --  Trusted worker attachment/routing, not client operations. Registration
   --  does not create a persisted value: restore precedes any initial Set.
   --  At most one storage operation is pending, while cached Gets continue.
   procedure Restore
     (Object : in out State; ID : Config_Collections.Registered_ID;
      Session, Token : Number; Result : out Config_Objects.Outcome);
   procedure Pending
     (Object : State; Request : out Config_Worker_Protocol.Frame;
      Contract : out CCL.Objects.Binding; Available : out Boolean);
   function Pending_Session (Object : State) return Number;
   function Pending_Token (Object : State) return Number;
   function Ready_In_Session
     (Object : State; ID : Config_Collections.Registered_ID; Session : Number) return Boolean;
   --  ONLY call from the authenticated worker-completion path. A token is
   --  correlation, not authority. Mismatched old completions are ignored.
   procedure Complete
     (Object : in out State; Response : Config_Worker_Protocol.Frame;
      Result : out Config_Objects.Outcome);
   procedure Worker_Lost (Object : in out State; Session : Number);
private
   type Cache_Array is array (Config_Collections.Registered_ID) of Config_Objects.State;
   type State is limited record
      Catalog : Config_Collections.State;
      Cache : Cache_Array;
      Pending_ID : Config_Collections.Collection_ID := Config_Collections.No_Collection;
      Request : Config_Worker_Protocol.Frame;
      Contract : CCL.Objects.Binding;
      Last_Token : Number := 0;
   end record;
end Config_Typed_Store;
