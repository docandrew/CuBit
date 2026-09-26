with Interfaces;
with CCL.Objects;
with Config_Authority;

--  Config's single-dispatcher collection catalog and subject-bound handles.
--  Registration is a trusted control-plane operation, NOT a client upsert or
--  a durable commit. Reuse the installed Config scope grants on EVERY access.
package Config_Collections with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Number is Interfaces.Unsigned_64;
   subtype Subject_ID is Config_Authority.Subject_ID;
   Maximum_Collections : constant := 16;
   Maximum_Handles : constant := 64;
   Maximum_Name : constant := 128;
   subtype Name_Length is Natural range 0 .. Maximum_Name;
   subtype Collection_Name is String (1 .. Maximum_Name);
   type Collection_ID is range 0 .. Maximum_Collections;
   No_Collection : constant Collection_ID := 0;
   subtype Registered_ID is Collection_ID range 1 .. Maximum_Collections;
   subtype Handle is Number;
   No_Handle : constant Handle := 0;
   Machine_Context : constant Number := 0;
   type Result is
     (Registered, Already_Registered, Opened, Closed, Resolved, Denied,
      Missing, Invalid_Definition, Schema_Conflict, Unsupported_Context,
      Capacity_Exceeded, Identity_Exhausted);
   type State is limited private;

   --  Audit model: successful resolution requires this exact live grant-set
   --  revision, subject, collection and operation. No generated runtime code.
   function Authorized_For
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Token : Handle; Operation : Config_Authority.Operation;
      ID : Collection_ID) return Boolean with Ghost;
   function Latest_Issued (Object : State) return Number with Ghost;

   --  The launch/schema-registration shell supplies an APPROVED binding.
   --  A client's claimed schema digest is not sufficient to call Register.
   --  Registration never installs Config scope grants and cannot replace an
   --  existing collection's type. Persistent creation is a separate operation.
   procedure Register
     (Object : in out State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Collection_ID; Status : out Result)
     with Post => (if Status in Registered | Already_Registered then ID /= No_Collection
                   else ID = No_Collection);
   procedure Check_Registration
     (Object : State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Collection_ID; Status : out Result)
     with Post => (if Status in Registered | Already_Registered then
                     ID /= No_Collection and Name'Length in 1 .. Maximum_Name
                   else ID = No_Collection);
   -- Read-only admission. Registered identifies the available slot but does
   -- not reserve/publish it; the single dispatcher serializes creation.
   procedure Open
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Name : String; Context : Number;
      Requested : Config_Authority.Rights; Expected : CCL.Objects.Schema_Key;
      Token : out Handle; Status : out Result)
     with Post => ((Token > Latest_Issued (Object)'Old) = (Status = Opened)) and then
       (if Status = Opened then Token = Latest_Issued (Object) else Token = No_Handle);
   --  No namespace/context is accepted after opening. Handles are nondelegable
   --  service tokens, bound to the authenticated IPC subject and minted rights.
   --  Checking current scope grants AND their installation revision prevents a
   --  cached handle bypassing revoke or resurrecting after a later regrant.
   procedure Resolve
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Token : Handle; Operation : Config_Authority.Operation;
      ID : out Collection_ID; Status : out Result)
     with Post => (if Status = Resolved then
       ID /= No_Collection and then Authorized_For (Object, Authority, Subject, Token, Operation, ID)
       else ID = No_Collection);
   procedure Close
     (Object : in out State; Subject : Subject_ID; Token : Handle; Status : out Result);
   procedure Revoke_Subject (Object : in out State; Subject : Subject_ID);

   --  Dispatcher/worker routing only, after Resolve. Never expose table IDs
   --  as authorization to clients. Definitions remain stable for this lifetime.
   procedure Describe
     (Object : State; ID : Registered_ID; Name : out Collection_Name;
      Length : out Name_Length; Contract : out CCL.Objects.Binding; Found : out Boolean);
   function Schema (Object : State; ID : Registered_ID) return CCL.Objects.Schema_Key;
private
   type Definition is record
      Name : String (1 .. Maximum_Name) := [others => Character'Val (0)];
      Length : Name_Length := 0;
      Contract : CCL.Objects.Binding;
   end record;
   type Definition_Array is array (Registered_ID) of Definition;
   type Held_Handle is record
      Token : Handle := No_Handle;
      Owner : Subject_ID := Config_Authority.No_Subject;
      Grant_Revision : Number := 0;
      ID : Registered_ID := Registered_ID'First;
      Allowed : Config_Authority.Rights := [others => False];
   end record;
   type Handle_Array is array (Positive range 1 .. Maximum_Handles) of Held_Handle;
   type State is limited record
      Definitions : Definition_Array;
      Handles : Handle_Array;
      Last_Token : Number := 0;
   end record;
   function Latest_Issued (Object : State) return Number is (Object.Last_Token);
end Config_Collections;
