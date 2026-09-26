with CCL.Objects.Views;

-- Optional, out-of-line storage for native object programs. Scalar machines
-- do not acquire a large object pool. Keep this single-owner machine alive
-- across asynchronous calls; no borrowed IPC frame or host pointer is retained.
package CCL.VM.Native_Objects with SPARK_Mode is
   type Machine is limited private;
   procedure Initialize (Item : Validated_Program; Fuel : Natural; State : in out Machine)
     with Pre => Is_Valid (Item);
   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Machine;
      Instructions : Natural; Result : out Execution_Result)
     with Pre => Is_Valid (Item);
   function Snapshot (State : Machine) return Machine_Snapshot;
   procedure Inspect
     (Item : Validated_Program; State : Machine;
      Result : out Inspection_Snapshot)
     with Pre => Is_Valid (Item);
   -- Read-only debugger views. Object positions are opaque machine-local
   -- references, not host pointers or permission to export arbitrary values.
   -- An uninitialized/stopped store exposes no operand or local references.
   procedure Complete_Object
     (Item : Validated_Program; State : in out Machine;
      Contract : CCL.Objects.Binding; Response : CCL.Objects.Image; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   function Pending_Call (Item : Validated_Program; State : Machine)
     return Execution_Result with Pre => Is_Valid (Item);
   function Accepts_Object_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding)
      return Boolean with Pre => Is_Valid (Item);
   -- Non-mutating host preflight before submitting an effect or consuming a
   -- completion. Scalar native images are unboxed; aggregates retain snapshots.
   -- Neither function authenticates an IPC receipt or associates two lifetimes.
   -- Trusted host completion only: authenticate/correlate the IPC receipt to
   -- this pending call and run before invoking this API. Contract must be the
   -- approved binding pinned by that import, never metadata from its reply.
   -- The VM validates type/data; it does not authenticate raw IPC messages.
   procedure Complete_Scalar
     (Item : Validated_Program; State : in out Machine; Response : Value; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   procedure Complete_Resource
     (Item : Validated_Program; State : in out Machine;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   procedure Acknowledge_Host_Submission
     (Item : Validated_Program; State : in out Machine; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   function Ready_For_Completion (Item : Validated_Program; State : Machine)
     return Boolean with Pre => Is_Valid (Item);
   -- Owned imports must first acknowledge submission, establishing the borrow
   -- or move. Object completion before that point leaves the call untouched.
   procedure Export_Argument
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   procedure Export_Result
     (Item : Validated_Program; State : Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   -- Export only the currently waiting argument or completed result. A caller
   -- cannot present a guessed index or a reference from another machine.
   -- Contract is independently approved metadata, not an authority grant.
   procedure Stop (State : in out Machine);
   -- Releases local snapshots, not an in-flight service operation. The host
   -- still owns any grants/completion tokens and must drain/retire those safely.
private
   subtype Stored_Index is Object_Position range 1 .. MAX_OBJECT_VALUES;
   type Object_Array is array (Stored_Index) of CCL.Objects.Views.Snapshot;
   type Machine is limited record
      Core : Machine_State;
      Objects : Object_Array;
      Used : Object_Position := 0;
      Initialized : Boolean := False;
   end record;
end CCL.VM.Native_Objects;
