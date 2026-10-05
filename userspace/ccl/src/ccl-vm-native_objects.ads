with CCL.Objects;
with CCL.Streams;

-- The host boundary for programs whose imports take or return native object
-- images. Keep this single-owner machine alive across asynchronous calls; no
-- borrowed IPC frame or host pointer is retained.
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
   -- completion. Scalar native images are unboxed; aggregates are copied into
   -- the value arena.
   -- Neither function authenticates an IPC receipt or associates two lifetimes.
   -- Trusted host completion only: authenticate/correlate the IPC receipt to
   -- this pending call and run before invoking this API. Contract must be the
   -- approved binding pinned by that import, never metadata from its reply.
   -- The VM validates type/data; it does not authenticate raw IPC messages.
   --  Answer a run suspended on a stream view (Execution_Result's
   --  Stream_Requested). Elements are admitted only as the program's own
   --  T or List<T>; a count is an Integer. A refusal stops the run with
   --  the matching Stream_ status.
   procedure Complete_Stream_View
     (Item : Validated_Program; State : in out Machine; Reply : CCL.Streams.View_Reply)
     with Pre => Is_Valid (Item);
   --  Answer a call whose import returns text: Response, within the
   --  import's Result_Text_Limit, copied into the run's text region. A
   --  longer reply is refused like a failed call; a full region stops the
   --  run with Text_Storage_Exhausted.
   procedure Complete_Text
     (Item : Validated_Program; State : in out Machine; Response : String; Accepted : Boolean)
     with Pre => Is_Valid (Item);
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
   -- Releases the machine's values, not an in-flight service operation. The host
   -- still owns any grants/completion tokens and must drain/retire those safely.
private
   --  Records and payload variants live in the core machine's value arena:
   --  a completed image is copied in, an exported value copied out.
   type Machine is limited record
      Core : Machine_State;
      Initialized : Boolean := False;
   end record;
end CCL.VM.Native_Objects;
