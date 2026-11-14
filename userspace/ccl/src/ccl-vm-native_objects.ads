with CCL.Objects;
with CCL.Objects.Views;
with CCL.Streams;

-- The host boundary for programs whose imports take or return native object
-- images. Keep this single-owner machine alive across asynchronous calls; no
-- borrowed IPC frame or host pointer is retained.
package CCL.VM.Native_Objects with SPARK_Mode is
   --  A machine of the rooms of its core (CCL.VM.Sized_Machine_State);
   --  Machine is the default room.
   type Sized_Machine
     (Node_Room : Node_Room_Count; Slot_Room : Slot_Room_Count;
      Text_Room : Text_Regions.Byte_Room; String_Room : Text_Regions.Value_Room_Count;
      Element_Room : List_Regions.Element_Room; List_Room : List_Regions.Value_Room_Count) is limited private;
   subtype Machine is Sized_Machine
     (Node_Room => DEFAULT_VALUE_NODES, Slot_Room => DEFAULT_VALUE_SLOTS,
      Text_Room => DEFAULT_TEXT_BYTES, String_Room => DEFAULT_TEXT_VALUES,
      Element_Room => DEFAULT_LIST_ELEMENTS, List_Room => DEFAULT_LIST_VALUES);
   function Rooms_Of (State : Sized_Machine) return Rooms is
     ((State.Node_Room, State.Slot_Room, State.Text_Room, State.String_Room, State.Element_Room,
       State.List_Room));
   --  Growable: the host grows the machine to hold each object result
   --  (Complete_Captured, Needed_Rooms, Grow), so an object import is not
   --  refused before its effect for want of the default room; a machine
   --  that does not grow admits only results its rooms can hold, checked
   --  before the effect from the result type's worst case.
   procedure Initialize
     (Item : Validated_Program; Fuel : Natural; State : in out Sized_Machine; Growable : Boolean := False)
     with Pre => Is_Valid (Item);
   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Sized_Machine;
      Instructions : Natural; Result : out Execution_Result)
     with Pre => Is_Valid (Item);
   function Snapshot (State : Sized_Machine) return Machine_Snapshot;
   procedure Inspect
     (Item : Validated_Program; State : Sized_Machine;
      Result : out Inspection_Snapshot)
     with Pre => Is_Valid (Item);
   -- Read-only debugger views. Object positions are opaque machine-local
   -- references, not host pointers or permission to export arbitrary values.
   -- An uninitialized/stopped store exposes no operand or local references.
   procedure Complete_Object
     (Item : Validated_Program; State : in out Sized_Machine;
      Contract : CCL.Objects.Binding; Response : CCL.Objects.Image; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   --  The same for a value already captured (validated against Contract
   --  and indexed) in View, of any size. Fits False, and nothing done,
   --  when the machine's rooms cannot hold it: the host grows the machine
   --  to Needed_Rooms and completes the call on the grown one.
   procedure Complete_Captured
     (Item : Validated_Program; State : in out Sized_Machine;
      Contract : CCL.Objects.Binding; View : CCL.Objects.Views.Sized_Snapshot; Accepted : Boolean;
      Fits : out Boolean)
     with Pre => Is_Valid (Item);
   --  Rooms that hold what State holds and View's value, with room to
   --  spare (doubled), within the ceilings; Possible False when the
   --  ceilings cannot hold it (CAPACITY).
   procedure Needed_Rooms
     (Item : Validated_Program; State : Sized_Machine; View : CCL.Objects.Views.Sized_Snapshot;
      Wanted : out Rooms; Possible : out Boolean)
     with Pre => Is_Valid (Item);
   --  The waiting call's result needs more than the ceilings hold: the run
   --  stops with Object_Storage_Exhausted (CAPACITY).
   procedure Stop_Exhausted (State : in out Sized_Machine);
   --  Into becomes From in its larger rooms (From is left as it was).
   function Can_Hold (From, Into : Sized_Machine) return Boolean;
   procedure Grow (From : Sized_Machine; Into : in out Sized_Machine)
     with Pre => Can_Hold (From, Into);
   --  A completed result's literal (Execution_Result's Has_Literal), into
   --  Output of its Literal_Length.
   procedure Result_Literal
     (Item : Validated_Program; State : Sized_Machine; Output : out String; Printed : out Boolean)
     with Pre => Is_Valid (Item) and then Output'First = 1;
   function Pending_Call (Item : Validated_Program; State : Sized_Machine)
     return Execution_Result with Pre => Is_Valid (Item);
   function Accepts_Object_Result
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding)
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
     (Item : Validated_Program; State : in out Sized_Machine; Reply : CCL.Streams.View_Reply)
     with Pre => Is_Valid (Item);
   --  Answer a call whose import returns text: Response, within the
   --  import's Result_Text_Limit, copied into the run's text region. A
   --  longer reply is refused like a failed call; a full region stops the
   --  run with Text_Storage_Exhausted.
   procedure Complete_Text
     (Item : Validated_Program; State : in out Sized_Machine; Response : String; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   procedure Complete_Scalar
     (Item : Validated_Program; State : in out Sized_Machine; Response : Value; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   procedure Complete_Resource
     (Item : Validated_Program; State : in out Sized_Machine;
      Owner : CCL.Resources.Registry; Resource : CCL.Resources.Reference;
      Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   procedure Acknowledge_Host_Submission
     (Item : Validated_Program; State : in out Sized_Machine; Accepted : Boolean)
     with Pre => Is_Valid (Item);
   function Ready_For_Completion (Item : Validated_Program; State : Sized_Machine)
     return Boolean with Pre => Is_Valid (Item);
   -- Owned imports must first acknowledge submission, establishing the borrow
   -- or move. Object completion before that point leaves the call untouched.
   procedure Export_Argument
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   procedure Export_Result
     (Item : Validated_Program; State : Sized_Machine; Contract : CCL.Objects.Binding;
      Value : out CCL.Objects.Image; Accepted : out Boolean)
     with Pre => Is_Valid (Item);
   -- Export only the currently waiting argument or completed result. A caller
   -- cannot present a guessed index or a reference from another machine.
   -- Contract is independently approved metadata, not an authority grant.
   procedure Stop (State : in out Sized_Machine);
   -- Releases the machine's values, not an in-flight service operation. The host
   -- still owns any grants/completion tokens and must drain/retire those safely.
private
   --  Records and payload variants live in the core machine's value arena:
   --  a completed image is copied in, an exported value copied out.
   type Sized_Machine
     (Node_Room : Node_Room_Count; Slot_Room : Slot_Room_Count;
      Text_Room : Text_Regions.Byte_Room; String_Room : Text_Regions.Value_Room_Count;
      Element_Room : List_Regions.Element_Room; List_Room : List_Regions.Value_Room_Count) is limited
   record
      Core : Sized_Machine_State (Node_Room, Slot_Room, Text_Room, String_Room, Element_Room, List_Room);
      Initialized : Boolean := False;
      Growable : Boolean := False;
   end record;
end CCL.VM.Native_Objects;
