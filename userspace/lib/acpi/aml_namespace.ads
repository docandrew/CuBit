pragma Ada_2022;
with AML_Delays;
with AML_Pending_Members;
with AML_Retained_Roots;
with AML_Retained_Identities;
with AML_Objects.Byte_References;
with AML_Identity;
with AML_Frame_Handles;
with AML_Frame_Roots;
with AML_References;
with AML_Objects.Package_References;
with AML_Objects.Reachability;
with AML_Objects.Root_Snapshots;
with AML_Object_Identifiers;
with AML_Clock;
with AML_Names;
with AML_Decode;
with AML_Data;
with AML_Execute;
with AML_Table_Backing;
with AML_Objects;
with AML_Field_Data;
--  Stable namespace IDs; temporary method declarations are reclaimed at last exit. IDs are scoped to one State lifetime;
--  they are not capabilities or externally reusable service handles.
generic
   Capacity : Positive;
   with procedure Perform_Delay
     (Item : AML_Delays.Request; Result : out AML_Delays.Outcome);
   with procedure Read_Microseconds
     (Value : out AML_Decode.Integer_Value; Available : out Boolean) is AML_Clock.No_Sample;
   Max_Pending_Members : Positive := AML_Objects.Max_Elements;
   Max_Pending_Segments : Positive := AML_Objects.Max_Elements;
   Max_Node_Incarnation : AML_References.Incarnation_Budget := AML_References.Node_Incarnation'Last;
   Max_Invocations : AML_Frame_Handles.Invocation_Serial := AML_Frame_Handles.Invocation_Serial'Last;
   Max_Retained_Roots : Positive := Capacity;
   Max_Retained_Incarnation : AML_Retained_Identities.Incarnation_Budget := AML_Retained_Identities.Incarnation'Last;
   Max_Frame_Roots : Positive := AML_Frame_Handles.Max_Active_Frames;
   Aggregate_Method_Capacity : Positive := AML_Execute.Max_Method_Bytes;
package AML_Namespace with SPARK_Mode is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Origin;
   use type AML_References.Node_Incarnation;
   use type AML_References.Reference;
   use type AML_Frame_Handles.Invocation_Serial;
   use type AML_Frame_Handles.Invocation_Domain;
   use type AML_Execute.Invocation_Status;
   use type AML_Names.Parse_Status;
   use type AML_Objects.Object_Kind;
   use type AML_Objects.Usage;
   subtype Node_ID is Natural range 0 .. Capacity;
   -- Aggregate offsets/counts are independent of individual method lengths.
   subtype Aggregate_Method_Count is Natural range 0 .. Aggregate_Method_Capacity;
   Root : constant Node_ID := 0;
   type State is private;
   -- High-water slot count. During execution, removed temporary slots below a
   -- live slot remain reserved; Present/Child distinguish these from live nodes.
   function Count (Tree : State) return Node_ID;
   function Last_Incarnation (Tree : State) return AML_References.Node_Incarnation;
   -- Exact state frame except the nonrewinding incarnation issuer. No namespace
   -- record, method byte, object, active-call count or pending member is ignored.
   function Cleanup_Frame (Tree, Prior : State) return Boolean;
   function Allocating_Cleanup_Frame (Tree, Prior : State) return Boolean with Ghost;
   function Incarnation_Of (Tree : State; Node : Node_ID) return AML_References.Node_Incarnation
     with Pre => Node > Root and then Node <= Count (Tree);
   function Present (Tree : State; Node : Node_ID) return Boolean
     with Pre => Node <= Count (Tree);
   function Value_Usage (Tree : State) return AML_Objects.Usage;
   function Method_Usage (Tree : State) return Aggregate_Method_Count;
   function Parent (Tree : State; Node : Node_ID) return Node_ID
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Parent'Result = Root
                   else Parent'Result < Node);
   function Name (Tree : State; Node : Node_ID) return AML_Names.Segment
     with Pre => Node > Root and then Node <= Count (Tree);
   function Empty return State with Post => Count (Empty'Result) = 0
     and then Method_Usage (Empty'Result) = 0
     and then Value_Usage (Empty'Result) = AML_Objects.Usage'(others => 0);

   function Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Node_ID
     with Pre => Scope <= Count (Tree),
          Post => Child'Result <= Count (Tree) and then
            (if Child'Result /= Root then
               Present (Tree, Child'Result) and then
               Parent (Tree, Child'Result) = Scope and then
               Name (Tree, Child'Result) = Part
             else (for all I in 1 .. Count (Tree) =>
                     not Present (Tree, I) or else Parent (Tree, I) /= Scope or else Name (Tree, I) /= Part));

   type Lookup_Status is (Found, Not_Found, Above_Root, Invalid_Path);
   type Lookup_Result (Status : Lookup_Status := Not_Found) is record
      case Status is
         when Found => Node : Node_ID;
         when others => null;
      end case;
   end record;
   --  Existing-object lookup, not declaration placement. Only an unprefixed
   --  single segment uses ancestor search. Null paths identify the base scope;
   --  opcode-specific NullName/target semantics remain the caller's job.
   function Resolve
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result)
      return Lookup_Result
     with Pre => Scope <= Count (Tree),
          Post => (if Resolve'Result.Status = Found then
                     Resolve'Result.Node <= Count (Tree) and then
                     Path.Kind = AML_Names.Accepted and then
                     (if Path.Count > 0 then
                        Resolve'Result.Node /= Root and then
                        Name (Tree, Resolve'Result.Node) = Path.Parts (Path.Count)));

   -- Insertion preserves existing records and both payload stores exactly.
   function Insert_Frame (Tree, Prior : State) return Boolean with Ghost;
   type Insert_Status is (Inserted, Duplicate, Full, Invalid_Name);
   procedure Insert
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Node : out Node_ID; Result : out Insert_Status)
     with Pre => Scope <= Count (Tree),
          Post => Insert_Frame (Tree, Tree'Old)
            and then Method_Usage (Tree) = Method_Usage (Tree'Old)
            and then (if Result = Inserted then
                Count (Tree) = Count (Tree'Old) + 1 and then
                Node = Count (Tree) and then Present (Tree, Node) and then
                Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
                and then (for all I in 1 .. Count (Tree'Old) =>
                  Parent (Tree, I) = Parent (Tree'Old, I) and then
                  Name (Tree, I) = Name (Tree'Old, I))
             else Tree = Tree'Old and then Node = Root);
   type Object_Kind is (Scope_Object, Device_Object, Integer_Object, String_Object, Buffer_Object, Package_Object, Method_Object,
                       Table_Region_Object, Table_Field_Object, Uninitialized_Region_Object, Reference_Object, Uninitialized_Name_Object, Event_Object, Mutex_Object, Power_Resource_Object, Processor_Object, Thermal_Zone_Object, Operation_Region_Object, Region_Field_Object);
   function Kind (Tree : State; Node : Node_ID) return Object_Kind
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Kind'Result = Scope_Object);
   type Sync_Level is range 0 .. 15;
   -- Preserve noncanonical reserved bits for compatibility metadata. This is
   -- not an acquired mutex and does not imply synchronization support.
   type Mutex_Metadata is record
      Raw_Flags : AML_Decode.Byte := 0;
      Level : Sync_Level := 0;
      Canonical : Boolean := True;
   end record;
   function Mutex_Data (Tree : State; Node : Node_ID) return Mutex_Metadata
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Present (Tree, Node) and then Kind (Tree, Node) = Mutex_Object;
   -- Encoded firmware attributes only: PBlock_Address confers no I/O authority.
   type Processor_Block_Address is mod 2 ** 32;
   type Resource_Order is range 0 .. 65_535;
   type Processor_Attributes is record
      ID : AML_Decode.Byte := 0;
      PBlock_Address : Processor_Block_Address := 0;
      PBlock_Length : AML_Decode.Byte := 0;
   end record;
   type Power_Attributes is record
      System_Level : AML_Decode.Byte := 0;
      Order : Resource_Order := 0;
   end record;
   function Processor_Data (Tree : State; Node : Node_ID) return Processor_Attributes
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Present (Tree, Node) and then Kind (Tree, Node) = Processor_Object;
   function Power_Data (Tree : State; Node : Node_ID) return Power_Attributes
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Present (Tree, Node) and then Kind (Tree, Node) = Power_Resource_Object;
   -- Literal firmware metadata only. No address-space handler or capability.
   type Region_Space_ID is new AML_Decode.Byte;
   type Region_Access_State is (No_Access);
   type Operation_Region_Attributes is record
      Space : Region_Space_ID := 0;
      Address, Length : AML_Decode.Integer_Value := 0;
      Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
      Access_State : Region_Access_State := No_Access;
   end record;
   -- PkgLength encodes exactly 28 bits; cumulative field position is UINT32.
   subtype Encoded_Field_Bits is AML_Decode.Field_Bit_Length;
   type Field_Bit_Position is range 0 .. 16#FFFF_FFFF#;
   type Region_Field_Attributes is record
      Region : Node_ID := Root;
      Incarnation : AML_References.Node_Incarnation := 0;
      Offset : Field_Bit_Position := 0;
      Bits : Encoded_Field_Bits := 0;
      -- Raw declaration flags and most recent encoded access attributes.
      -- No access-type normalization or usable field handler is implied.
      Raw_Flags, Access_Type, Attribute, Access_Length : AML_Decode.Byte := 0;
      Access_State : Region_Access_State := No_Access;
   end record;
   function Operation_Region_Data (Tree : State; Node : Node_ID) return Operation_Region_Attributes
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Present (Tree, Node) and then Kind (Tree, Node) = Operation_Region_Object;
   function Region_Field_Data (Tree : State; Node : Node_ID) return Region_Field_Attributes
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Present (Tree, Node) and then Kind (Tree, Node) = Region_Field_Object;
   -- Table identities are local to the containing service lifetime, not
   -- physical addresses. Only its admitted immutable table catalog may bind
   -- these records. A field keeps the resolved region value, not a recyclable
   -- namespace node ID. Dynamic declaration callers supply their method Owner.
   type Table_Region is record
      Table : Positive := 1;
      Extent : Positive := 1;
   end record;
   type Table_Field is record
      Region : Table_Region;
      Offset, Bits : Natural := 0;
   end record;
   function Region_Data (Tree : State; Node : Node_ID) return Table_Region
     with Pre => Node > Root and then Node <= Count (Tree) and then
       Present (Tree, Node) and then Kind (Tree, Node) = Table_Region_Object;
   function Field_Data (Tree : State; Node : Node_ID) return Table_Field
     with Pre => Node > Root and then Node <= Count (Tree) and then
       Present (Tree, Node) and then Kind (Tree, Node) = Table_Field_Object,
       Post => AML_Field_Data.Fits (Field_Data'Result.Region.Extent,
                                    Field_Data'Result.Offset, Field_Data'Result.Bits);
   type Bind_Status is (Bound, Binding_Duplicate, Binding_Full, Binding_Invalid);
   procedure Bind_Table_Region
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Region : Table_Region; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Scope <= Count (Tree) and then Owner <= Count (Tree),
       Post => Insert_Frame (Tree, Tree'Old) and then
         (if Result /= Bound then Tree = Tree'Old and then Node = Root
          else Count (Tree) = Count (Tree'Old) + 1 and then Node = Count (Tree)
            and then Present (Tree, Node) and then Kind (Tree, Node) = Table_Region_Object
            and then Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
            and then Region_Data (Tree, Node) = Region);
   procedure Bind_Table_Field
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Field : Table_Field; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Scope <= Count (Tree) and then Owner <= Count (Tree),
       Post => Insert_Frame (Tree, Tree'Old) and then
         (if Result /= Bound then Tree = Tree'Old and then Node = Root
          else Count (Tree) = Count (Tree'Old) + 1 and then Node = Count (Tree)
            and then Present (Tree, Node) and then Kind (Tree, Node) = Table_Field_Object
            and then Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
            and then Field_Data (Tree, Node) = Field);
   function Has_Integer (Tree : State; Node : Node_ID) return Boolean
     with Pre => Node <= Count (Tree);
   function Integer_Data (Tree : State; Node : Node_ID)
      return AML_Decode.Integer_Value
     with Pre => Node <= Count (Tree) and then Has_Integer (Tree, Node);
   function String_Data (Tree : State; Node : Node_ID) return String
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = String_Object;
   function Buffer_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = Buffer_Object;
   function Method_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = Method_Object,
          Post => Method_Data'Result'Length <= AML_Execute.Max_Method_Bytes;
   function Value_Store (Tree : State) return AML_Objects.State;
   function Data_Object (Tree : State; Node : Node_ID) return AML_Objects.Object_ID
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Kind (Tree, Node) in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object,
          Post => AML_Objects.Is_Live (Value_Store (Tree), Data_Object'Result);
   -- Storage operation for an already resolved integer destination. AML Store
   -- conversion and reference semantics belong to the evaluator, not here.
   function Integer_Updated
     (Tree, Prior : State; Node : Node_ID; Value : AML_Decode.Integer_Value)
      return Boolean with Ghost,
      Pre => Node > Root and then Node <= Count (Prior)
        and then Has_Integer (Prior, Node);
   procedure Set_Integer
     (Tree : in out State; Node : Node_ID; Value : AML_Decode.Integer_Value)
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Has_Integer (Tree, Node),
       Post => Count (Tree) = Count (Tree'Old)
         and then Value_Usage (Tree) = Value_Usage (Tree'Old)
         and then Integer_Updated (Tree, Tree'Old, Node, Value)
         and then Has_Integer (Tree, Node)
         and then Integer_Data (Tree, Node) = Value;
   function Invoke
     (Tree : State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural) return AML_Execute.Execution_Result
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7,
          Post => Invoke'Result.Charged <= Budget;
   -- Mutations completed before an execution error remain visible, as in AML.
   procedure Invoke_Mutable
     (Tree : in out State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7
       and then not Result'Constrained,
       Post => Result.Charged <= Budget;
   -- Table bytes remain external and immutable; buffer-valued field reads may
   -- allocate objects in Tree.Values. Metadata inspection never materializes.
   procedure Invoke_With_Tables
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out AML_Execute.Execution_Result)
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7
         and then not Result'Constrained,
       Post => Result.Charged <= Budget;

   type Load_Status is
     (Loaded, Bad_Name, Bad_Integer, Unsupported_Opcode, Missing_Scope,
      Duplicate_Name, Storage_Full, Bad_Package, Nesting_Limit, Bad_String, Value_Limit, Bad_Buffer, Bad_Method);
   --  Term-list subset: integer Name, Scope and Device. Input is the AML
   --  payload of an already admitted immutable table, not its SDT header.
   --  Unknown opcodes fail the whole transaction. No table activation effects.
   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
     with Post => (if Result /= Loaded then Tree = Tree'Old);
   type Initialization_Report is record
      Bound, Missing, Unsupported : Natural := 0;
   end record;
   function Pending_Members (Tree : State) return Natural;
   function Initialization_Frame (Tree, Prior : State) return Boolean with Ghost;
   procedure Initialize_Members (Tree : in out State; Report : out Initialization_Report)
     with Post => Pending_Members (Tree) = 0
       and then Initialization_Frame (Tree, Tree'Old)
       and then Report.Bound + Report.Missing + Report.Unsupported = Pending_Members (Tree'Old);
package Owned with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type AML_Identity.Identity;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_Decode.Byte;
   use type AML_Objects.Byte_References.Result_Status;
   type Arena is limited private with Default_Initial_Condition =>
     Valid (Arena) and then not Initialized (Arena) and then Snapshot (Arena) = Empty
       and then Node_Count (Arena) = 0 and then Methods_Used (Arena) = 0
       and then Values_Used (Arena) = AML_Objects.Usage'(others => 0)
       and then Frame_Root_Count (Arena) = 0;
   subtype Reference is AML_References.Reference;
   type Name_Reservation is private;
   No_Name_Reservation : constant Name_Reservation;
   use type AML_References.Reference_Kind;
   use type AML_Execute.Execution_Status;
   use type AML_Execute.Datum_Kind;
   use type AML_Objects.Package_References.Result_Status;
   function Valid (A : Arena) return Boolean;

   function Frame_Root_Count (A : Arena) return Natural;
   subtype Root_Values is AML_Execute.Expression_Values;
   subtype Object_Root_Set is AML_Objects.Reachability.Keep_Set;
   type Root_Workspace is limited private;
   type Root_Trace_Status is (Roots_Traced, Uninitialized_Owner,
      Invalid_Root_Value, Invalid_Pending_Root, Invalid_Expression_Root);
   -- Read-only closure of live namespace data, published pins, pending package
   -- members, tracked active frame cells/held values/snapshots, and supplied values.
   -- Any invalid expression snapshot rejects the entire trace. Expression
   -- temporaries still require coverage; this is not permission to collect.
   -- Frame descriptors do not prolong a frame. Expired/foreign descriptors
   -- remain data and have no object edge. Invalid direct object handles fail.
   function Owner_Roots_Traced
     (A : Arena; Extra : Root_Values; Keep : Object_Root_Set;
      Scratch : Root_Workspace) return Boolean with Ghost, Pre => Valid (A);
   procedure Trace_Owner_Roots
     (A : Arena; Extra : Root_Values; Scratch : in out Root_Workspace;
      Keep : out Object_Root_Set; Status : out Root_Trace_Status)
     with Pre => Valid (A),
       Post => (if Status = Roots_Traced then Owner_Roots_Traced (A, Extra, Keep, Scratch)
         else (for all ID in Keep'Range => not Keep (ID)));

   use type AML_Objects.Root_Snapshots.Snapshot_Phase;
   function Expression_Snapshot_Matches (A : Arena; Values : Root_Values;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean with Ghost, Pre => Valid (A);
   type Snapshot_Build_Status is (Snapshot_Built, Snapshot_Uninitialized_Owner, Snapshot_Invalid_Value);
   -- Read-only authentication of an expression's saved values. This produces
   -- seeds only, not transitive closure or permission to collect. Expired/foreign
   -- reference descriptors remain data with no edge; invalid direct handles fail.
   procedure Build_Expression_Snapshot (A : Arena; Values : Root_Values;
      Roots : out AML_Objects.Root_Snapshots.Snapshot; Status : out Snapshot_Build_Status)
      with Pre => Valid (A), Post =>
        (if Status = Snapshot_Built then
           AML_Objects.Root_Snapshots.Phase (Roots) = AML_Objects.Root_Snapshots.Published
           and then Expression_Snapshot_Matches (A, Values, Roots)
         else AML_Objects.Root_Snapshots.Phase (Roots) = AML_Objects.Root_Snapshots.Rejected);

   subtype Retained_Root_Count is Natural range 0 .. Max_Retained_Roots;
   type Retained_Root is private;
   No_Retained_Root : constant Retained_Root;
   type Retain_Status is (Retained, Invalid_Value, Root_Limit, Identity_Exhausted);
   type Release_Status is (Released, Invalid_Root);
   type Retention_State is private;
   function Retention_Model (A : Arena) return Retention_State with Ghost;
   function Retained_Count (A : Arena) return Retained_Root_Count;
   function Retention_Added (A : Arena; Before : Retention_State;
                             Root : Retained_Root; Value : AML_Execute.Datum) return Boolean with Ghost;
   function Retention_Removed (A : Arena; Before : Retention_State;
                               Root : Retained_Root) return Boolean with Ghost;
   function Retention_Cleared (A : Arena; Before : Retention_State) return Boolean with Ghost;
   function Retention_Read (A : Arena; Root : Retained_Root;
                            Value : AML_Execute.Datum;
                            Status : AML_Execute.Execution_Status) return Boolean with Ghost;
   procedure Retain (A : in out Arena; Value : AML_Execute.Datum;
                     Root : out Retained_Root; Status : out Retain_Status) with
     Pre => Valid (A), Post => Valid (A) and then
       Snapshot (A) = Snapshot (A)'Old and then Generation (A) = Generation (A)'Old
       and then Invocation_Count (A) = Invocation_Count (A)'Old
       and then (if Status = Retained then Retained_Count (A) = Retained_Count (A)'Old+1
          and then Retention_Added (A, Retention_Model (A)'Old, Root, Value)
         else Root = No_Retained_Root and then Retention_Model (A) = Retention_Model (A)'Old);
   procedure Read_Retained (A : Arena; Root : Retained_Root;
                            Value : out AML_Execute.Datum;
                            Status : out AML_Execute.Execution_Status) with
     Pre => Valid (A) and then not Value'Constrained,
     Post => Retention_Read (A, Root, Value, Status);
   procedure Release (A : in out Arena; Root : in out Retained_Root;
                      Status : out Release_Status) with
     Pre => Valid (A), Post => Valid (A) and then
       Snapshot (A) = Snapshot (A)'Old and then Generation (A) = Generation (A)'Old
       and then Invocation_Count (A) = Invocation_Count (A)'Old
       and then (if Status = Released then Root = No_Retained_Root
          and then Retained_Count (A) = Retained_Count (A)'Old-1
          and then Retention_Removed (A, Retention_Model (A)'Old, Root'Old)
         else Root = Root'Old and then Retention_Model (A) = Retention_Model (A)'Old);

   function Invocation_Count (A : Arena) return AML_Frame_Handles.Invocation_Serial;
   function Generation (A : Arena) return AML_Identity.Identity with Ghost;
   function Initialized (A : Arena) return Boolean with
     Post => Initialized'Result = (Generation (A) /= AML_Identity.No_Identity);
   function Model (A : Arena) return AML_Objects.State with Ghost;
   function Matches (A : Arena; R : Reference) return Boolean with Pre => Valid (A);
   function Target (R : Reference) return AML_Objects.Object_ID with Ghost;
   function Offset (R : Reference) return Natural with Ghost;
   function Byte_At (A : Arena; R : Reference) return AML_Decode.Byte with Ghost,
     Pre => Valid (A) and then Matches (A, R)
       and then AML_References.Kind (R) = AML_References.Byte_Slot;
   function Snapshot (A : Arena) return State;
   function Node_Count (A : Arena) return Node_ID with
     Post => Node_Count'Result = Count (Snapshot (A));
   function Present (A : Arena; Node : Node_ID) return Boolean with
     Pre => Node <= Node_Count (A),
     Post => Present'Result = Present (Snapshot (A), Node);
   function Kind (A : Arena; Node : Node_ID) return Object_Kind with
     Pre => Node <= Node_Count (A),
     Post => Kind'Result = Kind (Snapshot (A), Node);
   function Mutex_Data (A : Arena; Node : Node_ID) return Mutex_Metadata with
     Pre => Initialized (A) and then Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Mutex_Object;
   function Processor_Data (A : Arena; Node : Node_ID) return Processor_Attributes with
     Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Processor_Object;
   function Power_Data (A : Arena; Node : Node_ID) return Power_Attributes with
     Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Power_Resource_Object;
   function Operation_Region_Data (A : Arena; Node : Node_ID) return Operation_Region_Attributes
     with Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Operation_Region_Object,
       Post => Operation_Region_Data'Result = Operation_Region_Data (Snapshot (A), Node);
   function Region_Field_Data (A : Arena; Node : Node_ID) return Region_Field_Attributes
     with Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Region_Field_Object,
       Post => Region_Field_Data'Result = Region_Field_Data (Snapshot (A), Node);
   function Region_Data (A : Arena; Node : Node_ID) return Table_Region with
     Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Table_Region_Object,
     Post => Region_Data'Result = Region_Data (Snapshot (A), Node);
   function Field_Data (A : Arena; Node : Node_ID) return Table_Field with
     Pre => Node > Root and then Node <= Node_Count (A)
       and then Present (A, Node) and then Kind (A, Node) = Table_Field_Object,
     Post => Field_Data'Result = Field_Data (Snapshot (A), Node)
       and then AML_Field_Data.Fits (Field_Data'Result.Region.Extent,
         Field_Data'Result.Offset, Field_Data'Result.Bits);
   function Values_Used (A : Arena) return AML_Objects.Usage with
     Post => Values_Used'Result = Value_Usage (Snapshot (A));
   function Methods_Used (A : Arena) return Aggregate_Method_Count with
     Post => Methods_Used'Result = Method_Usage (Snapshot (A));
   procedure Load (A : in out Arena; Data : AML_Decode.Bytes;
     Width : AML_Decode.Integer_Width; Result : out Load_Status)
     with Pre => Valid (A) and then Generation (A) /= AML_Identity.No_Identity,
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then (if Result /= Loaded then Snapshot (A) = Snapshot (A)'Old);
   function Pending_Members (A : Arena) return Natural
     with Post => Pending_Members'Result = AML_Namespace.Pending_Members (Snapshot (A));
   procedure Initialize_Members (A : in out Arena; Report : out Initialization_Report)
     with Pre => Valid (A), Post => Valid (A)
       and then Generation (A) = Generation (A)'Old
       and then AML_Namespace.Pending_Members (Snapshot (A)) = 0
       and then Initialization_Frame (Snapshot (A), Snapshot (A)'Old);
   procedure Reset (A : in out Arena; Success : out Boolean)
     with Pre => Valid (A),
     Post => Valid (A) and then
       (if Success then Generation (A) /= AML_Identity.No_Identity
          and then Generation (A) /= Generation (A)'Old
          and then AML_Objects.Live_Count (Model (A)) = 0
          and then Snapshot (A) = Empty
          and then Invocation_Count (A) = 0
          and then Retention_Cleared (A, Retention_Model (A)'Old)
        else Retention_Model (A) = Retention_Model (A)'Old and then Invocation_Count (A) = Invocation_Count (A)'Old and then Generation (A) = Generation (A)'Old and then Model (A) = Model (A)'Old
          and then Snapshot (A) = Snapshot (A)'Old);
   procedure Bind_Table_Region
     (A : in out Arena; Scope : Node_ID; Part : AML_Names.Segment;
      Region : Table_Region; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Valid (A) and then Scope <= Count (Snapshot (A))
       and then Owner <= Count (Snapshot (A)),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Model (A) = Model (A)'Old
       and then Insert_Frame (Snapshot (A), Snapshot (A)'Old)
       and then (if Result /= Bound then Snapshot (A) = Snapshot (A)'Old and then Node = Root
         else Count (Snapshot (A)) = Count (Snapshot (A))'Old + 1
           and then Node = Count (Snapshot (A)) and then Present (Snapshot (A), Node)
           and then Kind (Snapshot (A), Node) = Table_Region_Object
           and then Parent (Snapshot (A), Node) = Scope
           and then Name (Snapshot (A), Node) = Part
           and then Region_Data (Snapshot (A), Node) = Region);
   procedure Bind_Table_Field
     (A : in out Arena; Scope : Node_ID; Part : AML_Names.Segment;
      Field : Table_Field; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Valid (A) and then Scope <= Count (Snapshot (A))
       and then Owner <= Count (Snapshot (A)),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Model (A) = Model (A)'Old
       and then Insert_Frame (Snapshot (A), Snapshot (A)'Old)
       and then (if Result /= Bound then Snapshot (A) = Snapshot (A)'Old and then Node = Root
         else Count (Snapshot (A)) = Count (Snapshot (A))'Old + 1
           and then Node = Count (Snapshot (A)) and then Present (Snapshot (A), Node)
           and then Kind (Snapshot (A), Node) = Table_Field_Object
           and then Parent (Snapshot (A), Node) = Scope
           and then Name (Snapshot (A), Node) = Part
           and then Field_Data (Snapshot (A), Node) = Field);
   procedure Append (A : in out Arena; Data : AML_Decode.Bytes;
     ID : out AML_Objects.Object_ID; Status : out AML_Objects.Allocation_Status)
     with Pre => Valid (A) and then Generation (A) /= AML_Identity.No_Identity,
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then AML_Objects.Extends (Model (A), Model (A)'Old)
       and then (if Status = AML_Objects.Allocated then
         AML_Objects.Is_Live (Model (A), ID)
         and then AML_Objects.Fresh_Allocation (Model (A), Model (A)'Old, ID)
         and then AML_Objects.Kind (Model (A), ID) = AML_Objects.Buffer_Object
         else ID = 0 and then Model (A) = Model (A)'Old);
   procedure Make (A : Arena; ID : AML_Objects.Object_ID;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Objects.Byte_References.Result_Status)
     with Pre => Valid (A),
     Post => (if Status = AML_Objects.Byte_References.Ready then Matches (A, R)
       and then AML_References.Kind (R) = AML_References.Byte_Slot
       and then Target (R) = ID
       and then AML_Decode.Integer_Value (Offset (R)) = Index);
   procedure Read (A : Arena; R : Reference; Value : out AML_Decode.Byte;
     Success : out Boolean) with Pre => Valid (A),
     Post => Success = (Matches (A, R) and then AML_References.Kind (R) = AML_References.Byte_Slot)
       and then (if Success then Value = Byte_At (A, R) else Value = 0);
   procedure Write (A : in out Arena; R : Reference; Value : AML_Decode.Byte;
     Success : out Boolean) with Pre => Valid (A),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Success = (Matches (A, R)'Old and then AML_References.Kind (R) = AML_References.Byte_Slot)
       and then (if Success then Matches (A, R) and then Byte_At (A, R) = Value
         and then AML_Objects.Stored_Byte_Updated
           (Model (A), Model (A)'Old, Target (R), Offset (R), Value)
         else Model (A) = Model (A)'Old);
   function Element_At (A : Arena; R : Reference) return AML_Objects.Object_ID with Ghost,
     Pre => Valid (A) and then Matches (A, R)
       and then AML_References.Kind (R) = AML_References.Package_Slot;
   procedure Make_Element (A : Arena; ID : AML_Objects.Object_ID;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Objects.Package_References.Result_Status)
     with Pre => Valid (A),
     Post => (if Status = AML_Objects.Package_References.Ready then Matches (A, R)
       and then AML_References.Kind (R) = AML_References.Package_Slot
       and then Target (R) = ID and then AML_Decode.Integer_Value (Offset (R)) = Index);
   procedure Read_Element (A : Arena; R : Reference; Value : out AML_Objects.Object_ID;
     Success : out Boolean) with Pre => Valid (A),
     Post => Success = (Matches (A, R) and then AML_References.Kind (R) = AML_References.Package_Slot)
       and then (if Success then Value = Element_At (A, R) else Value = 0);
   procedure Write_Element (A : in out Arena; R : Reference; Value : AML_Objects.Object_ID;
     Success : out Boolean) with Pre => Valid (A),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Success = (Matches (A, R)'Old
         and then AML_References.Kind (R) = AML_References.Package_Slot
         and then (Value = AML_Objects.No_Object or else AML_Objects.Is_Live (Model (A)'Old, Value)))
       and then (if Success then Matches (A, R) and then Element_At (A, R) = Value
         and then AML_Objects.Element_Updated
           (Model (A), Model (A)'Old, Target (R), Offset (R), Value)
         else Model (A) = Model (A)'Old);

   -- Integer source arm of indexed Store. Package elements receive a fresh
   -- integer object; byte slots receive the low byte. Other source kinds are
   -- handled separately; this is not a complete AML Store entry point.
   procedure Store_Integer (A : in out Arena; R : Reference;
     Value : AML_Decode.Integer_Value; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Status in AML_Execute.Returned | AML_Execute.Unsupported_Value | AML_Execute.Value_Limit
       and then (if Status /= AML_Execute.Returned then Model (A) = Model (A)'Old
         else Matches (A, R) and then
           (if AML_References.Kind (R) = AML_References.Byte_Slot then
              Byte_At (A, R) = AML_Decode.Byte (Value mod 256)
            else AML_References.Kind (R) = AML_References.Package_Slot
              and then Element_At (A, R) > 0
              and then AML_Objects.Kind (Model (A), Element_At (A, R)) = AML_Objects.Integer_Object
              and then AML_Objects.Integer_Data (Model (A), Element_At (A, R)) = Value));

   -- Semantic Snapshot excludes issuance metadata, which is explicitly
   -- observed here and never rolled back while this arena identity is live.
   procedure Begin_Invocation
     (A : in out Arena; Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out AML_Execute.Invocation_Status)
     with Pre => Valid (A),
       Post => Valid (A) and then Snapshot (A) = Snapshot (A)'Old
         and then Generation (A) = Generation (A)'Old
         and then
           (if Initialized (A) and then Invocation_Count (A)'Old < Max_Invocations then
              Status = AML_Execute.Available
              and then AML_Frame_Handles.Has_Authority (Domain)
              and then Invocation_Count (A) = Invocation_Count (A)'Old + 1
            else Domain = AML_Frame_Handles.No_Domain
              and then Invocation_Count (A) = Invocation_Count (A)'Old
              and then Status = (if Initialized (A) then AML_Execute.Exhausted
                                 else AML_Execute.Unsupported_Context));
   function Reservation_Matches (A : Arena; Token : Name_Reservation) return Boolean
     with Pre => Valid (A);
   function Reservation_Reference (Token : Name_Reservation) return Reference;
   -- Reserve only under an active, incarnation-checked method owner. The name
   -- is visible but has no value until a write or completion attaches one.
   procedure Reserve_Name
     (A : in out Arena; Scope : Node_ID; Path : AML_Names.Name_Result;
      Token : out Name_Reservation; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Model (A) = Model (A)'Old
         and then (if Status = AML_Execute.Returned then Reservation_Matches (A, Token)
           and then Node_Count (A) = Node_Count (A)'Old + 1
           and then Insert_Frame (Snapshot (A), Snapshot (A)'Old)
         else Snapshot (A) = Snapshot (A)'Old and then Token = No_Name_Reservation);
   function Completion_Frame (Tree, Prior : State; Node : Node_ID) return Boolean with Ghost;
   -- Source is required/authenticated only while the binding is uninitialized.
   -- A value assigned during count evaluation is preserved; Source is ignored.
   procedure Complete_Name
     (A : in out Arena; Token : Name_Reservation;
      Source : AML_References.Object_Handle; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Model (A) = Model (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A))'Old
         and then Node_Count (A) = Node_Count (A)'Old
         and then (if Status = AML_Execute.Returned then not Reservation_Matches (A, Token)
           and then Matches (A, Reservation_Reference (Token))
           and then Completion_Frame (Snapshot (A), Snapshot (A)'Old,
             Node_ID (AML_References.Named_Node (Reservation_Reference (Token))))
         else Snapshot (A) = Snapshot (A)'Old);
   procedure Complete_Runtime_Buffer
     (A : in out Arena; Token : Name_Reservation;
      Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes;
      Count : AML_Data.Count_Result; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A), Post => Valid (A)
       and then Generation (A) = Generation (A)'Old
       and then (if Status /= AML_Execute.Returned then Snapshot (A) = Snapshot (A)'Old);
   function Abort_Frame (Tree, Prior : State; Node : Node_ID) return Boolean with Ghost;
   procedure Abort_Name
     (A : in out Arena; Token : Name_Reservation; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Model (A) = Model (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A))'Old
         and then (if Status = AML_Execute.Returned then
           not Matches (A, Reservation_Reference (Token))
           and then Abort_Frame (Snapshot (A), Snapshot (A)'Old,
             Node_ID (AML_References.Named_Node (Reservation_Reference (Token))))
         else Snapshot (A) = Snapshot (A)'Old);
   function Named_Identity_Matches (A : Arena; R : Reference) return Boolean
     with Pre => Valid (A);
   procedure Make_Named_Identity
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
     with Pre => Valid (A),
       Post => (if Success then Named_Identity_Matches (A, R)
                else R = AML_References.No_Reference);
   procedure Describe_Named_Identity
     (A : Arena; Ref : Reference; Result : out AML_Execute.Reference_Metadata)
     with Pre => Valid (A) and then not Result'Constrained;
   procedure Make_Named_Reference
     (A : Arena; Node : Natural; R : out Reference; Success : out Boolean)
     with Pre => Valid (A),
       Post => (if Success then Matches (A, R)
                  and then AML_References.Kind (R) = AML_References.Named_Cell
                else R = AML_References.No_Reference);
   function Has_Source (A : Arena; H : AML_References.Object_Handle) return Boolean
     with Pre => Valid (A);
   -- Rebuild value metadata from authenticated arena storage, not cached IDs,
   -- type codes or conversions supplied by an evaluator operand.
   procedure Read_Source
     (A : Arena; Source : AML_References.Object_Handle;
      Value : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Value'Constrained,
     Post => Status = (if Has_Source (A, Source) then AML_Execute.Returned
                      else AML_Execute.Unsupported_Value)
       and then (if Has_Source (A, Source) and then
         AML_Objects.Kind (Model (A), AML_References.Source (Source)) = AML_Objects.Integer_Object
       then Value.Value_Kind = AML_Execute.Integer_Datum
         and then Value.Number = AML_Objects.Integer_Data (Model (A), AML_References.Source (Source))
         and then Value.Origin = AML_Objects.Origin_Of (Model (A), AML_References.Source (Source)));
   use type AML_Execute.Reference_Store_Mode;
   use type AML_Execute.Datum;
   procedure Refresh_Value
     (A : Arena; Item : in out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Item'Constrained,
       Post => Status in AML_Execute.Returned | AML_Execute.Unsupported_Value
         and then (if Status /= AML_Execute.Returned then Item = Item'Old);
   procedure Convert_To_Integer
     (A : Arena; Width : AML_Decode.Integer_Width;
      Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Item'Constrained,
       Post => Status in AML_Execute.Returned | AML_Execute.Unsupported_Value | AML_Execute.Empty_Buffer
         and then (if Status = AML_Execute.Returned then
           Item.Value_Kind = AML_Execute.Integer_Datum
             and then (if Item'Old.Value_Kind = AML_Execute.Integer_Datum then Item = Item'Old
               else Item.Origin = AML_Decode.Ordinary_Integer)
         else Item = Item'Old);
   -- Same-type string Store primitive. Target and source both require live
   -- arena handles; successful replacement retains the target object identity.
   procedure Store_String
     (A : in out Arena; Target : AML_References.Object_Handle;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Node_Count (A) = Node_Count (A)'Old
         and then Methods_Used (A) = Methods_Used (A)'Old
         and then Status in AML_Execute.Returned | AML_Execute.Unsupported_Value
           | AML_Execute.Value_Limit
         and then (if Status /= AML_Execute.Returned then Snapshot (A) = Snapshot (A)'Old);
   -- String/Buffer left operand, primitive right operand. Implicit right
   -- conversion uses owner-validated current storage, never cached metadata.
   -- Read-only package scan. Evaluator supplies resolved primitive match
   -- objects and normalized Start; raw match-byte admission is external.
   procedure Match_Package
     (A : Arena; Width : AML_Decode.Integer_Width;
      Package_Value, Match_1, Match_2 : AML_Execute.Datum;
      Operation_1, Operation_2 : AML_Execute.Match_Operation;
      Start : AML_Decode.Integer_Value; Max_Visited : AML_Execute.Match_Visit_Count;
      Value : out AML_Decode.Integer_Value; Visited : out AML_Execute.Match_Visit_Count;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
       Post => Snapshot (A) = Snapshot (A)'Old
         and then Status in AML_Execute.Returned | AML_Execute.Unsupported_Value | AML_Execute.Package_Limit | AML_Execute.Budget_Exceeded
         and then Visited <= Max_Visited
         and then (if Status /= AML_Execute.Returned then Value = 0);

   procedure Compare_Byte_Values
     (A : Arena; Op : AML_Decode.Byte; Left, Right : AML_Execute.Datum;
      Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A), Post => Status in AML_Execute.Returned | AML_Execute.Unsupported_Value;
   procedure Clone_Value
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Copy'Constrained,
          Post => Valid (A) and then Generation (A) = Generation (A)'Old;
   -- Attach an independent, source-typed value to an existing data node.
   -- Unlike Store, the destination's prior type does not convert the source.
   procedure Replace_Value
     (A : in out Arena; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Node_Count (A) = Node_Count (A)'Old
       and then Methods_Used (A) = Methods_Used (A)'Old
       and then (if Status /= AML_Execute.Returned then Snapshot (A) = Snapshot (A)'Old);
   procedure Store_Reference_Value
     (A : in out Arena; R : Reference; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum; Status : out AML_Execute.Execution_Status;
      Mode : AML_Execute.Reference_Store_Mode := AML_Execute.Direct_Target)
     with Pre => Valid (A),
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then (if Status /= AML_Execute.Returned then Snapshot (A) = Snapshot (A)'Old);
   -- Structural resource-template composition only; no hardware authority.
   procedure Concatenate_Resources_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);

   procedure Concatenate_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Left, Right : AML_Execute.Concatenation_Operand;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);
   procedure To_Buffer_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);
   -- Start and Count must already be width-normalized. Noncanonical raw
   -- ranges are rejected without mutation; Integer source is normalized here.
   procedure Mid_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum;
      Start, Count : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);
   -- Length is width-normalized; result is always a fresh raw-byte String.
   procedure To_String_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Item : AML_Execute.Datum;
      Length : AML_Decode.Integer_Value;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);
   -- Integer/Buffer produce fresh formatted Strings; canonical String is borrowed.
   --  Reference destinations honor their supplied store policy. The evaluator
   --  selects explicit-result or argument-indirect policy for these opcodes.
   procedure Format_String_And_Attach
     (A : in out Arena; Width : AML_Decode.Integer_Width;
      Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
      Destination : AML_Execute.Concatenation_Destination;
      Result_Value, Cell_Value : out AML_Execute.Datum;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Result_Value'Constrained and then not Cell_Value'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old
           and then Result_Value.Value_Kind = AML_Execute.Integer_Datum and then Result_Value.Number = 0
           and then Cell_Value.Value_Kind = AML_Execute.Integer_Datum and then Cell_Value.Number = 0);
   procedure Copy_And_Attach
     (A : in out Arena; Destination : AML_Execute.Copy_Destination;
      Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
      Copy : out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Copy'Constrained,
       Post => Valid (A) and then Generation (A) = Generation (A)'Old
         and then Invocation_Count (A) = Invocation_Count (A)'Old
         and then Last_Incarnation (Snapshot (A)) = Last_Incarnation (Snapshot (A)'Old)
         and then Status in AML_Execute.Returned | AML_Execute.Unsupported_Value |
           AML_Execute.Unknown_Name | AML_Execute.Bad_Name | AML_Execute.Uninitialized | AML_Execute.Value_Limit
         and then (if Status /= AML_Execute.Returned then
           Snapshot (A) = Snapshot (A)'Old and then Copy.Value_Kind = AML_Execute.Integer_Datum
           and then Copy.Number = 0);
   -- Copy only an object owned by this arena incarnation. Numeric IDs from
   -- snapshots do not authorize a copy. Allocation failure publishes nothing.
   procedure Clone_Source
     (A : in out Arena; Source : AML_References.Object_Handle;
      Copy : out AML_References.Object_Handle;
      Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Status in AML_Execute.Returned | AML_Execute.Unsupported_Value
         | AML_Execute.Value_Limit
       and then (if Status = AML_Execute.Returned then
         Has_Source (A, Copy) and then Has_Source (A, Source)
         and then not AML_Objects.Is_Live (Model (A)'Old, AML_References.Source (Copy))
         and then AML_Objects.Extends (Model (A), Model (A)'Old)
         and then Node_Count (A) = Node_Count (A)'Old
         and then Methods_Used (A) = Methods_Used (A)'Old
       else Snapshot (A) = Snapshot (A)'Old and then not Has_Source (A, Copy));
   -- NAME package leaves are read-only value bindings, never Store authority.
   function Name_Member_Matches (A : Arena; R : Reference) return Boolean with Pre => Valid (A);
   procedure Resolve_Name_Member (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Value'Constrained,
       Post => Status in AML_Execute.Returned | AML_Execute.Uninitialized | AML_Execute.Unsupported_Value
         and then (if not Name_Member_Matches (A, R) then Status = AML_Execute.Unsupported_Value)
         and then (if Status /= AML_Execute.Returned then
           Value.Value_Kind = AML_Execute.Integer_Datum and then Value.Number = 0);
   function Referent_Object (A : Arena; R : Reference) return AML_Objects.Object_ID with Ghost,
     Pre => Valid (A) and then Matches (A, R)
       and then AML_References.Kind (R) in AML_References.Named_Cell | AML_References.Package_Slot;
   procedure Resolve_Value (A : Arena; R : Reference; Value : out AML_Execute.Datum;
     Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A) and then not Value'Constrained,
     Post => Status =
       (if not Matches (A, R) then AML_Execute.Unsupported_Value
        elsif AML_References.Kind (R) in AML_References.Named_Cell | AML_References.Package_Slot
          and then Referent_Object (A, R) = 0
          then AML_Execute.Uninitialized else AML_Execute.Returned)
       and then (if Status /= AML_Execute.Returned then
         Value.Value_Kind = AML_Execute.Integer_Datum and then Value.Number = 0
       elsif AML_References.Kind (R) = AML_References.Byte_Slot then
         Value.Value_Kind = AML_Execute.Integer_Datum
         and then Value.Number = AML_Decode.Integer_Value (Byte_At (A, R))
       elsif AML_Objects.Kind (Model (A), Referent_Object (A, R)) = AML_Objects.Integer_Object then
         Value.Value_Kind = AML_Execute.Integer_Datum
         and then Value.Number = AML_Objects.Integer_Data (Model (A), Referent_Object (A, R))
         and then Value.Origin = AML_Objects.Origin_Of (Model (A), Referent_Object (A, R))
       elsif AML_Objects.Kind (Model (A), Referent_Object (A, R)) = AML_Objects.Reference_Object then
         Value.Value_Kind = AML_Execute.Reference_Datum
         and then Value.Ref = AML_Objects.Reference_Data (Model (A), Referent_Object (A, R))
       else Value.Value_Kind = AML_Execute.Object_Datum
         and then Value.Object.ID = Referent_Object (A, R)
         and then Has_Source (A, Value.Object.Source)
         and then AML_References.Source (Value.Object.Source) = Value.Object.ID
         and then Value.Object.Size = AML_Objects.Length (Model (A), Referent_Object (A, R)));

   procedure Make_Source (A : Arena; ID : AML_Objects.Object_ID;
     H : out AML_References.Object_Handle; Success : out Boolean)
     with Pre => Valid (A),
     Post => Success = (Generation (A) /= AML_Identity.No_Identity
       and then AML_Objects.Is_Live (Model (A), ID))
       and then (if Success then Has_Source (A, H) and then AML_References.Source (H) = ID);
   procedure Make_Index (A : Arena; H : AML_References.Object_Handle;
     Index : AML_Decode.Integer_Value; R : out Reference;
     Status : out AML_Execute.Execution_Status)
     with Pre => Valid (A),
     Post => Status in AML_Execute.Returned | AML_Execute.Unsupported_Value
       and then (if Status = AML_Execute.Returned then Has_Source (A, H)
         and then Matches (A, R) and then Target (R) = AML_References.Source (H)
         and then AML_Decode.Integer_Value (Offset (R)) = Index);

   -- Public same-arena reentry is rejected before execution. Nested AML calls
   -- use the executor's own stack; they do not reenter this public operation.
   procedure Invoke (A : in out Arena; Input : aliased AML_Table_Backing.State;
     Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out AML_Execute.Execution_Result)
     with Pre => Valid (A) and then Generation (A) /= AML_Identity.No_Identity
       and then Node <= Count (Snapshot (A)) and then Argument_Count <= 7
       and then not Result'Constrained,
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Result.Charged <= Budget
       and then Frame_Root_Count (A) = Frame_Root_Count (A)'Old
       and then Invocation_Count (A) >= Invocation_Count (A)'Old
       and then (Invocation_Count (A) = Invocation_Count (A)'Old
         or else (Invocation_Count (A)'Old < Max_Invocations
           and then Invocation_Count (A) = Invocation_Count (A)'Old + 1));

   type Invocation_Retention_Status is
     (Result_Retained, No_Root_Required, Result_Root_Limit,
      Result_Identity_Exhausted, Invalid_Result, Invocation_Busy);
   function Retention_Reservation_Discarded (A : Arena; Before : Retention_State)
     return Boolean with Ghost;
   -- Reserves a private published integer placeholder before any AML effects.
   -- Compound results fill that slot before method/frame cleanup. Integer,
   -- no-return and error results release it without rewinding the pin issuer.
   procedure Invoke_Retained
     (A : in out Arena; Input : aliased AML_Table_Backing.State;
      Node : Node_ID; Args : AML_Execute.Value_Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out AML_Execute.Execution_Result;
      Result_Root : out Retained_Root; Retention : out Invocation_Retention_Status)
     with Pre => Valid (A) and then Initialized (A)
       and then Node <= Node_Count (A) and then Argument_Count <= 7
       and then not Result'Constrained,
     Post => Valid (A) and then Generation (A) = Generation (A)'Old
       and then Result.Charged <= Budget
       and then Frame_Root_Count (A) = Frame_Root_Count (A)'Old
       and then Invocation_Count (A) >= Invocation_Count (A)'Old
       and then (Invocation_Count (A) = Invocation_Count (A)'Old
         or else (Invocation_Count (A)'Old < Max_Invocations
           and then Invocation_Count (A) = Invocation_Count (A)'Old + 1))
       and then
         (if Retention = Invocation_Busy then
            Result_Root = No_Retained_Root and then Result.Status = AML_Execute.Unsupported_Value
            and then Result.Charged = 0 and then Snapshot (A) = Snapshot (A)'Old
            and then Invocation_Count (A) = Invocation_Count (A)'Old
            and then Retention_Model (A) = Retention_Model (A)'Old
          elsif Retention in Result_Root_Limit | Result_Identity_Exhausted then
            Result_Root = No_Retained_Root and then Result.Status = AML_Execute.Value_Limit
            and then Result.Charged = 0 and then Snapshot (A) = Snapshot (A)'Old
            and then Invocation_Count (A) = Invocation_Count (A)'Old
            and then Retention_Model (A) = Retention_Model (A)'Old
          elsif Retention = Result_Retained then
            Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
            and then Retained_Count (A) = Retained_Count (A)'Old + 1
            and then (if Result.Status = AML_Execute.Object_Returned then
              Retention_Added (A, Retention_Model (A)'Old, Result_Root,
                AML_Execute.Datum'(AML_Execute.Object_Datum, Result.Object))
              else Retention_Added (A, Retention_Model (A)'Old, Result_Root,
                AML_Execute.Datum'(AML_Execute.Reference_Datum, Result.Ref)))
          else Result_Root = No_Retained_Root
            and then Result.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
            and then Retained_Count (A) = Retained_Count (A)'Old
            and then Retention_Reservation_Discarded (A, Retention_Model (A)'Old)
            and then (if Retention = Invalid_Result then Result.Status = AML_Execute.Unsupported_Value));

   -- A separate arena lifetime: no conversion or raw object exports. Collection
   -- runs only at reviewed interpreter entry adapters, before transactions.
   generic
   package Collecting is
      pragma Unevaluated_Use_Of_Old (Allow);
      type Arena is limited private;
      type Value_Handle is private;
      function No_Value return Value_Handle;
      type Phase is (Uninitialized, Loading, Ready);
      type Access_Status is (Available, Invalid_Value, Uninitialized_Element,
         Wrong_Kind, Out_Of_Bounds, Root_Limit, Identity_Exhausted, Busy, Wrong_Phase);
      subtype Argument_Count is Natural range 0 .. 7;
      type Argument_Kind is (Immediate_Argument, Retained_Argument);
      type Argument (Kind : Argument_Kind := Immediate_Argument) is record
         case Kind is
            when Immediate_Argument => Number : AML_Decode.Integer_Value := 0;
            when Retained_Argument => Handle : Value_Handle;
         end case;
      end record;
      type Arguments is array (Natural range 0 .. 6) of Argument;
      type Result (Status : AML_Execute.Execution_Status := AML_Execute.No_Return) is record
         Charged : Natural := 0;
         case Status is
            when AML_Execute.Returned =>
               Number : AML_Decode.Integer_Value;
               Origin : AML_Decode.Integer_Origin;
            when AML_Execute.Object_Returned | AML_Execute.Reference_Returned =>
               Handle : Value_Handle;
            when others => null;
         end case;
      end record;
      type Value_Kind is (Integer_Description, String_Description,
         Buffer_Description, Package_Description, Reference_Description);
      type Value_Description (Kind : Value_Kind := Integer_Description) is record
         case Kind is
            when Integer_Description => Number : AML_Decode.Integer_Value;
               Origin : AML_Decode.Integer_Origin;
            when String_Description | Buffer_Description | Package_Description =>
               Length : Natural;
            when Reference_Description => Ref_Kind : AML_References.Reference_Kind;
         end case;
      end record;
      function Valid (A : Arena) return Boolean;
      function Retention_Model (A : Arena) return Retention_State with Ghost;
      function Pin_Added (A : Arena; Before : Retention_State;
         Handle : Value_Handle) return Boolean with Ghost;
      function Pin_Removed (A : Arena; Before : Retention_State;
         Handle : Value_Handle) return Boolean with Ghost;
      function Pins_Cleared (A : Arena; Before : Retention_State) return Boolean with Ghost;
      type Collection_Count is new AML_Decode.Integer_Value;
      type Collection_Statistics is record
         Attempted, Completed, Rejected : Collection_Count := 0;
         Freed_Objects, Freed_Bytes, Freed_Elements : Collection_Count := 0;
      end record;
      -- Lifetime cumulative saturating counters; Reset does not erase evidence.
      function Reclamation_Metrics (A : Arena) return Collection_Statistics;
      function Current (A : Arena) return Phase;
      function Retained_Count (A : Arena) return Retained_Root_Count;
      function Node_Count (A : Arena) return Node_ID;
      -- Node selectors are current-arena metadata, not durable identities.
      function Present (A : Arena; Node : Node_ID) return Boolean;
      -- Detached equality-only audit data: no inspection or import operation.
      type Audit_Value is private;
      function Audit (A : Arena) return Audit_Value;
      function Initialization_Frame (Current, Prior : Audit_Value) return Boolean with Ghost;
      function Generation (A : Arena) return AML_Identity.Identity with Ghost;
      type Usage_Description is record
         Nodes : Node_ID := Root;
         Values : AML_Objects.Usage := (others => 0);
         Methods : Aggregate_Method_Count := 0;
         Pending : Natural := 0;
      end record;
      function Observe_Usage (A : Arena) return Usage_Description;
      procedure Find_Child (A : Arena; Parent : Node_ID; Part : AML_Names.Segment;
         Node : out Node_ID; Status : out Access_Status);
      procedure Bind_Table_Region (A : in out Arena; Scope : Node_ID;
         Part : AML_Names.Segment; Region : Table_Region; Node : out Node_ID;
         Outcome : out Bind_Status; Status : out Access_Status; Owner : Node_ID := Root);
      procedure Bind_Table_Field (A : in out Arena; Scope : Node_ID;
         Part : AML_Names.Segment; Region_Node : Node_ID; Offset, Bits : Natural;
         Node : out Node_ID; Outcome : out Bind_Status; Status : out Access_Status;
         Owner : Node_ID := Root);
      procedure Read_Table_Field (A : Arena; Node : Node_ID;
         Field : out Table_Field; Status : out Access_Status);
      -- Reads data only. No AML execution, field materialization or dereference.
      -- Compound success transfers one independent pin to the caller.
      procedure Observe_Named_Value (A : in out Arena; Node : Node_ID;
         Outcome : out Result; Status : out Access_Status)
         with Pre => Valid (A) and then not Outcome'Constrained;
      procedure Initialize (A : in out Arena; Status : out Access_Status);
      procedure Reset (A : in out Arena; Status : out Access_Status) with
         Pre => Valid (A), Post => Valid (A) and then
           (if Status = Available then Current (A) = Loading and then Retained_Count (A) = 0
              and then Pins_Cleared (A, Retention_Model (A)'Old)
            else Retention_Model (A) = Retention_Model (A)'Old
              and then Current (A) = Current (A)'Old);
      procedure Load (A : in out Arena; Data : AML_Decode.Bytes;
         Width : AML_Decode.Integer_Width; Outcome : out Load_Status;
         Status : out Access_Status);
      procedure Seal (A : in out Arena; Report : out Initialization_Report;
         Status : out Access_Status);
      procedure Invoke (A : in out Arena; Input : aliased AML_Table_Backing.State;
         Node : Node_ID; Args : Arguments; Count : Argument_Count;
         Budget : Natural; Outcome : out Result; Status : out Access_Status)
         with Pre => Valid (A) and then not Outcome'Constrained,
         Post => Valid (A) and then Outcome.Charged <= Budget
           and then Current (A) = Current (A)'Old
           and then (if Outcome.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
              then Status = Available and then Outcome.Handle /= No_Value
                and then Retained_Count (A) = Retained_Count (A)'Old + 1
                and then Pin_Added (A, Retention_Model (A)'Old, Outcome.Handle)
              else Retained_Count (A) = Retained_Count (A)'Old);
      procedure Release (A : in out Arena; Handle : in out Value_Handle;
         Status : out Access_Status) with Pre => Valid (A), Post => Valid (A) and then
           (if Status = Available then Handle = No_Value
              and then Pin_Removed (A, Retention_Model (A)'Old, Handle'Old)
              and then Retained_Count (A) = Retained_Count (A)'Old - 1
            else Handle = Handle'Old and then Retention_Model (A) = Retention_Model (A)'Old);
      procedure Describe (A : Arena; Handle : Value_Handle;
         Description : out Value_Description; Status : out Access_Status)
         with Pre => not Description'Constrained;
      -- Copies bytes, never a storage view. Offset is zero based.
      procedure Read_Bytes (A : Arena; Handle : Value_Handle; Offset : Natural;
         Data : out AML_Decode.Bytes; Copied : out Natural; Status : out Access_Status)
         with Pre => Valid (A), Post => Copied <= Data'Length
           and then (if Status /= Available then Copied = 0)
           and then (for all I in Data'Range =>
              (if I - Data'First >= Copied then Data (I) = 0));
      -- Successful outputs own independent pins; parent release is then safe.
      procedure Read_Element (A : in out Arena; Handle : Value_Handle;
         Index : Natural; Element : out Value_Handle; Status : out Access_Status)
         with Pre => Valid (A), Post => Valid (A) and then
           (if Status = Available then Element /= No_Value
              and then Pin_Added (A, Retention_Model (A)'Old, Element)
              and then Retained_Count (A) = Retained_Count (A)'Old + 1
            else Element = No_Value and then Retention_Model (A) = Retention_Model (A)'Old);
      procedure Dereference (A : in out Arena; Handle : Value_Handle;
         Value : out Value_Handle; Status : out Access_Status)
         with Pre => Valid (A), Post => Valid (A) and then
           (if Status = Available then Value /= No_Value
              and then Pin_Added (A, Retention_Model (A)'Old, Value)
              and then Retained_Count (A) = Retained_Count (A)'Old + 1
            else Value = No_Value and then Retention_Model (A) = Retention_Model (A)'Old);
   private
      type Audit_Value is record
         Tree : State;
      end record;
      type Value_Handle is record
         Root : Retained_Root := No_Retained_Root;
      end record;
      type Arena is limited record
         Inner : Owned.Arena;
         State : Phase := Uninitialized;
         In_Progress : Boolean := False;
         Collections : Collection_Statistics;
      end record;
   end Collecting;

private
   type Root_Workspace is limited record
      Seeds : Object_Root_Set := [others => False];
      Targets : AML_Objects.Reachability.Reference_Targets :=
        [others => AML_Object_Identifiers.No_Address];
      Walk : AML_Objects.Reachability.Workspace;
   end record;
   package Frame_Pins is new AML_Frame_Roots
     (AML_Execute.Datum, (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer), Max_Frame_Roots);
   package Pins is new AML_Retained_Roots
     (AML_Execute.Datum, (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer),
      Max_Retained_Roots, Max_Retained_Incarnation);
   type Retained_Root is record
      Pin : Pins.Token := Pins.No_Token;
   end record;
   No_Retained_Root : constant Retained_Root := (others => <>);
   type Retention_State is new Pins.Model;
   type Name_Reservation is record
      Ref : Reference := AML_References.No_Reference;
      Owner : Node_ID := Root;
      Owner_Stamp : AML_References.Node_Incarnation := 0;
   end record;
   No_Name_Reservation : constant Name_Reservation := (others => <>);
   type Arena is limited record
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Tree : State := Empty;
      Invocation_Issued : AML_Frame_Handles.Invocation_Serial := 0;
      Retention : Pins.State;
      Frame_Roots : Frame_Pins.State := Frame_Pins.Empty;
   end record;

end Owned;

private
   type Entry_Record is record
      Alive : Boolean := True;
      Initializing : Boolean := False;
      Incarnation : AML_References.Node_Incarnation := 0;
      Owner : Node_ID := Root;
      Active_Calls : Natural := 0;
      Up : Node_ID := Root;
      Part : AML_Names.Segment := "____";
      Object_Type : Object_Kind := Scope_Object;
      Object_Ref : AML_Objects.Object_ID := 0;
      Mutex_Flags : AML_Decode.Byte := 0;
      Processor_Info : Processor_Attributes;
      Power_Info : Power_Attributes;
      Operation_Info : Operation_Region_Attributes;
      Region_Field_Info : Region_Field_Attributes;
      Table_Binding : Table_Field;
      Method_Offset : Aggregate_Method_Count := 0;
      Method_Size : AML_Execute.Method_Length := 0;
      Method_Flags : AML_Decode.Byte := 0;
      Method_Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_Record;
   package Pending is new AML_Pending_Members
     (Node_ID, Max_Pending_Members, Max_Pending_Segments);
   type State is record
      Journal : Pending.State := Pending.Empty;
      Timer_State : AML_Clock.State := AML_Clock.Fresh;
      Used : Node_ID := 0;
      Last_Stamp : AML_References.Node_Incarnation := 0;
      Items : Entries;
      Values : AML_Objects.State := AML_Objects.Empty;
      Code : AML_Decode.Bytes (1 .. Aggregate_Method_Capacity) := [others => 0];
      Code_Used : Aggregate_Method_Count := 0;
   end record
     with Type_Invariant =>
       Pending.Valid (State.Journal) and then
       AML_Objects.Valid (State.Values) and then
       State.Last_Stamp <= Max_Node_Incarnation and then
       (for all I in 1 .. State.Used => State.Items (I).Incarnation > 0
          and then State.Items (I).Incarnation <= State.Last_Stamp and then State.Items (I).Up < I and then State.Items (I).Owner < I and then
          (if State.Items (I).Initializing then
             State.Items (I).Alive and then State.Items (I).Owner > Root
             and then State.Items (State.Items (I).Owner).Alive
             and then State.Items (State.Items (I).Owner).Object_Type = Method_Object
             and then State.Items (State.Items (I).Owner).Active_Calls > 0
             and then State.Items (I).Object_Type in Uninitialized_Name_Object | Integer_Object
               | String_Object | Buffer_Object | Package_Object | Reference_Object) and then
          (if State.Items (I).Object_Type = Uninitialized_Name_Object then
             State.Items (I).Object_Ref = 0
             and then (not State.Items (I).Alive or else State.Items (I).Initializing)) and then
          (if State.Items (I).Object_Type = Region_Field_Object then
             State.Items (I).Region_Field_Info.Region > Root
             and then State.Items (I).Region_Field_Info.Region < I
             and then State.Items (State.Items (I).Region_Field_Info.Region).Object_Type = Operation_Region_Object
             and then State.Items (State.Items (I).Region_Field_Info.Region).Incarnation = State.Items (I).Region_Field_Info.Incarnation
             and then Field_Bit_Position (State.Items (I).Region_Field_Info.Bits) <=
               Field_Bit_Position'Last - State.Items (I).Region_Field_Info.Offset) and then
          (if State.Items (I).Object_Type = Table_Field_Object then
             AML_Field_Data.Fits (State.Items (I).Table_Binding.Region.Extent,
               State.Items (I).Table_Binding.Offset, State.Items (I).Table_Binding.Bits)) and then
          State.Items (I).Method_Offset <= State.Code_Used and then
          State.Items (I).Method_Size <= State.Code_Used - State.Items (I).Method_Offset and then
          (if State.Items (I).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object | Reference_Object then
              AML_Objects.Is_Live (State.Values, State.Items (I).Object_Ref) and then
              AML_Objects.Kind (State.Values, State.Items (I).Object_Ref) =
                (case State.Items (I).Object_Type is
                   when Integer_Object => AML_Objects.Integer_Object,
                   when String_Object => AML_Objects.String_Object,
                   when Buffer_Object => AML_Objects.Buffer_Object,
                   when Reference_Object => AML_Objects.Reference_Object,
                   when others => AML_Objects.Package_Object)));
end AML_Namespace;
