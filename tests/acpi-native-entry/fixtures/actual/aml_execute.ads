pragma Ada_2022;
with AML_Mutexes;
with AML_Fatal;
with AML_Declaration_Contexts;
with AML_Execution_Limits;
with AML_Container_Headers;
with AML_Delays;
with AML_Frame_Handles;
with AML_Root_Slots;
with AML_Decode;
with AML_Events;
with AML_Byte_Sources;
with AML_Buffer_Layouts;
with AML_Data;
with AML_Names;
with AML_Coercions;
with AML_References;
with AML_Metadata_Kinds;
package AML_Execute with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   Create_DWord_Field_Op : constant AML_Decode.Byte := 16#8A#;
   Create_Word_Field_Op : constant AML_Decode.Byte := 16#8B#;
   Create_Byte_Field_Op : constant AML_Decode.Byte := 16#8C#;
   Create_Bit_Field_Op : constant AML_Decode.Byte := 16#8D#;
   Create_QWord_Field_Op : constant AML_Decode.Byte := 16#8F#;
   Create_Field_Extension : constant AML_Decode.Byte := 16#13#;
   Operation_Region_Extension : constant AML_Decode.Byte := 16#80#;
   type Invocation_Status is (Available, Unsupported_Context, Exhausted);
   type Arguments is array (Natural range 0 .. 6) of AML_Decode.Integer_Value;
   -- ID and conversion caches describe a value. Source separately carries
   -- owner-bound authority for indexing; snapshot-only values leave it empty.
   type Object_Value is record
      -- Optional authority for indexing a live object; ID alone grants none.
      Source : AML_References.Object_Handle := AML_References.No_Object_Handle;
      ID : Natural := 0;
      Type_Code : Natural range 0 .. 16 := 0;
      Size : Natural := 0;
      Conversion_32, Conversion_64 : AML_Coercions.Result;
   end record;
   type Metadata_Mode is (Live_Namespace, Captured_Namespace);
   type Metadata_Value (Mode : Metadata_Mode := Live_Namespace) is record
      case Mode is
         when Live_Namespace => Namespace_Identity : AML_References.Reference;
         when Captured_Namespace => Snapshot_Source : AML_References.Object_Handle;
      end case;
   end record;
   type Datum_Kind is (Integer_Datum, Object_Datum, Reference_Datum, Metadata_Datum);
   type Datum (Value_Kind : Datum_Kind := Integer_Datum) is record
      case Value_Kind is
         when Integer_Datum =>
            Number : AML_Decode.Integer_Value := 0;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Object_Datum => Object : Object_Value;
         when Reference_Datum => Ref : AML_References.Reference;
         when Metadata_Datum => Metadata : Metadata_Value;
      end case;
   end record;
   -- Numeric data and owner authority travel separately. A detached scalar is
   -- a value; an owned scalar additionally names its exact authenticated object.
   -- Internal synchronous census protocol; no hardware or object authority.
   type Owner_Census_Status is
     (Census_Collecting, Census_Shared, Census_No_Owner,
      Census_Incomplete, Census_Invalid);
   type Scalar_Provenance_Kind is (Detached_Scalar, Owned_Integer_Scalar);
   type Scalar_Provenance (Kind : Scalar_Provenance_Kind := Detached_Scalar) is record
      case Kind is
         when Detached_Scalar => null;
         when Owned_Integer_Scalar => Source : AML_References.Object_Handle;
      end case;
   end record;
   Detached_Provenance : constant Scalar_Provenance := (Kind => Detached_Scalar);
   type Copy_Destination_Kind is (Named_Destination, Referenced_Destination);
   type Copy_Destination (Kind : Copy_Destination_Kind := Named_Destination) is record
      case Kind is
         when Named_Destination => Scope : Natural; Path : AML_Names.Name_Result;
         when Referenced_Destination => Ref : AML_References.Reference;
      end case;
   end record;
   type Expression_Values is array (Positive range <>) of Datum;
   type Value_Arguments is array (Natural range 0 .. 6) of Datum;
   function As_Values (Args : Arguments) return Value_Arguments with
     Post => (for all I in Args'Range => As_Values'Result (I).Value_Kind = Integer_Datum
       and then As_Values'Result (I).Number = Args (I));
   type Execution_Status is
     (Returned, Object_Returned, Reference_Returned, Metadata_Returned, No_Return, Truncated, Unsupported, Uninitialized,
      Missing_Argument, Budget_Exceeded, Invalid_Method, Argument_Mismatch, Mutex_Order, Duplicate_Name, Namespace_Limit, Expression_Limit, Block_Limit, Bad_Package, Invalid_Control,
      Unknown_Name, Unsupported_Value, Bad_Name, Call_Limit, Division_By_Zero, Empty_Buffer, Missing_Result, Value_Limit, Numeric_Overflow, Invalid_Delay, Delay_Failed, Package_Limit, Invalid_Match_Operation, No_Resource_End_Tag, Invalid_Resource_Type, Bad_Resource_Length, Resource_Buffer_Length, Invalid_Field_Length, Buffer_Field_Limit, Notify_Limit, Mutex_Not_Acquired, Field_Indeterminate, Fatal_Requested, Module_Completed);
   -- Module_Completed is success only at authenticated root EOF, never failure.
   subtype Execution_Failure is Execution_Status range No_Return .. Fatal_Requested;
   -- Status-only adapters cannot fabricate a fatal request without its payload.
   subtype Payload_Free_Failure is Execution_Status range No_Return .. Field_Indeterminate;
   type Execution_Result (Status : Execution_Status := No_Return) is record
      Charged : Natural;
      case Status is
         when Returned =>
            Value : AML_Decode.Integer_Value;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Object_Returned => Object : Object_Value;
         when Reference_Returned => Ref : AML_References.Reference;
         when Metadata_Returned => Metadata : Metadata_Value;
         when Fatal_Requested => Fatal : AML_Fatal.Request;
         when others => null;
      end case;
   end record;
   type Binding_Status is (Integer_Binding, Method_Binding, Missing_Binding, Non_Integer_Binding, Failed_Binding, Reference_Binding, Metadata_Binding);
   type Binding_Result (Status : Binding_Status := Missing_Binding) is record
      case Status is
         when Reference_Binding => Ref : AML_References.Reference;
         when Metadata_Binding => Metadata : Metadata_Value;
         when Failed_Binding => Failure : Execution_Status;
         when Integer_Binding =>
            Value : AML_Decode.Integer_Value;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Method_Binding =>
            Method_ID : Natural;
            Parameters : Natural range 0 .. 7;
         when Non_Integer_Binding =>
            Object : Object_Value;
         when others => null;
      end case;
   end record;
   Max_Method_Bytes : constant := AML_Execution_Limits.Max_Method_Code_Bytes;
   subtype Method_Length is Natural range 0 .. Max_Method_Bytes;
   type Method_Implementation is (AML_Implementation, Interface_Query);
   -- Internal methods have no AML code span. Only their authenticated owner
   -- supplies a scalar result; default adapters explicitly remain unavailable.
   type Internal_Method_Status is (Internal_Unavailable, Internal_Rejected, Internal_Returned);
   type Internal_Method_Reply is record
      Status : Internal_Method_Status := Internal_Unavailable;
      Value : AML_Decode.Integer_Value := 0;
   end record;
   type Method_Definition
     (Exists : Boolean := False; Length : Method_Length := 0) is record
      case Exists is
         when True =>
            Implementation : Method_Implementation := AML_Implementation;
            Code : aliased AML_Byte_Sources.Storage (Length);
            Width : AML_Decode.Integer_Width;
            Flags : AML_Decode.Byte;
            Scope : Natural;
         when False => null;
      end case;
   end record;
   Max_Expression_Depth : constant Positive := 64;
   subtype Call_Budget is Natural range 0 .. 32;
   subtype Sync_Level is Natural range 0 .. 15;
   -- This synchronous executor requires exclusive ownership of its context.
   -- No operation currently yields to another AML thread or acquires a Mutex.
   function Method_Level (Flags : AML_Decode.Byte) return Sync_Level;
   -- Direct targets apply implicit destination conversion; Arg indirection
   -- replaces the binding. Explicit results replace differing simple types
   -- while retaining matching Integer/String/Buffer objects. Differing
   -- authenticated compound results share their canonical source object.
   subtype Match_Visit_Count is Natural;

   type Match_Operation is
     (Always_True, Equal_To, Less_Or_Equal, Less_Than, Greater_Or_Equal, Greater_Than);

   type Explicit_String_Mode is (Decimal_String, Hexadecimal_String);

   type Reference_Store_Mode is (Direct_Target, Argument_Indirect_Target, Explicit_Result_Target);
   -- Insert after Reference_Store_Mode. These are transient operation inputs.
   type Concatenation_Operand_Kind is (Data_Operand, Namespace_Operand);
   type Concatenation_Descriptor_Kind is (Device_Descriptor, Region_Descriptor, Event_Descriptor, Mutex_Descriptor, Power_Descriptor, Processor_Descriptor, Thermal_Descriptor);
   type Concatenation_Operand
     (Kind : Concatenation_Operand_Kind := Data_Operand) is record
      case Kind is
         when Data_Operand => Value : Datum;
         when Namespace_Operand =>
            Identity : AML_References.Reference;
            Descriptor : Concatenation_Descriptor_Kind;
      end case;
   end record;
   type Concatenation_Destination_Kind is
     (Detached_Result, Prepared_Cell_Copy, Named_Attachment, Reference_Attachment);
   type Concatenation_Destination
     (Kind : Concatenation_Destination_Kind := Detached_Result) is record
      case Kind is
         when Detached_Result | Prepared_Cell_Copy => null;
         when Named_Attachment => Scope : Natural; Path : AML_Names.Name_Result;
         when Reference_Attachment =>
            Ref : AML_References.Reference;
            Mode : Reference_Store_Mode;
      end case;
   end record;


   type Write_Status is (Written, Write_Missing, Write_Unsupported, Write_Value_Limit, Write_Empty_Buffer, Write_Buffer_Field_Limit, Write_Field_Indeterminate);
   type Declaration_Status is (Declared, Declaration_Duplicate, Declaration_Missing,
                               Declaration_Full, Declaration_Unsupported);
   type Literal_Kind is (String_Literal, Buffer_Literal, Package_Literal);
   subtype Named_Metadata_Type is AML_Metadata_Kinds.Kind;
   Scope_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Scope_Metadata;
   Field_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Field_Metadata;
   Device_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Device_Metadata;
   Event_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Event_Metadata;
   Method_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Method_Metadata;
   Mutex_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Mutex_Metadata;
   Region_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Region_Metadata;
   Power_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Power_Metadata;
   Processor_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Processor_Metadata;
   Thermal_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Thermal_Metadata;
   Buffer_Field_Metadata : constant Named_Metadata_Type := AML_Metadata_Kinds.Buffer_Field_Metadata;
   type Reference_Metadata_Kind is (Continue_Reference, Metadata_Only, Invalid_Reference);
   type Reference_Metadata (Kind : Reference_Metadata_Kind := Invalid_Reference) is record
      case Kind is
         when Metadata_Only => Object_Type : Named_Metadata_Type;
         when others => null;
      end case;
   end record;
   type Binding_Purpose is (Inspect_Binding, Evaluate_Binding, Reference_Target, Namespace_Identity);
   -- Immutable external data travels explicitly through recursive calls. A
   -- limited formal prevents copying the backing store into executor frames.
   generic
      type Context is limited private;
      type Read_Context (<>) is limited private;
      with function Context_Valid (Environment : Context) return Boolean;
      with procedure Lookup
        (Environment : in out Context; Input : aliased Read_Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Purpose : Binding_Purpose;
         Binding : out Binding_Result);
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
      with procedure Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status);
      with procedure Begin_Call
        (Environment : in out Context; Intent : AML_Declaration_Contexts.Request;
         Scope : Natural; Frame : AML_Frame_Handles.Frame_Handle;
         Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease);
      with procedure Define_Method
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
      with procedure Define_Fields
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
         Status : out Execution_Status);
      with procedure Materialize
        (Environment : in out Context; Scope : Natural; Kind : Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Binding : out Binding_Result);
      -- Reserve before evaluating selectors so duplicates and recursive lookup
      -- observe declaration order. End_Call owns cleanup after any failure.
      with procedure Reserve_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out Execution_Status);
      with procedure Complete_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Input : aliased Read_Context; Token : AML_Declaration_Contexts.Region_Reservation;
         Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : Datum;
         Status : out Execution_Status);
      -- Platform supplies a fresh monotonic reading in 100 ns units at each
      -- opcode evaluation. Unavailable clocks fail explicitly.
      with procedure Read_Timer
        (Environment : in out Context; Value : out AML_Decode.Integer_Value;
         Available : out Boolean);
      with procedure Resolve_Reference
        (Environment : in out Context; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width;
         Value : out Datum; Provenance : out Scalar_Provenance;
         Status : out Execution_Status);
      -- Authenticate the exact live integer object; never resolve a name again.
      -- Failure supplies no authority and is not a detached-value fallback.
      with procedure Read_Integer_Source
        (Environment : Context; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value;
         Origin : out AML_Decode.Integer_Origin; Status : out Execution_Status);
      -- Mutate only this authenticated live integer object, without allocation,
      -- collection, name lookup or reentry; failure must leave it unchanged.
      with procedure Normalize_Integer_Source
        (Environment : in out Context; Source : AML_References.Object_Handle;
         Width : AML_Decode.Integer_Width; Value : out AML_Decode.Integer_Value;
         Origin : out AML_Decode.Integer_Origin; Status : out Execution_Status);
      -- Session-local workspace only; no allocation, collection, owner mutation
      -- or reentry between Begin and Finish. Add authenticates actual canonical
      -- values. Invalid/incomplete results never establish an unshared object.
      with procedure Owner_Census_Begin
        (Environment : Context; Source : AML_References.Object_Handle;
         Status : out Owner_Census_Status);
      with procedure Owner_Census_Add
        (Environment : Context; Value : Datum; Status : out Owner_Census_Status);
      with procedure Owner_Census_Finish
        (Environment : Context; Status : out Owner_Census_Status);
      -- Inputs are evaluated before this atomic owner transaction. Integer
      -- quantities are normalized bit offsets/counts; Source is authenticated
      -- and converted to Buffer by the owner, never by cached descriptors.
      with procedure Define_Buffer_Field
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Source : Datum;
         Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean;
         Status : out Execution_Status);
      with procedure Create_Index
        (Environment : in out Context; Source : AML_References.Object_Handle;
         Index : AML_Decode.Integer_Value; Ref : out AML_References.Reference;
         Status : out Execution_Status);
      with procedure Store_Reference
        (Environment : in out Context; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Item : Datum; Status : out Execution_Status;
         Mode : Reference_Store_Mode);
      -- CopyObject has source-type replacement semantics, independent of Store.
      with procedure Clone_Value
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : Datum; Copy : out Datum; Status : out Execution_Status);
      -- Object comparisons are mediated by the owner, never cached descriptors.
      with procedure Compare_Objects
        (Environment : in out Context; Op : AML_Decode.Byte;
         Left, Right : Datum; Width : AML_Decode.Integer_Width;
         Value : out AML_Decode.Integer_Value; Status : out Execution_Status);
      -- Refresh mutable-object caches through the owning storage context.
      with procedure Refresh_Value
        (Environment : Context; Item : in out Datum;
         Status : out Execution_Status);
      with procedure Begin_Invocation
        (Environment : in out Context; Domain : out AML_Frame_Handles.Invocation_Domain;
         Status : out Invocation_Status);
      -- Bounded DataObject declaration; not unrestricted TermArg evaluation.
      with procedure Define_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Consumed : out Natural; Status : out Execution_Status);
      with procedure Copy_And_Attach
        (Environment : in out Context; Destination : Copy_Destination;
         Width : AML_Decode.Integer_Width; Item : Datum;
         Copy : out Datum; Status : out Execution_Status);
      -- Observe authenticated identity only; never evaluate a method or read a field.
      with procedure Describe_Named_Identity
        (Environment : Context; Ref : AML_References.Reference;
         Result : out AML_Execute.Reference_Metadata);
      -- Caller must supply a valid Context and unconstrained Result. These
      -- obligations are asserted at the call site; GNAT 15.3 crashes when
      -- instantiating this discriminated-out generic formal with a Pre aspect.
      type Name_Reservation_Token is private;
      No_Name_Reservation_Token : Name_Reservation_Token;
      with procedure Reserve_Runtime_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Name_Reservation_Token; Status : out Execution_Status);
      with procedure Complete_Runtime_Buffer
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Name_Reservation_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes;
         Count : AML_Data.Count_Result; Status : out Execution_Status);
      with procedure Abort_Runtime_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Name_Reservation_Token;
         Status : out Execution_Status);
      -- Local best-effort observation only: no mutation through Context,
      -- blocking IPC, re-entry, retained Datum authority, or AML allocation.
      -- Explicit disabled actuals still evaluate source/fuel/errors normally.
      with procedure Observe_Debug
        (Environment : Context; Scope, Position : Natural;
         Width : AML_Decode.Integer_Width; Value : Datum);
      with procedure Convert_To_Integer
        (Environment : Context; Width : AML_Decode.Integer_Width;
         Item : in out Datum; Status : out Execution_Status);
      with procedure Concatenate_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Left, Right : Concatenation_Operand;
         Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum;
         Status : out Execution_Status);
      with procedure To_Buffer_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : Datum; Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum;
         Status : out Execution_Status);
      with procedure Mid_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : Datum; Start, Count : AML_Decode.Integer_Value;
         Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum;
         Status : out Execution_Status);
      with procedure To_String_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : Datum; Length : AML_Decode.Integer_Value;
         Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum;
         Status : out Execution_Status);
      with procedure Format_String_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Mode : Explicit_String_Mode; Item : Datum;
         Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum;
         Status : out Execution_Status);
      with procedure Match_Package
        (Environment : Context; Width : AML_Decode.Integer_Width;
         Package_Value, Match_1, Match_2 : Datum;
         Operation_1, Operation_2 : Match_Operation;
         Start : AML_Decode.Integer_Value; Max_Visited : Match_Visit_Count;
         Value : out AML_Decode.Integer_Value; Visited : out Match_Visit_Count;
         Status : out Execution_Status);
      with procedure Concatenate_Resources_And_Attach
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Left, Right : Datum; Destination : Concatenation_Destination;
         Result_Value, Cell_Value : out Datum; Status : out Execution_Status);
      -- Synchronous top-level handoff before End_Call/Close_Frame. The caller
      -- preallocates storage; this hook must not allocate AML objects, block,
      -- re-enter evaluation or change the namespace. Nested results remain
      -- borrowed by their caller and do not invoke this hook.
      with procedure Handoff_Result
        (Environment : in out Context; Result : Execution_Result) is null;
      -- Maintain caller-owned roots after every successful frame-cell write,
      -- including writes through references to suspended callers. Close emits
      -- an uninitialized value for every cell. Full handles distinguish frame
      -- generations and invocation domains; indices alone are not identities.
      -- The observer must accept every update using preallocated storage. It
      -- must not allocate AML objects, collect, block, reenter or alter values.
      -- This covers frame cells only; expression/actual/return roots are separate.
      with procedure Publish_Frame_Root
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
         Cell : AML_Frame_Handles.Cell_ID; Initialized : Boolean; Value : Datum) is null;
      -- Admission must reserve enough root storage for all cells before args
      -- are installed or Begin_Call can run. Defaults preserve untracked callers.
      with function Frame_Root_Capacity (Environment : Context) return Boolean is Context_Valid;
      with procedure Open_Frame_Root
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle) is null;
      with procedure Close_Frame_Root
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle) is null;
      -- Supplemental values outlive individual Operand calls. The observer has
      -- the same nonallocating/noncollecting requirements as frame publication.
      with procedure Publish_Held_Root
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
         Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Datum) is null;
      -- Publish live values at covered evaluator boundaries, including nested
      -- dispatch and literal materialization; clear them when their scope ends.
      -- The observer must authenticate values and use preallocated frame storage.
      -- No allocation, collection, reentry, blocking or value mutation is allowed.
      -- Other allocation boundaries still require explicit root coverage.
      with procedure Publish_Expression_Roots
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
         Values : Expression_Values) is null;
      -- Completed requires genuine requested timing. Providers must not reenter
      -- AML, reset the active arena, or independently collect its live roots.
      -- Failed may follow external effects; callers must not assume rollback.
      with procedure Wait_For_Delay
        (Environment : in out Context; Item : AML_Delays.Request;
         Result : out AML_Delays.Outcome);
      -- Admission only: authenticate Target and its notify-capable class even
      -- when no observer is interested. No IPC, AML reentry, or silent loss.
      -- Returned means accepted (or valid with no interested observer);
      -- Notify_Limit means bounded admission failed without changing the queue.
      with procedure Queue_Notify
        (Environment : in out Context; Target : AML_References.Reference;
         Value : AML_Decode.Integer_Value; Status : out Execution_Status);
      with procedure Describe_Metadata_Value
        (Environment : Context; Item : Metadata_Value; Result : out Reference_Metadata);
      -- Buffer literals carry a normalized size and original initializer bytes.
      -- Evaluator roots are published before this allocation-capable callback.
      with procedure Materialize_Buffer
        (Environment : in out Context; Scope : Natural;
         Length : AML_Buffer_Layouts.Buffer_Length; Initializer : AML_Decode.Bytes;
         Binding : out Binding_Result);
      -- Only admitted module roots use Alias in this slice. Method Alias retains
      -- its existing Unsupported result without evaluating declaration operands.
      with procedure Define_Alias
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
         Status : out Execution_Status);
      -- Enter/leave lexical namespace only; the authenticated lease owner is unchanged.
      with procedure Enter_Container
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out Execution_Status);
      with procedure Leave_Container
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out Execution_Status);
      with procedure Evaluate_Internal_Method
        (Environment : Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Implementation : Method_Implementation;
         Args : Value_Arguments; Count : Natural;
         Reply : in out Internal_Method_Reply) is null;
      -- Optional owner hook. The evaluator initializes Status to Unsupported;
      -- adapters without runtime declarations retain that explicit rejection.
      with procedure Define_Mutex
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte;
         Status : in out Execution_Status) is null;
      -- Runtime executing declaration only. These optional callbacks do not
      -- evaluate saved table AML and never grant address-space authority.
      with procedure Reserve_Operation_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result; Space : AML_Decode.Byte;
         Token : in out AML_Declaration_Contexts.Operation_Region_Reservation;
         Status : in out Execution_Status) is null;
      with procedure Complete_Operation_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : AML_Declaration_Contexts.Operation_Region_Reservation;
         Width : AML_Decode.Integer_Width; Address, Length : AML_Decode.Integer_Value;
         Status : in out Execution_Status) is null;
      -- Serialized entry is owner-authenticated and shares the invocation's
      -- explicit-lock level. Default adapters reject rather than simulate it.
      Require_Invocation_Authority : Boolean := False;
      with procedure Operate_Mutex
        (Environment : in out Context; Item : Datum; Action : AML_Mutexes.Operation;
         Timeout : AML_Mutexes.Timeout; Status : in out Execution_Status) is null;
      with procedure Enter_Serialized_Method
        (Environment : in out Context; Method : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Status : in out Execution_Status) is null;
      with procedure Leave_Serialized_Method
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
         Status : in out Execution_Status) is null;
      with procedure Define_Event
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Status : in out Execution_Status) is null;
      with procedure Operate_Event
        (Environment : in out Context; Item : Datum; Action : AML_Events.Action;
         Timeout : AML_Events.Timeout; Timed_Out : in out Boolean;
         Status : in out Execution_Status) is null;
      Block_Capacity : AML_Execution_Limits.Continuation_Capacity :=
        AML_Execution_Limits.Default_Continuation_Capacity;
      -- Owner-session construction authority. Adapters without a live object
      -- arena must reject these operations explicitly, never synthesize roots.
      type Runtime_Package_Token is private;
      No_Runtime_Package : Runtime_Package_Token;
      with procedure Begin_Runtime_Package
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Count, Child_Byte_Bound : Natural;
         Parent : Runtime_Package_Token; Name : Name_Reservation_Token;
         Width : AML_Decode.Integer_Width; Token : out Runtime_Package_Token;
         Binding : out Binding_Result);
      with procedure Append_Runtime_Package
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Item : Datum;
         Status : out Execution_Status);
      with procedure Defer_Runtime_Package_Name
        (Environment : Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out Execution_Status);
      with procedure Finish_Runtime_Package
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Runtime_Package_Token; Binding : out Binding_Result);
      -- Called on every expression exit BEFORE its root publication is cleared.
      -- This operation may invalidate session records, but must not allocate,
      -- collect, execute AML, or restore a pre-evaluation arena snapshot.
      with procedure Abort_Runtime_Packages
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Status : out Execution_Status);
      -- False preserves the preexisting pure static materialization path.
      Runtime_Package_Supported : Boolean := False;
      -- Metadata only. The owner authenticates both field identities and the
      -- declaration lifetime; this hook cannot authorize an index/data access.
      with procedure Define_Indexed_Fields
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Index_Path, Data_Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
         Status : in out Execution_Status) is null;
      with procedure Define_Banked_Fields
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Region_Path, Bank_Path : AML_Names.Name_Result;
         Bank_Value : AML_Decode.Integer_Value; Flags : AML_Decode.Byte;
         Entries : AML_Decode.Bytes; Status : in out Execution_Status) is null;
      -- Context validity is checked around the actual call by Invoke_Delay.
      -- A formal-aspect instantiation triggers GNAT 15.3 decl.cc:479.
   procedure Execute_From_Source
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0;
      Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
      Implementation : Method_Implementation := AML_Implementation)
     with Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment),
          Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(3));
   generic
      type Context is limited private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width) return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
      with procedure Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status);
      with procedure Begin_Call
        (Environment : in out Context; Scope : Natural; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Scope : Natural);
      with procedure Define_Method
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
   procedure Execute_Typed
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
     with Global => null, Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment);
   generic
      type Context is limited private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width) return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
      with procedure Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status);
      with procedure Begin_Call
        (Environment : in out Context; Scope : Natural; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Scope : Natural);
      with procedure Define_Method
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
      with procedure Admit_Session
        (Environment : in out Context; Domain : out AML_Frame_Handles.Invocation_Domain;
         Status : out Invocation_Status);
      with procedure Open_Session_Frame
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle);
      with procedure Close_Session_Frame
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle);
      with procedure Enter_Session_Method
        (Environment : in out Context; Method : Natural; Frame : AML_Frame_Handles.Frame_Handle;
         Status : in out Execution_Status);
      with procedure Leave_Session_Method
        (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
         Status : in out Execution_Status);
   -- Procedural session adapter. Callback effects are intentionally not hidden
   -- behind Global => null; the pure wrappers below retain their original path.
   procedure Execute_Session_Typed
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
     with Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment);
   generic
      type Context is private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width) return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
   function Run_Typed
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0) return Execution_Result
     with Global => null, Pre => Argument_Count <= 7 and then Context_Valid (Environment),
          Post => Run_Typed'Result.Charged <= Budget;
   generic
      type Context is private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width)
         return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
   function Run_Bound
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0) return Execution_Result
     with Global => null, Pre => Argument_Count <= 7 and then Context_Valid (Environment),
          Post => Run_Bound'Result.Charged <= Budget;
   --  Integer-argument entrypoints into the typed core with bounded control
   --  flow. Run has no namespace; Run_Bound uses a read-only binding. No I/O.
   --  External argument
   --  values survive direct reads/returns; literals, arithmetic results and
   --  predicate conversion observe the table integer width.
   --  Charge one unit before each statement and each source operand.
   function Run
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result
     with Pre => Argument_Count <= 7,
          Post => Run'Result.Charged <= Budget;
end AML_Execute;
