pragma Ada_2022;
with AML_Delays;
with AML_Frame_Handles;
with AML_Root_Slots;
with AML_Decode;
with AML_Data;
with AML_Names;
with AML_Coercions;
with AML_References;
package AML_Execute with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
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
   type Datum_Kind is (Integer_Datum, Object_Datum, Reference_Datum);
   type Datum (Value_Kind : Datum_Kind := Integer_Datum) is record
      case Value_Kind is
         when Integer_Datum =>
            Number : AML_Decode.Integer_Value := 0;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Object_Datum => Object : Object_Value;
         when Reference_Datum => Ref : AML_References.Reference;
      end case;
   end record;
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
     (Returned, Object_Returned, Reference_Returned, No_Return, Truncated, Unsupported, Uninitialized,
      Missing_Argument, Budget_Exceeded, Invalid_Method, Argument_Mismatch, Mutex_Order, Duplicate_Name, Namespace_Limit, Expression_Limit, Block_Limit, Bad_Package, Invalid_Control,
      Unknown_Name, Unsupported_Value, Bad_Name, Call_Limit, Division_By_Zero, Empty_Buffer, Missing_Result, Value_Limit, Numeric_Overflow, Invalid_Delay, Delay_Failed, Package_Limit, Invalid_Match_Operation, No_Resource_End_Tag, Invalid_Resource_Type, Bad_Resource_Length, Resource_Buffer_Length);
   type Execution_Result (Status : Execution_Status := No_Return) is record
      Charged : Natural;
      case Status is
         when Returned =>
            Value : AML_Decode.Integer_Value;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Object_Returned => Object : Object_Value;
         when Reference_Returned => Ref : AML_References.Reference;
         when others => null;
      end case;
   end record;
   type Binding_Status is (Integer_Binding, Method_Binding, Missing_Binding, Non_Integer_Binding, Failed_Binding, Reference_Binding);
   type Binding_Result (Status : Binding_Status := Missing_Binding) is record
      case Status is
         when Reference_Binding => Ref : AML_References.Reference;
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
   Max_Method_Bytes : constant := 65536;
   subtype Method_Length is Natural range 0 .. Max_Method_Bytes;
   type Method_Definition
     (Exists : Boolean := False; Length : Method_Length := 0) is record
      case Exists is
         when True =>
            Code : AML_Decode.Bytes (1 .. Length);
            Width : AML_Decode.Integer_Width;
            Flags : AML_Decode.Byte;
            Scope : Natural;
         when False => null;
      end case;
   end record;
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


   type Write_Status is (Written, Write_Missing, Write_Unsupported, Write_Value_Limit, Write_Empty_Buffer);
   type Declaration_Status is (Declared, Declaration_Duplicate, Declaration_Missing,
                               Declaration_Full, Declaration_Unsupported);
   type Literal_Kind is (String_Literal, Buffer_Literal, Package_Literal);
   type Named_Metadata_Type is (Field_Metadata, Device_Metadata, Event_Metadata, Method_Metadata, Mutex_Metadata, Region_Metadata, Power_Metadata, Processor_Metadata, Thermal_Metadata);
   for Named_Metadata_Type use
     (Field_Metadata => 5, Device_Metadata => 6, Method_Metadata => 8, Region_Metadata => 10, Event_Metadata => 7, Mutex_Metadata => 9, Power_Metadata => 11, Processor_Metadata => 12, Thermal_Metadata => 13);
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
        (Environment : in out Context; Scope : Natural; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Scope : Natural);
      with procedure Define_Method
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
      with procedure Define_Fields
        (Environment : in out Context; Scope : Natural; Region : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
         Status : out Execution_Status);
      with procedure Materialize
        (Environment : in out Context; Scope : Natural; Kind : Literal_Kind; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Binding : out Binding_Result);
      -- Reserve before evaluating selectors so duplicates and recursive lookup
      -- observe declaration order. End_Call owns cleanup after any failure.
      with procedure Reserve_Region
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Natural; Status : out Execution_Status);
      with procedure Complete_Region
        (Environment : in out Context; Input : aliased Read_Context; Token : Natural;
         Width : AML_Decode.Integer_Width; Signature, OEM, Table_ID : Datum;
         Status : out Execution_Status);
      -- Platform supplies a fresh monotonic reading in 100 ns units at each
      -- opcode evaluation. Unavailable clocks fail explicitly.
      with procedure Read_Timer
        (Environment : in out Context; Value : out AML_Decode.Integer_Value;
         Available : out Boolean);
      with procedure Resolve_Reference
        (Environment : in out Context; Ref : AML_References.Reference;
         Value : out Datum; Status : out Execution_Status);
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
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
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
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Name_Reservation_Token; Status : out Execution_Status);
      with procedure Complete_Runtime_Buffer
        (Environment : in out Context; Token : Name_Reservation_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes;
         Count : AML_Data.Count_Result; Status : out Execution_Status);
      with procedure Abort_Runtime_Name
        (Environment : in out Context; Token : Name_Reservation_Token;
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
      -- Context validity is checked around the actual call by Invoke_Delay.
      -- A formal-aspect instantiation triggers GNAT 15.3 decl.cc:479.
   procedure Execute_With_Input
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
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
