pragma Ada_2022;
with AML_Profile_Events;
with AML_Field_Layout;
with ACPI_FADT.Transactions;
with AML_Mutexes;
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
with AML_References;
with AML_Metadata_Kinds;
with AML_Execution_Types;
package AML_Execute with SPARK_Mode, Pure is
   use AML_Execution_Types;
   --  Removed at the interpreter cutover (INTERPRETER-REDESIGN step 3)
   subtype Operation_Region_Syntax is AML_Execution_Types.Operation_Region_Syntax;
   subtype Invocation_Status is AML_Execution_Types.Invocation_Status;
   subtype Arguments is AML_Execution_Types.Arguments;
   subtype Object_Value is AML_Execution_Types.Object_Value;
   subtype Metadata_Mode is AML_Execution_Types.Metadata_Mode;
   subtype Metadata_Value is AML_Execution_Types.Metadata_Value;
   subtype Datum_Kind is AML_Execution_Types.Datum_Kind;
   subtype Datum is AML_Execution_Types.Datum;
   subtype Scalar_Provenance_Kind is AML_Execution_Types.Scalar_Provenance_Kind;
   subtype Scalar_Provenance is AML_Execution_Types.Scalar_Provenance;
   subtype Copy_Destination_Kind is AML_Execution_Types.Copy_Destination_Kind;
   subtype Copy_Destination is AML_Execution_Types.Copy_Destination;
   subtype Expression_Values is AML_Execution_Types.Expression_Values;
   subtype Value_Arguments is AML_Execution_Types.Value_Arguments;
   subtype Runtime_Error_Code is AML_Execution_Types.Runtime_Error_Code;
   subtype Execution_Status is AML_Execution_Types.Execution_Status;
   subtype Execution_Failure is AML_Execution_Types.Execution_Failure;
   subtype Execution_Result is AML_Execution_Types.Execution_Result;
   subtype Field_Context_Ticket is AML_Execution_Types.Field_Context_Ticket;
   Max_Method_Bytes : constant := AML_Execution_Types.Max_Method_Bytes;
   subtype Method_Length is AML_Execution_Types.Method_Length;
   subtype Method_Implementation is AML_Execution_Types.Method_Implementation;
   subtype Match_Visit_Count is AML_Execution_Types.Match_Visit_Count;
   subtype Match_Operation is AML_Execution_Types.Match_Operation;
   subtype Explicit_String_Mode is AML_Execution_Types.Explicit_String_Mode;
   subtype Reference_Store_Mode is AML_Execution_Types.Reference_Store_Mode;
   subtype Concatenation_Operand_Kind is AML_Execution_Types.Concatenation_Operand_Kind;
   subtype Concatenation_Descriptor_Kind is AML_Execution_Types.Concatenation_Descriptor_Kind;
   subtype Concatenation_Operand is AML_Execution_Types.Concatenation_Operand;
   subtype Concatenation_Destination_Kind is AML_Execution_Types.Concatenation_Destination_Kind;
   subtype Concatenation_Destination is AML_Execution_Types.Concatenation_Destination;
   subtype Named_Metadata_Type is AML_Execution_Types.Named_Metadata_Type;
   subtype Reference_Metadata_Kind is AML_Execution_Types.Reference_Metadata_Kind;
   subtype Reference_Metadata is AML_Execution_Types.Reference_Metadata;
   Device_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Device_Descriptor;
   Region_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Region_Descriptor;
   Event_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Event_Descriptor;
   Mutex_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Mutex_Descriptor;
   Power_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Power_Descriptor;
   Processor_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Processor_Descriptor;
   Thermal_Descriptor : constant Concatenation_Descriptor_Kind := AML_Execution_Types.Thermal_Descriptor;
   Detached_Result : constant Concatenation_Destination_Kind := AML_Execution_Types.Detached_Result;
   Prepared_Cell_Copy : constant Concatenation_Destination_Kind := AML_Execution_Types.Prepared_Cell_Copy;
   Named_Attachment : constant Concatenation_Destination_Kind := AML_Execution_Types.Named_Attachment;
   Reference_Attachment : constant Concatenation_Destination_Kind := AML_Execution_Types.Reference_Attachment;
   Data_Operand : constant Concatenation_Operand_Kind := AML_Execution_Types.Data_Operand;
   Namespace_Operand : constant Concatenation_Operand_Kind := AML_Execution_Types.Namespace_Operand;
   Named_Destination : constant Copy_Destination_Kind := AML_Execution_Types.Named_Destination;
   Referenced_Destination : constant Copy_Destination_Kind := AML_Execution_Types.Referenced_Destination;
   Integer_Datum : constant Datum_Kind := AML_Execution_Types.Integer_Datum;
   Object_Datum : constant Datum_Kind := AML_Execution_Types.Object_Datum;
   Reference_Datum : constant Datum_Kind := AML_Execution_Types.Reference_Datum;
   Metadata_Datum : constant Datum_Kind := AML_Execution_Types.Metadata_Datum;
   Returned : constant Execution_Status := AML_Execution_Types.Returned;
   Object_Returned : constant Execution_Status := AML_Execution_Types.Object_Returned;
   Reference_Returned : constant Execution_Status := AML_Execution_Types.Reference_Returned;
   Metadata_Returned : constant Execution_Status := AML_Execution_Types.Metadata_Returned;
   No_Return : constant Execution_Status := AML_Execution_Types.No_Return;
   Truncated : constant Execution_Status := AML_Execution_Types.Truncated;
   Unsupported : constant Execution_Status := AML_Execution_Types.Unsupported;
   Uninitialized : constant Execution_Status := AML_Execution_Types.Uninitialized;
   Missing_Argument : constant Execution_Status := AML_Execution_Types.Missing_Argument;
   Budget_Exceeded : constant Execution_Status := AML_Execution_Types.Budget_Exceeded;
   Invalid_Method : constant Execution_Status := AML_Execution_Types.Invalid_Method;
   Argument_Mismatch : constant Execution_Status := AML_Execution_Types.Argument_Mismatch;
   Mutex_Order : constant Execution_Status := AML_Execution_Types.Mutex_Order;
   Duplicate_Name : constant Execution_Status := AML_Execution_Types.Duplicate_Name;
   Namespace_Limit : constant Execution_Status := AML_Execution_Types.Namespace_Limit;
   Expression_Limit : constant Execution_Status := AML_Execution_Types.Expression_Limit;
   Block_Limit : constant Execution_Status := AML_Execution_Types.Block_Limit;
   Bad_Package : constant Execution_Status := AML_Execution_Types.Bad_Package;
   Invalid_Control : constant Execution_Status := AML_Execution_Types.Invalid_Control;
   Unknown_Name : constant Execution_Status := AML_Execution_Types.Unknown_Name;
   Unsupported_Value : constant Execution_Status := AML_Execution_Types.Unsupported_Value;
   Bad_Name : constant Execution_Status := AML_Execution_Types.Bad_Name;
   Call_Limit : constant Execution_Status := AML_Execution_Types.Call_Limit;
   Division_By_Zero : constant Execution_Status := AML_Execution_Types.Division_By_Zero;
   Empty_Buffer : constant Execution_Status := AML_Execution_Types.Empty_Buffer;
   Missing_Result : constant Execution_Status := AML_Execution_Types.Missing_Result;
   Value_Limit : constant Execution_Status := AML_Execution_Types.Value_Limit;
   Numeric_Overflow : constant Execution_Status := AML_Execution_Types.Numeric_Overflow;
   Invalid_Delay : constant Execution_Status := AML_Execution_Types.Invalid_Delay;
   Delay_Failed : constant Execution_Status := AML_Execution_Types.Delay_Failed;
   Package_Limit : constant Execution_Status := AML_Execution_Types.Package_Limit;
   Buffer_Index_Limit : constant Execution_Status := AML_Execution_Types.Buffer_Index_Limit;
   String_Index_Limit : constant Execution_Status := AML_Execution_Types.String_Index_Limit;
   Invalid_Match_Operation : constant Execution_Status := AML_Execution_Types.Invalid_Match_Operation;
   No_Resource_End_Tag : constant Execution_Status := AML_Execution_Types.No_Resource_End_Tag;
   Invalid_Resource_Type : constant Execution_Status := AML_Execution_Types.Invalid_Resource_Type;
   Bad_Resource_Length : constant Execution_Status := AML_Execution_Types.Bad_Resource_Length;
   Resource_Buffer_Length : constant Execution_Status := AML_Execution_Types.Resource_Buffer_Length;
   Invalid_Field_Length : constant Execution_Status := AML_Execution_Types.Invalid_Field_Length;
   Buffer_Field_Limit : constant Execution_Status := AML_Execution_Types.Buffer_Field_Limit;
   Notify_Limit : constant Execution_Status := AML_Execution_Types.Notify_Limit;
   Mutex_Not_Acquired : constant Execution_Status := AML_Execution_Types.Mutex_Not_Acquired;
   Invalid_Table_Length : constant Execution_Status := AML_Execution_Types.Invalid_Table_Length;
   Missing_Operand : constant Execution_Status := AML_Execution_Types.Missing_Operand;
   Field_Indeterminate : constant Execution_Status := AML_Execution_Types.Field_Indeterminate;
   Runtime_Exception : constant Execution_Status := AML_Execution_Types.Runtime_Exception;
   Fatal_Requested : constant Execution_Status := AML_Execution_Types.Fatal_Requested;
   Module_Completed : constant Execution_Status := AML_Execution_Types.Module_Completed;
   Decimal_String : constant Explicit_String_Mode := AML_Execution_Types.Decimal_String;
   Hexadecimal_String : constant Explicit_String_Mode := AML_Execution_Types.Hexadecimal_String;
   Available : constant Invocation_Status := AML_Execution_Types.Available;
   Unsupported_Context : constant Invocation_Status := AML_Execution_Types.Unsupported_Context;
   Exhausted : constant Invocation_Status := AML_Execution_Types.Exhausted;
   Always_True : constant Match_Operation := AML_Execution_Types.Always_True;
   Equal_To : constant Match_Operation := AML_Execution_Types.Equal_To;
   Less_Or_Equal : constant Match_Operation := AML_Execution_Types.Less_Or_Equal;
   Less_Than : constant Match_Operation := AML_Execution_Types.Less_Than;
   Greater_Or_Equal : constant Match_Operation := AML_Execution_Types.Greater_Or_Equal;
   Greater_Than : constant Match_Operation := AML_Execution_Types.Greater_Than;
   Live_Namespace : constant Metadata_Mode := AML_Execution_Types.Live_Namespace;
   Captured_Namespace : constant Metadata_Mode := AML_Execution_Types.Captured_Namespace;
   AML_Implementation : constant Method_Implementation := AML_Execution_Types.AML_Implementation;
   Interface_Query : constant Method_Implementation := AML_Execution_Types.Interface_Query;
   Continue_Reference : constant Reference_Metadata_Kind := AML_Execution_Types.Continue_Reference;
   Metadata_Only : constant Reference_Metadata_Kind := AML_Execution_Types.Metadata_Only;
   Invalid_Reference : constant Reference_Metadata_Kind := AML_Execution_Types.Invalid_Reference;
   Direct_Target : constant Reference_Store_Mode := AML_Execution_Types.Direct_Target;
   Argument_Indirect_Target : constant Reference_Store_Mode := AML_Execution_Types.Argument_Indirect_Target;
   Explicit_Result_Target : constant Reference_Store_Mode := AML_Execution_Types.Explicit_Result_Target;
   Detached_Scalar : constant Scalar_Provenance_Kind := AML_Execution_Types.Detached_Scalar;
   Owned_Integer_Scalar : constant Scalar_Provenance_Kind := AML_Execution_Types.Owned_Integer_Scalar;

   use type AML_Decode.Integer_Value;
   Create_DWord_Field_Op : constant AML_Decode.Byte := 16#8A#;
   Create_Word_Field_Op : constant AML_Decode.Byte := 16#8B#;
   Create_Byte_Field_Op : constant AML_Decode.Byte := 16#8C#;
   Create_Bit_Field_Op : constant AML_Decode.Byte := 16#8D#;
   Create_QWord_Field_Op : constant AML_Decode.Byte := 16#8F#;
   Create_Field_Extension : constant AML_Decode.Byte := 16#13#;
   Operation_Region_Extension : constant AML_Decode.Byte := 16#80#;
   --  An authenticated post-publication lookup can offer one immediate local
   --  _REG callback. A default value offers none and grants no handler rights.
   type Operation_Region_Notice is record
      Available : Boolean := False;
      Method_ID : Natural := 0;
      Space : AML_Decode.Byte := 0;
      Generation : Natural := 0;
   end record;
   -- Internal synchronous census protocol; no hardware or object authority.
   type Owner_Census_Status is
     (Census_Collecting, Census_Shared, Census_No_Owner,
      Census_Incomplete, Census_Invalid);
   Detached_Provenance : constant Scalar_Provenance := (Kind => Detached_Scalar);
   function As_Values (Args : Arguments) return Value_Arguments with
     Post => (for all I in Args'Range => As_Values'Result (I).Value_Kind = Integer_Datum
       and then As_Values'Result (I).Number = Args (I));
   -- Recovery sites describe a failed declaration or consumed name term; they grant no authority.
   type Module_Name_Error_Site is (Operation_Region_Name_Error, Field_Region_Name_Error, Parsed_Name_Term_Error);
   -- Status-only adapters cannot fabricate a runtime exception or fatal request without its payload.
   subtype Payload_Free_Failure is Execution_Status range No_Return .. Field_Indeterminate;
   type Binding_Status is (Integer_Binding, Method_Binding, Missing_Binding, Non_Integer_Binding, Failed_Binding, Reference_Binding, Metadata_Binding, Field_Binding);
   type Binding_Result (Status : Binding_Status := Missing_Binding) is record
      case Status is
         when Reference_Binding | Field_Binding => Ref : AML_References.Reference;
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
   No_Field_Context : constant Field_Context_Ticket := 0;
   type Field_Context_Phase is (Context_Ready, Context_Value, Context_Failed, Context_Read);
   type Field_Context_Step is record
      Phase : Field_Context_Phase := Context_Ready;
      Property : Binding_Result := (Status => Missing_Binding);
      Failure : Payload_Free_Failure := Unsupported_Value;
      Read_Geometry : AML_Field_Layout.Register_Geometry :=
        (Region => (Space => AML_Field_Layout.Memory_Space, Base => 0, Length => 0,
          PCI => (Known => False)), Byte_Offset => 0, Width => ACPI_FADT.Transactions.Byte_Access);
   end record;
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
   -- Trusted embedding policy,not an AML operand or hardware permission.
   -- Owner providers must still independently admit each requested operation.
   type Module_Effects_Policy is (Restrict_Module_Effects, Use_Owner_Providers);
   -- This synchronous executor requires exclusive ownership of its context.
   -- No operation currently yields to another AML thread or acquires a Mutex.
   function Method_Level (Flags : AML_Decode.Byte) return Sync_Level;





   type Write_Status is (Written, Write_Missing, Write_Unsupported, Write_Value_Limit, Write_Empty_Buffer, Write_Buffer_Field_Limit, Write_Field_Indeterminate, Write_Missing_Operand);
   type Declaration_Status is (Declared, Declaration_Duplicate, Declaration_Missing,
                               Declaration_Full, Declaration_Unsupported);
   type Literal_Kind is (String_Literal, Buffer_Literal, Package_Literal);
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
   type Binding_Purpose is (Inspect_Binding, Evaluate_Binding, Reference_Target, Namespace_Identity);
   -- Immutable external data travels explicitly through recursive calls. A
   -- limited formal prevents copying the backing store into executor frames.
   Load_Table_Operand_Count : constant Positive := 6;
   subtype Load_Table_Operand_Index is Natural range 0 .. Load_Table_Operand_Count - 1;
   type Load_Table_Operands is array (Load_Table_Operand_Index) of Datum;
   type Load_Table_Disposition is (Load_Not_Prepared, Load_Handled, Load_Module_Prepared);
   type Load_Table_Token is new Natural;
   No_Load_Table : constant Load_Table_Token := 0;
   type Load_Table_Plan is record
      Disposition : Load_Table_Disposition := Load_Not_Prepared;
      Token : Load_Table_Token := No_Load_Table;
      Scope, Body_Length : Natural := 0;
   end record;
   -- Raw evaluated operands. Conversion, table selection, declaration ownership,
   -- cleanup and readiness belong to the authenticated namespace owner.
   generic
      type Context (<>) is limited private;
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
         Syntax : Operation_Region_Syntax;
         Status : in out Execution_Status) is null;
      -- Called only after successful publication and outside module-root
      -- execution. The owner must validate Token's live node/incarnation and
      -- local two-argument method, then use trusted provider readiness.
      with procedure Describe_Operation_Region_Notice
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : AML_Declaration_Contexts.Operation_Region_Reservation;
         Notice : in out Operation_Region_Notice) is null;
      with procedure Finish_Operation_Region_Notice
        (Environment : in out Context;
         Token : AML_Declaration_Contexts.Operation_Region_Reservation;
         Notice : Operation_Region_Notice; Succeeded : Boolean;
         Accepted : in out Boolean) is null;
      -- Serialized entry is owner-authenticated and shares the invocation's
      -- explicit-lock level. Default adapters reject rather than simulate it.
      Require_Invocation_Authority : Boolean := False;
      with procedure Operate_Mutex
        (Environment : in out Context; Item : Datum; Action : AML_Mutexes.Operation;
         Timeout : AML_Mutexes.Timeout; Timed_Out : in out Boolean;
         Status : in out Execution_Status) is null;
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
      -- Progress deferred field members at package finish, before committing
      -- the builder. Hooks authenticate metadata/destinations; only the VM
      -- executes AML or performs the field read in its current invocation.
      -- Defaults leave existing adapters unchanged (Done is initialized True).
      with procedure Next_Runtime_Package_Field
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : Runtime_Package_Token; Cursor : in out Natural;
         Binding : in out Binding_Result; Done : in out Boolean) is null;
      with procedure Complete_Runtime_Package_Field
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : Runtime_Package_Token; Cursor : Natural;
         Item : Datum; Status : in out Execution_Status) is null;
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
      -- Called once with all six operands retained in the current expression
      -- census and resolved through the normal call-argument transport. The
      -- owner must authenticate Lease, preserve the caller, and charge no more
      -- than Budget. Status begins Unsupported; the default does no work.
      -- This hook does not itself admit a table or grant hardware authority.
      with procedure Request_Load_Table
        (Environment : in out Context; Input : aliased Read_Context;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Operands : Load_Table_Operands;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status; Plan : in out Load_Table_Plan) is null;
      -- Source is a retained namespace identity. Resolve/authenticate only after
      -- the target expression has been evaluated. Default refuses the request.
      with procedure Request_Load_Source
        (Environment : in out Context; Input : aliased Read_Context;
         Lease : AML_Declaration_Contexts.Lease; Scope : Natural;
         Width : AML_Decode.Integer_Width; Source : Datum;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status; Plan : in out Load_Table_Plan) is null;
      -- Prepared plans are finalized once even when copying/admission/execution
      -- fails. Copy only immutable owned body bytes; no physical-address input.
      -- The VM charges Body_Length before allocating/copying this temporary
      -- storage, and executes it using the existing invocation's Run routine.
      with procedure Copy_Load_Table_Body
        (Environment : in out Context; Input : aliased Read_Context;
         Plan : Load_Table_Plan; Data : in out AML_Decode.Bytes;
         Status : in out Execution_Status) is null;
      -- Called after immutable body copy succeeds and immediately before Run.
      -- A reservation is not consumed on copy/call/budget admission failure.
      with procedure Begin_Load_Table_Module
        (Environment : in out Context; Input : aliased Read_Context;
         Plan : Load_Table_Plan; Status : in out Execution_Status) is null;
      with procedure Finish_Load_Table
        (Environment : in out Context; Input : aliased Read_Context;
         Lease : AML_Declaration_Contexts.Lease; Plan : Load_Table_Plan;
         Operands : Load_Table_Operands; Module_Result : Execution_Result;
         Budget : Natural; Charged : in out Natural; Succeeded : in out Boolean;
         Status : in out Execution_Status) is null;
      -- Offered only after a fully consumed root Divide statement has failed
      -- in module-level execution and its expression roots/packages are closed.
      -- The owner records the diagnostic and may continue at this exact next
      -- term. No rewinding/replay, allocation, collection or AML reentry here.
      -- Default policy propagates the error. Resource/uncertain/fatal failures
      -- and failures inside incomplete expressions are never offered.
      with procedure Handle_Module_Divide_Error
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Resume_Offset, Charged : Natural; Continue_Execution : in out Boolean) is null;
      -- After declaration/expression cleanup, offer a missing-name error at
      -- the current parser cursor. The owner authenticates the module lease,
      -- retires an incomplete region token, and records a typed observation.
      -- It must not rewind, evaluate AML, allocate, collect, or grant I/O.
      with procedure Handle_Module_Name_Error
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Site : Module_Name_Error_Site;
         Token : AML_Declaration_Contexts.Operation_Region_Reservation;
         Flags : AML_Decode.Byte; Field_Entries : AML_Decode.Bytes;
         Resume_Offset, Charged : Natural; Continue_Execution : in out Boolean) is null;
      -- Numeric source operands are collected before resolving their values.
      -- An owner may defer an invalid field as an authenticated Name_Member;
      -- no value, storage, field geometry or I/O permission is synthesized.
      -- Defaults retain the failure. This callback is read-only and cannot
      -- allocate, collect, reenter AML, or change namespace lifetime.
      with procedure Defer_Uninitialized_Field
        (Environment : Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Ref : in out AML_References.Reference; Accepted : in out Boolean) is null;
      -- Offered only for a fully consumed numeric statement, after ordinary
      -- expression cleanup. Incomplete/resource/fatal failures never qualify.
      with procedure Handle_Module_Uninitialized_Error
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope, Resume_Offset, Charged : Natural;
         Continue_Execution : in out Boolean) is null;
      -- Context validity is checked around the actual call by Invoke_Delay.
      -- A formal-aspect instantiation triggers GNAT 15.3 decl.cc:479.
      -- Owner-only metadata loading during a restricted bootstrap session.
      -- This does not relax inherited hardware, delay or notification gates.
      Allow_Restricted_Table_Loads : Boolean := False;
      -- Metadata setup may request normal zero-argument AML property calls.
      -- Actuals must return normally, never evaluate AML/reenter themselves,
      -- and validate the live reference/ticket before consuming any result.
      -- Advance borrows its Value only for that call; retaining an object or
      -- reference requires independently established live owner roots.
      -- Ticket zero owns no cleanup; nested refusal cannot finish its caller.
      with procedure Begin_Field_Context
        (Environment : in out Context; Ref : AML_References.Reference;
         Ticket : in out Field_Context_Ticket; Step : in out Field_Context_Step) is null;
      with procedure Advance_Field_Context
        (Environment : in out Context; Ref : AML_References.Reference;
         Ticket : Field_Context_Ticket; Value : Datum;
         Step : in out Field_Context_Step) is null;
      with procedure Finish_Field_Context
        (Environment : in out Context; Ref : AML_References.Reference;
         Ticket : Field_Context_Ticket; Completed : Boolean) is null;
      -- Passive diagnostics only; actual must return normally without AML,
      -- allocation, logging, hardware, reentry or an effect on execution policy.
      Profile_Enabled : Boolean := False;
      with procedure Observe_Profile (Item : AML_Profile_Events.Event) is null;
      -- Authenticate a live String and resolve its ASL path in the current
      -- lexical scope. Actuals perform no allocation, collection, AML reentry
      -- or hardware access. The caller initializes a refusal and no reference;
      -- the default leaves that refusal unchanged.
      with procedure Resolve_String_Name
        (Environment : in out Context; Scope : Natural; Item : Datum;
         Budget : Natural; Charged : in out Natural;
         Ref : in out AML_References.Reference;
         Status : in out Execution_Status) is null;
      -- Metadata only. The installed provider still admits and owns every IO.
      -- Default rejection is retained by the caller-initialized status.
      with procedure Read_Field_Context
        (Environment : in out Context; Ref : AML_References.Reference;
         Ticket : Field_Context_Ticket; Geometry : AML_Field_Layout.Register_Geometry;
         Value : in out Datum; Status : in out Execution_Status) is null;
      -- Separate from module diagnostic hooks: this enables normal nested
      -- AML calls only at precise, fully consumed runtime exception sites.
      Enable_Runtime_Error_Handler : Boolean := False;
      with procedure Runtime_Error_Thread (Tag : in out AML_Decode.Integer_Value) is null;
   procedure Execute_From_Source
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0;
      Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
      Implementation : Method_Implementation := AML_Implementation;
      Initial_Effects : Module_Effects_Policy := Restrict_Module_Effects)
     with Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment),
          Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(3));
   generic
      type Context (<>) is limited private;
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
      type Context (<>) is limited private;
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
      type Context (<>) is private;
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
      type Context (<>) is private;
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
