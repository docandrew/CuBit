pragma Ada_2022;
with AML_Decode;
with AML_References;
with AML_Coercions;
with AML_Names;
with AML_Runtime_Exceptions;
with AML_Fatal;
with AML_Execution_Limits;
with AML_Metadata_Kinds;
package AML_Execution_Types with SPARK_Mode, Pure is
   -- Exact integer-literal syntax observed in an executed declaration.
   -- These facts alone establish neither table origin nor address authority.
   type Operation_Region_Syntax is record
      Address_Is_Literal, Length_Is_Literal : Boolean := False;
   end record;

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
   type Scalar_Provenance_Kind is (Detached_Scalar, Owned_Integer_Scalar);

   type Scalar_Provenance (Kind : Scalar_Provenance_Kind := Detached_Scalar) is record
      case Kind is
         when Detached_Scalar => null;
         when Owned_Integer_Scalar => Source : AML_References.Object_Handle;
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

   -- A payload status carries the exact nonzero replacement selected by _ERR.
   -- AE_OK is represented by ordinary continuation, never a runtime failure.
   subtype Runtime_Error_Code is AML_Runtime_Exceptions.Status_Code range
     1 .. AML_Runtime_Exceptions.Status_Code'Last;

   type Execution_Status is
     (Returned, Object_Returned, Reference_Returned, Metadata_Returned, No_Return, Truncated, Unsupported, Uninitialized,
      Missing_Argument, Budget_Exceeded, Invalid_Method, Argument_Mismatch, Mutex_Order, Duplicate_Name, Namespace_Limit, Expression_Limit, Block_Limit, Bad_Package, Invalid_Control,
      Unknown_Name, Unsupported_Value, Bad_Name, Call_Limit, Division_By_Zero, Empty_Buffer, Missing_Result, Value_Limit, Numeric_Overflow, Invalid_Delay, Delay_Failed, Package_Limit, Buffer_Index_Limit, String_Index_Limit, Invalid_Match_Operation, No_Resource_End_Tag, Invalid_Resource_Type, Bad_Resource_Length, Resource_Buffer_Length, Invalid_Field_Length, Buffer_Field_Limit, Notify_Limit, Mutex_Not_Acquired, Invalid_Table_Length, Missing_Operand, Field_Indeterminate, Runtime_Exception, Fatal_Requested, Module_Completed);

   -- Module_Completed is success only at authenticated root EOF, never failure.
   subtype Execution_Failure is Execution_Status range No_Return .. Fatal_Requested;

   type Execution_Result (Status : Execution_Status := No_Return) is record
      Charged : Natural;
      case Status is
         when Returned =>
            Value : AML_Decode.Integer_Value;
            Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
         when Object_Returned => Object : Object_Value;
         when Reference_Returned => Ref : AML_References.Reference;
         when Metadata_Returned => Metadata : Metadata_Value;
         when Runtime_Exception => Exception_Code : Runtime_Error_Code;
         when Fatal_Requested => Fatal : AML_Fatal.Request;
         when others => null;
      end case;
   end record;

   -- Internal progress only: no device descriptor or hardware authority.
   type Field_Context_Ticket is new AML_Decode.Integer_Value;

   Max_Method_Bytes : constant := AML_Execution_Limits.Max_Method_Code_Bytes;

   subtype Method_Length is Natural range 0 .. Max_Method_Bytes;

   type Method_Implementation is (AML_Implementation, Interface_Query);

   subtype Match_Visit_Count is Natural;

   type Match_Operation is
     (Always_True, Equal_To, Less_Or_Equal, Less_Than, Greater_Or_Equal, Greater_Than);

   type Explicit_String_Mode is (Decimal_String, Hexadecimal_String);

   -- Direct targets apply implicit destination conversion; Arg indirection
   -- replaces the binding. Explicit results replace differing simple types
   -- while retaining matching Integer/String/Buffer objects. Differing
   -- authenticated compound results share their canonical source object.
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

   subtype Named_Metadata_Type is AML_Metadata_Kinds.Kind;

   type Reference_Metadata_Kind is (Continue_Reference, Metadata_Only, Invalid_Reference);

   type Reference_Metadata (Kind : Reference_Metadata_Kind := Invalid_Reference) is record
      case Kind is
         when Metadata_Only => Object_Type : Named_Metadata_Type;
         when others => null;
      end case;
   end record;

end AML_Execution_Types;
