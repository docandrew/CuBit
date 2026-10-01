with Interfaces;
with CCL.VM;
with CCL.Catalog;

--  CCLB module format version 8 (docs/ccl-bytecode-format.md): one CBOR
--  item in the restricted profile shared with CCL.Objects.Persistence. It
--  uses definite lengths, shortest-form heads (cbor_ada rejects anything
--  else), no maps, tags or floats, and no trailing data, so a module has
--  exactly one encoding and can be hashed and signed as it is.
--
--  This codec is outside the CCL core (userspace/ccl/src) so embedding the
--  core does not pull in CBOR; only loaders of .cclb bytes include it.
--  Decode always ends with the ordinary bytecode verifier.
--
--    ["CCLB", 8, [fuel, memory, in_flight],
--     ownership_types, data_types, matches,
--     [dynamic_locals, locals], imports, functions, constants, code]
--
--  ownership_types: [[mode, [[verb, effect, next_type] ...]] ...]
--  data_types:      [[shape, name, [[part_name, type] ...], low, high] ...]
--                   (declared types in registry order; a part may name a
--                   later type only as the list of its own type)
--  matches:         [[type, [target x Maximum_Components]] ...]
--  locals:          [[kind, ownership_type, data_type] ...]
--  imports:         [[argument, result, authority, ownership_argument,
--                     local, transfer, cancellation, parameters,
--                     success_verb, failure_verb, cancel_verb, major,
--                     minor, operation, argument_type, result_type,
--                     digest, argument_schema, result_schema] ...]
--                   (digest and schemas: 32-byte byte strings)
--  functions:       [[entry, [[kind, type] ...], result_kind, result_type] ...]
--  constants:       [text ...]  (byte strings; Push_Text's pool)
--  code:            [[op, local, verb, type, alternative, immediate,
--                     target, import] ...]
package CCL.Format with
   SPARK_Mode => On
is
   use Interfaces;

   FORMAT_VERSION : constant := 8;
   FORMAT_MAGIC   : constant String := "CCLB";

   --  Shape codes of data type definitions.
   SHAPE_PRODUCT  : constant := 1;
   SHAPE_SUM      : constant := 2;
   SHAPE_RESOURCE : constant := 3;
   SHAPE_SEQUENCE : constant := 4;
   SHAPE_CALLABLE : constant := 5;
   SHAPE_BOUNDED  : constant := 6;

   --  Fields of one import and one instruction (fixed-length arrays).
   IMPORT_FIELDS      : constant := 19;
   INSTRUCTION_FIELDS : constant := 8;
   DIGEST_BYTES       : constant := 32;

   MAX_MODULE_SIZE : constant := 65_536;

   MAX_MODULE_FUEL       : constant := 1_000_000;
   MAX_MODULE_MEMORY     : constant := 16 * 1_024 * 1_024;
   MAX_MODULE_IN_FLIGHT  : constant := 1;

   subtype Byte_Index is Natural range 0 .. MAX_MODULE_SIZE - 1;
   subtype Module_Length is Natural range 0 .. MAX_MODULE_SIZE;
   type Byte_Array is array (Byte_Index) of Unsigned_8;

   type Resource_Limits is record
      Fuel       : Natural range 0 .. MAX_MODULE_FUEL := 0;
      Memory     : Natural range 0 .. MAX_MODULE_MEMORY := 0;
      In_Flight  : Natural range 0 .. MAX_MODULE_IN_FLIGHT := 0;
   end record;

   type Format_Error is
     (Format_Valid,
      Buffer_Too_Small,
      --  Not one CBOR item of the profile with the layout above: wrong major
      --  type, count, head form, a bound exceeded, or trailing data.
      Malformed_Encoding,
      Bad_Magic,
      Unsupported_Version,
      Invalid_Resource_Limit,
      Invalid_Value_Kind,
      Invalid_Authority,
      Invalid_Transfer_Mode,
      Invalid_Cancellation_Mode,
      Invalid_Ownership_Metadata,
      Invalid_Type_Metadata,
      Invalid_Linkage,
      Runtime_Binding_In_Module,
      Invalid_Opcode,
      Invalid_Operand,
      Noncanonical_Instruction,
      Invalid_Function,
      Unsupported_Ownership_Metadata,
      Bytecode_Invalid);

   procedure Encode
     (Candidate  : CCL.VM.Program;
      Linkage    : CCL.Catalog.Linkage_Table;
      Limits     : Resource_Limits;
      Data       : out Byte_Array;
      Length     : out Module_Length;
      Error      : out Format_Error;
      Validation : out CCL.VM.Validation_Error);

   --  Convenience form for authority-free modules. It rejects any program
   --  containing imports because portable imports require explicit linkage.
   procedure Encode
     (Candidate  : CCL.VM.Program;
      Limits     : Resource_Limits;
      Data       : out Byte_Array;
      Length     : out Module_Length;
      Error      : out Format_Error;
      Validation : out CCL.VM.Validation_Error);

   procedure Decode
     (Data       : Byte_Array;
      Length     : Module_Length;
      Program    : out CCL.VM.Program;
      Linkage    : out CCL.Catalog.Linkage_Table;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out CCL.VM.Validation_Error);

   --  Convenience form for authority-free modules. Imported modules must use
   --  the overload that returns linkage for explicit admission.
   procedure Decode
     (Data       : Byte_Array;
      Length     : Module_Length;
      Program    : out CCL.VM.Validated_Program;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out CCL.VM.Validation_Error);
end CCL.Format;
