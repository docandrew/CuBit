with Interfaces;
with CCL.VM;
with CCL.Ownership;
with CCL.Catalog;

package CCL.Format with
   SPARK_Mode => On
is
   use Interfaces;

   FORMAT_VERSION  : constant := 3;
   HEADER_SIZE     : constant := 32;
   TYPE_SIZE       : constant := 36;
   LOCAL_SIZE      : constant := 4;
   --  Portable imports contain descriptor-pinned linkage and never contain a
   --  runtime binding, capability slot, endpoint, driver ID, or process ID.
   IMPORT_SIZE     : constant := 56;
   INSTRUCTION_SIZE : constant := 16;

   --  Named offsets are part of the canonical CCLB v3 ABI. Keeping them here
   --  prevents the codec, validation logic, and corruption tests from
   --  independently inventing byte positions.
   MAGIC_OFFSET                    : constant := 0;
   VERSION_OFFSET                  : constant := 4;
   HEADER_SIZE_OFFSET              : constant := 6;
   TOTAL_LENGTH_OFFSET             : constant := 8;
   INSTRUCTION_COUNT_OFFSET        : constant := 12;
   IMPORT_COUNT_OFFSET             : constant := 14;
   FUEL_LIMIT_OFFSET               : constant := 16;
   MEMORY_LIMIT_OFFSET             : constant := 20;
   IN_FLIGHT_LIMIT_OFFSET          : constant := 24;
   LOCAL_COUNT_OFFSET              : constant := 26;
   TYPE_COUNT_OFFSET               : constant := 27;
   DYNAMIC_LOCAL_COUNT_OFFSET      : constant := 28;
   HEADER_RESERVED_OFFSET          : constant := 29;

   TYPE_MODE_OFFSET                : constant := 0;
   TYPE_DISPOSITION_COUNT_OFFSET   : constant := 1;
   TYPE_RESERVED_OFFSET            : constant := 2;
   TYPE_DISPOSITIONS_OFFSET        : constant := 4;
   DISPOSITION_SIZE                : constant := 4;
   DISPOSITION_VERB_OFFSET         : constant := 0;
   DISPOSITION_EFFECT_OFFSET       : constant := 1;
   DISPOSITION_NEXT_TYPE_OFFSET    : constant := 2;
   DISPOSITION_RESERVED_OFFSET     : constant := 3;

   LOCAL_KIND_OFFSET               : constant := 0;
   LOCAL_TYPE_OFFSET               : constant := 1;
   LOCAL_RESERVED_OFFSET           : constant := 2;

   IMPORT_ARGUMENT_OFFSET       : constant := 0;
   IMPORT_RESULT_OFFSET         : constant := 1;
   IMPORT_AUTHORITY_OFFSET      : constant := 2;
   IMPORT_OWNERSHIP_OFFSET      : constant := 3;
   IMPORT_LOCAL_OFFSET          : constant := 4;
   IMPORT_TRANSFER_OFFSET       : constant := 5;
   IMPORT_CANCELLATION_OFFSET   : constant := 6;
   IMPORT_PARAMETER_COUNT_OFFSET : constant := 7;
   IMPORT_SUCCESS_VERB_OFFSET   : constant := 8;
   IMPORT_FAILURE_VERB_OFFSET   : constant := 9;
   IMPORT_CANCEL_VERB_OFFSET    : constant := 10;
   IMPORT_RESERVED_OFFSET       : constant := 11;
   IMPORT_MAJOR_VERSION_OFFSET  : constant := 12;
   IMPORT_MINOR_VERSION_OFFSET  : constant := 14;
   IMPORT_OPERATION_OFFSET      : constant := 16;
   IMPORT_IDENTITY_RESERVED_OFFSET : constant := 17;
   IMPORT_DIGEST_OFFSET         : constant := 24;
   DIGEST_WORD_SIZE             : constant := 8;

   INSTRUCTION_OPCODE_OFFSET       : constant := 0;
   INSTRUCTION_LOCAL_OFFSET        : constant := 1;
   INSTRUCTION_VERB_OFFSET         : constant := 2;
   INSTRUCTION_RESERVED_OFFSET     : constant := 3;
   INSTRUCTION_IMMEDIATE_OFFSET    : constant := 4;
   INSTRUCTION_TARGET_OFFSET       : constant := 12;
   INSTRUCTION_IMPORT_OFFSET       : constant := 14;
   INSTRUCTION_TRAILING_RESERVED_OFFSET : constant := 15;
   MAX_MODULE_SIZE : constant :=
     HEADER_SIZE + CCL.Ownership.MAX_TYPES * TYPE_SIZE +
     CCL.Ownership.MAX_BINDINGS * LOCAL_SIZE +
     CCL.VM.MAX_IMPORTS * IMPORT_SIZE +
     CCL.VM.MAX_INSTRUCTIONS * INSTRUCTION_SIZE;

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
      Bad_Magic,
      Unsupported_Version,
      Bad_Header_Size,
      Bad_Total_Length,
      Bad_Reserved_Field,
      Invalid_Resource_Limit,
      Invalid_Value_Kind,
      Invalid_Authority,
      Invalid_Transfer_Mode,
      Invalid_Cancellation_Mode,
      Invalid_Ownership_Metadata,
      Invalid_Linkage,
      Runtime_Binding_In_Module,
      Invalid_Opcode,
      Invalid_Operand,
      Noncanonical_Instruction,
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
