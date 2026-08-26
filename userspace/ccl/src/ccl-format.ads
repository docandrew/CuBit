with Interfaces;
with CCL.VM;
with CCL.Ownership;

package CCL.Format with
   SPARK_Mode => On
is
   use Interfaces;

   FORMAT_VERSION  : constant := 2;
   HEADER_SIZE     : constant := 32;
   TYPE_SIZE       : constant := 36;
   LOCAL_SIZE      : constant := 4;
   IMPORT_SIZE     : constant := 8;
   INSTRUCTION_SIZE : constant := 16;
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
      Invalid_Ownership_Metadata,
      Invalid_Opcode,
      Invalid_Operand,
      Noncanonical_Instruction,
      Unsupported_Ownership_Metadata,
      Bytecode_Invalid);

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
      Program    : out CCL.VM.Validated_Program;
      Limits     : out Resource_Limits;
      Error      : out Format_Error;
      Validation : out CCL.VM.Validation_Error);
end CCL.Format;
