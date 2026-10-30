pragma Ada_2022;
with Interfaces;
-- Copied pin descriptions, not pin capabilities or applied configurations.
package ACPI_Resource_Data.Pin_Descriptors with SPARK_Mode, Pure is
   type Descriptor_Kind is (Pin_Function, Pin_Configuration, Pin_Group,
      Group_Function, Group_Configuration);
   type Resource_Role is (Produces_And_Consumes, Consumes_Resource);
   type Function_Number is new Interfaces.Unsigned_16;
   type Pull_Configuration is new Firmware_Tables.Byte;
   type Configuration_Type is new Firmware_Tables.Byte;
   type Configuration_Value is new Interfaces.Unsigned_32;
   type Pin_Number is new Interfaces.Unsigned_16;
   Pin_Bytes : constant Positive := 2;
   subtype Pin_Count is Natural range 0 .. Payload_Length'Last / Pin_Bytes;
   type Byte_Span is record
      First : Byte_Offset := 0;
      Length : Byte_Count := 0;
   end record;
   type Pin_Information (Kind : Descriptor_Kind := Pin_Group) is record
      case Kind is
         when Pin_Function => Pull : Pull_Configuration; Function_ID : Function_Number;
         when Group_Function => Group_Function_ID : Function_Number;
         when Pin_Configuration | Group_Configuration =>
            Configuration : Configuration_Type;
            Value : Configuration_Value;
         when Pin_Group => null;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Unsupported_Revision, Reserved_Bits_Set, Reserved_Pull,
      Reserved_Configuration_Type, Bad_Source_Index, Bad_Offsets, Bad_Pin_Count,
      Bad_Label);
   type Decode_Result (Status : Decode_Status := Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Information : Pin_Information;
            Shared : Boolean;
            Role : Resource_Role;
            Count : Pin_Count;
            Pins, Source, Label, Vendor : Byte_Span;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Revision-one fixed fields and descriptor-local partitions are validated.
   -- Labels are nonempty NUL-terminated strings; Source remains opaque bytes.
   -- Count=0/Pins=(0,0) for group references; Source=(0,0) for Pin_Group;
   -- Label=(0,0) for direct function/configuration. Pin_Function is implicitly
   -- a consumer. Configuration values remain raw, including N/A/vendor values;
   -- controller/type-specific validity, label resolution and conflicts are later.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global => null,
      Post => (if Decode'Result.Status = Decoded then
         Decode'Result.Next_Offset > Offset
         and then Decode'Result.Next_Offset <= Byte_Count (Data'Length)
         and then Decode'Result.Vendor.First >= Offset
         and then Decode'Result.Vendor.First + Decode'Result.Vendor.Length = Decode'Result.Next_Offset
         and then Decode'Result.Pins.First <= Decode'Result.Next_Offset
         and then Decode'Result.Pins.Length <= Decode'Result.Next_Offset - Decode'Result.Pins.First
         and then Decode'Result.Pins.Length = Byte_Count (Decode'Result.Count * Pin_Bytes));
   type Pin_Result (Available : Boolean := False) is record
      case Available is
         when True => Value : Pin_Number;
         when False => null;
      end case;
   end record;
   function Matches_Wire_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive; Item : Pin_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   -- Revalidates the exact buffer rather than trusting caller-supplied spans.
   function Read_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive) return Pin_Result with Global => null,
      Post => Matches_Wire_Pin (Data, Offset, Index, Read_Pin'Result);
end ACPI_Resource_Data.Pin_Descriptors;
