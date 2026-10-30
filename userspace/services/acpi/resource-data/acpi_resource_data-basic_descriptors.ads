pragma Ada_2022;
with Interfaces;
-- Typed descriptions only. No platform validation, address mapping or rights.
package ACPI_Resource_Data.Basic_Descriptors with SPARK_Mode, Pure is
   type Port_Address is range 0 .. 65_535;
   type IRQ_Mask is new Interfaces.Unsigned_16;
   type Memory_Base_32 is new Interfaces.Unsigned_32;
   type Memory_Length_32 is new Interfaces.Unsigned_32;
   type Interrupt_Mode is (Level_Triggered, Edge_Triggered);
   type Interrupt_Polarity is (Active_High, Active_Low);
   type IO_Decoding is (Decode_10, Decode_16);
   type IRQ_Description is record
      Mask : IRQ_Mask;
      Mode : Interrupt_Mode;
      Polarity : Interrupt_Polarity;
      Shared, Wake_Capable, Information_Present : Boolean;
   end record;
   type IO_Description is record
      Decoding : IO_Decoding;
      Minimum, Maximum : Port_Address;
      Alignment, Length : Firmware_Tables.Byte;
   end record;
   type Fixed_IO_Description is record
      -- Preserve the complete encoded base; the descriptor denotes 10-bit
      -- decoding. Handling unused high bits belongs to admission policy.
      Base : Port_Address;
      Length : Firmware_Tables.Byte;
   end record;
   type Fixed_Memory_Description is record
      Base : Memory_Base_32;
      Length : Memory_Length_32;
      Declared_Writeable : Boolean; -- Firmware claim, never write permission.
      Information : Firmware_Tables.Byte;
   end record;
   type Resource_Kind is (IRQ_Resource, IO_Resource, Fixed_IO_Resource,
      Fixed_Memory_32_Resource);
   type Resource (Kind : Resource_Kind := IRQ_Resource) is record
      case Kind is
         when IRQ_Resource => IRQ : IRQ_Description;
         when IO_Resource => IO : IO_Description;
         when Fixed_IO_Resource => Fixed_IO : Fixed_IO_Description;
         when Fixed_Memory_32_Resource => Memory : Fixed_Memory_Description;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Reserved_Bits_Set, Invalid_IRQ_Combination);
   type Decode_Result (Status : Decode_Status := Unsupported_Descriptor) is record
      case Status is
         when Decoded => Value : Resource; Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Re-read the frame: no caller-constructed span is trusted. IRQ information
   -- bits2:1 and memory information bits7:1 are ignored as the specification
   -- requires. Reserved IRQ/IO bits and invalid PIC mode/polarity pairs fail.
   -- Range ordering, alignment and platform feasibility remain descriptive.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Decode_Result with Global => null,
       Post => (if Decode'Result.Status = Decoded then
         Decode'Result.Next_Offset > Offset
         and then Decode'Result.Next_Offset <= Byte_Count (Data'Length));
end ACPI_Resource_Data.Basic_Descriptors;
