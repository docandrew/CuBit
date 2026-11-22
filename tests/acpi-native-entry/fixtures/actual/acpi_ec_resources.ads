pragma Ada_2022;
with Firmware_Tables;
with ACPI_FADT;
with ACPI_Resource_Data;
--  Semantic projection of copied _CRS bytes. Every result is a firmware claim,
--  never admission, mapping, register authority or a GPIO-controller binding.
package ACPI_EC_Resources with SPARK_Mode, Pure is
   type Register_Role is (Data_Register, Control_Register);
   type Register_Claim is record
      Address : ACPI_FADT.Generic_Address;
      IO_Decode_10 : Boolean := False;
      Writeability_Declared : Boolean := False;
      Declared_Writeable : Boolean := False;
   end record;
   type Claims is array (Register_Role) of Register_Claim;
   type Status is (Described, Bad_Template, Unsupported_Descriptor,
      Malformed_Descriptor, Unsupported_Layout, Missing_Registers,
      Extra_Registers, Aliased_Registers, Ambiguous_Interrupt);
   type Result (Outcome : Status := Bad_Template) is record
      case Outcome is
         when Described =>
            Registers : Claims;
            GPIO_Interrupt_Present : Boolean;
            GPIO_Offset : ACPI_Resource_Data.Byte_Offset;
         when others => null;
      end case;
   end record;
   --  First address region is data, second is command/status. Accept only
   --  unambiguous static one-byte layouts with no translation/source dependency.
   --  Preserve 10-bit IO decoding and memory writeability claims for admission.
   --  Known IRQ/extended IRQ/GPIO interrupt descriptors are validated; an unknown
   --  descriptor is refused rather than skipped across a possible address slot.
   --  GPIO_Offset locates a descriptor in these exact bytes; re-decode it before
   --  use. It is neither a source ID nor a pointer. Zero addresses stay claims.
   function Describe (Data : Firmware_Tables.Bytes;
      Maximum_Items : ACPI_Resource_Data.Item_Count := ACPI_Resource_Data.Default_Item_Limit)
      return Result with Global => null;
end ACPI_EC_Resources;
