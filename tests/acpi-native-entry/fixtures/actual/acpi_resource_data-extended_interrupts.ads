pragma Ada_2022;
with Interfaces;
-- Copied interrupt descriptions. No controller lookup, routing or authority.
package ACPI_Resource_Data.Extended_Interrupts with SPARK_Mode, Pure is
   Maximum_Interrupts : constant Positive := Natural(Firmware_Tables.Byte'Last);
   subtype Interrupt_Count is Positive range 1 .. Maximum_Interrupts;
   type Interrupt_Number is new Interfaces.Unsigned_32;
   type Interrupt_Array is array(Interrupt_Count) of Interrupt_Number;
   type Interrupt_Mode is (Level_Triggered, Edge_Triggered);
   type Interrupt_Polarity is (Active_High, Active_Low);
   type Resource_Role is (Producer, Consumer);
   type Descriptor_Use is (Current_Resources, Possible_Resources, Set_Resources);
   type Source_Index_Policy is (Preserve_Unnegotiated, Require_Zero);
   type Source_Field (Present : Boolean:=False) is record
      case Present is
         when True =>
            Index : Firmware_Tables.Byte;
            Name_Offset : Byte_Offset;
            Name_Length : Byte_Count; -- Excludes terminating NUL; may be zero.
         when False => null;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Reserved_Bits_Set, Bad_Interrupt_Count, Wrong_Usage_Count,
      Bad_Source, Bad_Source_Index);
   type Decode_Result(Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Role : Resource_Role;
            Mode : Interrupt_Mode;
            Polarity : Interrupt_Polarity;
            Shared, Wake_Capable : Boolean;
            Count : Interrupt_Count;
            Interrupts : Interrupt_Array := [others=>0];
            Source : Source_Field;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- _CRS/_SRS require a single interrupt. _PRS may describe alternatives.
   -- Source index zero is required when the applicable platform capability was
   -- negotiated; startup must supply that policy independently. Name offsets
   -- describe bytes in this exact copy, never an authenticated namespace path.
   -- Names are opaque bytes here: no syntax validation or resolution is done.
   function Decode(Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Purpose : Descriptor_Use:=Current_Resources;
      Source_Indices : Source_Index_Policy:=Preserve_Unnegotiated)
      return Decode_Result with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length)
        and then (if Decode'Result.Source.Present then
          Decode'Result.Source.Name_Offset>Offset
          and then Decode'Result.Source.Name_Offset<Decode'Result.Next_Offset
          and then Decode'Result.Source.Name_Length=
            Decode'Result.Next_Offset-Decode'Result.Source.Name_Offset-1)
        and then (for all I in Interrupt_Count =>
          (if I>Decode'Result.Count then Decode'Result.Interrupts(I)=0)));
end ACPI_Resource_Data.Extended_Interrupts;
