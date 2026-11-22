pragma Ada_2022;
with Interfaces;
-- Copied connection descriptions only, not pin capabilities or controller lookup.
package ACPI_Resource_Data.GPIO_Connections with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_16;
   Minimum_Connection_Bytes : constant Byte_Count := 25;
   Wire_Byte_Radix : constant Interfaces.Unsigned_16 := 256;
   function Wire_Word (Data : Firmware_Tables.Bytes; Position : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16 (Data (Data'First + Natural (Position))) +
      Wire_Byte_Radix * Interfaces.Unsigned_16 (Data (Data'First + Natural (Position) + 1)))
     with Ghost, Global => null,
       Pre => Position <= Byte_Count (Data'Length)
         and then Byte_Count (Data'Length) - Position >= 2;
   type Connection_Kind is (Interrupt_Connection, IO_Connection);
   type Resource_Role is (Produces_And_Consumes, Consumes_Resource);
   type Interrupt_Mode is (Level_Triggered, Edge_Triggered);
   type Interrupt_Polarity is (Active_High, Active_Low, Active_Both);
   type IO_Restriction is (Unrestricted, Input_Only, Output_Only, Unrestricted_Preserve);
   type Pin_Configuration is new Firmware_Tables.Byte;
   type Drive_Hundredths_Milliampere is new Interfaces.Unsigned_16;
   type Debounce_Hundredths_Millisecond is new Interfaces.Unsigned_16;
   type Pin_Number is new Interfaces.Unsigned_16;
   No_Pin : constant Pin_Number := 16#FFFF#; -- Retained; consumers must ignore it.
   Pin_Bytes : constant Positive := 2;
   Maximum_Pin_Count : constant Positive := Payload_Length'Last / Pin_Bytes;
   subtype Pin_Count is Positive range 1 .. Maximum_Pin_Count;
   type Byte_Span is record
      First : Byte_Offset := 0;
      Length : Byte_Count := 0;
   end record;
   type Connection_Information (Kind : Connection_Kind := IO_Connection) is record
      case Kind is
         when Interrupt_Connection =>
            Mode : Interrupt_Mode;
            Polarity : Interrupt_Polarity;
            Wake_Capable : Boolean;
         when IO_Connection => Restriction : IO_Restriction;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Unsupported_Revision, Reserved_Connection_Type,
      Reserved_Bits_Set, Reserved_Configuration, Bad_Interrupt_Polarity,
      Bad_Source_Index, Bad_Offsets, Bad_Pin_Count);
   type Decode_Result (Status : Decode_Status := Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Information : Connection_Information;
            Role : Resource_Role;
            Shared : Boolean;
            Configuration : Pin_Configuration;
            Drive_Strength : Drive_Hundredths_Milliampere;
            Debounce_Timeout : Debounce_Hundredths_Millisecond;
            Count : Pin_Count;
            Pins, Source, Vendor : Byte_Span;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Offsets are absolute within this exact Data copy. Source is the complete
   -- opaque byte span (including a NUL if present); no syntax/name resolution.
   -- Preserve pin order, duplicate pins, No_Pin and vendor bytes. Interrupts
   -- require one pin; I/O may contain a caller-sized list. No fixed output array.
   function Matches_Wire_Header (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Matches_Wire_Offsets (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Matches_Wire_Lengths (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Matches_Wire_Common (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Matches_Wire_Connection (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global => null,
      Post => (if Decode'Result.Status = Decoded then
        Decode'Result.Next_Offset > Offset
        and then Decode'Result.Next_Offset <= Byte_Count (Data'Length)
        and then Decode'Result.Pins.First > Offset
        and then Decode'Result.Pins.Length = Byte_Count (Decode'Result.Count * Pin_Bytes)
        and then Decode'Result.Pins.First + Decode'Result.Pins.Length = Decode'Result.Source.First
        and then Decode'Result.Source.First + Decode'Result.Source.Length = Decode'Result.Vendor.First
        and then Decode'Result.Vendor.First + Decode'Result.Vendor.Length = Decode'Result.Next_Offset
        and then Matches_Wire_Header (Data, Offset, Decode'Result)
        and then Matches_Wire_Offsets (Data, Offset, Decode'Result)
        and then Matches_Wire_Lengths (Data, Offset, Decode'Result)
        and then Matches_Wire_Common (Data, Offset, Decode'Result)
        and then Matches_Wire_Connection (Data, Offset, Decode'Result));
   type Pin_Result (Available : Boolean := False) is record
      case Available is
         when True => Value : Pin_Number;
         when False => null;
      end case;
   end record;
   -- Revalidates the descriptor instead of trusting a caller-fabricated span.
   function Matches_Wire_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive; Item : Pin_Result) return Boolean with Ghost, Global => null,
      Annotate => (GNATprove, Inline_For_Proof);
   function Read_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive) return Pin_Result with Global => null,
      Post => Matches_Wire_Pin (Data, Offset, Index, Read_Pin'Result);
private
   use type Firmware_Tables.Byte;
   GPIO_Name : constant Item_Name := 12;
   Minimum_Length : constant Payload_Length := 22;
   Fixed_Bytes : constant Byte_Count := 23;
   Revision_Field : constant Byte_Offset := 3;
   Kind_Field : constant Byte_Offset := 4;
   General_Field : constant Byte_Offset := 5;
   Specific_Field : constant Byte_Offset := 7;
   Configuration_Field : constant Byte_Offset := 9;
   Drive_Field : constant Byte_Offset := 10;
   Debounce_Field : constant Byte_Offset := 12;
   Pins_Field : constant Byte_Offset := 14;
   Source_Index_Field : constant Byte_Offset := 16;
   Source_Field : constant Byte_Offset := 17;
   Vendor_Field : constant Byte_Offset := 19;
   Vendor_Length_Field : constant Byte_Offset := 21;
   Revision_One : constant Firmware_Tables.Byte := 1;
   General_Reserved : constant Interfaces.Unsigned_16 := 16#FFFE#;
   Interrupt_Reserved : constant Interfaces.Unsigned_16 := 16#FFE0#;
   IO_Reserved : constant Interfaces.Unsigned_16 := 16#FFF4#;
   Mode_Bit : constant Interfaces.Unsigned_16 := 1;
   Polarity_Divisor : constant Interfaces.Unsigned_16 := 2;
   Two_Bits : constant Interfaces.Unsigned_16 := 3;
   Shared_Bit : constant Interfaces.Unsigned_16 := 8;
   Wake_Bit : constant Interfaces.Unsigned_16 := 16;
   Last_Standard_Configuration : constant Firmware_Tables.Byte := 3;
   First_Vendor_Configuration : constant Firmware_Tables.Byte := 128;
   Word_Bytes : constant Byte_Count := Byte_Count (Pin_Bytes);
   Byte_Radix : constant Interfaces.Unsigned_16 := 256;
   function Matches_Wire_Header (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean is
     (if Item.Status /= Decoded or else Offset > Byte_Count (Data'Length)
       or else Byte_Count (Data'Length) - Offset < Minimum_Connection_Bytes then False
      else
       (declare Framing : constant Frame := Read_Frame (Data, Offset); begin
          Framing.Status = ACPI_Resource_Data.Decoded
          and then Framing.Encoding = Large_Item and then Framing.Name = GPIO_Name
          and then Framing.Length >= Minimum_Length and then Item.Next_Offset = Framing.Next_Offset));
   function Matches_Wire_Offsets (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean is
     (if Item.Status /= Decoded or else Offset > Byte_Count (Data'Length)
       or else Byte_Count (Data'Length) - Offset < Minimum_Connection_Bytes then False
      else Item.Pins.First >= Offset
        and then Item.Pins.First - Offset = Byte_Count (Wire_Word (Data, Offset + Pins_Field))
        and then Item.Source.First >= Offset
        and then Item.Source.First - Offset = Byte_Count (Wire_Word (Data, Offset + Source_Field))
        and then Item.Vendor.First >= Offset
        and then Item.Vendor.First - Offset = Byte_Count (Wire_Word (Data, Offset + Vendor_Field)));
   function Matches_Wire_Lengths (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean is
     (if Item.Status /= Decoded or else Offset > Byte_Count (Data'Length)
       or else Byte_Count (Data'Length) - Offset < Minimum_Connection_Bytes
       or else Item.Next_Offset <= Offset then False
      else
       (declare
           Pins : constant Byte_Count := Byte_Count (Wire_Word (Data, Offset + Pins_Field));
           Source : constant Byte_Count := Byte_Count (Wire_Word (Data, Offset + Source_Field));
           Vendor : constant Byte_Count := Byte_Count (Wire_Word (Data, Offset + Vendor_Field));
           Vendor_Length : constant Byte_Count := Byte_Count (Wire_Word (Data, Offset + Vendor_Length_Field));
        begin
          Pins >= Fixed_Bytes and then Source >= Pins
          and then Source - Pins >= Word_Bytes and then (Source - Pins) mod Word_Bytes = 0
          and then Vendor >= Source and then Vendor <= Item.Next_Offset - Offset
          and then Vendor_Length = Item.Next_Offset - Offset - Vendor
          and then Item.Pins.Length = Source - Pins
          and then Item.Source.Length = Vendor - Source and then Item.Vendor.Length = Vendor_Length
          and then Byte_Count (Item.Count) = (Source - Pins) / Word_Bytes));
   function Matches_Wire_Common (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean is
     (if Item.Status /= Decoded or else Offset > Byte_Count (Data'Length)
       or else Byte_Count (Data'Length) - Offset < Minimum_Connection_Bytes then False
      else
       (declare
           General : constant Interfaces.Unsigned_16 := Wire_Word (Data, Offset + General_Field);
           Specific : constant Interfaces.Unsigned_16 := Wire_Word (Data, Offset + Specific_Field);
           Mode_Radix : constant Interfaces.Unsigned_16 := 2;
           Sharing_Divisor : constant Interfaces.Unsigned_16 := 8;
        begin
          Data (Data'First + Natural (Offset + Revision_Field)) = Revision_One
          and then Data (Data'First + Natural (Offset + Kind_Field)) <= 1
          and then General <= 1
          and then Data (Data'First + Natural (Offset + Source_Index_Field)) = 0
          and then (Data (Data'First + Natural (Offset + Configuration_Field)) <= 3
            or else Data (Data'First + Natural (Offset + Configuration_Field)) >= 128)
          and then Item.Configuration = Pin_Configuration (Data (Data'First + Natural (Offset + Configuration_Field)))
          and then Interfaces.Unsigned_16 (Item.Drive_Strength) = Wire_Word (Data, Offset + Drive_Field)
          and then Interfaces.Unsigned_16 (Item.Debounce_Timeout) = Wire_Word (Data, Offset + Debounce_Field)
          and then Item.Role = (if General = 0 then Produces_And_Consumes else Consumes_Resource)
          and then Item.Shared = ((Specific / Sharing_Divisor) mod Mode_Radix = 1)
          and then Item.Information.Kind =
            (if Data (Data'First + Natural (Offset + Kind_Field)) = 0 then Interrupt_Connection else IO_Connection)));
   function Matches_Wire_Connection (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Item : Decode_Result) return Boolean is
     (if Item.Status /= Decoded or else Offset > Byte_Count (Data'Length)
       or else Byte_Count (Data'Length) - Offset < Minimum_Connection_Bytes then False
      else
       (declare
           Specific : constant Interfaces.Unsigned_16 := Wire_Word (Data, Offset + Specific_Field);
           Mode_Radix : constant Interfaces.Unsigned_16 := 2;
           Polarity_Radix : constant Interfaces.Unsigned_16 := 4;
           Wake_Divisor : constant Interfaces.Unsigned_16 := 16;
        begin
          (if Item.Information.Kind = Interrupt_Connection then
            Specific < 32 and then (Specific / Mode_Radix) mod Polarity_Radix <= 2
            and then ((Specific / Mode_Radix) mod Polarity_Radix /= 2 or else Specific mod Mode_Radix = 1)
            and then Item.Count = 1
            and then Item.Information.Mode = (if Specific mod Mode_Radix = 0 then Level_Triggered else Edge_Triggered)
            and then Interrupt_Polarity'Pos (Item.Information.Polarity) = Natural ((Specific / Mode_Radix) mod Polarity_Radix)
            and then Item.Information.Wake_Capable = ((Specific / Wake_Divisor) mod Mode_Radix = 1)
           else Specific < 16 and then (Specific / Polarity_Radix) mod Mode_Radix = 0
            and then IO_Restriction'Pos (Item.Information.Restriction) = Natural (Specific mod Polarity_Radix))));
   function Matches_Wire_Pin (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Index : Positive; Item : Pin_Result) return Boolean is
     (declare
         Description : constant Decode_Result := Decode (Data, Offset);
      begin
        (if Description.Status /= Decoded or else Index > Description.Count then
           not Item.Available
         else Item.Available and then
           (declare
               Position : constant Byte_Offset := Description.Pins.First + Byte_Count ((Index - 1) * Pin_Bytes);
               Pin_Byte_Radix : constant Pin_Number := 256;
            begin Item.Value = Pin_Number (Data (Data'First + Natural (Position)))
              + Pin_Byte_Radix * Pin_Number (Data (Data'First + Natural (Position) + 1)))));
end ACPI_Resource_Data.GPIO_Connections;
