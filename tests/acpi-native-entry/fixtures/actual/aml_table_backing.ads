pragma Ada_2022;
with Firmware_Tables;
with AML_Byte_Sources;
with Firmware_Tables.Identifiers;
with AML_Field_Data;
with AML_Decode;
-- Owned bytes and bounded table spans. This is not a physical-memory view.
-- Explicit aliased inputs are passed by reference, including through the
-- generic executor. The service owns construction and keeps this input immutable
-- throughout invocation; defensive read checks do not trust supplied spans.
package AML_Table_Backing with SPARK_Mode, Pure is
   use type AML_Decode.Status;
   use type AML_Decode.Byte;
   type Span is record
      Offset, Extent : Natural := 0;
   end record;
   type Spans is array (Positive range <>) of Span;
   type State (Table_Capacity, Byte_Capacity : Positive) is record
      Count : Natural := 0;
      Tables : Spans (1 .. Table_Capacity);
      Data : aliased AML_Byte_Sources.Storage (Byte_Capacity);
   end record;
   function Valid_Span (Input : aliased State; Index : Positive) return Boolean with
      Post => (if Valid_Span'Result then
         Index <= Input.Table_Capacity and then
         Input.Tables (Index).Extent >= Firmware_Tables.Table_Header_Size and then
         Input.Tables (Index).Offset <= Input.Byte_Capacity and then
         Input.Tables (Index).Extent <= Input.Byte_Capacity - Input.Tables (Index).Offset);
   -- Matching and selection inspect only owned table headers; an index is not
   -- a hardware capability or a physical address. Malformed inventories fail.
   function Matches_Table
     (Input : aliased State; Index : Positive;
      Requested : Firmware_Tables.Identifiers.Selection) return Boolean;
   function Find_Table
     (Input : aliased State; Requested : Firmware_Tables.Identifiers.Selection)
      return Natural with
      Post => Find_Table'Result <= Input.Table_Capacity and then
        (if Find_Table'Result > 0 then
           Find_Table'Result <= Input.Count and then
           Matches_Table (Input, Find_Table'Result, Requested) and then
           (for all I in 1 .. Find_Table'Result - 1 =>
              not Matches_Table (Input, I, Requested)));
   -- A descriptor is meaningful only with the same immutable owned Input.
   -- Valid_Field rechecks the inventory and bounds; this grants no authority.
   type Field_Span is record
      Table : Positive := 1;
      Extent, Offset, Bits : Natural := 0;
   end record;
   function Field_Length (Field : Field_Span) return Natural is
     (Field.Bits / 8 + (if Field.Bits mod 8 = 0 then 0 else 1));
   function Valid_Field (Input : aliased State; Field : Field_Span) return Boolean;
   type Field_Result (Status : AML_Decode.Status := AML_Decode.Truncated) is record
      case Status is
         when AML_Decode.Accepted => Field : Field_Span;
         when others => null;
      end case;
   end record;
   function Validate_Field
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural)
      return Field_Result
     with Post => (if Validate_Field'Result.Status = AML_Decode.Accepted then
       Validate_Field'Result.Field = (Table, Extent, Offset, Bits)
       and then Valid_Field (Input, Validate_Field'Result.Field));
   subtype Bit_In_Byte is Natural range 0 .. 7;
   function Source_Bit
     (Input : aliased State; Table : Positive; Offset : Natural) return Boolean
     with Pre => Input.Count <= Input.Table_Capacity and then Table <= Input.Count
       and then Valid_Span (Input, Table)
       and then Offset / 8 < Input.Tables (Table).Extent;
   function Read_Byte
     (Input : aliased State; Field : Field_Span; Index : Natural)
      return AML_Decode.Byte
     with Pre => Valid_Field (Input, Field) and then Index < Field_Length (Field),
       Post => (for all Bit in Bit_In_Byte =>
         ((Natural (Read_Byte'Result) / AML_Field_Data.Scale (Bit)) mod 2 = 1) =
           (Bit < Field.Bits - Index * 8 and then
              Source_Bit (Input, Field.Table, Field.Offset + Index * 8 + Bit)));
   type Window_Result (Status : AML_Decode.Status := AML_Decode.Truncated) is record
      case Status is
         when AML_Decode.Accepted =>
            Total_Length, First_Byte : Natural;
            Length : Natural range 0 .. AML_Decode.Max_Buffer_Length;
            Content : AML_Decode.Buffer_Storage;
         when others => null;
      end case;
   end record;
   -- Count is the exact requested result window, never an implicit truncation.
   function Read_Window
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural;
      First_Byte, Count : Natural) return Window_Result
     with Post => (if Read_Window'Result.Status = AML_Decode.Accepted then
       Valid_Field (Input, (Table, Extent, Offset, Bits))
       and then Read_Window'Result.Total_Length = Field_Length ((Table, Extent, Offset, Bits))
       and then Read_Window'Result.First_Byte = First_Byte
       and then Read_Window'Result.Length = Count
       and then First_Byte <= Read_Window'Result.Total_Length
       and then Count <= Read_Window'Result.Total_Length - First_Byte
       and then (for all I in 1 .. Count => Read_Window'Result.Content (I) =
         Read_Byte (Input, (Table, Extent, Offset, Bits), First_Byte + I - 1))
       and then (for all I in Count + 1 .. AML_Decode.Max_Buffer_Length =>
         Read_Window'Result.Content (I) = 0));
   function Read_Field
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural)
      return AML_Field_Data.Read_Result;
end AML_Table_Backing;
