pragma Ada_2022;
with Firmware_Tables;
with Firmware_Tables.Identifiers;
with AML_Field_Data;
-- Owned bytes and bounded table spans. This is not a physical-memory view.
-- Explicit aliased inputs are passed by reference, including through the
-- generic executor. The service owns construction and keeps this input immutable
-- throughout invocation; defensive read checks do not trust supplied spans.
package AML_Table_Backing with SPARK_Mode, Pure is
   type Span is record
      Offset, Extent : Natural := 0;
   end record;
   type Spans is array (Positive range <>) of Span;
   type State (Table_Capacity, Byte_Capacity : Positive) is record
      Count : Natural := 0;
      Tables : Spans (1 .. Table_Capacity);
      Data : Firmware_Tables.Bytes (1 .. Byte_Capacity) := [others => 0];
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
   function Read_Field
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural)
      return AML_Field_Data.Read_Result;
end AML_Table_Backing;
