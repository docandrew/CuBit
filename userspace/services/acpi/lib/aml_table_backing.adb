pragma Ada_2022;
package body AML_Table_Backing with SPARK_Mode is
   function Valid_Span (Input : aliased State; Index : Positive) return Boolean is
     (Index <= Input.Table_Capacity and then
      Input.Tables (Index).Extent >= Firmware_Tables.Table_Header_Size and then
      Input.Tables (Index).Offset <= Input.Byte_Capacity and then
      Input.Tables (Index).Extent <= Input.Byte_Capacity - Input.Tables (Index).Offset);

   function Matches_Table
     (Input : aliased State; Index : Positive;
      Requested : Firmware_Tables.Identifiers.Selection) return Boolean
   is
   begin
      if Input.Count > Input.Table_Capacity or else Index > Input.Count
        or else not Valid_Span (Input, Index)
      then return False; end if;
      return Firmware_Tables.Identifiers.Matches
        (Firmware_Tables.Identifiers.Read_Identity
           (Input.Data.Bytes (Input.Tables (Index).Offset + 1 ..
              Input.Tables (Index).Offset + Firmware_Tables.Table_Header_Size)), Requested);
   end Matches_Table;

   function Find_Table
     (Input : aliased State; Requested : Firmware_Tables.Identifiers.Selection)
      return Natural
   is
   begin
      if Input.Count > Input.Table_Capacity then return 0; end if;
      for I in 1 .. Input.Count loop
         if not Valid_Span (Input, I) then return 0; end if;
      end loop;
      for I in 1 .. Input.Count loop
         if Matches_Table (Input, I, Requested) then return I; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not Matches_Table (Input, J, Requested));
      end loop;
      return 0;
   end Find_Table;

   function Valid_Field (Input : aliased State; Field : Field_Span) return Boolean is
     (Input.Count <= Input.Table_Capacity and then Field.Table <= Input.Count
      and then Valid_Span (Input, Field.Table)
      and then Field.Extent = Input.Tables (Field.Table).Extent
      and then AML_Field_Data.Fits (Field.Extent, Field.Offset, Field.Bits));

   function Validate_Field
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural)
      return Field_Result is
   begin
      if Input.Count > Input.Table_Capacity or else Table > Input.Count
        or else not Valid_Span (Input, Table)
        or else Extent /= Input.Tables (Table).Extent
      then return (Status => AML_Decode.Malformed); end if;
      if not AML_Field_Data.Fits (Extent, Offset, Bits) then
         return (Status => AML_Decode.Truncated);
      end if;
      return (Status => AML_Decode.Accepted, Field => (Table, Extent, Offset, Bits));
   end Validate_Field;

   function Source_Bit
     (Input : aliased State; Table : Positive; Offset : Natural) return Boolean is
     ((Natural (Input.Data.Bytes (Input.Tables (Table).Offset + Offset / 8 + 1)) /
       AML_Field_Data.Scale (Offset mod 8)) mod 2 = 1);

   function Read_Byte
     (Input : aliased State; Field : Field_Span; Index : Natural)
      return AML_Decode.Byte
   is
      Bit_Offset : constant Natural := Field.Offset + Index * 8;
      Count : constant Positive := Positive'Min (8, Field.Bits - Index * 8);
      Address : constant Positive := Input.Tables (Field.Table).Offset + Bit_Offset / 8 + 1;
      Value : Natural := Natural (Input.Data.Bytes (Address)) / AML_Field_Data.Scale (Bit_Offset mod 8);
   begin
      if Count + Bit_Offset mod 8 > 8 then
         Value := Value + Natural (Input.Data.Bytes (Address + 1)) * AML_Field_Data.Scale (8 - Bit_Offset mod 8);
      end if;
      return AML_Decode.Byte (Value mod AML_Field_Data.Scale (Count));
   end Read_Byte;

   function Read_Window
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural;
      First_Byte, Count : Natural) return Window_Result
   is
      Validated : constant Field_Result := Validate_Field (Input, Table, Extent, Offset, Bits);
      Content : AML_Decode.Buffer_Storage := [others => 0];
      Total : Natural;
   begin
      case Validated.Status is
         when AML_Decode.Accepted => null;
         when AML_Decode.Truncated => return (Status => AML_Decode.Truncated);
         when AML_Decode.Unsupported => return (Status => AML_Decode.Unsupported);
         when AML_Decode.Malformed => return (Status => AML_Decode.Malformed);
         when AML_Decode.Limit_Exceeded => return (Status => AML_Decode.Limit_Exceeded);
      end case;
      Total := Field_Length (Validated.Field);
      if Count > AML_Decode.Max_Buffer_Length then return (Status => AML_Decode.Limit_Exceeded); end if;
      if First_Byte > Total or else Count > Total - First_Byte then return (Status => AML_Decode.Truncated); end if;
      for I in 1 .. Count loop
         Content (I) := Read_Byte (Input, Validated.Field, First_Byte + I - 1);
         pragma Loop_Invariant
           (for all J in 1 .. I => Content (J) =
             Read_Byte (Input, Validated.Field, First_Byte + J - 1));
         pragma Loop_Invariant
           (for all J in I + 1 .. AML_Decode.Max_Buffer_Length => Content (J) = 0);
      end loop;
      return (Status => AML_Decode.Accepted, Total_Length => Total,
              First_Byte => First_Byte, Length => Count, Content => Content);
   end Read_Window;

   function Read_Field
     (Input : aliased State; Table : Positive; Extent, Offset, Bits : Natural)
      return AML_Field_Data.Read_Result is
   begin
      if Input.Count > Input.Table_Capacity or else Table > Input.Count then
         return (Status => AML_Decode.Malformed);
      end if;
      declare
         Item : constant Span := Input.Tables (Table);
      begin
         if Extent /= Item.Extent or else Item.Extent < Firmware_Tables.Table_Header_Size
           or else Item.Offset > Input.Byte_Capacity
           or else Item.Extent > Input.Byte_Capacity - Item.Offset
         then return (Status => AML_Decode.Malformed); end if;
         return AML_Field_Data.Read_Bits
           (AML_Decode.Bytes (Input.Data.Bytes (Item.Offset + 1 .. Item.Offset + Item.Extent)),
            Offset, Bits);
      end;
   end Read_Field;
end AML_Table_Backing;
