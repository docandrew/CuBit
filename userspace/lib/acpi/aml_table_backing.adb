pragma Ada_2022;
with AML_Decode;
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
           (Input.Data (Input.Tables (Index).Offset + 1 ..
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
           (AML_Decode.Bytes (Input.Data (Item.Offset + 1 .. Item.Offset + Item.Extent)),
            Offset, Bits);
      end;
   end Read_Field;
end AML_Table_Backing;
