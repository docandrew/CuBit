pragma Ada_2022;
with Interfaces; use Interfaces;
package body Firmware_Tables.MCFG with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   Base_Offset : constant := 0;
   Segment_Offset : constant := 8;
   First_Bus_Offset : constant := 10;
   Last_Bus_Offset : constant := 11;
   Reserved_Offset : constant := 12;
   Word_Width : constant := 2;
   DWord_Width : constant := 4;
   function Read_Word (Data : Bytes; Offset : Natural) return Unsigned_16
     with Pre => Data'Length >= Word_Width
       and then Offset <= Data'Length - Word_Width
   is
   begin
      return Unsigned_16 (Data (Data'First + Offset)) or
        Shift_Left (Unsigned_16 (Data (Data'First + Offset + 1)), Bits_Per_Byte);
   end Read_Word;
   function Read_DWord (Data : Bytes; Offset : Natural) return Unsigned_32
     with Pre => Data'Length >= DWord_Width
       and then Offset <= Data'Length - DWord_Width
   is
   begin
      return Unsigned_32 (Data (Data'First + Offset)) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), Bits_Per_Byte) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 2 * Bits_Per_Byte) or
      Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 3 * Bits_Per_Byte);
   end Read_DWord;
   function Read_LE (Data : Bytes; Offset : Natural; Width : Positive)
     return Unsigned_64
     with Pre => Width <= 8 and then Data'Length >= Width
       and then Offset <= Data'Length - Width
   is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. Width - 1 loop
         Value := Value or Shift_Left
           (Unsigned_64 (Data (Data'First + Offset + I)), Bits_Per_Byte * I);
      end loop;
      return Value;
   end Read_LE;
   function Decode (Data : Bytes) return Table_Metadata is
      Header : constant Table_Result := Read_Table (Data, "MCFG");
   begin
      if Header.Status /= Accepted then return (Valid => False); end if;
      if Header.Extent /= Data'Length or else Header.Extent < Fixed_Size
        or else (Header.Extent - Fixed_Size) mod Allocation_Size /= 0
      then return (Valid => False); end if;
      return (Valid => True, Revision => Header.Revision,
              Reserved => Read_LE (Data, Table_Header_Size, 8),
              Count => (Header.Extent - Fixed_Size) / Allocation_Size);
   end Decode;
   function Allocation_At (Data : Bytes; Index : Positive) return Allocation
     with Pre => Data'Length >= Fixed_Size + Allocation_Size
       and then Index <= (Data'Length - Fixed_Size) / Allocation_Size
   is
      Offset : constant Natural := Fixed_Size + (Index - 1) * Allocation_Size;
   begin
      return
           (Base => Read_LE (Data, Offset + Base_Offset, 8),
            Segment => Read_Word (Data, Offset + Segment_Offset),
            First_Bus => Data (Data'First + Offset + First_Bus_Offset),
            Last_Bus => Data (Data'First + Offset + Last_Bus_Offset),
            Reserved => Read_DWord (Data, Offset + Reserved_Offset));
   end Allocation_At;
   function Read_Allocation (Data : Bytes; Index : Natural)
     return Allocation_Result
   is
      Table : constant Table_Metadata := Decode (Data);
   begin
      if not Table.Valid or else Index = 0 or else Index > Table.Count then
         return (Valid => False);
      end if;
      return (Valid => True, Value => Allocation_At (Data, Index));
   end Read_Allocation;
   procedure Scan
     (Data : Bytes; State : in out Visitor_State; Table : out Table_Metadata)
   is
   begin
      Table := Decode (Data);
      if not Table.Valid then return; end if;
      for K in 1 .. Table.Count loop
         Visit (State, Allocation_At (Data, K));
      end loop;
   end Scan;
end Firmware_Tables.MCFG;
