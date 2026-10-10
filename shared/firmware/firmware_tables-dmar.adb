pragma Ada_2022;
with Interfaces; use Interfaces;
package body Firmware_Tables.DMAR with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   Byte_Width : constant := 1;
   Word_Width : constant := 2;
   DWord_Width : constant := 4;
   QWord_Width : constant := 8;
   Record_Length_Offset : constant := 2;
   Scope_Length_Offset : constant := 1;
   Flags_Offset : constant := 4;
   Register_Size_Offset : constant := 5;
   Segment_Offset : constant := 6;
   Address_Offset : constant := 8;
   Limit_Offset : constant := 16;
   Domain_Offset : constant := 16;
   Device_Number_Offset : constant := 7;
   Name_Offset : constant := 8;
   Scope_Flags_Offset : constant := 2;
   Scope_Reserved_Offset : constant := 3;
   Enumeration_Offset : constant := 4;
   Bus_Offset : constant := 5;
   Hardware_Unit_Size : constant := 16;
   Reserved_Memory_Size : constant := 24;
   Cache_Size : constant := 8;
   Hardware_Affinity_Size : constant := 20;
   Namespace_Minimum_Size : constant := Name_Offset + Byte_Width;
   Endpoint_Code : constant Byte := 1;
   Bridge_Code : constant Byte := 2;
   IOAPIC_Code : constant Byte := 3;
   HPET_Code : constant Byte := 4;
   Namespace_Code : constant Byte := 5;
   function LE (Data : Bytes; Offset : Natural; Width : Positive)
     return Unsigned_64
     with Pre => Width <= QWord_Width and then Data'Length >= Width
       and then Offset <= Data'Length - Width
   is
      V : Unsigned_64 := 0;
   begin
      for I in 0 .. Width - 1 loop
         V := V or Shift_Left (Unsigned_64 (Data (Data'First + Offset + I)),
                               Bits_Per_Byte * I);
      end loop;
      return V;
   end LE;
   Hardware_Unit_Code : constant Unsigned_16 := 0;
   Reserved_Memory_Code : constant Unsigned_16 := 1;
   ATS_Root_Code : constant Unsigned_16 := 2;
   Hardware_Affinity_Code : constant Unsigned_16 := 3;
   Namespace_Device_Code : constant Unsigned_16 := 4;
   SATC_Code : constant Unsigned_16 := 5;
   SIDP_Code : constant Unsigned_16 := 6;
   function Kind (Code : Unsigned_16) return Record_Kind is
     (case Code is when Hardware_Unit_Code => Hardware_Unit,
      when Reserved_Memory_Code => Reserved_Memory,
      when ATS_Root_Code => ATS_Root,
      when Hardware_Affinity_Code => Hardware_Affinity,
      when Namespace_Device_Code => Namespace_Device,
      when SATC_Code => SATC,
      when SIDP_Code => SIDP,
      when others => Unknown);
   function Minimum (K : Record_Kind) return Record_Length is
     (case K is when Hardware_Unit => Hardware_Unit_Size,
      when Reserved_Memory => Reserved_Memory_Size,
      when ATS_Root | SATC | SIDP => Cache_Size,
      when Hardware_Affinity => Hardware_Affinity_Size,
      when Namespace_Device => Namespace_Minimum_Size,
      when Unknown => Record_Header_Size);
   function Has_Scopes (K : Record_Kind) return Boolean is
     (K in Hardware_Unit | Reserved_Memory | ATS_Root | SATC | SIDP);
   function Known_Path (Code : Byte) return Boolean is
     (Code in Endpoint_Code | Bridge_Code | IOAPIC_Code | HPET_Code | Namespace_Code);
   type Scope_Scan (Valid : Boolean := False) is record
      case Valid is
         when True => Count : Scope_Count;
         when False => null;
      end case;
   end record;
   function Scan_Scopes (Data : Bytes; Start, Size : Natural) return Scope_Scan
     with Pre => Start <= Data'Length and then Size <= Data'Length - Start
       and then Size <= Record_Length'Last
   is
      Used : Natural := 0;
      Count : Scope_Count := 0;
   begin
      while Used < Size loop
         if Size - Used < Scope_Header_Size then return (Valid => False); end if;
         declare
            Offset : constant Natural := Start + Used;
            Code : constant Byte := Data (Data'First + Offset);
            Length : constant Natural := Natural (Data (Data'First + Offset + Scope_Length_Offset));
         begin
            if Length < Scope_Header_Size or else Length > Size - Used then
               return (Valid => False);
            end if;
            if Known_Path (Code) then
               if Length < Scope_Header_Size + Path_Pair_Size
                 or else (Length - Scope_Header_Size) mod Path_Pair_Size /= 0
                 or else (Code in HPET_Code | Namespace_Code
                          and then Length /= Scope_Header_Size + Path_Pair_Size)
               then return (Valid => False); end if;
            end if;
            Used := Used + Length;
            Count := Count + 1;
         end;
      end loop;
      return (Valid => True, Count => Count);
   end Scan_Scopes;
   function Name_Size (Data : Bytes; Start : Natural; Size : Positive) return Natural
     with Pre => Start <= Data'Length and then Size <= Data'Length - Start
   is
   begin
      for I in 0 .. Size - 1 loop
         if Data (Data'First + Start + I) = 0 then return I; end if;
      end loop;
      return Size;
   end Name_Size;
   function Decode (Data : Bytes) return Table_Metadata is
      Header : constant Table_Result := Read_Table (Data, "DMAR");
      Cursor : Natural := Fixed_Size;
      Count : Record_Count := 0;
   begin
      if Header.Status /= Accepted then return (Valid => False); end if;
      if Header.Extent /= Data'Length or else Data'Length < Fixed_Size then
         return (Valid => False);
      end if;
      while Cursor < Data'Length loop
         if Data'Length - Cursor < Record_Header_Size then return (Valid => False); end if;
         declare
            K : constant Record_Kind := Kind (Unsigned_16 (LE (Data, Cursor, Word_Width)));
            Size : constant Natural := Natural (LE (Data, Cursor + Record_Length_Offset, Word_Width));
         begin
            if Size < Minimum (K) or else Size > Data'Length - Cursor then
               return (Valid => False);
            end if;
            if Has_Scopes (K) then
               if not Scan_Scopes (Data, Cursor + Minimum (K), Size - Minimum (K)).Valid
               then return (Valid => False); end if;
            elsif K = Namespace_Device then
               if Name_Size (Data, Cursor + Name_Offset, Size - Name_Offset) = Size - Name_Offset
               then return (Valid => False); end if;
            end if;
            Cursor := Cursor + Size;
            Count := Count + 1;
         end;
      end loop;
      return (Valid => True, Revision => Header.Revision,
              Host_Width => Data (Data'First + Table_Header_Size),
              Flags => Data (Data'First + Table_Header_Size + Byte_Width), Count => Count);
   end Decode;
   function Read_Record (Data : Bytes; Index : Natural) return Record_Result is
      Table : constant Table_Metadata := Decode (Data);
      Cursor : Natural := Fixed_Size;
   begin
      if not Table.Valid or else Index = 0 or else Index > Table.Count then
         return (Valid => False);
      end if;
      for I in 1 .. Index - 1 loop
         Cursor := Cursor + Natural (LE (Data, Cursor + Record_Length_Offset, Word_Width));
      end loop;
      declare
         Code : constant Unsigned_16 := Unsigned_16 (LE (Data, Cursor, Word_Width));
         K : constant Record_Kind := Kind (Code);
         V : Record_Data (K);
         function U (Offset : Natural; Width : Positive) return Unsigned_64 is
           (LE (Data, Cursor + Offset, Width));
      begin
         V.Wire_Type := Code; V.Offset := Cursor;
         V.Length := Natural (U (Record_Length_Offset, Word_Width));
         V.Scopes := 0;
         if Has_Scopes (K) then
            V.Scopes := Scan_Scopes (Data, Cursor + Minimum (K), V.Length - Minimum (K)).Count;
         end if;
         case K is
            when Hardware_Unit =>
               V.Unit_Flags := Byte (U (Flags_Offset, Byte_Width));
               V.Register_Size := Byte (U (Register_Size_Offset, Byte_Width));
               V.Unit_Segment := Segment_Number (U (Segment_Offset, Word_Width));
               V.Register_Base := U (Address_Offset, QWord_Width);
            when Reserved_Memory =>
               V.Memory_Segment := Segment_Number (U (Segment_Offset, Word_Width));
               V.Base_Address := U (Address_Offset, QWord_Width);
               V.Inclusive_Limit := U (Limit_Offset, QWord_Width);
            when ATS_Root | SATC =>
               V.Cache_Flags := Byte (U (Flags_Offset, Byte_Width));
               V.Cache_Segment := Segment_Number (U (Segment_Offset, Word_Width));
            when Hardware_Affinity =>
               V.Affinity_Base := U (Address_Offset, QWord_Width);
               V.Domain := Proximity_Domain (U (Domain_Offset, DWord_Width));
            when Namespace_Device =>
               V.Device_Number := Byte (U (Device_Number_Offset, Byte_Width));
               V.Name_Offset := Cursor + Name_Offset;
               V.Name_Length := Name_Size (Data, V.Name_Offset, V.Length - Name_Offset);
            when SIDP => V.Device_Segment := Segment_Number (U (Segment_Offset, Word_Width));
            when Unknown => null;
         end case;
         return (Valid => True, Value => V);
      end;
   end Read_Record;
   function Read_Scope (Data : Bytes; Record_Index, Scope_Index : Natural)
     return Scope_Result
   is
      R : constant Record_Result := Read_Record (Data, Record_Index);
      Cursor : Natural;
   begin
      if not R.Valid or else Scope_Index = 0 or else Scope_Index > R.Value.Scopes
      then return (Valid => False); end if;
      Cursor := R.Value.Offset + Minimum (R.Value.Kind);
      for I in 1 .. Scope_Index - 1 loop
         Cursor := Cursor + Natural (Data (Data'First + Cursor + Scope_Length_Offset));
      end loop;
      declare
         Code : constant Byte := Data (Data'First + Cursor);
         Size : constant Scope_Length := Natural (Data (Data'First + Cursor + Scope_Length_Offset));
      begin
         return (Valid => True, Wire_Type => Code, Offset => Cursor, Length => Size,
           Flags => Data (Data'First + Cursor + Scope_Flags_Offset),
           Reserved => Data (Data'First + Cursor + Scope_Reserved_Offset),
           Enumeration_ID => Data (Data'First + Cursor + Enumeration_Offset),
           Start_Bus => Data (Data'First + Cursor + Bus_Offset),
           Known_Path => Known_Path (Code),
           Paths => (if Known_Path (Code) then (Size - Scope_Header_Size) / Path_Pair_Size else 0));
      end;
   end Read_Scope;
   function Read_Path
     (Data : Bytes; Record_Index, Scope_Index, Path_Index : Natural)
     return Path_Result
   is
      S : constant Scope_Result := Read_Scope (Data, Record_Index, Scope_Index);
   begin
      if not S.Valid or else Path_Index = 0 or else Path_Index > S.Paths then
         return (Valid => False);
      end if;
      declare Offset : constant Natural :=
        S.Offset + Scope_Header_Size + (Path_Index - 1) * Path_Pair_Size;
      begin
         return (Valid => True, Device => Data (Data'First + Offset),
                 Func => Data (Data'First + Offset + Byte_Width));
      end;
   end Read_Path;
end Firmware_Tables.DMAR;
