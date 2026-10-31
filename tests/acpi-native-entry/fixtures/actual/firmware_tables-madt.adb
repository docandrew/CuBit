pragma Ada_2022;
with Interfaces; use Interfaces;
package body Firmware_Tables.MADT with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   Byte_Width : constant := 1;
   Word_Width : constant := 2;
   DWord_Width : constant := 4;
   QWord_Width : constant := 8;
   Length_Offset : constant := 1;
   CPU_UID_Offset : constant := 2;
   CPU_ID_Offset : constant := 3;
   CPU_Flags_Offset : constant := 4;
   IO_ID_Offset : constant := 2;
   IO_Address_Offset : constant := 4;
   IO_Base_Offset : constant := 8;
   Bus_Offset : constant := 2;
   Source_Offset : constant := 3;
   Global_Interrupt_Offset : constant := 4;
   Override_Flags_Offset : constant := 8;
   NMI_Flags_Offset : constant := 2;
   Local_NMI_Flags_Offset : constant := 3;
   Local_LINT_Offset : constant := 5;
   Override_Address_Offset : constant := 4;
   X2_ID_Offset : constant := 4;
   X2_Flags_Offset : constant := 8;
   X2_UID_Offset : constant := 12;
   X2_NMI_UID_Offset : constant := 4;
   X2_LINT_Offset : constant := 8;
   Local_APIC_Code : constant Byte := 0;
   IO_APIC_Code : constant Byte := 1;
   Source_Override_Code : constant Byte := 2;
   NMI_Source_Code : constant Byte := 3;
   Local_NMI_Code : constant Byte := 4;
   Address_Override_Code : constant Byte := 5;
   X2APIC_Code : constant Byte := 9;
   X2APIC_NMI_Code : constant Byte := 10;
   Local_APIC_Size : constant := 8;
   IO_APIC_Size : constant := 12;
   Source_Override_Size : constant := 10;
   NMI_Source_Size : constant := 8;
   Local_NMI_Size : constant := 6;
   Address_Override_Size : constant := 12;
   X2APIC_Size : constant := 16;
   X2APIC_NMI_Size : constant := 12;

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
   function Kind (Code : Byte) return Record_Kind is
     (case Code is when Local_APIC_Code => Local_APIC,
      when IO_APIC_Code => IO_APIC,
      when Source_Override_Code => Source_Override,
      when NMI_Source_Code => NMI_Source,
      when Local_NMI_Code => Local_NMI,
      when Address_Override_Code => Address_Override,
      when X2APIC_Code => Local_X2APIC,
      when X2APIC_NMI_Code => X2APIC_NMI, when others => Unknown);
   function Minimum (K : Record_Kind) return Record_Length is
     (case K is when Local_APIC => Local_APIC_Size,
      when NMI_Source => NMI_Source_Size, when IO_APIC => IO_APIC_Size,
      when Address_Override => Address_Override_Size,
      when X2APIC_NMI => X2APIC_NMI_Size,
      when Source_Override => Source_Override_Size,
      when Local_NMI => Local_NMI_Size,
      when Local_X2APIC => X2APIC_Size, when Unknown => Record_Header_Size);
   function Decode (Data : Bytes) return Table_Metadata is
      Header : constant Table_Result := Read_Table (Data, "APIC");
      Cursor : Natural := Fixed_Size;
      Count : Record_Count := 0;
   begin
      if Header.Status /= Accepted then return (Valid => False); end if;
      if Header.Extent /= Data'Length or else Header.Extent < Fixed_Size then
         return (Valid => False);
      end if;
      while Cursor < Data'Length loop
         if Data'Length - Cursor < Record_Header_Size then
            return (Valid => False);
         end if;
         declare
            Code : constant Byte := Data (Data'First + Cursor);
            Size : constant Natural := Natural (Data (Data'First + Cursor + Length_Offset));
         begin
            if Size < Minimum (Kind (Code)) or else Size > Data'Length - Cursor
            then return (Valid => False); end if;
            Cursor := Cursor + Size;
            Count := Count + 1;
         end;
      end loop;
      return (Valid => True, Revision => Header.Revision,
              Local_Address => LE (Data, Table_Header_Size, DWord_Width),
              Flags => Unsigned_32 (LE (Data, Table_Header_Size + DWord_Width, DWord_Width)),
              Count => Count);
   end Decode;
   function Read_Record (Data : Bytes; Index : Natural) return Record_Result is
      Table : constant Table_Metadata := Decode (Data);
      Cursor : Natural := Fixed_Size;
   begin
      if not Table.Valid or else Index = 0 or else Index > Table.Count then
         return (Valid => False);
      end if;
      for I in 1 .. Index - 1 loop
         Cursor := Cursor + Natural (Data (Data'First + Cursor + Length_Offset));
      end loop;
      declare
         Code : constant Byte := Data (Data'First + Cursor);
         K : constant Record_Kind := Kind (Code);
         V : Record_Data (K);
         function U (Offset : Natural; Width : Positive) return Unsigned_64 is
           (LE (Data, Cursor + Offset, Width));
      begin
         V.Wire_Type := Code;
         V.Offset := Cursor;
         V.Length := Natural (U (Length_Offset, Byte_Width));
         case K is
            when Local_APIC =>
               V.UID := Unsigned_32 (U (CPU_UID_Offset, Byte_Width));
               V.Controller := Unsigned_32 (U (CPU_ID_Offset, Byte_Width));
               V.CPU_Flags := Unsigned_32 (U (CPU_Flags_Offset, DWord_Width));
            when IO_APIC =>
               V.IO_ID := Byte (U (IO_ID_Offset, Byte_Width));
               V.IO_Address := U (IO_Address_Offset, DWord_Width);
               V.Interrupt_Base := Unsigned_32 (U (IO_Base_Offset, DWord_Width));
            when Source_Override =>
               V.Bus := Byte (U (Bus_Offset, Byte_Width)); V.Source := Byte (U (Source_Offset, Byte_Width));
               V.Global_Interrupt := Unsigned_32 (U (Global_Interrupt_Offset, DWord_Width));
               V.Override_Flags := Unsigned_16 (U (Override_Flags_Offset, Word_Width));
            when NMI_Source =>
               V.NMI_Flags := Unsigned_16 (U (NMI_Flags_Offset, Word_Width));
               V.NMI_Interrupt := Unsigned_32 (U (Global_Interrupt_Offset, DWord_Width));
            when Local_NMI =>
               V.NMI_UID := Unsigned_32 (U (CPU_UID_Offset, Byte_Width));
               V.Local_Flags := Unsigned_16 (U (Local_NMI_Flags_Offset, Word_Width));
               V.LINT := Byte (U (Local_LINT_Offset, Byte_Width));
            when Address_Override => V.Address := U (Override_Address_Offset, QWord_Width);
            when Local_X2APIC =>
               V.Controller := Unsigned_32 (U (X2_ID_Offset, DWord_Width));
               V.CPU_Flags := Unsigned_32 (U (X2_Flags_Offset, DWord_Width));
               V.UID := Unsigned_32 (U (X2_UID_Offset, DWord_Width));
            when X2APIC_NMI =>
               V.Local_Flags := Unsigned_16 (U (NMI_Flags_Offset, Word_Width));
               V.NMI_UID := Unsigned_32 (U (X2_NMI_UID_Offset, DWord_Width));
               V.LINT := Byte (U (X2_LINT_Offset, Byte_Width));
            when Unknown => null;
         end case;
         return (Valid => True, Value => V);
      end;
   end Read_Record;
end Firmware_Tables.MADT;
