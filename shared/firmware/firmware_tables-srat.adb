pragma Ada_2022;
with Interfaces; use Interfaces;
package body Firmware_Tables.SRAT with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   Byte_Width : constant := 1;
   QWord_Width : constant := 8;
   Length_Offset : constant := 1;
   Table_Revision_Width : constant := 4;
   Reserved_Offset : constant := Table_Header_Size + Table_Revision_Width;
   Local_APIC_Code : constant Byte := 0;
   Local_APIC_Size : constant := 16;
   Domain_Low_Offset : constant := 2;
   Domain_Low_Width : constant := 1;
   APIC_ID_Offset : constant := 3;
   APIC_ID_Width : constant := 1;
   APIC_Flags_Offset : constant := 4;
   APIC_Flags_Width : constant := 4;
   SAPIC_EID_Offset : constant := 8;
   SAPIC_EID_Width : constant := 1;
   Domain_High_Offset : constant := 9;
   Domain_High_Width : constant := 3;
   APIC_Clock_Offset : constant := 12;
   APIC_Clock_Width : constant := 4;
   Memory_Affinity_Code : constant Byte := 1;
   Memory_Affinity_Size : constant := 40;
   Memory_Domain_Offset : constant := 2;
   Memory_Domain_Width : constant := 4;
   Base_Address_Offset : constant := 8;
   Base_Address_Width : constant := 8;
   Address_Length_Offset : constant := 16;
   Address_Length_Width : constant := 8;
   Memory_Flags_Offset : constant := 28;
   Memory_Flags_Width : constant := 4;
   X2APIC_Code : constant Byte := 2;
   X2APIC_Size : constant := 24;
   X2_Domain_Offset : constant := 4;
   X2_Domain_Width : constant := 4;
   X2_ID_Offset : constant := 8;
   X2_ID_Width : constant := 4;
   X2_Flags_Offset : constant := 12;
   X2_Flags_Width : constant := 4;
   X2_Clock_Offset : constant := 16;
   X2_Clock_Width : constant := 4;
   GICC_Code : constant Byte := 3;
   GICC_Size : constant := 18;
   GICC_Domain_Offset : constant := 2;
   GICC_Domain_Width : constant := 4;
   GICC_UID_Offset : constant := 6;
   GICC_UID_Width : constant := 4;
   GICC_Flags_Offset : constant := 10;
   GICC_Flags_Width : constant := 4;
   GICC_Clock_Offset : constant := 14;
   GICC_Clock_Width : constant := 4;
   GIC_ITS_Code : constant Byte := 4;
   GIC_ITS_Size : constant := 12;
   ITS_Domain_Offset : constant := 2;
   ITS_Domain_Width : constant := 4;
   ITS_ID_Offset : constant := 8;
   ITS_ID_Width : constant := 4;
   Generic_Initiator_Code : constant Byte := 5;
   Generic_Initiator_Size : constant := 32;
   Handle_Type_Offset : constant := 3;
   Handle_Type_Width : constant := 1;
   Generic_Domain_Offset : constant := 4;
   Generic_Domain_Width : constant := 4;
   Handle_Offset : constant := 8;
   Generic_Flags_Offset : constant := 24;
   Generic_Flags_Width : constant := 4;
   Generic_Port_Code : constant Byte := 6;
   Generic_Port_Size : constant := 32;
   RINTC_Code : constant Byte := 7;
   RINTC_Size : constant := 20;
   RINTC_Domain_Offset : constant := 4;
   RINTC_Domain_Width : constant := 4;
   RINTC_UID_Offset : constant := 8;
   RINTC_UID_Width : constant := 4;
   RINTC_Flags_Offset : constant := 12;
   RINTC_Flags_Width : constant := 4;
   RINTC_Clock_Offset : constant := 16;
   RINTC_Clock_Width : constant := 4;
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
      when Memory_Affinity_Code => Memory_Affinity,
      when X2APIC_Code => X2APIC,
      when GICC_Code => GICC,
      when GIC_ITS_Code => GIC_ITS,
      when Generic_Initiator_Code => Generic_Initiator,
      when Generic_Port_Code => Generic_Port,
      when RINTC_Code => RINTC,
      when others => Unknown);
   function Minimum (K : Record_Kind) return Record_Length is
     (case K is when Local_APIC => Local_APIC_Size,
      when Memory_Affinity => Memory_Affinity_Size,
      when X2APIC => X2APIC_Size,
      when GICC => GICC_Size,
      when GIC_ITS => GIC_ITS_Size,
      when Generic_Initiator => Generic_Initiator_Size,
      when Generic_Port => Generic_Port_Size,
      when RINTC => RINTC_Size,
      when Unknown => Record_Header_Size);
   function Decode (Data : Bytes) return Table_Metadata is
      Header : constant Table_Result := Read_Table (Data, "SRAT");
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
              Table_Revision => Unsigned_32 (LE (Data, Table_Header_Size, Table_Revision_Width)),
              Reserved => LE (Data, Reserved_Offset, QWord_Width),
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
               V.Domain_Low := Byte (U (Domain_Low_Offset, Domain_Low_Width));
               V.APIC_ID := Byte (U (APIC_ID_Offset, APIC_ID_Width));
               V.APIC_Flags := Affinity_Flags (U (APIC_Flags_Offset, APIC_Flags_Width));
               V.SAPIC_EID := Byte (U (SAPIC_EID_Offset, SAPIC_EID_Width));
               V.Domain_High := Domain_High_Bits (U (Domain_High_Offset, Domain_High_Width));
               V.APIC_Clock := Clock_Domain (U (APIC_Clock_Offset, APIC_Clock_Width));
            when Memory_Affinity =>
               V.Memory_Domain := Proximity_Domain (U (Memory_Domain_Offset, Memory_Domain_Width));
               V.Base_Address := Address_Value (U (Base_Address_Offset, Base_Address_Width));
               V.Address_Length := Address_Value (U (Address_Length_Offset, Address_Length_Width));
               V.Memory_Flags := Affinity_Flags (U (Memory_Flags_Offset, Memory_Flags_Width));
            when X2APIC =>
               V.X2_Domain := Proximity_Domain (U (X2_Domain_Offset, X2_Domain_Width));
               V.X2_ID := Processor_ID (U (X2_ID_Offset, X2_ID_Width));
               V.X2_Flags := Affinity_Flags (U (X2_Flags_Offset, X2_Flags_Width));
               V.X2_Clock := Clock_Domain (U (X2_Clock_Offset, X2_Clock_Width));
            when GICC =>
               V.GICC_Domain := Proximity_Domain (U (GICC_Domain_Offset, GICC_Domain_Width));
               V.GICC_UID := Processor_ID (U (GICC_UID_Offset, GICC_UID_Width));
               V.GICC_Flags := Affinity_Flags (U (GICC_Flags_Offset, GICC_Flags_Width));
               V.GICC_Clock := Clock_Domain (U (GICC_Clock_Offset, GICC_Clock_Width));
            when GIC_ITS =>
               V.ITS_Domain := Proximity_Domain (U (ITS_Domain_Offset, ITS_Domain_Width));
               V.ITS_ID := ITS_Identifier (U (ITS_ID_Offset, ITS_ID_Width));
            when Generic_Initiator | Generic_Port =>
               V.Handle_Type := Byte (U (Handle_Type_Offset, Handle_Type_Width));
               V.Generic_Domain := Proximity_Domain (U (Generic_Domain_Offset, Generic_Domain_Width));
               for I in V.Handle'Range loop
                  V.Handle (I) := Byte (U (Handle_Offset + I, Byte_Width));
               end loop;
               V.Generic_Flags := Affinity_Flags (U (Generic_Flags_Offset, Generic_Flags_Width));
            when RINTC =>
               V.RINTC_Domain := Proximity_Domain (U (RINTC_Domain_Offset, RINTC_Domain_Width));
               V.RINTC_UID := Processor_ID (U (RINTC_UID_Offset, RINTC_UID_Width));
               V.RINTC_Flags := Affinity_Flags (U (RINTC_Flags_Offset, RINTC_Flags_Width));
               V.RINTC_Clock := Clock_Domain (U (RINTC_Clock_Offset, RINTC_Clock_Width));
            when Unknown => null;
         end case;
         return (Valid => True, Value => V);
      end;
   end Read_Record;
end Firmware_Tables.SRAT;
