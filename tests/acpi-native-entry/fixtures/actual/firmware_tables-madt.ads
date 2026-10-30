pragma Ada_2022;
with Interfaces;
-- Untrusted copied metadata: no addresses returned here confer authority.
package Firmware_Tables.MADT with SPARK_Mode, Pure is
   Fixed_Size : constant := Table_Header_Size + 8;
   Record_Header_Size : constant := 2;
   subtype Record_Length is Natural range 2 .. 255;
   subtype Record_Count is Natural range
     0 .. (Natural'Last - Fixed_Size) / Record_Header_Size;
   subtype Processor_UID is Interfaces.Unsigned_32;
   subtype APIC_ID is Interfaces.Unsigned_32;
   subtype Interrupt_Number is Interfaces.Unsigned_32;
   subtype Interrupt_Flags is Interfaces.Unsigned_16;
   subtype Processor_Flags is Interfaces.Unsigned_32;
   type Record_Kind is
     (Local_APIC, IO_APIC, Source_Override, NMI_Source, Local_NMI,
      Address_Override, Local_X2APIC, X2APIC_NMI, Unknown);
   type Table_Metadata (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Revision : Byte;
            Local_Address : Address_Value;
            Flags : Interfaces.Unsigned_32;
            Count : Record_Count;
         when False => null;
      end case;
   end record;
   type Record_Data (Kind : Record_Kind := Unknown) is record
      Wire_Type : Byte;
      Offset : Natural;
      Length : Record_Length;
      -- Exact byte slice includes reserved and future extension bytes.
      case Kind is
         when Local_APIC | Local_X2APIC =>
            UID : Processor_UID;
            Controller : APIC_ID;
            CPU_Flags : Processor_Flags;
         when IO_APIC =>
            IO_ID : Byte;
            IO_Address : Address_Value;
            Interrupt_Base : Interrupt_Number;
         when Source_Override =>
            Bus, Source : Byte;
            Global_Interrupt : Interrupt_Number;
            Override_Flags : Interrupt_Flags;
         when NMI_Source =>
            NMI_Interrupt : Interrupt_Number;
            NMI_Flags : Interrupt_Flags;
         when Local_NMI | X2APIC_NMI =>
            NMI_UID : Processor_UID;
            LINT : Byte;
            Local_Flags : Interrupt_Flags;
         when Address_Override => Address : Address_Value;
         when Unknown => null;
      end case;
   end record;
   type Record_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Record_Data;
         when False => null;
      end case;
   end record;
   -- Requires exact SDT extent/checksum and validates ALL records before
   -- publication. Known types permit extension bytes. Unknown types retain
   -- their bounded byte slice. Reserved bits, IDs, polarity, trigger mode,
   -- duplicates and address suitability require separate topology policy.
   function Decode (Data : Bytes) return Table_Metadata;
   -- One-based index, independently revalidates buffer. Invalid tail rejects
   -- even an otherwise valid prefix; no prefix is published as a full table.
   function Read_Record (Data : Bytes; Index : Natural) return Record_Result;
end Firmware_Tables.MADT;
