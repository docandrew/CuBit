pragma Ada_2022;
with Interfaces;
-- Pure copied wire metadata only: no topology decisions or hardware authority.
package Firmware_Tables.SRAT with SPARK_Mode, Pure is
   Fixed_Size : constant := Table_Header_Size + 12;
   Record_Header_Size : constant := 2;
   subtype Record_Length is Natural range Record_Header_Size .. 255;
   subtype Record_Count is Natural range
     0 .. (Natural'Last - Fixed_Size) / Record_Header_Size;
   subtype Proximity_Domain is Interfaces.Unsigned_32;
   subtype Processor_ID is Interfaces.Unsigned_32;
   subtype ITS_Identifier is Interfaces.Unsigned_32;
   subtype Affinity_Flags is Interfaces.Unsigned_32;
   subtype Clock_Domain is Interfaces.Unsigned_32;
   subtype Domain_High_Bits is Interfaces.Unsigned_32 range 0 .. 16#FF_FFFF#;
   Handle_Size : constant := 16;
   type Device_Handle is array (Natural range 0 .. Handle_Size - 1) of Byte;
   type Record_Kind is (Local_APIC, Memory_Affinity, X2APIC, GICC, GIC_ITS, Generic_Initiator, Generic_Port, RINTC, Unknown);
   type Table_Metadata (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Revision : Byte;
            Table_Revision : Interfaces.Unsigned_32;
            Reserved : Interfaces.Unsigned_64;
            Count : Record_Count;
         when False => null;
      end case;
   end record;
   type Record_Data (Kind : Record_Kind := Unknown) is record
      Wire_Type : Byte;
      Offset : Natural;
      Length : Record_Length;
      case Kind is
         when Local_APIC =>
            Domain_Low : Byte;
            APIC_ID : Byte;
            APIC_Flags : Affinity_Flags;
            SAPIC_EID : Byte;
            Domain_High : Domain_High_Bits;
            APIC_Clock : Clock_Domain;
         when Memory_Affinity =>
            Memory_Domain : Proximity_Domain;
            Base_Address : Address_Value;
            Address_Length : Address_Value;
            Memory_Flags : Affinity_Flags;
         when X2APIC =>
            X2_Domain : Proximity_Domain;
            X2_ID : Processor_ID;
            X2_Flags : Affinity_Flags;
            X2_Clock : Clock_Domain;
         when GICC =>
            GICC_Domain : Proximity_Domain;
            GICC_UID : Processor_ID;
            GICC_Flags : Affinity_Flags;
            GICC_Clock : Clock_Domain;
         when GIC_ITS =>
            ITS_Domain : Proximity_Domain;
            ITS_ID : ITS_Identifier;
         when Generic_Initiator | Generic_Port =>
            Handle_Type : Byte;
            Generic_Domain : Proximity_Domain;
            Handle : Device_Handle;
            Generic_Flags : Affinity_Flags;
         when RINTC =>
            RINTC_Domain : Proximity_Domain;
            RINTC_UID : Processor_ID;
            RINTC_Flags : Affinity_Flags;
            RINTC_Clock : Clock_Domain;
         when Unknown => null;
      end case;
   end record;
   type Record_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Record_Data;
         when False => null;
      end case;
   end record;
   -- Exact SDT extent/checksum and ALL record extents are checked first.
   -- Known extensions and unknown kinds retain an offset/length in Data.
   -- Revision, reserved bits, disabled entries and raw addresses are metadata,
   -- not topology admission. Type0 retains domain low/high wire bits separately;
   -- interpreting legacy revisions belongs to a distinct compatibility policy.
   function Decode (Data : Bytes) return Table_Metadata;
   -- Independently revalidates Data; zero/out-of-range indices fail. A bad
   -- tail invalidates even a requested otherwise-valid first record.
   function Read_Record (Data : Bytes; Index : Natural) return Record_Result;
end Firmware_Tables.SRAT;
