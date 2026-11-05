pragma Ada_2022;
with Interfaces;
-- Untrusted copied metadata only; no address confers authority.
package Firmware_Tables.DMAR with SPARK_Mode, Pure is
   Fixed_Size : constant := Table_Header_Size + 12;
   Record_Header_Size : constant := 4;
   Scope_Header_Size : constant := 6;
   Path_Pair_Size : constant := 2;
   subtype Record_Length is Natural range Record_Header_Size .. 65_535;
   subtype Scope_Length is Natural range Scope_Header_Size .. 255;
   subtype Record_Count is Natural range
     0 .. (Natural'Last - Fixed_Size) / Record_Header_Size;
   subtype Scope_Count is Natural range 0 .. 65_535 / Scope_Header_Size;
   subtype Path_Count is Natural range 0 .. (255 - Scope_Header_Size) / Path_Pair_Size;
   subtype Segment_Number is Interfaces.Unsigned_16;
   subtype Proximity_Domain is Interfaces.Unsigned_32;
   type Record_Kind is (Hardware_Unit, Reserved_Memory, ATS_Root,
                       Hardware_Affinity, Namespace_Device, SATC, SIDP, Unknown);
   type Table_Metadata (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Revision, Host_Width, Flags : Byte;
            Count : Record_Count;
         when False => null;
      end case;
   end record;
   type Record_Data (Kind : Record_Kind := Unknown) is record
      Wire_Type : Interfaces.Unsigned_16;
      Offset : Natural;
      Length : Record_Length;
      Scopes : Scope_Count;
      case Kind is
         when Hardware_Unit =>
            Unit_Flags, Register_Size : Byte;
            Unit_Segment : Segment_Number;
            Register_Base : Address_Value;
         when Reserved_Memory =>
            Memory_Segment : Segment_Number;
            Base_Address, Inclusive_Limit : Address_Value;
         when ATS_Root | SATC =>
            Cache_Flags : Byte;
            Cache_Segment : Segment_Number;
         when Hardware_Affinity =>
            Affinity_Base : Address_Value;
            Domain : Proximity_Domain;
         when Namespace_Device =>
            Device_Number : Byte;
            Name_Offset, Name_Length : Natural;
         when SIDP => Device_Segment : Segment_Number;
         when Unknown => null;
      end case;
   end record;
   type Record_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Record_Data;
         when False => null;
      end case;
   end record;
   type Scope_Result (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Wire_Type, Flags, Reserved, Enumeration_ID, Start_Bus : Byte;
            Offset : Natural;
            Length : Scope_Length;
            Known_Path : Boolean;
            Paths : Path_Count;
         when False => null;
      end case;
   end record;
   type Path_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Device, Func : Byte;
         when False => null;
      end case;
   end record;
   -- Exact table extent/checksum plus ALL outer/nested structure is required.
   -- Unknown types retain bounded opaque payloads. Known scope tails are fully
   -- parsed, never accepted as arbitrary extension bytes. ANDD names require
   -- a bounded NUL, but namespace syntax and trailing bytes remain metadata.
   -- Flags, host width, register size, addresses, BDF values, scope-to-parent
   -- compatibility and topology policy are intentionally not admitted here.
   function Decode (Data : Bytes) return Table_Metadata;
   -- One-based indices. Each accessor revalidates the complete supplied Data;
   -- no caller-controlled offset/descriptor is trusted to select memory.
   function Read_Record (Data : Bytes; Index : Natural) return Record_Result;
   function Read_Scope (Data : Bytes; Record_Index, Scope_Index : Natural)
     return Scope_Result;
   function Read_Path
     (Data : Bytes; Record_Index, Scope_Index, Path_Index : Natural)
     return Path_Result;
end Firmware_Tables.DMAR;
