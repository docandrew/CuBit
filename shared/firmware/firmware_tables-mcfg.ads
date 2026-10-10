pragma Ada_2022;
with Interfaces;
-- Pure wire metadata only: no mapping, register access, or authority.
package Firmware_Tables.MCFG with SPARK_Mode, Pure is
   Fixed_Size : constant := Table_Header_Size + 8;
   Allocation_Size : constant := 16;
   subtype Allocation_Count is Natural range
     0 .. (Natural'Last - Fixed_Size) / Allocation_Size;
   subtype Segment_Number is Interfaces.Unsigned_16;
   subtype Bus_Number is Interfaces.Unsigned_8;
   type Allocation is record
      Base : Address_Value;
      Segment : Segment_Number;
      First_Bus, Last_Bus : Bus_Number;
      Reserved : Interfaces.Unsigned_32;
   end record;
   type Table_Metadata (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Revision : Byte;
            Reserved : Interfaces.Unsigned_64;
            Count : Allocation_Count;
         when False => null;
      end case;
   end record;
   type Allocation_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Allocation;
         when False => null;
      end case;
   end record;
   -- Exact extent/checksum and whole allocation records are required. Empty
   -- arrays are structurally representable. Revision/reserved bits, zero or
   -- unaligned bases, and reversed/overlapping bus ranges are retained: they
   -- require separate compatibility/resource policy before any hardware use.
   function Decode (Data : Bytes) return Table_Metadata;
   -- Revalidates supplied bytes; no descriptor can authorize another buffer.
   -- Zero and out-of-range indices fail without returning an allocation.
   function Read_Allocation (Data : Bytes; Index : Natural)
     return Allocation_Result;
end Firmware_Tables.MCFG;
