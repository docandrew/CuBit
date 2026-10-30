pragma Ada_2022;
package Firmware_Tables.SLIT with SPARK_Mode, Pure is
   Locality_Count_Size : constant := 8;
   Fixed_Size : constant := Table_Header_Size + Locality_Count_Size;
   subtype Locality_Count is Natural;
   subtype Locality_Index is Natural;
   subtype Distance is Byte;
   type Table_Metadata (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Revision : Byte;
            Count : Locality_Count;
         when False => null;
      end case;
   end record;
   type Distance_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Distance;
         when False => null;
      end case;
   end record;
   -- Requires a checksum-valid, exact-size table with a square distance matrix.
   -- Matrix geometry is checked without multiplying an untrusted 64-bit count.
   -- Raw revision/distances and an empty matrix are metadata; validity does not
   -- assert ACPI distance policy, topology availability or hardware authority.
   function Decode (Data : Bytes) return Table_Metadata;
   -- Zero-based row/column indices, independently bounded. Input is revalidated
   -- on every access; no saved metadata can be applied to an unrelated buffer.
   function Read_Distance
     (Data : Bytes; From_Locality, To_Locality : Locality_Index)
      return Distance_Result;
end Firmware_Tables.SLIT;
