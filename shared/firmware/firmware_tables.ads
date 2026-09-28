pragma Ada_2022;
with Interfaces;

--  Shared by boot admission and future userspace firmware consumers. No address
--  overlays, allocation, hardware access, or kernel/runtime dependencies.
package Firmware_Tables with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   subtype Address_Value is Interfaces.Unsigned_64;
   subtype Root_Address is Address_Value range 1 .. Address_Value'Last;
   type Bytes is array (Positive range <>) of Byte;
   subtype Signature is String (1 .. 4);

   type Admission is
     (Accepted, Truncated, Wrong_Signature, Bad_Checksum,
      Unsupported_Revision, Invalid_Length, Missing_Root);
   type Root_Kind is (RSDT, XSDT);
   type Root_Result (Status : Admission := Truncated) is record
      case Status is
         when Accepted =>
            Kind : Root_Kind;
            Address : Root_Address;
            Extent : Positive;
         when others => null;
      end case;
   end record;
   type Table_Result (Status : Admission := Truncated) is record
      case Status is
         when Accepted =>
            Extent : Positive;
            Revision : Byte;
         when others => null;
      end case;
   end record;
   Table_Header_Size : constant := 36;

   --  Input must be a stable, readable copy supplied by the raw-memory adapter.
   --  Trailing bytes are ignored; checksums cover exactly the declared extent.
   --  Returned addresses are untrusted numbers, NOT validated memory mappings.
   function Read_Root (Data : Bytes) return Root_Result
     with Post =>
       (if Read_Root'Result.Status = Accepted then
          Read_Root'Result.Extent in 20 .. Data'Length);
   function Read_Table
     (Data : Bytes; Expected : Signature) return Table_Result
     with Post =>
       (if Read_Table'Result.Status = Accepted then
          Read_Table'Result.Extent in Table_Header_Size .. Data'Length);
end Firmware_Tables;
