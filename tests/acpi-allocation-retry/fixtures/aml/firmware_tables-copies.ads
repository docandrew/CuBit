pragma Ada_2022;
with Firmware_Tables.Catalog;
-- Copies immutable SDTs into caller-owned storage. Does not allocate, map or
-- release physical memory. Source and Destination must not alias.
package Firmware_Tables.Copies with SPARK_Mode, Pure is
   use type Byte;
   function Matches (Expected : Catalog.Descriptor; Data : Bytes) return Boolean
     with Post => (if Matches'Result then
       Data'Length = Expected.Extent and then Expected.Name /= "FACS");
   -- Destination may include page padding. Failure leaves the entire buffer
   -- zero; success preserves all table bytes and leaves its padding zero.
   procedure Copy_Validated
     (Expected : Catalog.Descriptor; Source : Bytes;
      Destination : out Bytes; Success : out Boolean) with
     Post =>
       (if Success then
          Destination'Length >= Expected.Extent
          and then Source'Length = Expected.Extent
          and then Matches (Expected,
            Destination (Destination'First .. Destination'First + (Expected.Extent - 1)))
          and then (for all I in 0 .. Expected.Extent - 1 =>
            Destination (Destination'First + I) = Source (Source'First + I))
          and then (for all I in Destination'Range =>
            (if I - Destination'First >= Expected.Extent then Destination (I) = 0))
        else (for all B of Destination => B = 0));
end Firmware_Tables.Copies;
