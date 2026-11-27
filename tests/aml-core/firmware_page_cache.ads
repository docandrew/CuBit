pragma Ada_2022;
with Interfaces; use Interfaces;

-- Ordinary x86-64 paging leaves, not EPT. PAT selector preservation only:
-- effective memory type also depends on CPU PAT/MTRR state.
package Firmware_Page_Cache with SPARK_Mode, Pure is
   type Leaf_Kind is (Page_4K, Page_2M, Page_1G);
   subtype Cache_Index is Natural range 0 .. 7;
   function Leaf_Bytes (Kind : Leaf_Kind) return Unsigned_64 is
     (case Kind is when Page_4K => 4096, when Page_2M => 2 ** 21,
                   when Page_1G => 2 ** 30);
   function Selector (Raw : Unsigned_64; Kind : Leaf_Kind) return Cache_Index is
     ((if (Raw and 8) /= 0 then 1 else 0)
      + (if (Raw and 16) /= 0 then 2 else 0)
      + (if (Raw and (if Kind = Page_4K then 128 else 4096)) /= 0 then 4 else 0));
   function Address_Of (Raw, Virtual : Unsigned_64; Kind : Leaf_Kind) return Unsigned_64 is
     (((Raw and 16#000F_FFFF_FFFF_F000#) / Leaf_Bytes (Kind)) * Leaf_Bytes (Kind)
      + ((Virtual mod Leaf_Bytes (Kind)) / 4096) * 4096);

   type Description (Valid : Boolean := False) is record
      case Valid is
         when True => Frame : Unsigned_64; Cache : Cache_Index;
         when False => null;
      end case;
   end record;
   function Decode (Raw, Virtual, Maximum : Unsigned_64; Kind : Leaf_Kind)
     return Description
     with Post => (if Decode'Result.Valid then
       Decode'Result.Frame mod 4096 = 0
       and then Decode'Result.Frame <= Maximum
       and then Maximum - Decode'Result.Frame >= 4095
       and then Decode'Result.Frame = Address_Of (Raw, Virtual, Kind)
       and then Decode'Result.Cache = Selector (Raw, Kind));

   -- Virtmem's abstract flags use bit12 for PAT for every page size. makePTE
   -- subsequently translates it to raw P1 bit7. These are NOT raw PTE bits.
   function Read_Only_Flags (Cache : Cache_Index) return Unsigned_64 is
     (16#8000_0000_0000_0005#
      + (if Cache mod 2 = 1 then 8 else 0)
      + (if Cache / 2 mod 2 = 1 then 16 else 0)
      + (if Cache / 4 = 1 then 4096 else 0))
     with Post =>
       (Read_Only_Flags'Result and 16#8000_0000_0000_0005#) = 16#8000_0000_0000_0005#
       and then (Read_Only_Flags'Result and not Unsigned_64'(16#8000_0000_0000_101D#)) = 0
       and then Selector (Read_Only_Flags'Result, Page_2M) = Cache;
end Firmware_Page_Cache;
