pragma Ada_2022;
with Interfaces; use Interfaces;

-- Multiboot-v1 transport decoder only. Firmware page/ownership policy lives
-- separately in Firmware_Frames. No address overlays or hardware reads here.
package Multiboot_Memory_Map with SPARK_Mode, Pure is
   subtype Buffer_Index is Natural range 0 .. Natural'Last - 1;
   type Bytes is array (Buffer_Index range <>) of Unsigned_8;
   type Region_Kind is (Usable, Reserved, ACPI_Reclaim, ACPI_NVS, Defective);
   type Decoded_Region is record
      First, Last : Unsigned_64 := 0;
      Kind : Region_Kind := Reserved;
      Empty : Boolean := True;
   end record;
   type Entries is array (Positive range <>) of Decoded_Region;
   type Status is (Success, Empty_Map, Truncated_Header, Short_Record,
                   Truncated_Record, Address_Out_Of_Range, Capacity_Exceeded);

   function Valid (Item : Decoded_Region; Maximum : Unsigned_64) return Boolean is
     (Item.Empty or else (Item.First <= Item.Last and then Item.Last <= Maximum));

   procedure Next_Entry
     (Data : Bytes; Position : Natural; Maximum : Unsigned_64;
      Following : out Natural; Item : out Decoded_Region; Result : out Status) with
     Pre => Position <= Data'Length,
     Post =>
       (if Result = Success then Following > Position and then
          Following <= Data'Length and then Valid (Item, Maximum)
        else Following = Position);

   -- Count is the publication boundary: failed parses publish zero entries.
   -- Extended record tails are skipped by their declared size, not interpreted.
   procedure Parse
     (Data : Bytes; Maximum : Unsigned_64; Output : out Entries;
      Count : out Natural; Result : out Status) with
     Post => Count <= Output'Length and then
       (if Result /= Success then Count = 0
        else Count > 0 and then
          (for all I in Output'First .. Output'First + (Count - 1) =>
             Valid (Output (I), Maximum)));
end Multiboot_Memory_Map;
