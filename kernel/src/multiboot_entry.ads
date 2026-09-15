pragma Ada_2022;
with Interfaces; use Interfaces;
with Multiboot_Memory_Map;

-- Numeric/byte admission only. Mapping and raw-address reads are adapters.
package Multiboot_Entry with SPARK_Mode, Pure is
   use type Multiboot_Memory_Map.Bytes;
   Loader_Magic : constant Unsigned_32 := 16#2BADB002#;
   -- GRUB's actual v1 ABI aligns the color union at 112, not the manual's
   -- diagram offset 110 (GRUB issue 63499). Consume through blue mask byte117.
   Header_Bytes : constant := 118;
   Snapshot_Bytes : constant := 116;
   Bootstrap_Limit : constant Unsigned_64 := 2 ** 30;
   subtype Header is Multiboot_Memory_Map.Bytes (0 .. Header_Bytes - 1);
   subtype Snapshot_Header is Multiboot_Memory_Map.Bytes (0 .. Snapshot_Bytes - 1);
   type Status is (Success, Wrong_Loader, Header_Outside_Mapping,
                   Missing_Memory_Map, Missing_Framebuffer,
                   Unsupported_Framebuffer);

   procedure Admit_Address
     (Magic : Unsigned_32; Base, Limit : Unsigned_64; Result : out Status) with
     Post => (if Result = Success then Magic = Loader_Magic and then
       Base > 0 and then Base < Limit and then
       Header_Bytes <= Limit - Base);

   -- Keep only fields consumed by CuBit, and only when advertised. Unused
   -- fields/reserved flag bits and the text-mode RGB union tail stay zero.
   procedure Snapshot (Data : Header; Output : out Snapshot_Header;
                       Result : out Status) with
     Post => (if Result /= Success then (for all B of Output => B = 0)
       else Output (0) = (Data (0) and 16#48#) and then
            Output (1) = 16#10# and then
            Output (109) in 1 .. 2 and then
            (if Output (109) = 1 then Output (110 .. 115) = Data (112 .. 117)) and then
            (if Output (109) = 2 then
               (for all I in 110 .. 115 => Output (I) = 0)));
end Multiboot_Entry;
