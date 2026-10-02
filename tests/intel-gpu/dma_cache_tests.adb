with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
procedure DMA_Cache_Tests is
   Buffer : array (0 .. 4095) of Unsigned_8 := [others => 16#A5#]
     with Alignment => 4096, Volatile;
   Address : constant Unsigned_64 := Unsigned_64 (To_Integer (Buffer'Address));
   function Flush (Base, Bytes : Unsigned_64) return Boolean
     renames Intel_GPU_DMA_Cache.Flush_Range;
begin
   pragma Assert (not Flush (0, 4096));
   pragma Assert (not Flush (Address, 0));
   pragma Assert (not Flush (Unsigned_64'Last - 4095, 4096));
   pragma Assert (not Flush (Address, 16 * 1024 * 1024 + 1));
   pragma Assert (not Flush (Address + 1, 4096));
   pragma Assert (not Flush (Address, 4095));
   pragma Assert (Flush (Address, 4096));
   for I in Buffer'Range loop
      pragma Assert (Buffer (I) = 16#A5#);
   end loop;
   Ada.Text_IO.Put_Line
     ("DMA cache PASS: host x86 CLFLUSH on aligned page preserves contents; invalid extents rejected (not device coherence proof)");
end DMA_Cache_Tests;
