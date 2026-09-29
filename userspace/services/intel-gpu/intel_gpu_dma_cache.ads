with Interfaces;
package Intel_GPU_DMA_Cache is
   -- x86-only cache maintenance for exclusively CPU-owned, mapped RAM.
   -- Caller guarantees every byte is mapped and retained and no device/CPU
   -- writes concurrently. This does not grant ownership or publish GPU PTEs.
   -- Scheduling must preserve compatible CLFLUSH support across these CPUs.
   function Flush_Range (Address, Bytes : Interfaces.Unsigned_64) return Boolean;
end Intel_GPU_DMA_Cache;
