with Interfaces;
package Intel_GPU_GGTT with SPARK_Mode is
   use Interfaces;
   Table_BAR_Offset : constant Unsigned_64 := 8 * 1024 * 1024;
   Maximum_Table_Bytes : constant Unsigned_64 := 8 * 1024 * 1024;
   -- Conservative first-upload policy: system-memory DMA addresses below
   -- 4GiB only. This is not the hardware's maximum address width. The caller
   -- supplies a device-visible DMA address, never a CPU virtual address.
   -- Zero return rejects null, unaligned or out-of-policy addresses.
   -- Encoding neither reserves GPU VA nor proves ownership/cache coherence.
   function Encode_System_Page (DMA_Address : Unsigned_64) return Unsigned_64
   with Global => null,
     Post => (if DMA_Address /= 0 and then DMA_Address < 2 ** 32 and then
                 DMA_Address mod 4096 = 0
              then Encode_System_Page'Result = DMA_Address + 1
              else Encode_System_Page'Result = 0);
   -- PCI GGC at offset 0x50, GGMS bits 7:6 on ADLN. Zero disables the
   -- table; all-ones PCI reads are rejected rather than interpreted as 8MiB.
   function Table_Size (GGC : Unsigned_16) return Unsigned_64
   with Global => null,
     Post => Table_Size'Result in 0 | 2 * 1024 * 1024 |
       4 * 1024 * 1024 | 8 * 1024 * 1024;
   type Window is record
      Valid : Boolean := False;
      First_Entry, Entry_Count, BAR_Offset, Mapping_Bytes : Unsigned_64 := 0;
   end record;
   -- ADLN/Gen8+ table geometry only. Table_Bytes must come from validated
   -- platform discovery. GPU byte addresses are NOT CPU physical addresses.
   -- Returns the page-rounded MMIO window covering the requested entries.
   -- This confers no right to overwrite existing mappings or assume ownership.
   function Plan_Window
     (Table_Bytes, GPU_Start, Buffer_Bytes : Unsigned_64) return Window
   with Global => null,
     Post => (if Plan_Window'Result.Valid then
       Plan_Window'Result.Entry_Count > 0 and then
       Plan_Window'Result.First_Entry <= Table_Bytes / 8 and then
       Plan_Window'Result.Entry_Count <=
         Table_Bytes / 8 - Plan_Window'Result.First_Entry and then
       Plan_Window'Result.BAR_Offset >= Table_BAR_Offset and then
       Plan_Window'Result.BAR_Offset mod 4096 = 0 and then
       Plan_Window'Result.Mapping_Bytes mod 4096 = 0 and then
       Plan_Window'Result.BAR_Offset - Table_BAR_Offset +
         Plan_Window'Result.Mapping_Bytes <= Table_Bytes);
end Intel_GPU_GGTT;
