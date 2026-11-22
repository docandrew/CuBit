with Interfaces;
package Intel_GPU_ADLN_PPGTT with SPARK_Mode is
   use Interfaces;
   -- Encoding only, not allocation, authorization, publication or isolation.
   -- Caller supplies retained device-visible system-memory DMA pages. Initial
   -- bring-up policy matches our allocator: below4GiB, not a hardware limit.
   function Valid_DMA_Page (Address : Unsigned_64) return Boolean is
     (Address /= 0 and then Address < 2 ** 32 and then Address mod 4096 = 0);
   type Cache_Policy is (Write_Back, Write_Combining, Write_Through, Uncached);
   type Page_Access is (Read_Only, Read_Write);
   -- PAT setup must already match Intel_GPU_ADLN_PAT. No local-memory,
   -- huge-page, compression or newer-generation PAT flags are accepted.
   function Cache_Bits (Policy : Cache_Policy) return Unsigned_64 is
     (case Policy is when Write_Back => 0, when Write_Combining => 8,
                    when Write_Through => 16, when Uncached => 24);
   function Encode_Leaf
     (DMA_Address : Unsigned_64; Policy : Cache_Policy; Access_Mode : Page_Access)
      return Unsigned_64
   with Global => null,
     Post => (if Valid_DMA_Page (DMA_Address) and Access_Mode = Read_Write then
       Encode_Leaf'Result = DMA_Address + 3 + Cache_Bits (Policy)
       else Encode_Leaf'Result = 0);
   -- Gen11/12 read-only fault erratum: reject Read_Only, never upgrade to RW.
   -- Context isolation must instead keep unauthorized backing out of the VM.
   function Encode_Directory (DMA_Address : Unsigned_64) return Unsigned_64
   with Global => null,
     Post => (if Valid_DMA_Page (DMA_Address) then
       Encode_Directory'Result = DMA_Address + 3 else Encode_Directory'Result = 0);
   subtype Table_Index is Natural range 0 .. 511;
   subtype Page_Offset is Natural range 0 .. 4095;
   type Walk is record
      Valid : Boolean := False;
      PML4, PDP, PD, PT : Table_Index := 0;
      Offset : Page_Offset := 0;
   end record;
   -- Raw48-bit GPU VA, not a CPU pointer or sign-extended command address.
   function Locate (GPU_Address : Unsigned_64) return Walk
   with Global => null,
     Post => Locate'Result.Valid = (GPU_Address < 2 ** 48) and then
       (if Locate'Result.Valid then
          Long_Long_Integer (Locate'Result.PML4) * 2 ** 39 +
          Long_Long_Integer (Locate'Result.PDP) * 2 ** 30 +
          Long_Long_Integer (Locate'Result.PD) * 2 ** 21 +
          Long_Long_Integer (Locate'Result.PT) * 2 ** 12 +
          Long_Long_Integer (Locate'Result.Offset) = Long_Long_Integer (GPU_Address));
end Intel_GPU_ADLN_PPGTT;
