with Interfaces; use Interfaces;

-- Integer-only ELF64 wire metadata: no invalid enum or constrained signed
-- scalar is read from an untrusted program header before validation.
package ELF_Admission with SPARK_Mode, Pure is
   Page_Size : constant Unsigned_64 := 4096;
   Header_Size : constant Unsigned_64 := 56;
   Max_Headers : constant Unsigned_64 := 128;
   type Program_Header is record
      p_type, p_flags : Unsigned_32;
      p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align : Unsigned_64;
   end record with Size => 56 * 8;
   for Program_Header use record
      p_type at 0 range 0 .. 31;
      p_flags at 4 range 0 .. 31;
      p_offset at 8 range 0 .. 63;
      p_vaddr at 16 range 0 .. 63;
      p_paddr at 24 range 0 .. 63;
      p_filesz at 32 range 0 .. 63;
      p_memsz at 40 range 0 .. 63;
      p_align at 48 range 0 .. 63;
   end record;
   type Header_Table is array (Unsigned_16 range <>) of Program_Header;

   function Table_Fits (Image_Size, Offset, Count, Entry_Size : Unsigned_64)
     return Boolean is
     (Entry_Size = Header_Size and then Count in 1 .. Max_Headers and then
      Offset >= 64 and then Offset <= Image_Size and then
      Count <= (Image_Size - Offset) / Header_Size);

   -- CuBit's current loader accepts page-aligned PT_LOAD segments. Reserve
   -- room for the image-to-heap guard before the grant aperture / user stack.
   function Segment_Fits (H : Program_Header; Image_Size, Limit : Unsigned_64)
     return Boolean
   with Post => (if Segment_Fits'Result then
     H.p_filesz <= H.p_memsz and then H.p_offset <= Image_Size and then
     H.p_filesz <= Image_Size - H.p_offset and then
     H.p_vaddr <= Limit and then H.p_memsz <= Limit - H.p_vaddr and then
     H.p_vaddr mod Page_Size = 0);
end ELF_Admission;
