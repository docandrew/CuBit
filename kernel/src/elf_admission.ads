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

   -- Ceiling division without a potentially wrapping Bytes + Page_Size - 1.
   subtype Page_Count is Unsigned_64 range 0 .. 2 ** 52;
   function Pages_For (Bytes : Unsigned_64) return Page_Count is
     (Bytes / Page_Size + (if Bytes mod Page_Size = 0 then 0 else 1));

   -- The frame tracker uses Natural counts. Accumulate the actual ELF memory
   -- sizes, including zero-filled BSS, without borrowing stack/heap capacity.
   procedure Add_Image_Pages
     (Pages : in out Natural; Bytes : Unsigned_64; Success : out Boolean)
   with Post =>
     Success = (Unsigned_64 (Pages'Old) + Pages_For (Bytes) <= Unsigned_64 (Natural'Last)) and then
     (if Success then Unsigned_64 (Pages) = Unsigned_64 (Pages'Old) + Pages_For (Bytes)
      else Pages = Pages'Old);

   function Frame_Capacity
     (Image_Pages : Natural; Stack_Pages : Positive; Heap_Pages : Natural)
      return Natural
   with Post =>
     (if Unsigned_64 (Image_Pages) + Unsigned_64 (Stack_Pages) +
           Unsigned_64 (Heap_Pages) <= Unsigned_64 (Natural'Last)
      then Unsigned_64 (Frame_Capacity'Result) = Unsigned_64 (Image_Pages) +
             Unsigned_64 (Stack_Pages) + Unsigned_64 (Heap_Pages)
      else Frame_Capacity'Result = 0);

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
