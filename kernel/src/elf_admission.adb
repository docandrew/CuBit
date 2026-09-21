package body ELF_Admission with SPARK_Mode is
   procedure Add_Image_Pages
     (Pages : in out Natural; Bytes : Unsigned_64; Success : out Boolean)
   is
      Total : constant Unsigned_64 := Unsigned_64 (Pages) + Pages_For (Bytes);
   begin
      Success := Total <= Unsigned_64 (Natural'Last);
      if Success then Pages := Natural (Total); end if;
   end Add_Image_Pages;

   function Frame_Capacity
     (Image_Pages : Natural; Stack_Pages : Positive; Heap_Pages : Natural)
      return Natural
   is
      Total : constant Unsigned_64 := Unsigned_64 (Image_Pages) +
        Unsigned_64 (Stack_Pages) + Unsigned_64 (Heap_Pages);
   begin
      return (if Total <= Unsigned_64 (Natural'Last) then Natural (Total) else 0);
   end Frame_Capacity;

   function Segment_Fits (H : Program_Header; Image_Size, Limit : Unsigned_64)
     return Boolean
   is
   begin
      if Limit < 2 * Page_Size or else Limit > 16#0000_8000_0000_0000# then
         return False;
      elsif H.p_offset > Image_Size or else H.p_filesz > Image_Size - H.p_offset or else
        H.p_filesz > H.p_memsz then
         return False;
      elsif H.p_vaddr mod Page_Size /= 0 or else
        H.p_vaddr > Limit - 2 * Page_Size or else
        H.p_memsz > Limit - 2 * Page_Size - H.p_vaddr then
         return False;
      elsif H.p_align > 1 and then
        ((H.p_align and (H.p_align - 1)) /= 0 or else
         H.p_offset mod H.p_align /= H.p_vaddr mod H.p_align) then
         return False;
      end if;
      return True;
   end Segment_Fits;
end ELF_Admission;
