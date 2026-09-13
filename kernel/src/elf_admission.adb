package body ELF_Admission with SPARK_Mode is
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
