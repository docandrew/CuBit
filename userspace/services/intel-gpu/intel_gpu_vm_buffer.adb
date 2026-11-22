with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
package body Intel_GPU_VM_Buffer is
   procedure Change_Extent_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Unsigned_64; Remove : Boolean; Accepted : out Boolean;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access := Intel_GPU_ADLN_PPGTT.Read_Write) is
      package Views renames Intel_GPU_Buffer_Reply;
   begin
      Accepted := False;
      if not Views.Valid (Backing) or else Bytes = 0 or else
        (GPU or Offset or Bytes) mod 4096 /= 0 or else
        GPU = 0 or else GPU >= 2 ** 48 or else Bytes > 2 ** 48 - GPU or else
        Offset >= Views.Byte_Count (Backing) or else
        Bytes > Views.Byte_Count (Backing) - Offset
      then return; end if;
      declare
         Pages : VM.Data_Pages (1 .. Positive (Bytes / 4096));
      begin
         for Index in Pages'Range loop
            Pages (Index) := Views.Page_Address
              (Backing, Offset + Unsigned_64 (Index - 1) * 4096);
            if Pages (Index) = 0 then return; end if;
         end loop;
         -- One atomic builder call; never leave a prefix mapped on failure.
         if Remove then
            VM.Unmap_Pages (Object, GPU, Pages, Accepted);
         else
            VM.Map_Pages (Object, GPU, Pages, Intel_GPU_ADLN_PPGTT.Write_Back,
                          Access_Mode, Accepted);
         end if;
      end;
   end Change_Extent_Range;
   procedure Bind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Unsigned_64; Accepted : out Boolean;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access := Intel_GPU_ADLN_PPGTT.Read_Write) is
   begin
      Change_Extent_Range (Object, Backing, GPU, Offset, Bytes, False, Accepted, Access_Mode);
   end Bind_Range;
   procedure Unbind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Change_Extent_Range (Object, Backing, GPU, Offset, Bytes, True, Accepted);
   end Unbind_Range;
   function Matches_Range
     (Object : VM.Image; Backing : Intel_GPU_Buffer_Reply.Extent_View;
      GPU, Offset, Bytes : Unsigned_64) return Boolean is
      package Views renames Intel_GPU_Buffer_Reply;
      First_GPU, First_Offset, Length : Unsigned_64;
      Address : Unsigned_64;
   begin
      if not VM.Sealed (Object) or else not Views.Valid (Backing) or else
        Bytes = 0 or else GPU = 0 or else GPU >= 2 ** 48 or else
        Bytes > 2 ** 48 - GPU or else Offset >= Views.Byte_Count (Backing) or else
        Bytes > Views.Byte_Count (Backing) - Offset or else GPU mod 4096 /= Offset mod 4096
      then return False; end if;
      First_GPU := GPU - GPU mod 4096;
      First_Offset := Offset - Offset mod 4096;
      Length := ((Offset mod 4096 + Bytes + 4095) / 4096) * 4096;
      for Page in Unsigned_64 range 0 .. Length / 4096 - 1 loop
         Address := Views.Page_Address (Backing, First_Offset + Page * 4096);
         if Address = 0 or else
           VM.Lookup (Object, First_GPU + Page * 4096) /=
             Intel_GPU_ADLN_PPGTT.Encode_Leaf
               (Address, Intel_GPU_ADLN_PPGTT.Write_Back,
                Intel_GPU_ADLN_PPGTT.Read_Write)
         then return False; end if;
      end loop;
      return True;
   end Matches_Range;
   function Valid_Range
     (Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      if not Intel_GPU_Buffer_Reply.Valid (Backing) or else
        Bytes = 0 or else (GPU or Offset or Bytes) mod 4096 /= 0 or else
        GPU = 0 or else GPU >= 2 ** 48 or else Bytes > 2 ** 48 - GPU or else
        Offset > Backing.Bytes or else Bytes > Backing.Bytes - Offset
      then return False; end if;
      return True;
   end Valid_Range;
   function Matches_Range
     (Object : VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Unsigned_64) return Boolean is
      Page_GPU, Page_Offset, Extent : Unsigned_64;
   begin
      if not VM.Sealed (Object) or else Bytes = 0 or else
        Bytes > 4096 * Unsigned_64 (Intel_GPU_Buffer_Backing.Page_Count'Last) or else
        GPU mod 4096 /= Offset mod 4096
      then return False; end if;
      Page_GPU := GPU - GPU mod 4096;
      Page_Offset := Offset - Offset mod 4096;
      Extent := ((Offset mod 4096 + Bytes + 4095) / 4096) * 4096;
      if not Valid_Range (Backing, Page_GPU, Page_Offset, Extent) then return False; end if;
      for Page in Unsigned_64 range 0 .. Extent / 4096 - 1 loop
         if VM.Lookup (Object, Page_GPU + Page * 4096) /=
           Intel_GPU_ADLN_PPGTT.Encode_Leaf
             (Intel_GPU_Buffer_Reply.Page_Address (Backing, Page_Offset + Page * 4096),
              Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write)
         then return False; end if;
      end loop;
      return True;
   end Matches_Range;
   procedure Bind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Unsigned_64;
      Policy : Intel_GPU_ADLN_PPGTT.Cache_Policy;
      Access_Mode : Intel_GPU_ADLN_PPGTT.Page_Access;
      Accepted : out Boolean) is
      use type Intel_GPU_ADLN_PPGTT.Cache_Policy;
   begin
      Accepted := False;
      if Policy /= Intel_GPU_ADLN_PPGTT.Write_Back or else
        not Valid_Range (Backing, GPU, Offset, Bytes) then return; end if;
      declare
         Pages : VM.Data_Pages (1 .. Positive (Bytes / 4096));
      begin
         for Index in Pages'Range loop
            Pages (Index) := Intel_GPU_Buffer_Reply.Page_Address
              (Backing, Offset + Unsigned_64 (Index - 1) * 4096);
         end loop;
         VM.Map_Pages (Object, GPU, Pages, Policy, Access_Mode, Accepted);
      end;
   end Bind_Range;
   procedure Unbind_Range
     (Object : in out VM.Image; Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Valid_Range (Backing, GPU, Offset, Bytes) then return; end if;
      declare
         Pages : VM.Data_Pages (1 .. Positive (Bytes / 4096));
      begin
         for Index in Pages'Range loop
            Pages (Index) := Intel_GPU_Buffer_Reply.Page_Address
              (Backing, Offset + Unsigned_64 (Index - 1) * 4096);
         end loop;
         VM.Unmap_Pages (Object, GPU, Pages, Accepted);
      end;
   end Unbind_Range;
end Intel_GPU_VM_Buffer;
