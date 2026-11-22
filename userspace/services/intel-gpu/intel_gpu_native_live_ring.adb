with Interfaces; use Interfaces;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
package body Intel_GPU_Native_Live_Ring is
   Base, Bytes, Ring_Base : Unsigned_64 := 0;
   function Mapping_Valid return Boolean is
     (Base /= 0 and then Base mod 4096 = 0 and then Bytes >= 81920 and then
      Base < 2 ** 47 and then Bytes <= 2 ** 47 - Base);
   Active : Boolean := False;
   First, Last : Unsigned_32 := 0;
   Wrapped : Boolean := False;
   function Owned return Boolean is
     (Mapping_Valid and then Active and then CPU_Base = Base and then
      Backing_Bytes = Bytes and then Owner_Ready and then Coherent_Ready);
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Barrier;
   procedure Load_Tail (Value : out Unsigned_32; OK : out Boolean) is
   begin
      Value := 0; OK := False;
      if not Owned then return; end if;
      Barrier;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4124));
      begin Value := Word; end;
      OK := Owned;
   end Load_Tail;
   procedure Store_Word (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := False;
      if not Owned or else
        (if Wrapped then not (Offset >= First and Offset < Writer.Ring_Bytes) and
                            not (Offset < Last)
         else Offset < First or Offset >= Last) or else
        Offset mod 4 /= 0 then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Ring_Base + Unsigned_64 (Offset)));
      begin Word := Value; end;
      OK := Owned;
   end Store_Word;
   function Publish_Words (Offset, Bytes : Unsigned_32) return Boolean is
      Page_First, Page_Last : Unsigned_64;
   begin
      if not Owned or else
        (if Wrapped then not ((Offset = First and Bytes = Writer.Ring_Bytes - First) or
                              (Offset = 0 and Bytes = Last))
         else Offset /= First or Bytes /= Last - First)
      then return False; end if;
      Page_First := Unsigned_64 (Offset / 4096) * 4096;
      Page_Last := Unsigned_64 ((Offset + Bytes + 4095) / 4096) * 4096;
      -- Only command-ring pages: GPU reads them but never writes them.
      -- Do not flush the concurrently GPU-written saved-context page.
      return Intel_GPU_DMA_Cache.Flush_Range
        (Ring_Base + Page_First, Page_Last - Page_First) and then Owned;
   end Publish_Words;
   procedure Store_Tail (Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := False;
      if not Owned or else Value /= Last or else Value mod 8 /= 0 then return; end if;
      Barrier;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4124));
      begin Word := Value; end;
      OK := Owned;
   end Store_Tail;
   function Visible return Boolean is
   begin
      if not Owned then return False; end if;
      -- ADL-N coherent system memory. CT publication supplies another barrier
      -- before its descriptor tail; other memory types require another path.
      Barrier;
      return Owned;
   end Visible;
   procedure Fail (Object : in out Channel) is
   begin Writer.Fail (Object.Inner); end Fail;
   function Tail (Object : Channel) return Unsigned_32 is (Writer.Tail (Object.Inner));
   function Sequence (Object : Channel) return Unsigned_32 is (Writer.Sequence (Object.Inner));
   procedure Read_Saved_Pointers
     (Object : Channel; Head, Tail : out Unsigned_32; OK : out Boolean) is
   begin
      Head := 0; Tail := 0; OK := False;
      if Active then return; end if;
      Base := CPU_Base; Bytes := Backing_Bytes;
      if not Mapping_Valid or else
        (Object.Bound and then (Object.Base /= Base or Object.Bytes /= Bytes)) or else
        not Owner_Ready or else not Coherent_Ready
      then return; end if;
      Barrier;
      declare
         Saved_Head : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4116));
         Saved_Tail : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4124));
      begin Head := Saved_Head; Tail := Saved_Tail; end;
      Barrier;
      OK := CPU_Base = Base and then Backing_Bytes = Bytes and then
        Owner_Ready and then Coherent_Ready;
   end Read_Saved_Pointers;
   function Select_Channel (Object : in out Channel) return Boolean is
   begin
      if Active then Fail (Object); return False; end if;
      Base := CPU_Base; Bytes := Backing_Bytes;
      if not Mapping_Valid or else
        (Object.Bound and then (Object.Base /= Base or Object.Bytes /= Bytes))
      then Fail (Object); return False; end if;
      Object.Bound := True; Object.Base := Base; Object.Bytes := Bytes;
      Ring_Base := Base + 65536;
      return True;
   end Select_Channel;
   procedure Append (Object : in out Channel; Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Success : out Boolean) is
      Status : Writer.Result;
      use type Writer.Result;
   begin
      Success := False;
      if not Select_Channel (Object) then return; end if;
      First := Tail (Object);
      Wrapped := First > Writer.Ring_Bytes - Writer.Guard_Bytes - Writer.Segment_Bytes;
      Last := (if Wrapped then Writer.Segment_Bytes else First + Writer.Segment_Bytes);
      Active := True;
      Writer.Append (Object.Inner, Segment, Status);
      Active := False;
      Success := Status = Writer.Published;
   end Append;
   procedure Append (Object : in out Channel; Segment : Intel_GPU_ADLN_Barrier.Segment;
                     Success : out Boolean) is
      Status : Writer.Result;
      use type Writer.Result;
   begin
      Success := False;
      if not Select_Channel (Object) then return; end if;
      First := Tail (Object);
      Wrapped := First > Writer.Ring_Bytes - Writer.Guard_Bytes - Writer.Barrier_Bytes;
      Last := (if Wrapped then Writer.Barrier_Bytes else First + Writer.Barrier_Bytes);
      Active := True;
      Writer.Append (Object.Inner, Segment, Status);
      Active := False;
      Success := Status = Writer.Published;
   end Append;
end Intel_GPU_Native_Live_Ring;
