with Interfaces; use Interfaces;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
with Intel_GPU_Live_Ring_Publish;
package body Intel_GPU_Native_Live_Ring is
   Base : constant Unsigned_64 := 16#6108C000#;
   Ring_Base : constant Unsigned_64 := Base + 65536;
   Active : Boolean := False;
   First, Last : Unsigned_32 := 0;
   function Owned return Boolean is
     (Active and then Owner_Ready and then Coherent_Ready);
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
      if not Owned or else Offset < First or else Offset >= Last or else
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
      if not Owned or else Offset /= First or else Bytes /= Last - First
      then return False; end if;
      Page_First := Unsigned_64 (First / 4096) * 4096;
      Page_Last := Unsigned_64 ((Last + 4095) / 4096) * 4096;
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
   package Writer is new Intel_GPU_Live_Ring_Publish
     (Owned, Read_Marker, Load_Tail, Store_Word, Publish_Words, Store_Tail, Visible);
   Object : Writer.Channel;
   procedure Fail is
   begin Writer.Fail (Object); end Fail;
   function Tail return Unsigned_32 is (Writer.Tail (Object));
   function Sequence return Unsigned_32 is (Writer.Sequence (Object));
   procedure Read_Saved_Pointers (Head, Tail : out Unsigned_32; OK : out Boolean) is
   begin
      Head := 0; Tail := 0; OK := False;
      if not Owner_Ready or else not Coherent_Ready then return; end if;
      Barrier;
      declare
         Saved_Head : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4116));
         Saved_Tail : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 4124));
      begin Head := Saved_Head; Tail := Saved_Tail; end;
      Barrier;
      OK := Owner_Ready and then Coherent_Ready;
   end Read_Saved_Pointers;
   procedure Append (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Success : out Boolean) is
      Status : Writer.Result;
      use type Writer.Result;
   begin
      Success := False;
      if Active then Fail; return; end if;
      First := Tail;
      if First > Writer.Ring_Bytes - Writer.Guard_Bytes - Writer.Segment_Bytes
      then return; end if;
      Last := First + Writer.Segment_Bytes;
      Active := True;
      Writer.Append (Object, Segment, Status);
      Active := False;
      Success := Status = Writer.Published;
   end Append;
end Intel_GPU_Native_Live_Ring;
