with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
with Intel_GPU_Initial_Ring_Publish;
with Intel_GPU_Submission_Backing;
package body Intel_GPU_Native_Initial_Ring is
   Base : constant Unsigned_64 := 16#6108C000#;
   Ring_Bytes : constant Unsigned_32 :=
     Intel_GPU_ADLN_Context_Init.Command_Words'Length * 4;
   Active, Attempted : Boolean := False;
   function Owned return Boolean is
     (Active and then Owner_Ready and then Exclusive_Ready);
   function Ring_Word (Offset : Unsigned_32) return Boolean is
     (Offset in 65536 .. 65536 + Ring_Bytes - 4 and then Offset mod 4 = 0);
   procedure Store (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := False;
      if not Owned or else not (Ring_Word (Offset) or else
        (Offset = 4124 and then Value = Ring_Bytes)) then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
      begin Word := Value; end;
      OK := Owned;
   end Store;
   procedure Load (Offset : Unsigned_32; Value : out Unsigned_32;
                   OK : out Boolean) is
   begin
      Value := 0; OK := False;
      if not Owned or else not (Ring_Word (Offset) or Offset in 4116 | 4124)
      then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
      begin Value := Word; end;
      OK := Owned;
   end Load;
   function Flush (Offset, Bytes : Unsigned_32) return Boolean is
      Page : Unsigned_64;
   begin
      if not Owned then return False; end if;
      if Offset = 65536 and Bytes = Ring_Bytes then Page := Base + 65536;
      elsif Offset = 4124 and Bytes = 4 then Page := Base + 4096;
      else return False;
      end if;
      -- Cache helper requires complete aligned lines. Both pages are wholly
      -- owned and the context has never run, so no concurrent GPU state write.
      return Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) and then Owned;
   end Flush;
   package Writer is new Intel_GPU_Initial_Ring_Publish (Owned, Store, Load, Flush);
   Attempt : Writer.Attempt;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Value := Unsigned_64'Last; OK := False;
      if not Owner_Ready then return; end if;
      -- CPU never writes the HWSP after backing initialization. Flush the
      -- retained page before an aligned volatile64 read of the GPU marker.
      if not Intel_GPU_DMA_Cache.Flush_Range (Base, 4096) or else not Owner_Ready
      then return; end if;
      declare
         Marker : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + 16#D0#));
      begin Value := Marker; end;
      OK := Owner_Ready;
   end Read_Marker;
   procedure Read_Batch_Result (Value : out Unsigned_64; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Page : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Completion_Page) - Backing.First;
   begin
      Value := Unsigned_64'Last; OK := False;
      if not Owner_Ready then return; end if;
      -- CPU does not modify this page after initial materialization. The
      -- probe writes one DWORD; read64 also checks the zero upper sentinel.
      if not Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) or else not Owner_Ready
      then return; end if;
      declare
         Word : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Page));
      begin Value := Word; end;
      OK := Owner_Ready;
   end Read_Batch_Result;
   procedure Publish (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Success : out Boolean) is
      Status : Writer.Result;
      Marker : Unsigned_64;
      OK : Boolean;
      use type Writer.Result;
   begin
      Success := False;
      if Attempted or else not Segment.Valid then return; end if;
      Attempted := True;
      if not Owner_Ready or else not Exclusive_Ready then return; end if;
      Read_Marker (Marker, OK);
      if not OK or else Marker /= 0 then return; end if;
      Read_Batch_Result (Marker, OK);
      if not OK or else Marker /= 0 then return; end if;
      Active := True;
      Writer.Publish (Attempt, Segment, Status);
      Success := Status = Writer.Published;
      Active := False;
   end Publish;
end Intel_GPU_Native_Initial_Ring;
