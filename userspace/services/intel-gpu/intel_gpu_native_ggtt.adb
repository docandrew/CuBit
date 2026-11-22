with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_GGTT;
package body Intel_GPU_Native_GGTT is
   use Interfaces;
   -- Same fixed UC alias as Intel_GPU_GGTT_Mapping. No caller-selected pointer.
   Base : constant Integer_Address := 16#6400_0000#;
   function Accessible (Index : Unsigned_64) return Boolean is
      Bytes : constant Unsigned_64 := Mapping_Bytes;
   begin
      return Owner_Ready and then Bytes in 2_097_152 | 4_194_304 | 8_388_608
        and then Index < Bytes / 8;
   end Accessible;
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                       Success : out Boolean) is
   begin
      Value := Unsigned_64'Last; Success := False;
      if not Accessible (Index) then return; end if;
      declare
         Entry_Value : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (Base + Integer_Address (Index * 8));
      begin Value := Entry_Value; end;
      Success := Value /= Unsigned_64'Last and then Owner_Ready;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; Success : out Boolean) is
      DMA : constant Unsigned_64 := Value and not Unsigned_64'(4095);
   begin
      Success := False;
      if not Accessible (Index) or else
        Intel_GPU_GGTT.Encode_System_Page (DMA) /= Value or else Value = 0 or else
        not Write_Allowed (Index, Value)
      then return; end if;
      declare
         Entry_Value : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (Base + Integer_Address (Index * 8));
      begin
         -- Write_Allowed binds this exact index/value to an exclusively
         -- retained allocation after takeover. Old PTE bits are not ownership.
         -- Still reject an inaccessible aperture before issuing a store.
         if Entry_Value = Unsigned_64'Last or else not Owner_Ready or else
           not Write_Allowed (Index, Value) then return; end if;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         Entry_Value := Value;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      end;
      Success := Owner_Ready;
   end Write_PTE;
end Intel_GPU_Native_GGTT;
