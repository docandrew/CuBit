with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
package body Intel_GPU_DMA_Cache is
   function Flush_Range (Address, Bytes : Unsigned_64) return Boolean is
      A, B, C, D : Unsigned_32;
      Line_Bytes, Offset : Unsigned_64;
   begin
      if Address = 0 or else Bytes = 0 or else Bytes > 16 * 1024 * 1024 or else
        Address > Unsigned_64'Last - Bytes
      then return False; end if;
      Asm ("cpuid",
        Inputs => [Unsigned_32'Asm_Input ("a", 1), Unsigned_32'Asm_Input ("c", 0)],
        Outputs => [Unsigned_32'Asm_Output ("=a", A), Unsigned_32'Asm_Output ("=b", B),
                    Unsigned_32'Asm_Output ("=c", C), Unsigned_32'Asm_Output ("=d", D)],
        Volatile => True);
      if (D and 16#0008_0000#) = 0 then return False; end if;
      Line_Bytes := Unsigned_64 (Shift_Right (B, 8) and 255) * 8;
      if Line_Bytes = 0 or else Line_Bytes > 4096 or else
        (Line_Bytes and (Line_Bytes - 1)) /= 0 or else
        Address mod Line_Bytes /= 0 or else Bytes mod Line_Bytes /= 0
      then return False; end if;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      Offset := 0;
      while Offset < Bytes loop
         Asm ("clflush (%0)", Inputs => Unsigned_64'Asm_Input ("r", Address + Offset),
              Clobber => "memory", Volatile => True);
         Offset := Offset + Line_Bytes;
      end loop;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      return True;
   end Flush_Range;
end Intel_GPU_DMA_Cache;
