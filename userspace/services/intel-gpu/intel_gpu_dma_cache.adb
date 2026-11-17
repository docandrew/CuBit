with Intel_GPU_Physical_Extents;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
package body Intel_GPU_DMA_Cache is
   --  CPUID is serializing (and traps under a hypervisor), so it is read
   --  once, on the first flush, not on every frame's Present.
   type Flush_Instruction is (Unprobed, Unsupported, Use_Clflush, Use_Clflushopt);
   --  Line_Bytes is written before Method, and Method is atomic, so a thread
   --  that sees a probed Method also sees its line size. Concurrent probes
   --  compute the same values.
   Line_Bytes : Unsigned_64 := 0 with Volatile;
   Method     : Flush_Instruction := Unprobed with Atomic;

   --  CPUID.01H:EDX[19] CLFSH; CPUID.01H:EBX[15:8] line size in 8-byte
   --  units; CPUID.(EAX=07H,ECX=0):EBX[23] CLFLUSHOPT (Intel SDM Vol. 2A).
   CLFSH_Bit      : constant Unsigned_32 := 2 ** 19;
   CLFLUSHOPT_Bit : constant Unsigned_32 := 2 ** 23;
   Line_Unit      : constant := 8;
   Largest_Line   : constant := 4096;

   procedure CPUID (Leaf, Subleaf : Unsigned_32; A, B, C, D : out Unsigned_32) is
   begin
      Asm ("cpuid",
        Inputs => [Unsigned_32'Asm_Input ("a", Leaf), Unsigned_32'Asm_Input ("c", Subleaf)],
        Outputs => [Unsigned_32'Asm_Output ("=a", A), Unsigned_32'Asm_Output ("=b", B),
                    Unsigned_32'Asm_Output ("=c", C), Unsigned_32'Asm_Output ("=d", D)],
        Volatile => True);
   end CPUID;

   procedure Probe is
      A, B, C, D : Unsigned_32;
      Max_Leaf   : Unsigned_32;
      Size       : Unsigned_64;
   begin
      CPUID (0, 0, Max_Leaf, B, C, D);
      CPUID (1, 0, A, B, C, D);
      Size := Unsigned_64 (Shift_Right (B, 8) and 255) * Line_Unit;
      if (D and CLFSH_Bit) = 0 or else Size = 0 or else Size > Largest_Line or else
        (Size and (Size - 1)) /= 0
      then
         Method := Unsupported;
         return;
      end if;
      Line_Bytes := Size;
      if Max_Leaf >= 7 then
         CPUID (7, 0, A, B, C, D);
         if (B and CLFLUSHOPT_Bit) /= 0 then
            Method := Use_Clflushopt;
            return;
         end if;
      end if;
      Method := Use_Clflush;
   end Probe;

   function Flush_Range (Address, Bytes : Unsigned_64) return Boolean is
      Offset : Unsigned_64;
   begin
      if Address = 0 or else Bytes = 0 or else Bytes > Intel_GPU_Physical_Extents.Capacity or else
        Address > Unsigned_64'Last - Bytes
      then return False; end if;
      if Method = Unprobed then Probe; end if;
      if Method = Unsupported or else
        Address mod Line_Bytes /= 0 or else Bytes mod Line_Bytes /= 0
      then return False; end if;
      --  The fences order the CPU's earlier stores before the flushes and the
      --  flushes before the caller's later register write that hands the
      --  memory to the device. CLFLUSHOPT is weakly ordered among itself
      --  (the lines flush in parallel) but ordered by MFENCE, so the closing
      --  fence keeps the same guarantee CLFLUSH gave.
      Asm ("mfence", Clobber => "memory", Volatile => True);
      Offset := 0;
      if Method = Use_Clflushopt then
         while Offset < Bytes loop
            Asm ("clflushopt (%0)", Inputs => Unsigned_64'Asm_Input ("r", Address + Offset),
                 Clobber => "memory", Volatile => True);
            Offset := Offset + Line_Bytes;
         end loop;
      else
         while Offset < Bytes loop
            Asm ("clflush (%0)", Inputs => Unsigned_64'Asm_Input ("r", Address + Offset),
                 Clobber => "memory", Volatile => True);
            Offset := Offset + Line_Bytes;
         end loop;
      end if;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      return True;
   end Flush_Range;
end Intel_GPU_DMA_Cache;
