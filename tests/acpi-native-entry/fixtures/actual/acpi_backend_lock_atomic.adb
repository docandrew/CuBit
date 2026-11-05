pragma Ada_2022;
with System.Machine_Code;
package body ACPI_Backend_Lock_Atomic with SPARK_Mode => Off is
   use Interfaces;
   use System.Machine_Code;
   function Read_Word (Cell : not null access Word) return Unsigned_32 is
     (Unsigned_32 (Cell.all));
   function Compare_Exchange
     (Cell : not null access Word; Expected, Replacement : Unsigned_32) return Unsigned_32
   is
      Observed : Unsigned_32;
   begin
      -- Same operand/clobber convention as CuBit.Futex_Locks.CAS. The memory
      -- operand is the actual retained cell, never a by-value temporary.
      Asm ("lock cmpxchgl %2, %1",
        Outputs => [Unsigned_32'Asm_Output ("=a", Observed), Word'Asm_Output ("+m", Cell.all)],
        Inputs => [Unsigned_32'Asm_Input ("r", Replacement), Unsigned_32'Asm_Input ("a", Expected)],
        Clobber => "memory, cc", Volatile => True);
      return Observed;
   end Compare_Exchange;
end ACPI_Backend_Lock_Atomic;
