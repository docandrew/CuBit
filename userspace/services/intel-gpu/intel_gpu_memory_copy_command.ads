with Interfaces; use Interfaces;
with System;
with Intel_GPU_VA_Encoding;
package Intel_GPU_Memory_Copy_Command with SPARK_Mode is
   -- TGL PRM Vol2a-12.21, RenderCS MI_COPY_MEM_MEM pp979-980.
   -- Copies one DWORD, not a completion fence. Both addresses use PPGTT.
   -- Caller owns/retains both ranges and supplies required ordering; no
   -- mapping, authority or cache-coherency guarantee is created here.
   type Bit is mod 2 with Size => 1;
   type Bits_8 is mod 2 ** 8 with Size => 8;
   type Bits_13 is mod 2 ** 13 with Size => 13;
   type Bits_6 is mod 2 ** 6 with Size => 6;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Header is record
      Length : Bits_8 := 3;
      Reserved : Bits_13 := 0;
      Global_Destination : Bit := 0;
      Global_Source : Bit := 0;
      Opcode : Bits_6 := 16#2E#;
      Command_Type : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Header use record
      Length at 0 range 0 .. 7;
      Reserved at 0 range 8 .. 20;
      Global_Destination at 0 range 21 .. 21;
      Global_Source at 0 range 22 .. 22;
      Opcode at 0 range 23 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (Value : Header) return Unsigned_32 is
     (Unsigned_32 (Value.Length) or Shift_Left (Unsigned_32 (Value.Reserved), 8)
      or Shift_Left (Unsigned_32 (Value.Global_Destination), 21)
      or Shift_Left (Unsigned_32 (Value.Global_Source), 22)
      or Shift_Left (Unsigned_32 (Value.Opcode), 23)
      or Shift_Left (Unsigned_32 (Value.Command_Type), 29));
   function Address_Valid (Value : Unsigned_64) return Boolean is
     (Value /= 0 and Value < 2 ** 48 and Value mod 4 = 0);
   type Command_Words is array (Natural range 0 .. 4) of Unsigned_32;
   type Command is record
      Valid : Boolean := False;
      Words : Command_Words := [others => 0];
   end record;
   -- Raw48 allocator addresses, not CPU/physical addresses. Canonical upper
   -- bits are generated here; malformed/unaligned inputs fail closed.
   function Build (Source, Destination : Unsigned_64) return Command
     with Post => Build'Result.Valid =
       (Address_Valid (Source) and Address_Valid (Destination)) and then
       (if Build'Result.Valid then Build'Result.Words (0) = 16#17000003# and then
          (Unsigned_64 (Build'Result.Words (1)) or
           Shift_Left (Unsigned_64 (Build'Result.Words (2)), 32)) =
             Intel_GPU_VA_Encoding.Canonical (Destination) and then
          (Unsigned_64 (Build'Result.Words (3)) or
           Shift_Left (Unsigned_64 (Build'Result.Words (4)), 32)) =
             Intel_GPU_VA_Encoding.Canonical (Source)
        else (for all W of Build'Result.Words => W = 0));
end Intel_GPU_Memory_Copy_Command;
