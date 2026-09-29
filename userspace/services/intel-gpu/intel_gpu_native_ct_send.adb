with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
package body Intel_GPU_Native_CT_Send is
   use Interfaces;
   function Accessible return Boolean is
     (CPU_Base >= 4096 and then CPU_Base mod 4096 = 0 and then
      CPU_Base <= 16#0000_7FFF_FFFF_8000# and then Owner_Ready);
   procedure Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Fence;
   function Load (Offset : Unsigned_64) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (CPU_Base + Offset));
   begin return Value; end Load;
   procedure Store (Offset : Unsigned_64; Value : Unsigned_32) is
      Target : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (CPU_Base + Offset));
   begin Target := Value; end Store;
   procedure Read_Descriptor
     (Head, Tail, Status : out Unsigned_32; Success : out Boolean) is
   begin
      Head := 0; Tail := 0; Status := Unsigned_32'Last; Success := False;
      if not Accessible then return; end if;
      Head := Load (0);
      -- Observe firmware consumption before reusing ring words.
      Fence;
      Tail := Load (4); Status := Load (8);
      for I in Unsigned_64 range 3 .. 15 loop
         if Load (I * 4) /= 0 then return; end if;
      end loop;
      Success := Accessible;
   end Read_Descriptor;
   procedure Write_Word (Index, Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if Index >= 1024 or else not Accessible then return; end if;
      Store (8192 + Unsigned_64 (Index) * 4, Value);
      Success := Accessible;
   end Write_Word;
   procedure Write_Tail (Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if Value >= 1024 or else not Accessible then return; end if;
      Fence;
      Store (4, Value);
      Success := Accessible;
   end Write_Tail;
   procedure Make_Visible (Success : out Boolean) is
   begin
      Success := False;
      if not Accessible then return; end if;
      Fence;
      Success := Accessible;
   end Make_Visible;
end Intel_GPU_Native_CT_Send;
