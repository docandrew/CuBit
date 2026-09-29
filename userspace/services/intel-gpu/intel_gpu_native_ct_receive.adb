with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
package body Intel_GPU_Native_CT_Receive is
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
   procedure Read_Descriptor
     (Head, Tail, Status : out Unsigned_32; Success : out Boolean) is
   begin
      Head := 0; Tail := 0; Status := Unsigned_32'Last; Success := False;
      if not Accessible then return; end if;
      Head := Load (4096);
      Tail := Load (4100);
      -- Firmware publishes payload before tail; order subsequent CPU reads
      -- after this observation of the producer's tail.
      Fence;
      Status := Load (4104);
      for I in Unsigned_64 range 3 .. 15 loop
         if Load (4096 + I * 4) /= 0 then return; end if;
      end loop;
      Success := Accessible;
   end Read_Descriptor;
   procedure Read_Word (Index : Unsigned_32; Value : out Unsigned_32;
                        Success : out Boolean) is
   begin
      Value := 0; Success := False;
      if Index >= 4096 or else not Accessible then return; end if;
      Value := Load (12288 + Unsigned_64 (Index) * 4);
      Success := Accessible;
      if not Success then Value := 0; end if;
   end Read_Word;
   procedure Finish_Reads (Success : out Boolean) is
   begin
      Success := False;
      if not Accessible then return; end if;
      Fence;
      Success := Accessible;
   end Finish_Reads;
   procedure Write_Head (Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if Value >= 4096 or else not Accessible then return; end if;
      Fence;
      declare
         Head : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (CPU_Base + 4096));
      begin Head := Value; end;
      Success := Accessible;
   end Write_Head;
   procedure Make_Visible (Success : out Boolean) is
   begin
      Finish_Reads (Success);
   end Make_Visible;
end Intel_GPU_Native_CT_Receive;
