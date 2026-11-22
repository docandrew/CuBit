with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Reset_Pages;
package body Intel_GPU_Native_RCS_Start is
   use Interfaces;
   function Write_Allowed (Offset, Value : Unsigned_32) return Boolean is
      GPU : constant Unsigned_64 := Status_GPU;
   begin
      if GPU = 0 or else GPU mod 4096 /= 0 or else GPU > 16#FEE00000# - 4096 then
         return False;
      end if;
      return (case Offset is
        when 16#2098# => Value = Unsigned_32'Last,
        when 16#2080# => Value = Unsigned_32 (GPU),
        when 16#229C# => Value = 16#80008#,
        when 16#209C# => Value = 16#1000000#,
        when others => False);
   end Write_Allowed;
   function Address_For (Offset : Unsigned_32) return Integer_Address is
     (Integer_Address (Intel_GPU_Reset_Pages.Virtual_Base + 4096 +
       Unsigned_64 (Offset - 16#2000#)));
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Value : Unsigned_32;
   begin
      if Offset /= 16#209C# or else not Owner_Ready then return Unsigned_32'Last; end if;
      declare R : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_For (Offset));
      begin Value := R; end;
      return (if Owner_Ready then Value else Unsigned_32'Last);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if not Owner_Ready or else not Write_Allowed (Offset, Value) then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare R : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_For (Offset));
      begin R := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := Owner_Ready;
   end Write32;
end Intel_GPU_Native_RCS_Start;
