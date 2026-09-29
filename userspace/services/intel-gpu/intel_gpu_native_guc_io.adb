with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Reset_Pages;
package body Intel_GPU_Native_GuC_IO is
   use Interfaces;
   function Address_For (Offset : Unsigned_32; Writing : Boolean)
     return Unsigned_64 is
      Allowed : Boolean;
   begin
      if Offset mod 4 /= 0 then return 0; end if;
      if Writing then
         Allowed := Offset in 16#C050# | 16#C064# | 16#C340# | 16#13816C#
           or else Offset in 16#C180# .. 16#C1B8#
           or else Offset in 16#C200# .. 16#C2FC#
           or else Offset in 16#C300# .. 16#C314#;
      else
         Allowed := Offset in 16#C000# | 16#C050# | 16#C064# |
           16#C314# | 16#C340# | 16#13816C#;
      end if;
      if not Allowed then return 0; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Unsigned_64 (Offset) / 4096 * 4096 = Intel_GPU_Reset_Pages.Offset (Page) then
            return Intel_GPU_Reset_Pages.Virtual_Base + Unsigned_64 (Page) * 4096 +
              Unsigned_64 (Offset mod 4096);
         end if;
      end loop;
      return 0;
   end Address_For;
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Address_Value : constant Unsigned_64 := Address_For (Offset, False);
      Value : Unsigned_32;
   begin
      if Address_Value = 0 or else not Owner_Ready then return Unsigned_32'Last; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address_Value));
      begin Value := Register_Value; end;
      return (if Owner_Ready then Value else Unsigned_32'Last);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      Address_Value : constant Unsigned_64 := Address_For (Offset, True);
   begin
      Success := False;
      if Address_Value = 0 or else not Owner_Ready then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address_Value));
      begin Register_Value := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := Owner_Ready;
   end Write32;
end Intel_GPU_Native_GuC_IO;
