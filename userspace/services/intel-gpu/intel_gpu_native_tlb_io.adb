with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Reset_Pages;
with Intel_GPU_TLB_Registers;
package body Intel_GPU_Native_TLB_IO is
   Fault : Boolean := False;
   function Failed return Boolean is (Fault);
   function Address_For (Offset : Unsigned_32) return Unsigned_64 is
   begin
      if GuC_Only then
         if Offset /= Intel_GPU_TLB_Registers.GuC_Offset then return 0; end if;
      elsif Offset /= Intel_GPU_TLB_Registers.GFX_Offset and then
        Offset /= Intel_GPU_TLB_Registers.OA_Offset then return 0; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = Unsigned_64 (Offset / 4096 * 4096) then
            return Intel_GPU_Reset_Pages.Virtual_Base + Unsigned_64 (Page) * 4096 +
              Unsigned_64 (Offset mod 4096);
         end if;
      end loop;
      return 0;
   end Address_For;
   function Admit (Offset : Unsigned_32) return Boolean is
   begin
      if Fault then return False; end if;
      if Address_For (Offset) = 0 or else not Owner_Ready then
         Fault := True; return False;
      end if;
      return True;
   end Admit;
   procedure Read_Register (Offset : Unsigned_32; Value : out Unsigned_32;
                            OK : out Boolean) is
   begin
      Value := 0; OK := False;
      if not Admit (Offset) then return; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address_For (Offset)));
      begin Value := Register_Value; end;
      if not Owner_Ready then Fault := True; Value := 0; return; end if;
      OK := True;
   end Read_Register;
   procedure Write_Register (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := False;
      if Value /= 1 then Fault := True; return; end if;
      if not Admit (Offset) then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address_For (Offset)));
      begin Register_Value := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      if not Owner_Ready then Fault := True; return; end if;
      OK := True;
   end Write_Register;
end Intel_GPU_Native_TLB_IO;
