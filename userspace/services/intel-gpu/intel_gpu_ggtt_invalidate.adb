with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;
with Intel_GPU_Reset_Pages;
with Intel_GPU_Native_Reset;
with Intel_GPU_GGTT_Mapping;
package body Intel_GPU_GGTT_Invalidate is
   function Issue (Control_Pages_Ready : Boolean) return Boolean is
      Register_Offset : constant Unsigned_64 := 16#CEE8#;
      Address_Value : Integer_Address := 0;
   begin
      if not Control_Pages_Ready or else
        not Intel_GPU_Native_Reset.Last_Succeeded or else
        not Intel_GPU_GGTT_Mapping.Ready
      then return False; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = Register_Offset / 4096 * 4096 then
            Address_Value := Integer_Address (Intel_GPU_Reset_Pages.Virtual_Base) +
              Integer_Address (Page) * 4096 + Integer_Address (Register_Offset mod 4096);
         end if;
      end loop;
      if Address_Value = 0 then return False; end if;
      -- Both mappings are UC/NX. Match the Gen12 pre-CT i915 path: an exact
      -- 32-bit invalidate write, not a read/modify/write or invented ack loop.
      -- Keep CPU/compiler ordering explicit around preceding PTE stores.
      Asm ("mfence", Clobber => "memory", Volatile => True);
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Address_Value);
      begin
         Register_Value := 1;
      end;
      Asm ("mfence", Clobber => "memory", Volatile => True);
      return True;
   end Issue;
end Intel_GPU_GGTT_Invalidate;
