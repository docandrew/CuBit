with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_ADLN_MOCS;
with Intel_GPU_Reset_Pages;
package body Intel_GPU_Native_MOCS is
   use Interfaces;
   package Plan renames Intel_GPU_ADLN_MOCS;
   function Address_For (Offset : Unsigned_32) return Unsigned_64 is
      Allowed : Boolean := False;
   begin
      for I in Plan.Register_Index loop
         Allowed := Allowed or Offset = Plan.Offset (I);
      end loop;
      if not Allowed then return 0; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = Unsigned_64 (Offset / 4096 * 4096) then
            return Intel_GPU_Reset_Pages.Virtual_Base + Unsigned_64 (Page) * 4096 +
              Unsigned_64 (Offset mod 4096);
         end if;
      end loop;
      return 0;
   end Address_For;
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Address_Value : constant Unsigned_64 := Address_For (Offset);
      Value : Unsigned_32;
   begin
      if Address_Value = 0 or else not Owner_Ready then return Unsigned_32'Last; end if;
      declare Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (Address_Value));
      begin Value := Register_Value; end;
      return (if Owner_Ready then Value else Unsigned_32'Last);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      Address_Value : constant Unsigned_64 := Address_For (Offset);
      Allowed : Boolean := False;
   begin
      Success := False;
      for I in Plan.Register_Index loop
         Allowed := Allowed or (Offset = Plan.Offset (I) and Value = Plan.Value (I));
      end loop;
      if not Allowed or else Address_Value = 0 or else not Owner_Ready then return; end if;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Integer_Address (Address_Value));
      begin Register_Value := Value; end;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Success := Owner_Ready;
   end Write32;
end Intel_GPU_Native_MOCS;
