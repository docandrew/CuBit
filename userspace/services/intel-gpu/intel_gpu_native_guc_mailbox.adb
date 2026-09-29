with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_Reset_Pages;
package body Intel_GPU_Native_GuC_Mailbox is
   use Interfaces;
   Scratch : constant Unsigned_32 := 16#190240#;
   Interrupt_Register : constant Unsigned_32 := 16#1901F0#;
   -- i915 intel_guc_reg.h: GEN11_SOFT_SCRATCH, GEN11_GUC_HOST_INTERRUPT,
   -- GUC_SEND_TRIGGER. Main GT only, not the separate media GT mailbox.
   function Address_For (Offset : Unsigned_32; Writing : Boolean)
     return Unsigned_64 is
   begin
      if not ((Offset in Scratch .. Scratch + 12 and then Offset mod 4 = 0)
              or else (Writing and then Offset = Interrupt_Register))
      then return 0; end if;
      for Page in Intel_GPU_Reset_Pages.Page_Index loop
         if Intel_GPU_Reset_Pages.Offset (Page) = 16#190000# then
            return Intel_GPU_Reset_Pages.Virtual_Base + Unsigned_64 (Page) * 4096
              + Unsigned_64 (Offset mod 4096);
         end if;
      end loop;
      return 0;
   end Address_For;

   function Read_Word (Index : Natural) return Unsigned_32 is
      Value : Unsigned_32;
      Address_Value : Unsigned_64;
   begin
      if Index > 3 then return Unsigned_32'Last; end if;
      Address_Value := Address_For (Scratch + Unsigned_32 (Index) * 4, False);
      if Address_Value = 0 or else not Owner_Ready then return Unsigned_32'Last; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address_Value));
      begin Value := Register_Value; end;
      return (if Owner_Ready then Value else Unsigned_32'Last);
   end Read_Word;

   procedure Store (Offset, Value : Unsigned_32; Success : out Boolean) is
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
   end Store;

   procedure Write_Word
     (Index : Natural; Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if Index > 3 then return; end if;
      Store (Scratch + Unsigned_32 (Index) * 4, Value, Success);
   end Write_Word;

   procedure Notify (Success : out Boolean) is
   begin
      -- Posting read of the last request word belongs to the transport;
      -- do not read this write-only notification register.
      Store (Interrupt_Register, 1, Success);
   end Notify;
end Intel_GPU_Native_GuC_Mailbox;
