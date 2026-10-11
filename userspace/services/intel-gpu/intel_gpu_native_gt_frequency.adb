with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with Intel_GPU_GT_Frequency;
with Intel_GPU_Reset_Pages;
with Intel_GPU_SLPC;
package body Intel_GPU_Native_GT_Frequency is
   use Interfaces;
   package Pages renames Intel_GPU_Reset_Pages;
   Page_Bytes : constant := 4096;
   Request_Field : constant Unsigned_32 :=
     Shift_Left (Intel_GPU_GT_Frequency.Request_Mask, Intel_GPU_GT_Frequency.Request_Shift);
   -- Both registers must sit on the granted frequency page (also checked by
   -- tests/intel-gpu/reset_pages_tests.adb).
   function Page_Holds (Offset : Unsigned_32) return Boolean is
     (Pages.Offset (Pages.Frequency_Page) = Unsigned_64 (Offset / Page_Bytes * Page_Bytes));
   function Address_Of (Offset : Unsigned_32) return Integer_Address is
     (Integer_Address (Pages.Virtual_Base) +
      Integer_Address (Pages.Frequency_Page) * Page_Bytes +
      Integer_Address (Offset mod Page_Bytes));
   function Read (Offset : Unsigned_32) return Unsigned_32 is
      Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_Of (Offset));
   begin
      return Register_Value;
   end Read;
   procedure Write (Offset, Value : Unsigned_32) is
      Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (Address_Of (Offset));
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Register_Value := Value;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Write;
   procedure Unmask_ARAT (Before, After : out Unsigned_32; Success : out Boolean) is
   begin
      Before := 0; After := 0; Success := False;
      if not Owner_Ready or else not Page_Holds (Intel_GPU_SLPC.PM_Interrupt_Mask_Offset) then
         return;
      end if;
      Before := Read (Intel_GPU_SLPC.PM_Interrupt_Mask_Offset);
      Write (Intel_GPU_SLPC.PM_Interrupt_Mask_Offset, Intel_GPU_SLPC.Unmask_ARAT (Before));
      After := Read (Intel_GPU_SLPC.PM_Interrupt_Mask_Offset);
      Success := Owner_Ready and then
        (After and Intel_GPU_SLPC.ARAT_Expired_Mask_Bit) = 0;
   end Unmask_ARAT;
   procedure Request (Value : Unsigned_32; Readback : out Unsigned_32;
                      Success : out Boolean) is
   begin
      Readback := 0; Success := False;
      if not Owner_Ready or else (Value and not Request_Field) /= 0 or else
        not Page_Holds (Intel_GPU_GT_Frequency.RP_Request_Offset)
      then return; end if;
      Write (Intel_GPU_GT_Frequency.RP_Request_Offset, Value);
      Readback := Read (Intel_GPU_GT_Frequency.RP_Request_Offset);
      Success := Owner_Ready and then (Readback and Request_Field) = Value;
   end Request;
end Intel_GPU_Native_GT_Frequency;
