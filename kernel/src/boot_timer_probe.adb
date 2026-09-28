with Interfaces; use Interfaces;
with PIC;
with X86;

package body Boot_Timer_Probe with SPARK_Mode => Off is
   Previous : Unsigned_64 := 0;

   procedure Start is
   begin
      Entries := 0;
      Completed := 0;
      PIC_IRQ0 := 0;
      ISR_Bits := 0;
      Minimum_Gap := 0;
      Maximum_Gap := 0;
      Maximum_Handler := 0;
      Previous := 0;
      Enabled := True;
   end Start;

   procedure Stop is
   begin
      Enabled := False;
   end Stop;

   procedure Enter_IRQ (Stamp : out Unsigned_64) is
      In_Service : Unsigned_8;
      Gap : Unsigned_64;
   begin
      Stamp := X86.readOrderedTSC;
      if Entries /= 0 then
         Gap := Stamp - Previous;
         if Entries = 1 then
            Minimum_Gap := Gap;
         else
            Minimum_Gap := Unsigned_64'Min (Minimum_Gap, Gap);
         end if;
         Maximum_Gap := Unsigned_64'Max (Maximum_Gap, Gap);
      end if;
      Previous := Stamp;
      Entries := Entries + 1;
      -- Before the existing EOI: observe whether PIC IRQ0 is really in service.
      -- Restore the conventional IRR read selection, without acknowledging.
      X86.out8 (PIC.PIC1Command, 16#0B#);
      X86.in8 (PIC.PIC1Command, In_Service);
      X86.out8 (PIC.PIC1Command, 16#0A#);
      ISR_Bits := ISR_Bits or In_Service;
      if (In_Service and 1) /= 0 then
         PIC_IRQ0 := PIC_IRQ0 + 1;
      end if;
   end Enter_IRQ;

   procedure Leave_IRQ (Stamp : Unsigned_64) is
      Elapsed : constant Unsigned_64 := X86.readOrderedTSC - Stamp;
   begin
      Maximum_Handler := Unsigned_64'Max (Maximum_Handler, Elapsed);
      Completed := Completed + 1;
   end Leave_IRQ;
end Boot_Timer_Probe;
