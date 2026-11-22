with XHCI_Registers; use XHCI_Registers;

package body XHCI_Ports with SPARK_Mode => On is
   function Before_Enumeration (Status : Unsigned_32) return Reset_Action is
   begin
      if not Connected (Status) then
         return Disconnected;
      elsif In_Reset (Status) then
         --  A hot or warm reset is already underway; do not restart it.
         return Wait_For_Reset;
      elsif Enabled (Status) and then Is_Super_Speed (Status) then
         return Already_Enabled;
      else
         return Start_Reset;
      end if;
   end Before_Enumeration;

   function Reset_Write (Status : Unsigned_32) return Unsigned_32 is
     --  Preserve ordinary RW controls: power, indicators, and wake enables.
     --  PED is RW1C (writing one disables the port), as are change flags.
     --  Do not echo either, or the link-state strobe/warm-reset trigger.
     ((Status and PORTSC_PRESERVE) or PORTSC_PR);
end XHCI_Ports;
