package body XHCI_Ports with SPARK_Mode => On is
   function Before_Enumeration (Status : Unsigned_32) return Reset_Action is
   begin
      if (Status and 1) = 0 then
         return Disconnected;
      elsif (Status and 16#8000_0010#) /= 0 then
         --  A hot or warm reset is already underway; do not restart it.
         return Wait_For_Reset;
      elsif (Status and 2) /= 0 and then
        (Shift_Right (Status, 10) and 15) >= 4
      then
         return Already_Enabled;
      else
         return Start_Reset;
      end if;
   end Before_Enumeration;

   function Reset_Write (Status : Unsigned_32) return Unsigned_32 is
     --  Preserve ordinary RW controls: power, indicators, and wake enables.
     --  PED is RW1C (writing one disables the port), as are change flags.
     --  Do not echo either, or the link-state strobe/warm-reset trigger.
     ((Status and 16#0E00_C200#) or 16#10#);
end XHCI_Ports;
