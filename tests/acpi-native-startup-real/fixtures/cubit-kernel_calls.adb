-- Hosted raw-kernel boundary. No guest sleep/submit instruction is executed.
package body CuBit.Kernel_Calls is
   function Call(Number:CuBit.Kernel_ABI.System_Call;
      A0,A1,A2,A3,A4,A5:Unsigned_64:=0) return Unsigned_64 is
      pragma Unreferenced(Number,A0,A1,A2,A3,A4,A5);
   begin return raise Program_Error with "unexpected raw guest kernel call";end Call;
   function Submit(Slot:Unsigned_64;Label:Unsigned_32;Length:Unsigned_8;
      W0,W1,W2,W3:Unsigned_64;Token:Unsigned_64) return Unsigned_64 is
      pragma Unreferenced(Slot,Label,Length,W0,W1,W2,W3,Token);
   begin return raise Program_Error with "unexpected raw guest submit";end Submit;
end CuBit.Kernel_Calls;
