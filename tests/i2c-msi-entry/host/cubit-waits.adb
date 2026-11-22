pragma Ada_2022;
with CuBit.Messages;
package body CuBit.Waits is
   function Sleep_For (Milliseconds : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
   begin
      return CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, Milliseconds);
   end Sleep_For;
end CuBit.Waits;
