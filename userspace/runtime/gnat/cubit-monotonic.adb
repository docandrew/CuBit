with CuBit.Messages;
package body CuBit.Monotonic is
   use type Interfaces.Unsigned_64;
   function Read return Reading is
      Value : constant Interfaces.Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_READ_MONOTONIC_MICROSECONDS);
   begin
      if Value = Interfaces.Unsigned_64'Last then
         return (Available => False);
      end if;
      return (Available => True, Microseconds => Value);
   end Read;
end CuBit.Monotonic;
