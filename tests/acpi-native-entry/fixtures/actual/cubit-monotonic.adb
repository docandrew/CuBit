with CuBit.Published_Clock;
package body CuBit.Monotonic is
   function Read return Reading is
      Value     : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      CuBit.Published_Clock.Microseconds (Value, Available);
      if not Available then
         return (Available => False);
      end if;
      return (Available => True, Microseconds => Value);
   end Read;

   function Milliseconds return Interfaces.Unsigned_64 is
     (CuBit.Published_Clock.Milliseconds);
end CuBit.Monotonic;
