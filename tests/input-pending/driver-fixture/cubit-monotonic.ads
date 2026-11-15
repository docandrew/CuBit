with Interfaces;
--  Fixture clock: ps2's acquisition stamps read 100 ms + 250 us.
package CuBit.Monotonic is
   Fixture_Us : constant := 100_250;
   Fixture_Ms : constant := 100;
   type Reading (Available : Boolean := False) is record
      case Available is
         when True => Microseconds : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Read return Reading;
   Unavailable_Us : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   function Microseconds return Interfaces.Unsigned_64;
   function Milliseconds return Interfaces.Unsigned_64;
end CuBit.Monotonic;
