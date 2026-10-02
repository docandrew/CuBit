with Interfaces;
package CuBit.Monotonic is
   type Reading (Available : Boolean := False) is record
      case Available is
         when True => Microseconds : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Read return Reading;
end CuBit.Monotonic;
