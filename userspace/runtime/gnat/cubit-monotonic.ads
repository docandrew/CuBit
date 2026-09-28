with Interfaces;
package CuBit.Monotonic is
   type Reading (Available : Boolean := False) is record
      case Available is
         when True => Microseconds : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Read return Reading;
   --  High-resolution monotonic time in a boot-local backend epoch, unrelated
   --  to GETTIME's millisecond epoch or UTC. No defined continuity across
   --  suspend yet. Availability/resolution do not certify physical accuracy.
   --  Callers must bound waits independently to handle a stopped clock.
end CuBit.Monotonic;
