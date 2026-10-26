with Interfaces;
with CuBit.Messages;
package CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64 is (CuBit.Messages.Now);
end CuBit.Monotonic;
