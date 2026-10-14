with Interfaces; use Interfaces;
--  Hosted tests only: handles from numbers, so fixtures can stand in for
--  the kernel. Never part of a CuBit program.
package CuBit.Handles.Fixture is
   function Endpoint_Of (Word : Unsigned_64) return Endpoint;
   function Word_Of (Item : Endpoint) return Unsigned_64;
end CuBit.Handles.Fixture;
