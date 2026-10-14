with Interfaces;
--  Hosted tests only: handles from numbers. Never part of a CuBit program.
package CuBit.Handles.Fixture is
   function Endpoint_Of (Word : Interfaces.Unsigned_64) return Endpoint;
   function Word_Of (Item : Endpoint) return Interfaces.Unsigned_64;
end CuBit.Handles.Fixture;
