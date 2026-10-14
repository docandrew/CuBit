with Interfaces; use Interfaces;
--  Hosted tests only: handles from numbers and a look into derivations, so
--  the mocks can stand in for the kernel. Never part of a CuBit program.
package CuBit.Handles.Fixture is
   function Endpoint_Of (Word : Unsigned_64) return Endpoint;
   function Reply_Of (Word : Unsigned_64) return Reply;
   function Word_Of (Item : Endpoint) return Unsigned_64;
   function Word_Of (Item : Reply) return Unsigned_64;
   --  What a derivation copies: its source handle word, rights mask and tag.
   function Source_Of (Copy : Derivation) return Unsigned_64;
   function Rights_Of (Copy : Derivation) return Unsigned_64;
   function Tag_Of (Copy : Derivation) return Unsigned_64;
end CuBit.Handles.Fixture;
