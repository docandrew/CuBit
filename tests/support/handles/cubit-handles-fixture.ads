------------------------------------------------------------------------------
--  Hosted tests only (CAP-002): handles from numbers, so mocks can stand in
--  for the kernel. Never part of a CuBit program. Tests list this file, the
--  real cubit-handles.ads and shared/capabilities/capability_abi.ad[sb].
------------------------------------------------------------------------------

package CuBit.Handles.Fixture is
   --  A handle word: generation 1, the given slot.
   function Endpoint_Of (Slot : Unsigned_32) return Endpoint;
   function Reply_Of (Slot : Unsigned_32) return Reply;
   function Device_Memory_Of (Slot : Unsigned_32) return Device_Memory;
   function Word_Of (Item : Endpoint) return Unsigned_64;
   function Word_Of (Item : Reply) return Unsigned_64;
end CuBit.Handles.Fixture;
