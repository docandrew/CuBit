-- Hosted test model only; never a native runtime/provider implementation.
with Interfaces; package CuBit.Handles is
 package ABI is type Kind is (No_Capability,Endpoint); end ABI;
 type Endpoint is new Interfaces.Unsigned_64;
 No_Endpoint : constant Endpoint := 0;
 type Rights is record Read : Boolean := True; end record;
end CuBit.Handles;