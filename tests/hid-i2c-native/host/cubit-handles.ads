with Interfaces;
-- Hosted opaque handle model. Native semantic checks use the actual runtime.
package CuBit.Handles with Pure is
   type Endpoint is private;
   No_Endpoint : constant Endpoint;
private
   type Endpoint is new Interfaces.Unsigned_64;
   No_Endpoint : constant Endpoint := 0;
end CuBit.Handles;
