--  Hosted stand-in: an endpoint is a number, valid when nonzero.
package CuBit.Handles is
   type Endpoint is record
      Id : Natural := 0;
   end record;
   No_Endpoint : constant Endpoint := (Id => 0);
   function Valid (Item : Endpoint) return Boolean is (Item.Id /= 0);
end CuBit.Handles;
