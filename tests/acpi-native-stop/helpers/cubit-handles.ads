pragma Ada_2022;
package CuBit.Handles with Pure is
   type Endpoint is private;
   No_Endpoint, Model_Endpoint : constant Endpoint;
   function Valid (Item : Endpoint) return Boolean;
private
   type Endpoint is record Present : Boolean; end record;
   No_Endpoint : constant Endpoint := (Present => False);
   Model_Endpoint : constant Endpoint := (Present => True);
end CuBit.Handles;
