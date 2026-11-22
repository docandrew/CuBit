pragma Ada_2022;
package CuBit.Handles with Pure is
 type Endpoint is private;
 No_Endpoint : constant Endpoint;
 Test_Endpoint : constant Endpoint; -- hosted fixture only, not a production constructor
 function Valid(Item : Endpoint) return Boolean;
private
 type Endpoint is new Boolean;
 No_Endpoint : constant Endpoint := False;
 Test_Endpoint : constant Endpoint := True;
 function Valid(Item : Endpoint) return Boolean is (Boolean(Item));
end CuBit.Handles;
