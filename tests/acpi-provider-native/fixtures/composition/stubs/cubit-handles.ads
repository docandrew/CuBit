pragma Ada_2022;
package CuBit.Handles with Pure is
 type Endpoint is private;
 No_Endpoint, Test_Endpoint, Provider_Endpoint : constant Endpoint;
 function Valid(Item : Endpoint) return Boolean;
 function Is_Service(Item : Endpoint) return Boolean;
 function Is_Provider(Item : Endpoint) return Boolean;
private
 type Endpoint is (None, Service, Provider);
 No_Endpoint : constant Endpoint := None;
 Test_Endpoint : constant Endpoint := Service;
 Provider_Endpoint : constant Endpoint := Provider;
 function Valid(Item : Endpoint) return Boolean is (Item/=None);
 function Is_Service(Item : Endpoint) return Boolean is (Item=Service);
 function Is_Provider(Item : Endpoint) return Boolean is (Item=Provider);
end CuBit.Handles;
