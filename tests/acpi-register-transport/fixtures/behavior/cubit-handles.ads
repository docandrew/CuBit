pragma Ada_2022;
with Interfaces;
package CuBit.Handles with Pure is
   type Endpoint is private;
   No_Endpoint, Test_Endpoint, Other_Endpoint : constant Endpoint;
private
   type Endpoint is new Interfaces.Unsigned_64;
   No_Endpoint : constant Endpoint := 0;
   Test_Endpoint : constant Endpoint := 16#FEDC_BA98_7654_3210#;
   Other_Endpoint : constant Endpoint := 16#ABCD_1234_5678_9012#;
end CuBit.Handles;
