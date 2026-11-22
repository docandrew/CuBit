with Interfaces;
package GPIO_Bootstrap_Payload is
   type Request is record Epoch : Interfaces.Unsigned_64 := 0; end record;
end GPIO_Bootstrap_Payload;
