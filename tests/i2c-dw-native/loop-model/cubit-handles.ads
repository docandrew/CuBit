with Interfaces;
package CuBit.Handles is
   type Device_Memory is new Natural;
   type Reply is new Interfaces.Unsigned_64;
   package ABI is
      type Kind is (No_Capability, Device_Memory);
   end ABI;
   type Rights is record Read, Write : Boolean := False; end record;
end CuBit.Handles;
