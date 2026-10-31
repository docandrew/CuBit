package CuBit.Handles is
   type Device_Memory is new Natural;
   package ABI is
      type Kind is (No_Capability, Device_Memory);
   end ABI;
   type Rights is record
      Read, Write : Boolean := False;
   end record;
end CuBit.Handles;
