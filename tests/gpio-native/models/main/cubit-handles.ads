-- Hosted symbolic handles only. No values are native capability words.
package CuBit.Handles is
   type Device_Memory is new Natural;
   type Process is new Natural;
   type Notification is new Natural;
   type Endpoint is new Natural;
   type Reply is new Natural;
   No_Device_Memory : constant Device_Memory := 0;
   No_Process : constant Process := 0;
   No_Notification : constant Notification := 0;
   No_Endpoint : constant Endpoint := 0;
   function Valid (Item : Device_Memory) return Boolean is (Item /= No_Device_Memory);
   package ABI is
      type Kind is (No_Capability, Process, Device_Memory);
   end ABI;
end CuBit.Handles;
