with CuBit.Handles;
package Startup is
   type Optional is record Present : Boolean := False; Value : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint; end record;
   Controller, Registration : Optional;
   Ready, Registers : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   package Channels is
      Com_Cubit_Os_I2C_Transfer_Per_Client : constant := 4;
      Com_Cubit_Os_I2C_Transfer_Total : constant := 64;
      Com_Cubit_Os_I2C_Transfer_Class : constant := 1;
   end Channels;
end Startup;
