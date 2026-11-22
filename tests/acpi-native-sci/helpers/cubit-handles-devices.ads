with Interfaces;package CuBit.Handles.Devices is
 function In_16(Ports:IO_Ports;Port:Interfaces.Unsigned_16) return Interfaces.Unsigned_64;
 function In_8(Ports:IO_Ports;Port:Interfaces.Unsigned_16) return Interfaces.Unsigned_64;
 function Out_16(Ports:IO_Ports;Port:Interfaces.Unsigned_16;Value:Interfaces.Unsigned_16) return Boolean;
 function Out_8(Ports:IO_Ports;Port:Interfaces.Unsigned_16;Value:Interfaces.Unsigned_8) return Boolean;
end CuBit.Handles.Devices;