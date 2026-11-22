package CuBit.Handles with Pure is
 type IO_Ports is new Boolean; No_IO_Ports : constant IO_Ports := False;
 type Interrupt is new Boolean; No_Interrupt : constant Interrupt := False;
 type Device_Memory is new Boolean; No_Device_Memory : constant Device_Memory := False;
end CuBit.Handles;