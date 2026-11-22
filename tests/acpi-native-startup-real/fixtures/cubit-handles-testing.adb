package body CuBit.Handles.Testing is
 function Ports return IO_Ports is ((Word=>Port_Token));
 function Snapshot return Firmware_Snapshot is ((Word=>Snapshot_Token));
end CuBit.Handles.Testing;
