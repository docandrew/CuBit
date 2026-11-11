pragma Ada_2022;
package CuBit.Handles.Test_Broker is
 function Memory return Device_Memory;
 function Snapshot return Firmware_Snapshot;
 function Kept (Item : Derivation) return Interfaces.Unsigned_64;
end CuBit.Handles.Test_Broker;
