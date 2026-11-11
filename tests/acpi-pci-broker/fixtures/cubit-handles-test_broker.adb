pragma Ada_2022;
package body CuBit.Handles.Test_Broker is
 function Memory return Device_Memory is ((Word=>16#100000001#));
 function Snapshot return Firmware_Snapshot is ((Word=>16#100000002#));
 function Kept (Item : Derivation) return Interfaces.Unsigned_64 is (Item.Words(ABI.Rights));
end CuBit.Handles.Test_Broker;
