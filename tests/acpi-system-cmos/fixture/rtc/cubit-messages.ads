with Interfaces; use Interfaces; with CuBit.Handles;
package CuBit.Messages is
 function portInp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64;
 function portOutp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_8) return Unsigned_64;
 procedure debugPrint (str : String);
end CuBit.Messages;
