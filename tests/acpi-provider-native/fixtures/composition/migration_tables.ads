with Firmware_Tables;
package Migration_Tables is
   function Make (Payload : Firmware_Tables.Bytes; Secondary : Boolean := False)
     return Firmware_Tables.Bytes;
end Migration_Tables;
