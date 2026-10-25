pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Firmware_Snapshots is
   package K renames CuBit.Handles.Kernel;

   function Call
     (Authority : Firmware_Snapshot;
      Words     : Firmware_Snapshot_ABI.Request_Words;
      Reply     : System.Address) return Unsigned_64 is
     (K.Call (K.Firmware_Snapshot_Call, K.Word (Authority), Words.Op, Words.Table,
              Words.Offset, Words.Destination, Words.Capacity,
              Unsigned_64 (System.Storage_Elements.To_Integer (Reply))));
end CuBit.Handles.Firmware_Snapshots;
