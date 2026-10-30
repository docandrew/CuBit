pragma Ada_2022;
with Interfaces;

--  The manifest request for the firmware snapshot reader
--  (Request.Firmware_Snapshot, Firmware_Snapshot_ABI): a request, never a
--  grant. procmgr installs a read-only copy only when the program's startup
--  entry approves it, (firmware-snapshot approve-declared).
package CuBit.Firmware_Snapshot_Authority with Pure, SPARK_Mode is
   Manifest_Request : constant Interfaces.Unsigned_8 := 12;
end CuBit.Firmware_Snapshot_Authority;
