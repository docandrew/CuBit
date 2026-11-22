-- Hosted capability tokens only; no kernel handle is created by this package.
package CuBit.Handles.Testing with Pure is
   Port_First : constant Unsigned_16 := 16#0400#;
   Port_Count : constant Unsigned_32 := 64;
   Port_Token : constant Unsigned_64 := 16#1001#;
   Snapshot_Token : constant Unsigned_64 := 16#1002#;
   function Ports return IO_Ports;
   function Snapshot return Firmware_Snapshot;
end CuBit.Handles.Testing;
