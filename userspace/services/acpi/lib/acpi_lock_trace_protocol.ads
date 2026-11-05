pragma Ada_2022;
with Interfaces; with ACPI_Region_Protocol;
-- Coordinator-reserved diagnostic-only role35 labels. No hardware arguments.
package ACPI_Lock_Trace_Protocol with Pure, SPARK_Mode is
   Request_Label : constant Interfaces.Unsigned_32 := 16#02C9#;
   Reply_Label : constant Interfaces.Unsigned_32 := 16#02CA#;
   Version : constant Interfaces.Unsigned_64 := 1;
   type Outcome is (Accepted, Already_Dumped, Unauthorized, Malformed);
   subtype Packet is ACPI_Region_Protocol.Packet;
   function Request return Packet;
   function Valid_Request (Item : Packet) return Boolean;
   function Reply (Status : Outcome; Records : Natural) return Packet;
   type Reply_Result is record Valid : Boolean := False; Status : Outcome := Malformed; Records : Natural range 0 .. 12 := 0; end record;
   function Decode (Item : Packet) return Reply_Result;
end ACPI_Lock_Trace_Protocol;
