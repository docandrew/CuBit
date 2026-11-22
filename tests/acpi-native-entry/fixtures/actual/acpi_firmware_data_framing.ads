pragma Ada_2022;
with Interfaces;
with Capability_ABI;
with ACPI_Firmware_Data_Frames;
package ACPI_Firmware_Data_Framing with Pure, SPARK_Mode is
   package F renames ACPI_Firmware_Data_Frames;
   package IPC renames F.IPC;
   use type Interfaces.Unsigned_8;
   function Acceptance_Option return Interfaces.Unsigned_8 is
      (Capability_ABI.Flag_Byte (0,(Accepts_Capability => True,others => False)));
   -- Call_Accepting permits a capability in the REPLY. It does not transfer
   -- one in the request. Remove only this IPC option. All protocol, offered,
   -- transferred and refused flags stay visible to strict protocol decoding.
   function Normalize_Request (Item : IPC.Packet) return IPC.Packet is
      ((Item with delta Flags => (if Item.Flags = Acceptance_Option then 0 else Item.Flags)));
end ACPI_Firmware_Data_Framing;
