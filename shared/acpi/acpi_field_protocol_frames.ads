pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
package ACPI_Field_Protocol_Frames with Pure, SPARK_Mode is
   subtype Packet is ACPI_Region_Protocol.Packet;
   function Owned_Label (Label : Interfaces.Unsigned_32) return Boolean;
   function Request_Valid (Item : Packet) return Boolean;
   -- Data widths live in the caller's selected descriptor. This checks the
   -- complete frame/association; the data client checks its actual width.
   function Reply_Valid (Request, Reply : Packet) return Boolean;
end ACPI_Field_Protocol_Frames;
