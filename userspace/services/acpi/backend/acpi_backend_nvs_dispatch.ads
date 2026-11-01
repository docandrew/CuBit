pragma Ada_2022;
with Interfaces;
with ACPI_Backend_NVS_Records;
with ACPI_NVS_Protocol;
generic
   with package Records is new ACPI_Backend_NVS_Records (<>);
package ACPI_Backend_NVS_Dispatch with SPARK_Mode is
   -- Scope MUST come from authenticated kernel receive metadata, not Data.
   -- This dispatcher never installs records, grants authority or retries.
   procedure Dispatch
     (State : in out Records.Context; Scope : Interfaces.Unsigned_64;
      Item : ACPI_NVS_Protocol.Packet; Result : out ACPI_NVS_Protocol.Packet);
end ACPI_Backend_NVS_Dispatch;
