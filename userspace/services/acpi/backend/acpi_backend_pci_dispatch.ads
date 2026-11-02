pragma Ada_2022;
with Interfaces;
with ACPI_Backend_PCI_Reads;
with ACPI_PCI_Protocol;
generic
   with package Reader is new ACPI_Backend_PCI_Reads (<>);
package ACPI_Backend_PCI_Dispatch with SPARK_Mode is
   -- Scope is authenticated kernel receive metadata, never a request word.
   procedure Dispatch
     (State : in out Reader.Context; Scope : Interfaces.Unsigned_64;
      Item : ACPI_PCI_Protocol.Packet; Result : out ACPI_PCI_Protocol.Packet);
end ACPI_Backend_PCI_Dispatch;
