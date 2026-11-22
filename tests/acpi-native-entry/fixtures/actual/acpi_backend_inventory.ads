pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Catalog;
with ACPI_Inventory_Protocol;
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
package ACPI_Backend_Inventory is
   -- The receive loop serializes this read with catalog mutation. Stamp comes
   -- from authenticated kernel receive metadata, never from Item.Data.
   procedure Dispatch
     (State : Catalog.Context; Stamp : Interfaces.Unsigned_64;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet);
end ACPI_Backend_Inventory;
