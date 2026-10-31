pragma Ada_2022;
with CuBit.Messages;
with ACPI_Backend_Catalog;
with ACPI_Backend_Inventory;
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
package ACPI_Backend_Catalog_Endpoint is
   -- Kernel-received Message only; caller-supplied data never supplies Stamp.
   -- Serialized by the single backend receive loop. Reply metadata has no
   -- authority tag or outgoing capabilities.
   procedure Dispatch
     (State : in out Catalog.Context;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
private
   package Inventory is new ACPI_Backend_Inventory (Catalog);
end ACPI_Backend_Catalog_Endpoint;
