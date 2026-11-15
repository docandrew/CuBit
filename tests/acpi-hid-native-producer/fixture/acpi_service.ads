with Interfaces;
package ACPI_Service is
   type Node_Descriptor is new Interfaces.Unsigned_64;
   package Values is No_Node : constant Node_Descriptor := 0; end Values;
end ACPI_Service;
