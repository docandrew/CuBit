pragma Ada_2022;
with ACPI_SCI_Registers;
generic
-- Local read-only audit of this exact Fields/catalog/EC instance. It neither
-- lends authority nor establishes that an arbitrary MMIO device is harmless.
package ACPI_Local_Hardware.Pooled_Fields.SCI_Control is
   function Exclusive (Layout : ACPI_SCI_Registers.Layout) return Boolean;
end ACPI_Local_Hardware.Pooled_Fields.SCI_Control;
