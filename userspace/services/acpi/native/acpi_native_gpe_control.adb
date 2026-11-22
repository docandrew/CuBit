pragma Ada_2022;
with ACPI_One_Service;
with ACPI_Local_Hardware.Pooled_Fields.SCI_Control;
with ACPI_SCI_Events;
with ACPI_SCI_Registers;
package body ACPI_Native_GPE_Control is
   package Fields_Control is new ACPI_One_Service.Root.Fields.SCI_Control;
   function Exclusive return Boolean is
      Layout : constant ACPI_SCI_Registers.Layout_Result := ACPI_SCI_Events.Control_Layout;
   begin
      return Layout.Valid and then Fields_Control.Exclusive (Layout.Value);
   end Exclusive;
end ACPI_Native_GPE_Control;
