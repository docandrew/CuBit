with ACPI_Service;with AML_Objects;
package Composition_State is
 Service,Foreign:aliased ACPI_Service.State(1,4096,4096,1,
 Value_Object_Capacity=>AML_Objects.Max_Objects,Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
 function Results_Clear return Boolean is(ACPI_Service.Retained_Results(Service)=0);
end Composition_State;
