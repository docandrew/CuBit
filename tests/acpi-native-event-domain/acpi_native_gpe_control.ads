pragma Ada_2022;
package ACPI_Native_GPE_Control is
   Exclusive_State : Boolean := True;
   function Exclusive return Boolean is (Exclusive_State);
end ACPI_Native_GPE_Control;
