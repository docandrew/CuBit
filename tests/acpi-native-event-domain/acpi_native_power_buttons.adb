package body ACPI_Native_Power_Buttons is
   procedure Configure_Fixed (Available : Boolean) is
   begin Configured := Available; end Configure_Fixed;
   procedure Fixed_Press is
   begin Offered_Presses := Offered_Presses + 1; end Fixed_Press;
end ACPI_Native_Power_Buttons;
