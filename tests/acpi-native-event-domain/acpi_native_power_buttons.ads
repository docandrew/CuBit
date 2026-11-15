package ACPI_Native_Power_Buttons is
   Configured : Boolean := False;
   Offered_Presses : Natural := 0;
   procedure Configure_Fixed (Available : Boolean);
   procedure Fixed_Press;
end ACPI_Native_Power_Buttons;
