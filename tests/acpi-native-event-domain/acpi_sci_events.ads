with ACPI_SCI_Registers;
package ACPI_SCI_Events is
   type IO_Outcome is (Completed,Refused);
   type Disposition is (Handled,Unbound);
   Masks,Clears,Enables:Natural:=0;
   Mask_Refused,Clear_Refused,Enable_Refused:Boolean:=False;
   Enabled:Boolean:=False;
   Latched:Boolean:=True;
   Armed_State:Boolean:=True;
   -- No FADT fixed-power source in this battery-focused fixture.
   function Fixed_Power_Button_Available return Boolean is (False);
   function Is_Armed return Boolean is (Armed_State);
   procedure Mask_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
   procedure Clear_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
   procedure Enable_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
end ACPI_SCI_Events;
