with ACPI_SCI_Registers;
with ACPI_Requests;
package ACPI_SCI_Events is
   EC_Source : constant ACPI_SCI_Registers.GPE_Number := 16#6E#;
   General_Source : constant ACPI_SCI_Registers.GPE_Number := 9;
   General_Available, General_Enabled : Boolean := False;
   General_Masks, General_Clears, General_Enables : Natural := 0;
   type Server_Reference is access all ACPI_Requests.State;
   procedure Observe_Server(Server : Server_Reference);
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
   function GPE_Available(GPE:ACPI_SCI_Registers.GPE_Number) return Boolean is (GPE=EC_Source or else (General_Available and then GPE=General_Source));
   procedure Mask_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
   procedure Clear_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
   procedure Enable_GPE(GPE:ACPI_SCI_Registers.GPE_Number;Result:out IO_Outcome);
end ACPI_SCI_Events;
