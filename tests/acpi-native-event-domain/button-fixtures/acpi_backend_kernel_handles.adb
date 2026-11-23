with ACPI_One_Service;
package body ACPI_Backend_Kernel_Handles is
 procedure Read_Clock(Now:out ACPI_Backend_Binding_Types.Tick;Available:out Boolean) is
 begin
  Now:=ACPI_Backend_Binding_Types.Tick(ACPI_One_Service.Now);Available:=True;
  ACPI_One_Service.Delay_In_Sample_HID:=ACPI_One_Service.Button_Test_Delay;
  ACPI_One_Service.Fail_Next_HID:=ACPI_One_Service.Button_Test_Unsafe;
 end Read_Clock;
end ACPI_Backend_Kernel_Handles;
